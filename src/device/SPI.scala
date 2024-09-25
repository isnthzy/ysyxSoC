package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val node = APBSlaveNode(Seq(APBSlavePortParameters(
    Seq(APBSlaveParameters(
      address       = address,
      executable    = true,
      supportsRead  = true,
      supportsWrite = true)),
    beatBytes  = 4)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val spi_bundle = IO(new SPIIO)
    def FLASH_BASE = "h03000000".U
    def SPI_BASE   = "h10001000".U
    def SPI_RX     = "h0".U
    def SPI_TX     = "h0".U
    def SPI_CTRL   = "h10".U
    def SPI_DIVIDER= "h14".U
    def SPI_SS     = "h18".U
    val spi_master_divd = "h01".U
    val mspi = Module(new spi_top_apb)
    mspi.io.clock := clock
    mspi.io.reset := reset
    spi_bundle <> mspi.io.spi

    val (xip_idle     :: xip_set_divider :: xip_set_ss    :: xip_set_ctrl 
      :: xip_wait_bsy :: xip_close_ss    :: xip_resp_data :: Nil )=Enum(7)
    val xipState = RegInit(xip_idle)


    val flashAccess=(in.paddr>="h30000000".U 
                   &&in.paddr<="h3fffffff".U)
    val mspi_penable = RegInit(false.B)
    mspi.io.in.psel   :=false.B
    mspi.io.in.penable:=false.B
    mspi.io.in.pwrite :=false.B
    mspi.io.in.paddr  :=0.U
    mspi.io.in.pprot:=1.U
    mspi.io.in.pwdata:=0.U
    mspi.io.in.pstrb:=0.U
    in.pready:=false.B
    mspi.io.in.penable:=mspi_penable
    switch(xipState) {
      is(xip_idle) {
        when(flashAccess&&in.penable){
          mspi.io.in.psel  :=true.B
          mspi.io.in.paddr :=SPI_BASE + SPI_TX + 4.U //NOTE: 存入数据
          mspi.io.in.pwdata:=Cat("h03".U(8.W),in.paddr(23,0))
          mspi.io.in.pstrb :="hf".U
          mspi.io.in.pwrite:=true.B
          when(mspi.io.in.pready){
            xipState:=xip_set_divider
            mspi_penable:= ~mspi.io.in.pready
          }.otherwise{
            mspi_penable:= mspi.io.in.psel
          }
        }.otherwise{ //不是flash直接转发给spi
          mspi.io.in <> in
        }
      }
      is(xip_set_divider){
        mspi.io.in.psel  :=true.B
        mspi.io.in.paddr :=SPI_BASE + SPI_DIVIDER
        mspi.io.in.pwdata:=spi_master_divd
        mspi.io.in.pstrb :="b0011".U
        mspi.io.in.pwrite:=true.B
        when(mspi.io.in.pready){
          xipState:=xip_set_ss
          mspi_penable:= ~mspi.io.in.pready
        }.otherwise{
          mspi_penable:= mspi.io.in.psel
        }
      }
      is(xip_set_ss){
        mspi.io.in.psel  :=true.B
        mspi.io.in.paddr :=SPI_BASE + SPI_SS
        mspi.io.in.pwdata:="b00000001".U
        mspi.io.in.pstrb :="b0001".U
        mspi.io.in.pwrite:=true.B
        when(mspi.io.in.pready){
          xipState:=xip_set_ctrl
          mspi_penable:= ~mspi.io.in.pready
        }.otherwise{
          mspi_penable:= mspi.io.in.psel
        }
      }
      is(xip_set_ctrl){
        mspi.io.in.psel  :=true.B
        mspi.io.in.paddr :=SPI_BASE + SPI_CTRL
        mspi.io.in.pwdata:="b1000101000000".U
        //设置charlen和gobsy
        mspi.io.in.pstrb :="b0011".U
        mspi.io.in.pwrite:=true.B
        when(mspi.io.in.pready){
          xipState:=xip_wait_bsy
          mspi_penable:= ~mspi.io.in.pready
        }.otherwise{
          mspi_penable:= mspi.io.in.psel
        }
      }
      is(xip_wait_bsy){
        mspi.io.in.psel  :=false.B
        mspi.io.in.paddr :=SPI_BASE + SPI_CTRL
        
        mspi.io.in.pstrb :=0.U
        mspi.io.in.pwrite:=false.B
        mspi_penable:=false.B
        when(mspi.io.spi_irq_out){
          xipState:=xip_close_ss
        }
        // mspi.io.in.psel  :=true.B
        // mspi.io.in.paddr :=SPI_BASE + SPI_CTRL
        
        // mspi.io.in.pstrb :=0.U
        // mspi.io.in.pwrite:=false.B
        // //进入轮讯，gobsy为0时切下一个状态机
        // when(mspi.io.in.pready){
        //   when(mspi.io.in.prdata(8)===0.U){
        //     xipState:=xip_close_ss
        //   }.otherwise{
        //     xipState:=xip_wait_bsy
        //   }
        //   mspi_penable:= ~mspi.io.in.pready
        // }.otherwise{
        //   mspi_penable:= mspi.io.in.psel
        // }
      }
      is(xip_close_ss){
        mspi.io.in.psel  :=true.B
        mspi.io.in.paddr :=SPI_BASE + SPI_SS
        mspi.io.in.pwdata:="b00000000".U
        mspi.io.in.pstrb :="b0001".U
        mspi.io.in.pwrite:=true.B
        when(mspi.io.in.pready){
          xipState:=xip_resp_data
          mspi_penable:= ~mspi.io.in.pready
        }.otherwise{
          mspi_penable:= mspi.io.in.psel
        }
      }
      is(xip_resp_data){
        mspi.io.in.psel  :=true.B
        mspi.io.in.paddr :=SPI_BASE + SPI_RX
        mspi.io.in.pstrb :=0.U
        mspi.io.in.pwrite:=false.B
        when(mspi.io.in.pready){
          xipState:=xip_idle
          in.pready:=true.B
          in.prdata:=Cat(mspi.io.in.prdata(7 ,0 ),
                         mspi.io.in.prdata(15,8 ),
                         mspi.io.in.prdata(23,16),
                         mspi.io.in.prdata(31,24))
          mspi_penable:= ~mspi.io.in.pready
        }.otherwise{
          mspi_penable:= mspi.io.in.psel
        }
      }
    }

  }
}
