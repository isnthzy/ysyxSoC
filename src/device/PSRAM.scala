package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import dataclass.data

class QSPIIO extends Bundle {
  val sck = Output(Bool())
  val ce_n = Output(Bool())
  val dio = Analog(4.W)
}

class psram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val qspi = new QSPIIO
  })
}

class psram extends BlackBox {
  val io = IO(Flipped(new QSPIIO))
}

class psramRAM extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val wr   = Input(Bool())
    val addr = Input(UInt(32.W))
    val din  = Input(UInt(32.W))
    val wlen = Input(UInt(8.W))
    val dout = Output(UInt(32.W))
  })
  setInline("psramRAM.v",
    """import "DPI-C" function void psram_read(input int addr, output int data);
      |import "DPI-C" function void psram_write(input int addr, input int data, input byte wlen);
      |module psramRAM(
      |    input              valid,
      |    input              wr,
      |    input      [31:0]  addr,
      |    input      [31:0]  din,
      |    input      [ 7:0]  wlen,
      |    output reg [31:0]  dout
      |);
      |always@(posedge valid) begin
      |  if(wr) begin
      |    psram_write(addr, din, wlen);
      |  end
      |  else begin
      |    psram_read(addr, dout);
      |  end
      |end
      |endmodule
    """.stripMargin)
}

class psramWriteBuffBundle extends Bundle {
  val isWr  = Bool()
  val wdata = UInt(32.W)
  val waddr = UInt(24.W)
  val wlen  = UInt(4.W)
}

class psramChisel extends RawModule {
  val io = IO(Flipped(new QSPIIO))
  val dinValue = WireDefault(0.U(4.W))
  val dinEn    = WireDefault(false.B)
  val di = TriStateInBuf(io.dio, dinValue, dinEn) // change this if you need

  // withReset(io.ce_n.asAsyncReset) {
    val cmd_t :: addr_t :: wdata_t :: rdata_t :: Nil = Enum(4)
    val state   = withClockAndReset(( io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(cmd_t))
    val counterCmd = withClockAndReset(( io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(0.U(5.W)))
    val counterAddr= withClockAndReset((~io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(0.U(5.W)))
    val counterRData= withClockAndReset(( io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(0.U(5.W)))
    val counterWData= withClockAndReset((~io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(0.U(5.W)))
    val cmd     = withClockAndReset(( io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(0.U(8.W)))
    val addrReg = withClockAndReset((~io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(0.U(24.W)))
    val dataReg = withClockAndReset(( io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(0.U(32.W))) //wdata
    val byteDataHigh = withClockAndReset(( io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(0.U(4.W))) //wdata
    val rdataReg  = withClockAndReset(( io.sck).asClock, io.ce_n.asAsyncReset)(RegInit(0.U(32.W)))
    val wdataBuff = withClock((~io.sck).asClock)(Reg(new psramWriteBuffBundle()))
    val wen = io.ce_n && wdataBuff.isWr
    val ren = (state === rdata_t) && (counterAddr === 24.U) //读数据
    val psramRAM = Module(new psramRAM)
    psramRAM.io.valid := wen || ren
    psramRAM.io.wr    := wen
    psramRAM.io.addr  := Mux(wen,wdataBuff.waddr,addrReg)
    psramRAM.io.din   := wdataBuff.wdata
    psramRAM.io.wlen  := wdataBuff.wlen
    withClock(io.sck.asClock) {
      switch(state){
        is(cmd_t){
          counterCmd := counterCmd + 1.U
          cmd := Cat(cmd(6,0), di(0))
        }
        is(wdata_t){
          counterWData := counterWData + 1.U
          byteDataHigh := di(3,0)
          val byteWdata = dontTouch(Cat(byteDataHigh, di(3,0)))
          val shiftAmount = (counterWData - 2.U) * 4.U
          when(counterWData(0) === 0.U){
            dataReg := (dataReg & ~(31.U << shiftAmount)) | (byteWdata << shiftAmount)
          }
          //感谢chatgpt优化简写代码
        }
      }
      
    }
    withClock((~io.sck).asClock) {
      wdataBuff.isWr  := cmd === 0x38.U
      wdataBuff.wdata := dataReg
      wdataBuff.waddr := addrReg(23,0)
      wdataBuff.wlen  := counterWData >> 1
      switch(state){
        is(cmd_t){
          when(counterCmd === 7.U){
            state := addr_t
          }
        }
        is(addr_t){
          counterAddr := counterAddr + 4.U
          addrReg := Cat(addrReg(20,0), di(3,0))
          when(cmd === 0xeb.U){
            when(counterAddr === 24.U){
              counterAddr := 0.U
              state := rdata_t
            }
          }.elsewhen(cmd === 0x38.U){
            when(counterAddr === 24.U){
              counterAddr := 0.U
              state := wdata_t
            }
          }.otherwise{
            //assert(true.B,"Assertion failed: Unsupported command `%xh`, only support `38h` and `EBh` command\n", cmd)
          }
        }
        is(rdata_t){
          counterRData := counterRData + 1.U
          when(counterRData <= 6.U){
            rdataReg := psramRAM.io.dout
          }
          when(counterRData > 6.U){ //协议要求读数据时，需要等待6个周期
            when(counterRData(0) === 0.U){
              rdataReg := (rdataReg >> 8)
              dinValue := rdataReg(3,0)
            }.elsewhen(counterRData(0) === 1.U){
              dinValue := rdataReg(7,4)
            }
            dinEn    := true.B
          }
        }
      }
    }
  // }
}

class APBPSRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val qspi_bundle = IO(new QSPIIO)

    val mpsram = Module(new psram_top_apb)
    mpsram.io.clock := clock
    mpsram.io.reset := reset
    mpsram.io.in <> in
    qspi_bundle <> mpsram.io.qspi
  }
}
