package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.diplomacy.BufferParams.default

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val gpio = new GPIOIO
}

class gpio_top_apb extends BlackBox {
  val io = IO(new GPIOCtrlIO)
}

class bcd7seg extends Module{
  val seg = IO(new Bundle {
    val in = Input(UInt(4.W))
    val out= Output(UInt(7.W))
  })
  seg.out := MuxLookup(seg.in, 0.U)(Seq(
    0.U -> "b0000001".U, 1.U -> "b1001111".U, 
    2.U -> "b0010010".U, 3.U -> "b0000110".U,
    4.U -> "b1001100".U, 5.U -> "b0100100".U,
    6.U -> "b0100000".U, 7.U -> "b0001111".U, 
    8.U -> "b0000000".U, 9.U -> "b0000100".U,
    10.U -> "b0001000".U, 11.U -> "b1100000".U,
    12.U -> "b1110010".U, 13.U -> "b1000010".U,
    14.U -> "b0110000".U, 15.U -> "b0111000".U
  ))
}

class gpioChisel extends Module {
  val io = IO(new GPIOCtrlIO)
  //Output seg 驱动数码管
  //Output out 驱动LED
  //Input  in  读入拨码开关
  def GPIO_LED_ADDR = 0x0
  def GPIO_SW_ADDR  = 0x4
  def GPIO_SEG_ADDR = 0x8
  val ledReg = RegInit(VecInit(Seq.fill(2)(0.U(8.W))))
  val segReg = RegInit(VecInit(Seq.fill(4)(0.U(8.W))))
  val bcd7seg = Array.fill(8)(Module(new bcd7seg()).seg)
  val segOut = Wire(Vec(8, UInt(8.W)))
  for(i <- 0 until 8) {
    bcd7seg(i).in := segReg.asTypeOf(Vec(8,UInt(4.W)))(i)
    segOut(i) := Cat(bcd7seg(i).out,0.U)
  }
  val selAddr = Cat(io.in.paddr(3,2),0.U(2.W))
  io.gpio.seg := segOut
  io.gpio.out := ledReg.asUInt(15,0)

  val writeData = WireDefault(0.U(32.W))
  val writeMaskLen  = WireDefault(VecInit(Seq.fill(4)(false.B)))
  switch(io.in.pstrb) {
    is("b0001".U) { writeData := io.in.pwdata(7, 0);   writeMaskLen := "b0001".U.asTypeOf(writeMaskLen) }
    is("b0010".U) { writeData := io.in.pwdata(15, 8);  writeMaskLen := "b0001".U.asTypeOf(writeMaskLen) }
    is("b0100".U) { writeData := io.in.pwdata(23, 16); writeMaskLen := "b0001".U.asTypeOf(writeMaskLen) }
    is("b1000".U) { writeData := io.in.pwdata(31, 24); writeMaskLen := "b0001".U.asTypeOf(writeMaskLen) }
    is("b0011".U) { writeData := io.in.pwdata(15, 0);  writeMaskLen := "b0011".U.asTypeOf(writeMaskLen) }
    is("b1100".U) { writeData := io.in.pwdata(31, 16); writeMaskLen := "b0011".U.asTypeOf(writeMaskLen) }
    is("b1111".U) { writeData := io.in.pwdata; writeMaskLen := "b1111".U.asTypeOf(writeMaskLen) }
  }

  io.in.pready := false.B
  io.in.pslverr:= 0.U
  io.in.prdata := 0.U
  val (s_idle ::  s_read :: s_write :: Nil )=Enum(3)
  val state = RegInit(s_idle)
  switch(state) {
    is(s_idle) {
      when(io.in.penable) {
        when(io.in.pwrite) {
          state := s_write
        }.otherwise {
          state := s_read
        }
      }
    }
    is(s_read) {
      when(io.in.penable) {
        state := s_idle
        io.in.pready := true.B
      }
    }
    is(s_write) {
      when(io.in.penable) {
        state := s_idle
        io.in.pready := true.B
      }
    }
  }
  
  when(state === s_write) {
    when(selAddr === GPIO_LED_ADDR.U){
      for(i <- 0 until 2){
        when(io.in.paddr(1,0) === i.U){
          for(j <- 0 until (2 - i)){
            when(writeMaskLen(j)){
              ledReg(j + i) := writeData( 7 + j * 8, j * 8)
            }
          }
        }
      }
    }.elsewhen(selAddr === GPIO_SEG_ADDR.U){
      for(i <- 0 until 4){
        when(io.in.paddr(1,0) === i.U){
          for(j <- 0 until (4 - i)){
            when(writeMaskLen(j)){
              segReg(j + i) := writeData(7 + j * 8, j * 8)
            }
          }
        }
      }
    }
  }
  when(state === s_read) {
    when(selAddr === GPIO_SW_ADDR.U){
      io.in.prdata := io.gpio.in
    }
  }


}

class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val gpio_bundle = IO(new GPIOIO)

    val mgpio = Module(new gpioChisel)
    mgpio.io.clock := clock
    mgpio.io.reset := reset
    mgpio.io.in <> in
    gpio_bundle <> mgpio.io.gpio
  }
}
