package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class PS2IO extends Bundle {
  val clk = Input(Bool())
  val data = Input(Bool())
}

class PS2CtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val ps2 = new PS2IO
}

class PS2KeyBoard extends Module {
  val io = IO(new Bundle {
    val ps2 = new PS2IO
    val ready = Output(Bool())
    val nextdata_n = Input(Bool())
    val outKey  = Output(UInt(32.W))
  })

  val ps2Ready = RegInit(false.B)
  val data = Reg(UInt(8.W))
  val buffer = RegInit(VecInit(Seq.fill(10)(false.B)))
  val count  = RegInit(0.U(4.W))
  val ps2_clk_sync = Reg(UInt(3.W))
  // val empty = RegInit(true.B)
  val w_ptr = RegInit(0.U(4.W))
  val r_ptr = RegInit(0.U(4.W))
  val dataFifo  = Reg(Vec(16,UInt(8.W)))
  val brokFifo  = Reg(Vec(16,Bool()))
  val extendFifo = Reg(Vec(16,Bool()))

  io.ready := ps2Ready
  ps2_clk_sync := Cat(ps2_clk_sync(1, 0), io.ps2.clk)
  val sampling = ps2_clk_sync(2) & !ps2_clk_sync(1)

  when(ps2Ready){
    when(io.nextdata_n === false.B){
      r_ptr := r_ptr + 1.U
      brokFifo(r_ptr) := false.B
      extendFifo(r_ptr) := false.B
      when(r_ptr + 1.U === w_ptr){
        ps2Ready := false.B
      }
    }
  }
  when(sampling){
    when(count === 10.U){
      count := 0.U
      when((buffer(0) === 0.U) &&
           (io.ps2.data)     &&
           (buffer.asUInt(9,1).xorR)){
        
        when(buffer.asUInt(8,1) === 0xf0.U){
          brokFifo(w_ptr) := true.B
        }.elsewhen(buffer.asUInt(8,1) === 0xe0.U){
          extendFifo(w_ptr) := true.B
        }.otherwise{
          w_ptr := w_ptr + 1.U
          ps2Ready  := true.B
          dataFifo(w_ptr) := buffer.asUInt(8,1)
        }
      }
    }.otherwise{
      count := count + 1.U
      buffer(count) := io.ps2.data
    }
  }

  io.outKey := Cat(
    Mux(brokFifo(r_ptr),"hf0".U(8.W),0.U(8.W)),
    Mux(extendFifo(r_ptr),"he0".U(8.W),0.U(8.W)),
    dataFifo(r_ptr)
  )
}

class ps2_top_apb extends BlackBox {
  val io = IO(new PS2CtrlIO)
}

class ps2Chisel extends Module {
  val io = IO(new PS2CtrlIO)
  val keyboard = Module(new PS2KeyBoard)
  val selAddr = Cat(io.in.paddr(31,3), 0.U(3.W))

  val next_data_n_r = RegInit(true.B)
  keyboard.io.ps2 := io.ps2
  keyboard.io.nextdata_n := next_data_n_r
  io.in.pready := false.B
  io.in.pslverr:= 0.U
  io.in.prdata := 0.U
  val (s_idle :: s_read :: Nil )=Enum(2)
  val state = RegInit(s_idle)
  switch(state){
    is(s_idle){
      next_data_n_r := true.B
      when(io.in.psel){
        when(io.in.pwrite){
          assert(!io.in.pwrite, "do not support write operations")
        }.otherwise{
          state := s_read
        }
      }
    }
    is(s_read){
      next_data_n_r := true.B
      when(io.in.penable){
        when(selAddr === "h10011000".U){
          io.in.pready := true.B
          state := s_idle
          when(keyboard.io.ready){
            io.in.prdata := keyboard.io.outKey
            next_data_n_r := false.B
          }otherwise{
            io.in.prdata := 0.U
          }
        }
      }
    }
  }
}

class APBKeyboard(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val ps2_bundle = IO(new PS2IO)

    val mps2 = Module(new ps2Chisel)
    mps2.io.clock := clock
    mps2.io.reset := reset
    mps2.io.in <> in
    ps2_bundle <> mps2.io.ps2
  }
}
