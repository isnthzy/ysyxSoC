package ysyx

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class APBDelayerIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val out = new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
}

class apb_delayer extends BlackBox {
  val io = IO(new APBDelayerIO)
}

class APBDelayerChisel(val params: APBBundleParameters) extends Module {
  val io = IO(new APBDelayerIO)
  def R = 5.U
  val rCnt = RegInit(0.U(15.W))
  val respBuff = RegInit(0.U.asTypeOf(new Bundle {
    val pready    = Bool()
    val pslverr   = Bool()
    val prdata    = UInt(params.dataBits.W)
  }))
  when(io.in.psel && io.in.penable && io.out.pready) {
    respBuff.pready := io.out.pready
    respBuff.pslverr := io.out.pslverr
    respBuff.prdata := io.out.prdata
  }
  io.in.pready  := false.B
  io.in.pslverr := 0.U
  io.in.prdata  := 0.U
  when(io.in.psel && io.in.penable) {
    when(respBuff.pready) {
      rCnt := rCnt - 1.U
    }.otherwise{
      rCnt := rCnt + R
    }
    when(rCnt === 0.U) {
      io.in.pready  := respBuff.pready
      io.in.pslverr := respBuff.pslverr
      io.in.prdata  := respBuff.prdata
      respBuff := 0.U.asTypeOf(respBuff)
    }
  }
  io.out.psel    := Mux(respBuff.pready,false.B,io.in.psel)
  io.out.penable := Mux(respBuff.pready,false.B,io.in.penable)
  io.out.pwrite  := Mux(respBuff.pready,false.B,io.in.pwrite)
  io.out.paddr := io.in.paddr
  io.out.pprot := io.in.pprot
  io.out.pstrb := io.in.pstrb
  io.out.pwdata := io.in.pwdata
}

class APBDelayerWrapper(implicit p: Parameters) extends LazyModule {
  val node = APBIdentityNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val delayer = Module(new APBDelayerChisel(APBBundleParameters(addrBits = 32, dataBits = 32)))
      delayer.io.clock := clock
      delayer.io.reset := reset
      delayer.io.in <> in
      out <> delayer.io.out
    }
  }
}

object APBDelayer {
  def apply()(implicit p: Parameters): APBNode = {
    val apbdelay = LazyModule(new APBDelayerWrapper)
    apbdelay.node
  }
}
