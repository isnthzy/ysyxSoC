package ysyx

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class AXI4DelayerIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
  val out = new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4))
}

class axi4_delayer extends BlackBox {
  val io = IO(new AXI4DelayerIO)
}

class AXI4DelayerChisel extends Module {
  val io = IO(new AXI4DelayerIO)
  def R = 5.U
// io.out <> io.in
  val rQueue = Module(new Queue(new AXI4BundleR(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4))
  , entries = 8))
  val rCntQueue = Module(new Queue(UInt(12.W), entries = 8))
  val rCnt = RegInit(0.U(12.W))
  val rBuffIdx = RegInit(0.U(3.W))
  val waitRvalid = RegInit(false.B)
  io.in.ar <> io.out.ar
  io.out.r.ready := true.B
  when(io.in.ar.valid){
    rCnt := rCnt + R
    when(io.in.ar.fire){
      waitRvalid := true.B
    }
  }
  val rWaitTotalTime = RegInit(0.U(12.W))
  val setDelayBegin = RegInit(false.B)
  rQueue.io.enq.valid := false.B
  rQueue.io.enq.bits  := io.out.r.bits.asTypeOf(new AXI4BundleR(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
  rCntQueue.io.enq.valid := false.B
  rCntQueue.io.enq.bits  := rCnt - rWaitTotalTime
  when(waitRvalid && !(io.out.r.valid && io.out.r.bits.last)){
    rCnt := rCnt + R
  }
  when(waitRvalid && io.out.r.valid){
    rQueue.io.enq.valid := true.B
    rCntQueue.io.enq.valid := true.B
    rWaitTotalTime := rCnt
    when(io.out.r.bits.last){
      rCnt := 0.U
      rWaitTotalTime := 0.U
      waitRvalid := false.B
    }
  }
  
  val (s_idle ::s_resp :: Nil) = Enum(2)
  val rrespState = RegInit(s_idle)
  val respCnt = RegInit(0.U(12.W))
  rCntQueue.io.deq.ready := false.B
  rQueue.io.deq.ready := false.B
  io.in.r.bits := 0.U.asTypeOf(io.in.r.bits)
  io.in.r.valid := false.B
  switch(rrespState){
    is(s_idle){
      when(rCntQueue.io.deq.valid){
        respCnt := rCntQueue.io.deq.bits - 2.U //状态机转换消耗一个周期
        rrespState := s_resp
      }
    }
    is(s_resp){
      when(respCnt =/= 0.U){
        respCnt := respCnt - 1.U
      }.elsewhen(respCnt === 0.U){
        io.in.r.valid := true.B
        io.in.r.bits  := rQueue.io.deq.bits.asTypeOf(io.in.r.bits)
        when(io.in.r.ready){
          rrespState := s_idle
          rCntQueue.io.deq.ready := true.B
          rQueue.io.deq.ready:= true.B
        }
      }
    }
  }

  io.in.aw <> io.out.aw
  io.in.w <> io.out.w
  io.out.b.ready := true.B
  val waitBvalid = RegInit(false.B)
  val delayBegin = RegInit(false.B)
  val wCnt = RegInit(0.U(12.W))
  val bBuff = RegInit(0.U.asTypeOf(
    new AXI4BundleB(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4))))
  io.in.b.valid := false.B
  io.in.b.bits := 0.U.asTypeOf(io.in.b.bits)
  when(io.in.aw.valid||waitBvalid){
    wCnt := wCnt + R
    waitBvalid := true.B
  }
  when(io.out.b.valid){
    waitBvalid := false.B
    delayBegin := true.B
    bBuff := io.out.b.bits
  }
  when(delayBegin){
    when(wCnt =/= 0.U){
      wCnt := wCnt - 1.U
    }.elsewhen(wCnt === 0.U){
      io.in.b.valid := true.B
      io.in.b.bits := bBuff.asTypeOf(io.in.b.bits)
      when(io.in.b.ready){
        delayBegin := false.B
      }
    }
  }

}

class AXI4DelayerWrapper(implicit p: Parameters) extends LazyModule {
  val node = AXI4IdentityNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val delayer = Module(new AXI4DelayerChisel)
      delayer.io.clock := clock
      delayer.io.reset := reset
      delayer.io.in <> in
      out <> delayer.io.out
    }
  }
}

object AXI4Delayer {
  def apply()(implicit p: Parameters): AXI4Node = {
    val axi4delay = LazyModule(new AXI4DelayerWrapper)
    axi4delay.node
  }
}
