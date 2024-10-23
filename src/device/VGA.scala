package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class VGAIO extends Bundle {
  val r = Output(UInt(8.W))
  val g = Output(UInt(8.W))
  val b = Output(UInt(8.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val valid = Output(Bool())
}

class VGACtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
  val vga = new VGAIO
}

class vgaFbMem extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val ren   = Input(Bool())
    val raddr = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
    val wen   = Input(Bool())
    val waddr = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
  })
    setInline("vgaFbMem.v",
    """import "DPI-C" function void vga_fb_read( input int raddr,output int rdata);
      |import "DPI-C" function void vga_fb_write(input int waddr,input int write);
      |module vgaFbMem(
      |    input              clock,
      |    input              ren,
      |    input      [31:0]  raddr,
      |    output reg [31:0]  rdata,
      |    input              wen,
      |    input      [31:0]  waddr,
      |    input      [31:0]  wdata
      |);
      |
      |always@(posedge clock) begin
      |    if(wen) begin
      |      vga_fb_write(waddr, wdata);
      |    end
      |    if(ren) begin
      |      vga_fb_read( raddr, rdata);
      |    end
      |    else begin
      |      rdata <= 0;
      |    end
      |  end
      |endmodule
    """.stripMargin)
}

class vgaCtrl extends Module {
  val io = IO(new Bundle {
    val vga = new VGAIO()
    val wen   = Input(Bool())
    val waddr = Input(UInt(24.W))
    val wdata = Input(UInt(24.W))
  })
  def h_frontporch = 96.U
  def h_active     = 144.U
  def h_backporch  = 784.U
  def h_total      = 800.U

  def v_frontporch = 2.U
  def v_active     = 35.U
  def v_backporch  = 515.U
  def v_total      = 525.U

  // val mem = SyncReadMem(1024 * 512, UInt(24.W))
  val mem = Module(new vgaFbMem()) 
  //NOTE:不用syncreadmem用dpic的原因是为了提升verilator仿真速度
  mem.io.clock := clock

  val word_addr = io.waddr >> 2.U
  val ptr_h     = Wire(UInt(10.W))
  val ptr_v     = Wire(UInt(9.W))
  ptr_v := word_addr / 640.U
  ptr_h := word_addr % 640.U
  val write_ptr = Cat(ptr_h, ptr_v)

  mem.io.waddr:= write_ptr
  mem.io.wdata:= io.wdata
  mem.io.wen  := false.B
  when(io.wen) {
    mem.io.wen := true.B
    // mem.write(write_ptr, io.wdata)
  }

  val x_cnt = RegInit(1.U(10.W))
  val y_cnt = RegInit(1.U(10.W))
  val h_valid = Wire(Bool())
  val v_valid = Wire(Bool())

  when(x_cnt === h_total) {
    x_cnt := 1.U
    when(y_cnt === v_total) {
      y_cnt := 1.U
    }.otherwise{
      y_cnt := y_cnt + 1.U
    }
  }.otherwise{
    x_cnt := x_cnt + 1.U
  }

  io.vga.hsync := x_cnt > h_frontporch
  io.vga.vsync := y_cnt > v_frontporch

  h_valid := (x_cnt > h_active) & (x_cnt <= h_backporch)
  v_valid := (y_cnt > v_active) & (y_cnt <= v_backporch)
  io.vga.valid := h_valid & v_valid

  val h_addr = Mux(h_valid,(x_cnt - 145.U),0.U)
  val v_addr = Mux(v_valid,(y_cnt - 36.U),0.U)
  dontTouch(h_addr)
  dontTouch(v_addr)

  mem.io.raddr:= Cat(h_addr, v_addr(8,0))
  mem.io.ren := true.B
  val vga_data = mem.io.rdata
  // val vga_data = mem.read(Cat(h_addr, v_addr(8,0)))
  io.vga.r := vga_data(23,16)
  io.vga.g := vga_data(15,8)
  io.vga.b := vga_data(7,0)

}

class vga_top_apb extends BlackBox {
  val io = IO(new VGACtrlIO)
}

class vgaChisel extends Module {
  val io = IO(new VGACtrlIO)
  val vgaCtrl = Module(new vgaCtrl())
  io.vga := vgaCtrl.io.vga

  val (s_idle :: s_write :: Nil )=Enum(2)
  val state = RegInit(s_idle)
  io.in.pready := false.B
  io.in.pslverr:= 0.U
  io.in.prdata := 0.U
  vgaCtrl.io.wen := state === s_write
  vgaCtrl.io.waddr := io.in.paddr
  vgaCtrl.io.wdata := io.in.pwdata(23,0)
  switch(state) {
    is(s_idle) {
      when(io.in.psel) {
        when(io.in.pwrite) {
          state := s_write
        }.otherwise{
          assert(io.in.pwrite, "do not support reads operations")
        }
      }
    }
    is(s_write) {
      io.in.pready := true.B
      state := s_idle
    }
  }
}

class APBVGA(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val vga_bundle = IO(new VGAIO)

    val mvga = Module(new vgaChisel)
    mvga.io.clock := clock
    mvga.io.reset := reset
    mvga.io.in <> in
    vga_bundle <> mvga.io.vga
  }
}
