package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SDRAMIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(4.W))
  val dq0 = Analog(16.W)
  val dq1 = Analog(16.W)
}

class SDRAMPARTIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val num = Output(Bool())
  val cs  = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we  = Output(Bool())
  val a   = Output(UInt(13.W))
  val ba  = Output(UInt(2.W))
  val dqm = Output(UInt(4.W))
  val dq  = Analog(16.W)
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new AXI4Bundle(AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)))
    val sdram = new SDRAMIO
  })
}

class sdram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}

class sdramOpModeBundle extends Bundle{
  val wb = Bool()
  val opMode = UInt(2.W)
  val casLatency = UInt(3.W)
  val bt = Bool()
  val burstLength = UInt(3.W)
}

class sdramRAM extends BlackBox with HasBlackBoxInline {
  //为了提速访存速度，使用sdramRAM外挂在verilator中
  val io = IO(new Bundle {
    val clock = Input(Bool())
    val valid = Input(Bool())
    val wr    = Input(Bool())
    val row   = Input(UInt(16.W))
    val col   = Input(UInt(16.W))
    val bank  = Input(UInt(8.W))
    val num   = Input(Bool())
    val dqm   = Input(UInt(8.W))
    val din   = Input(UInt(16.W))
    val dout  = Output(UInt(16.W))
  })
  setInline("sdramRAM.v",
    """import "DPI-C" function void sdrambank_read( input shortint row,input shortint col,output shortint dout,input byte bank, input byte dqm);
      |import "DPI-C" function void sdrambank_write(input shortint row,input shortint col,input shortint din  ,input byte bank, input byte dqm);
      |module sdramRAM(
      |    input              clock,
      |    input              valid,
      |    input              wr,
      |    input      [15:0]  row,
      |    input      [15:0]  col,
      |    input      [ 7:0]  bank,
      |    input              num, //颗粒号
      |    input      [ 7:0]  dqm,
      |    input      [15:0]  din,
      |    output reg [15:0]  dout
      |);
      |wire [7:0] real_bank;
      |assign real_bank = bank + ({{7{1'b0}}, num} << 2); 
      |always@(posedge clock) begin
      |  if(valid) begin
      |    if(wr) begin
      |      sdrambank_write(row, col, din, real_bank, dqm);
      |    end
      |    else begin
      |      sdrambank_read(row, col, dout, real_bank, dqm);
      |    end
      |  end
      |end
      |endmodule
    """.stripMargin)
}

class sdramPart extends RawModule{
  val io = IO(Flipped(new SDRAMPARTIO))
  /*NOTE:实现SDRAM控制器会发送的命令
  其中PRECHARGE和AUTO REFRESH命令与存储单元的电气特性相关,可以将其实现成NOP.
  此外, Mode寄存器只需要实现CAS Latency和Burst Length
  CS#	RAS#	CAS#	WE#	命令名称	命令含义
  1	  X	    X	    X	  COMMAND INHIBIT	无命令
  0	  1	    1	    1	  NO OPERATION	NOP
  0	  0	    1	    1	  ACTIVE	激活目标存储体的一行
  0	  1	    0	    1	  READ	读出目标存储体的一列
  0	  1	    0	    0	  WRITE	写入目标存储体的一列
  0	  1	    1	    0	  BURST TERMINATE	停止当前的突发传输
  0	  0	    1	    0	  PRECHARGE	NOP
  0	  0	    0	    1	  AUTO REFRESH	NOP
  0	  0	    0	    0 	LOAD MODE REGISTER	设置Mode寄存器
  */
  val command = Wire(new Bundle {
    val inhibit=Bool()
    val nop=Bool()
    val active=Bool()
    val read=Bool()
    val write=Bool()
    val burstTerminate=Bool()
    val precharge=Bool()
    val autoRefresh=Bool()
    val loadModeRegister=Bool()
  })
  withClockAndReset(io.clk.asClock,~io.cke) {
    val dqinValue = WireDefault(0.U(16.W))
    val dqinEn    = WireDefault(false.B)
    val dqi = TriStateInBuf(io.dq, dqinValue, dqinEn) // change this if you need

    command.inhibit := io.cs
    command.nop     := io.ras && io.cas && io.we
    command.active  := !io.ras && io.cas && io.we
    command.read    := io.ras && !io.cas && io.we
    command.write   := io.ras && !io.cas && !io.we 
    command.burstTerminate   := !io.ras && !io.cas && io.we
    command.precharge        :=  io.ras && !io.cas && io.we
    command.autoRefresh      := !io.ras && !io.cas && io.we
    command.loadModeRegister := !io.ras && !io.cas && !io.we
    val isNop = command.nop || command.inhibit || command.precharge || command.autoRefresh
    val isLoadModeRegister = command.loadModeRegister
    val isRead   = command.read
    val isWrite  = command.write
    val isActive = command.active
    val isburstTerminate = command.burstTerminate
    val sdramRAM = Module(new sdramRAM)
    sdramRAM.io.num := io.num

    val ModeRegister = RegInit(0.U.asTypeOf(new sdramOpModeBundle))
    when(isLoadModeRegister){
      ModeRegister := io.a(9,0).asTypeOf(new sdramOpModeBundle)
    }
    //NOTE: 一个Bank容量为8192 * 512 * 16 bit 又因为16bit分为两个写掩码故拆为 8192 * 512 * 8bit

    val rowAddrBuff = RegInit(VecInit(Seq.fill(4)(0.U(13.W))))
    // val colAddrBuff = RegInit(0.U(12.W)) //行和列buff
    val bankBuff    = RegInit(0.U(2.W))
    val wdataBuff   = RegInit(0.U(16.W))
    val rowDataBuff = RegInit(VecInit(Seq.fill(4)(0.U.asTypeOf(Vec(1024,UInt(8.W))))))
    val casLatencyCnt = RegInit(0.U(3.W))
    val burstLengthCnt = RegInit(0.U(3.W))
    val requestColReg   = RegInit(0.U(9.W))
    val dqmBuff         = RegInit(0.U(2.W))
  
    val (s_idle :: s_active :: s_read :: s_write :: Nil )=Enum(4)
    val state = RegInit(s_idle)
    val ReadEn = WireDefault(false.B)
    val colAddr = WireDefault(0.U(9.W))
    sdramRAM.io.clock := io.clk
    sdramRAM.io.valid := state === s_write || state === s_read
    sdramRAM.io.row  := rowAddrBuff(bankBuff)
    sdramRAM.io.col  := colAddr
    sdramRAM.io.wr   := state === s_write
    sdramRAM.io.bank := bankBuff
    sdramRAM.io.din  := wdataBuff
    sdramRAM.io.dqm  := dqmBuff

    switch(state){
      is(s_idle){
        when(isActive){
          rowAddrBuff(io.ba) := io.a
          state := s_active
        }
        when(isRead){
          state := s_read
          requestColReg := io.a(8,0)
          bankBuff      := io.ba
          casLatencyCnt := ModeRegister.casLatency - 1.U
          //NOTE:状态机转换本身会消耗一周期，故casLatencyCnt需要减1
        }
        when(isWrite){
          state := s_write
          wdataBuff := dqi
          requestColReg := io.a(8,0)
          bankBuff      := io.ba
          dqmBuff       := io.dqm
        }
      }
      is(s_active){
        // colAddrBuff := io.a(8,0)

        when(isRead){
          state := s_read
          requestColReg := io.a(8,0)
          bankBuff      := io.ba
          casLatencyCnt := ModeRegister.casLatency - 1.U
          //NOTE:状态机转换本身会消耗一周期，故casLatencyCnt需要减1
        }
        when(isWrite){
          state := s_write
          wdataBuff := dqi
          requestColReg := io.a(8,0)
          bankBuff      := io.ba
          dqmBuff       := io.dqm
        }
      }
      is(s_read){
        //使用移位寄存器来决定读出数据的延迟和顺序
        when(casLatencyCnt =/= 1.U){
          casLatencyCnt := casLatencyCnt - 1.U
        }
        colAddr := requestColReg + burstLengthCnt
        when(casLatencyCnt === 1.U && burstLengthCnt <= ModeRegister.burstLength + 1.U){
          burstLengthCnt := burstLengthCnt + 1.U
          dqinValue := sdramRAM.io.dout
          dqinEn := true.B
        }
        when(burstLengthCnt === ModeRegister.burstLength + 1.U){ //因为sram性质，访问dpic的结果延迟一拍输出，所以要等延迟一拍的结果
          burstLengthCnt := 0.U
          state := s_idle
        }
      }
      is(s_write){
        when(burstLengthCnt < ModeRegister.burstLength){
          burstLengthCnt := burstLengthCnt + 1.U
        }
        colAddr := requestColReg + burstLengthCnt
        wdataBuff := dqi
        dqmBuff   := io.dqm
        when(burstLengthCnt === ModeRegister.burstLength){
          burstLengthCnt := 0.U
          state := s_idle
        }
      }
    }
  }
}

class sdramChisel extends RawModule { 
  val io=IO(Flipped(new SDRAMIO))
  val sdramPart = Array.fill(2){Module(new sdramPart).io}
  sdramPart(0).num := 0.U
  sdramPart(1).num := 1.U

  sdramPart(0).clk <> io.clk
  sdramPart(0).cke <> io.cke
  sdramPart(0).cs  <> io.cs
  sdramPart(0).ras <> io.ras
  sdramPart(0).cas <> io.cas
  sdramPart(0).we  <> io.we
  sdramPart(0).a   <> io.a
  sdramPart(0).ba  <> io.ba
  sdramPart(1).clk <> io.clk
  sdramPart(1).cke <> io.cke
  sdramPart(1).cs  <> io.cs
  sdramPart(1).ras <> io.ras
  sdramPart(1).cas <> io.cas
  sdramPart(1).we  <> io.we
  sdramPart(1).a   <> io.a
  sdramPart(1).ba  <> io.ba

  sdramPart(0).dqm <> io.dqm(1,0)
  sdramPart(1).dqm <> io.dqm(3,2) 

  sdramPart(0).dq  <> io.dq0
  sdramPart(1).dq  <> io.dq1


}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
  val beatBytes = 4
  val node = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    Seq(AXI4SlaveParameters(
        address       = address,
        executable    = true,
        supportsWrite = TransferSizes(1, beatBytes),
        supportsRead  = TransferSizes(1, beatBytes),
        interleavedId = Some(0))
    ),
    beatBytes  = beatBytes)))

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}

class APBSDRAM(address: Seq[AddressSet])(implicit p: Parameters) extends LazyModule {
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
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_apb)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}
