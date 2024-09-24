package ysyx

import chisel3._
import chisel3.util._

class bitrev extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class bitrevChisel extends RawModule { // we do not need clock and reset
  val bitrev_size=8
  val io = IO(Flipped(new SPIIO(1)))

  withClock(io.sck.asClock){
    val bitrev_buff=Reg(Vec(bitrev_size,UInt(1.W))) 
    val bit_cnt=Reg(UInt(log2Ceil(bitrev_size+1).W))
    io.miso:=true.B
    when(!io.ss){
      bit_cnt:=bit_cnt+1.U
      when(bit_cnt<bitrev_size.U){        
        io.miso:=false.B
        bitrev_buff(bit_cnt(log2Ceil(bitrev_size)-1,0)):=io.mosi
      }otherwise{
        io.miso:=bitrev_buff((bitrev_size-1).U-bit_cnt(log2Ceil(bitrev_size)-1,0))
      }
    }.otherwise{
      bitrev_buff:=0.U.asTypeOf(bitrev_buff)
      bit_cnt:=0.U
      io.miso:=true.B
    }
  }
}
