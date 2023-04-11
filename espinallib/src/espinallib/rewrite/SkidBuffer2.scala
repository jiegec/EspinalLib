package espinallib.rewrite

import espinallib.common.GenUtils
import espinallib.common.SkidBufferCommon
import spinal.core._

object SkidBuffer2State extends SpinalEnum {
  val sEmpty, sBusy, sFull = newElement()
}

// Skid buffer from http://fpgacpu.ca/fpga/Pipeline_Skid_Buffer.html
// Has two buffers, all output signals(io.s.ready, io.m.valid and io.m.payload) are registered
class SkidBuffer2[T <: Data](
    gen: => T
) extends SkidBufferCommon(gen) {
  val dataBufferWE = Bool()
  val dataWidth = widthOf(gen)
  val dataBufferReg = Reg(Bits(dataWidth bits)) init (0)
  when(dataBufferWE) {
    dataBufferReg := io.s.payload.asBits
  }

  val dataOutWE = Bool()
  val useBufferedData = Bool()
  val selectedData = Bits(dataWidth bits)

  when(useBufferedData) {
    selectedData := dataBufferReg
  } otherwise {
    selectedData := io.s.payload.asBits
  }

  val dataOutReg = Reg(Bits(dataWidth bits)) init (0)
  io.m.payload.assignFromBits(dataOutReg)
  when(dataOutWE) {
    dataOutReg := selectedData
  }

  val state = Reg(SkidBuffer2State) init (SkidBuffer2State.sEmpty)
  val stateNext = SkidBuffer2State.sEmpty()

  val inputReadyReg = Reg(Bool()) init (True)
  inputReadyReg := stateNext =/= SkidBuffer2State.sFull
  io.s.ready := inputReadyReg

  val outputValidReg = Reg(Bool()) init (False)
  outputValidReg := stateNext =/= SkidBuffer2State.sEmpty
  io.m.valid := outputValidReg

  val insert = io.s.fire
  val remove = io.m.fire

  val load = state === SkidBuffer2State.sEmpty && insert && ~remove
  val flow = state === SkidBuffer2State.sBusy && insert && remove
  val fill = state === SkidBuffer2State.sBusy && insert && ~remove
  val flush = state === SkidBuffer2State.sFull && ~insert && remove
  val unload = state === SkidBuffer2State.sBusy && ~insert && remove
  when(load) {
    stateNext := SkidBuffer2State.sBusy
  }.elsewhen(flow) {
    stateNext := SkidBuffer2State.sBusy
  }.elsewhen(fill) {
    stateNext := SkidBuffer2State.sFull
  }.elsewhen(flush) {
    stateNext := SkidBuffer2State.sBusy
  }.elsewhen(unload) {
    stateNext := SkidBuffer2State.sEmpty
  }.otherwise {
    stateNext := state
  }
  state := stateNext

  dataOutWE := load || flow || flush
  dataBufferWE := fill
  useBufferedData := flush
}

object SkidBuffer2Verilog extends GenUtils {
  work(
    new SkidBuffer2(UInt(32 bits))
  )
}
