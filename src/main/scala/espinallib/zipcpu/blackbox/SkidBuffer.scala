package espinallib.zipcpu.blackbox

import espinallib.common.{GenUtils, SkidBufferCommon}
import spinal.core._
import espinallib.common.Resource

// blackbox of https://github.com/ZipCPU/wb2axip/blob/master/rtl/skidbuffer.v
class SkidBufferBlackBox(
    dataWidth: Int = 8,
    lowPower: Boolean = false,
    outputReg: Boolean = true
) extends BlackBox {

  val io = new Bundle {
    val i_clk = in(Bool)
    val i_reset = in(Bool)

    val i_valid = in(Bool)
    val o_ready = out(Bool)
    val i_data = in(Bits(dataWidth bits))

    val o_valid = out(Bool)
    val i_ready = in(Bool)
    val o_data = out(Bits(dataWidth bits))
  }

  setBlackBoxName("skidbuffer")

  addGeneric("DW", dataWidth)
  addGeneric("OPT_LOWPOWER", lowPower)
  addGeneric("OPT_OUTREG", outputReg)

  // Map the clk
  mapCurrentClockDomain(clock = io.i_clk, reset = io.i_reset)

  noIoPrefix()

  addRTLPath(Resource.path("/wb2axip/rtl/skidbuffer.v"))
}

// wrapper for skidbuffer
class SkidBuffer[T <: Data](
    gen: => T,
    lowPower: Boolean = false,
    outputReg: Boolean = true
) extends SkidBufferCommon(gen) {
  val dataWidth = widthOf(gen)

  val skidBuffer =
    new SkidBufferBlackBox(dataWidth, lowPower, outputReg)

  skidBuffer.io.i_valid := io.s.valid
  io.s.ready := skidBuffer.io.o_ready
  skidBuffer.io.i_data := io.s.payload.asBits

  io.m.valid := skidBuffer.io.o_valid
  skidBuffer.io.i_ready := io.m.ready
  io.m.payload.assignFromBits(skidBuffer.io.o_data)
}

object SkidBufferVerilog extends GenUtils {
  work(
    new SkidBuffer(
      UInt(32 bits)
    )
  )
}
