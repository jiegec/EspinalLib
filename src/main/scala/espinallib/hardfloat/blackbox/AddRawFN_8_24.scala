package espinallib.hardfloat.blackbox

import espinallib.common.GenUtils
import espinallib.hardfloat.rewrite.RawFloat
import spinal.core._

// blackbox of https://github.com/ucb-bar/berkeley-hardfloat/blob/master/src/main/scala/AddRecFN.scala
// with expWidth=8, sigWidth=24
class AddRawFN_8_24BlackBox extends BlackBox {

  val expWidth = 8;
  val sigWidth = 24;

  val io = new Bundle {
    val subOp = in(Bool())
    val a = in(new RawFloat(expWidth, sigWidth))
    val b = in(new RawFloat(expWidth, sigWidth))
    val roundingMode = in(Bits(3 bits))
    val invalidExc = out(Bool())
    val rawOut = out(new RawFloat(expWidth, sigWidth + 2))
  }

  setDefinitionName("AddRawFN_8_24")

  addRTLPath("./hardfloat/AddRawFN_8_24.v")
}

class AddRawFN_8_24 extends Component {
  val expWidth = 8;
  val sigWidth = 24;

  val io = new Bundle {
    val subOp = in(Bool())
    val a = in(new RawFloat(expWidth, sigWidth))
    val b = in(new RawFloat(expWidth, sigWidth))
    val roundingMode = in(Bits(3 bits))
    val invalidExc = out(Bool())
    val rawOut = out(new RawFloat(expWidth, sigWidth + 2))
  }

  val blackBox = new AddRawFN_8_24BlackBox
  blackBox.io.subOp := io.subOp
  blackBox.io.a := io.a
  blackBox.io.b := io.b
  blackBox.io.roundingMode := io.roundingMode
  io.invalidExc := blackBox.io.invalidExc
  io.rawOut := blackBox.io.rawOut
}

object AddRawFN_8_24Verilog extends GenUtils {
  work(
    new AddRawFN_8_24()
  )
}
