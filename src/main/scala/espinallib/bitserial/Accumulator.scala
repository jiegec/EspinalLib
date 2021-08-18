package espinallib.bitserial

import spinal.core._
import spinal.lib._
import espinallib.common.GenUtils

class Accumulator(n: Int) extends Component {
  val io = new Bundle {
    val b = in(UInt(1 bits))

    val sum = out(UInt(1 bits))
  }

  val data = RegInit(Vec(U(0, 1 bits), n))

  val c = RegInit(U(0, 1 bits))

  val res = data(0) +^ io.b + c
  io.sum := res(0).asUInt
  data(n-1) := res(0).asUInt
  c := res(1).asUInt

  for (i <- 0 until n - 1) {
    data(i) := data(i + 1)
  }
}

object AccumulatorVerilog extends GenUtils {
  work(new Accumulator(16))
}
