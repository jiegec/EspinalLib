package espinallib.bitserial

import espinallib.common.GenUtils
import espinallib.common.VerilogBench
import spinal.core._

// accumulator
// from LSB to MSB
class Accumulator(n: Int) extends Component {
  val io = new Bundle {
    val b = in(UInt(1 bits))
    val signBit = in(Bool())

    val sum = out(UInt(1 bits))
  }

  val data = RegInit(Vec(U(0, 1 bits), n))

  val c = RegInit(U(0, 1 bits))

  val res = data(0) +^ io.b + c
  io.sum := res(0).asUInt
  data(n - 1) := res(0).asUInt
  // reset carry when the sign bit is encountered
  when(io.signBit) {
    c := 0
  }.otherwise {
    c := res(1).asUInt
  }

  for (i <- 0 until n - 1) {
    data(i) := data(i + 1)
  }
}

object AccumulatorVerilog extends GenUtils {
  work(new Accumulator(16))
}

object AccumulatorBench extends VerilogBench {
  bench(
    new Accumulator(16)
  )
}
