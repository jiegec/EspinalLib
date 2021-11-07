package espinallib.bitserial

import espinallib.common.GenUtils
import espinallib.common.VerilogBench
import spinal.core._

// unsigned adder
// from LSB to MSB
class Adder extends Component {
  val io = new Bundle {
    val a = in(UInt(1 bits))
    val b = in(UInt(1 bits))

    val sum = out(UInt(1 bits))
  }

  val c = RegInit(U(0, 1 bits))

  val res = io.a +^ io.b + c
  io.sum := res(0).asUInt
  c := res(1).asUInt
}

object AdderVerilog extends GenUtils {
  work(new Adder())
}

object AdderBench extends VerilogBench {
  bench(
    new Adder()
  )
}
