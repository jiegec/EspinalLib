package espinallib.hardfloat.rewrite

import spinal.core.{Bool, Mux, U, UInt}

// https://github.com/ucb-bar/berkeley-hardfloat/blob/master/src/main/scala/primitives.scala
object CountLeadingZeros {
  def apply(in: UInt): UInt = {
    val vals: Seq[(Bool, UInt)] = Array.tabulate(in.getWidth) { i =>
      (in(i), U(i))
    }
    inner(vals)
  }

  def inner(in: Seq[(Bool, UInt)]): UInt = {
    if (in.size == 1) {
      in.head._2
    } else {
      Mux(in.head._1, in.head._2, inner(in.tail))
    }
  }
}
