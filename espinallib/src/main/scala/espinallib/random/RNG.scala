package espinallib.random

import spinal.core._
import spinal.crypto.misc.LFSR
import spinal.crypto.PolynomialGF2
import scala.util.Random

class RNG(
    lfsrPolynomial: Seq[PolynomialGF2]
) extends Component {
  val maxOrder = lfsrPolynomial.map((poly) => poly.order).max
  val io = new Bundle {
    val random = out(Bits(maxOrder bits))
  }

  val all_lfsr = for ((poly, i) <- lfsrPolynomial zipWithIndex) yield {
    // have different steps
    val lfsr_reg = RegInit(B(Random.nextInt(1 << poly.order), poly.order bits))

    val lfsr = Seq.fill(i + 1)(Bits(poly.order bits))
    lfsr(0) := lfsr_reg
    for (j <- 0 until i) {
      lfsr(j + 1) := LFSR.Fibonacci(lfsr(j), poly)
    }
    lfsr_reg := LFSR.Fibonacci(lfsr(i), poly)
    lfsr_reg.resize(maxOrder bits)
  }
  io.random := all_lfsr.reduce(_ ^ _)
}
