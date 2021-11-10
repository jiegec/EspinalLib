package espinallib.random

import spinal.core._
import spinal.crypto.PolynomialGF2
import spinal.crypto.misc.LFSR

import scala.util.Random

/** Generate RNG with a series of LFSR */
class LFSRRNG(
    lfsrPolynomial: Seq[PolynomialGF2]
) extends Component {
  val maxOrder = lfsrPolynomial.map((poly) => poly.order).max
  val io = new Bundle {
    val random = out(Bits(maxOrder bits))
  }

  Random.setSeed(0x12345678)
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

object RandomNumbers {
  val rng = new Random(0x12345678)
  def randPositiveInt = RandomNumbers.rng.nextInt(0x7fffffff)
}

/** Barrel-shifted 96-bit LFSR. Paper: A. Veiga and E. Spinelli, "A pulse
  * generator with poisson-exponential distribution for emulation of radioactive
  * decay events," 2016 IEEE 7th Latin American Symposium on Circuits & Systems
  * (LASCAS), 2016, pp. 31-34, doi: 10.1109/LASCAS.2016.7451002.
  */
class LFSRRNG32 extends Component {
  val io = new Bundle {
    val random = out(Bits(32 bits))
  }

  val lfsr = RegInit(B(RandomNumbers.randPositiveInt, 96 bits))
  lfsr(95 downto 32) := lfsr(63 downto 0)
  for (i <- 0 until 32) {
    lfsr(31 - i) := (lfsr(95 - i) ^ lfsr(93 - i) ^ lfsr(48 - i) ^ lfsr(46 - i))
  }
  io.random := lfsr(31 downto 0)
}

/** Tausworthe generator. Paper: A. Veiga and E. Spinelli, "A pulse generator
  * with poisson-exponential distribution for emulation of radioactive decay
  * events," 2016 IEEE 7th Latin American Symposium on Circuits & Systems
  * (LASCAS), 2016, pp. 31-34, doi: 10.1109/LASCAS.2016.7451002.
  */
class TauswortheRNG32 extends Component {
  val io = new Bundle {
    val random = out(Bits(32 bits))
  }

  val s0 = RegInit(B(RandomNumbers.randPositiveInt, 32 bits))
  val s1 = RegInit(B(RandomNumbers.randPositiveInt, 32 bits))
  val s2 = RegInit(B(RandomNumbers.randPositiveInt, 32 bits))
  s0 := (((s0 & 0xfffffffeL) << 12) ^ (((s0 << 13).resized ^ s0) >> 19).resized).resized;
  s1 := (((s1 & 0xfffffff8L) << 4) ^ (((s1 << 2).resized ^ s1) >> 25).resized).resized;
  s2 := (((s2 & 0xfffffff0L) << 12) ^ (((s2 << 3).resized ^ s2) >> 11).resized).resized;
  io.random := s0 ^ s1 ^ s2
}
