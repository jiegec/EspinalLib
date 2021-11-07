package espinallib.random

import spinal.core._
import spinal.lib._
import spinal.crypto.misc.LFSR
import spinal.crypto.PolynomialGF2
import scala.util.Random

class PoissonGeneratorRequest(counterWidth: Int) extends Bundle {
  val threshold = UInt(counterWidth bits)
}

class PoissonGeneratorResponse(outputWidth: Int) extends Bundle {
  val res = UInt(outputWidth bits)
}

object PoissonGeneratorState extends SpinalEnum {
  val sReady, sAccumulate, sDone = newElement()
}

class PoissonGenerator(
    counterWidth: Int,
    lfsrPolynomial: Seq[PolynomialGF2],
    outputWidth: Int
) extends Component {
  val io = new Bundle {
    val req = slave(Stream(new PoissonGeneratorRequest(counterWidth)))
    val resp = master(Stream(new PoissonGeneratorResponse(outputWidth)))
  }

  val rand = Bits(counterWidth bits)
  val all_lfsr = for ((poly, i) <- lfsrPolynomial zipWithIndex) yield {
    // have different steps
    val lfsr_reg = RegInit(B(Random.nextInt(1 << poly.order), poly.order bits))

    val lfsr = Seq.fill(i + 1)(Bits(poly.order bits))
    lfsr(0) := lfsr_reg
    for (j <- 0 until i) {
      lfsr(j + 1) := LFSR.Fibonacci(lfsr(j), poly)
    }
    lfsr_reg := LFSR.Fibonacci(lfsr(i), poly)
    lfsr_reg.resize(counterWidth bits)
  }
  rand := all_lfsr.reduce(_ ^ _)

  val state = RegInit(PoissonGeneratorState.sReady)
  val currentThreshold = RegInit(U(0, counterWidth bits))
  val currentNumber = RegInit(U(0, counterWidth bits))
  val currentCycles = RegInit(U(0, outputWidth bits))

  // default
  io.req.ready := False
  io.resp.valid := False
  io.resp.res := currentCycles

  switch(state) {
    is(PoissonGeneratorState.sReady) {
      io.req.ready := True
      when(io.req.fire) {
        state := PoissonGeneratorState.sAccumulate
        currentThreshold := io.req.threshold
        currentNumber := rand.asUInt
        currentCycles := 0
      }
    }
    is(PoissonGeneratorState.sAccumulate) {
      when(currentNumber < currentThreshold) {
        state := PoissonGeneratorState.sDone
      } otherwise {
        currentCycles := currentCycles +| 1
      }
      currentNumber := (currentNumber * rand.asUInt) >> counterWidth
    }
    is(PoissonGeneratorState.sDone) {
      io.resp.valid := True
      when(io.resp.fire) {
        state := PoissonGeneratorState.sReady
      }
    }
  }
}
