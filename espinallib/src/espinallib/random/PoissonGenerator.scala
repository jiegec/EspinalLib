package espinallib.random

import spinal.core._
import spinal.lib._

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
    outputWidth: Int
) extends Component {
  val io = new Bundle {
    val req = slave(Stream(new PoissonGeneratorRequest(counterWidth)))
    val resp = master(Stream(new PoissonGeneratorResponse(outputWidth)))
  }

  val rand = Bits(counterWidth bits)
  val rng = new LFSRRNG32()
  rand := rng.io.random.resized

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
