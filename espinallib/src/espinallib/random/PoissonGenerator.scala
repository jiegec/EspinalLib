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

object PoissonGeneratorVectorState extends SpinalEnum {
  val sReady, sAccumulate, sDone = newElement()
}

class PoissonGeneratorVector(
    counterWidth: Int,
    outputWidth: Int,
    lanes: Int
) extends Component {
  val io = new Bundle {
    val req = slave(
      Stream(Vec(new PoissonGeneratorRequest(counterWidth), lanes))
    )
    val resp = master(
      Stream(Vec(new PoissonGeneratorResponse(outputWidth), lanes))
    )
  }

  val inner = for (i <- 0 until lanes) yield {
    val inner = new PoissonGenerator(counterWidth, outputWidth)
    inner.io.req.threshold := io.req.payload(i).threshold
    inner.io.req.valid := False
    inner.io.resp.ready := False
    io.resp.payload(i).res := inner.io.resp.res

    inner
  }

  val state = RegInit(PoissonGeneratorVectorState.sReady)

  io.req.ready := False
  io.resp.valid := False

  switch(state) {
    is(PoissonGeneratorVectorState.sReady) {
      // until all ready
      io.req.ready := inner.map((m) => m.io.req.ready).reduce(_ & _)
      when(io.req.fire) {
        inner.foreach((m) => m.io.req.valid := True)
      }
      state := PoissonGeneratorVectorState.sAccumulate
    }
    is(PoissonGeneratorState.sAccumulate) {
      io.resp.valid := inner.map((m) => m.io.resp.valid).reduce(_ & _)
      when(io.resp.fire) {
        inner.foreach((m) => m.io.resp.ready := True)
      }
      state := PoissonGeneratorVectorState.sReady
    }
  }
}
