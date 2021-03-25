package espinallib.rewrite

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim.{SimBitVectorPimper, SimBoolPimper, SimClockDomainHandlePimper, SimConfig, SimEnumPimper}

class BuffetSim extends AnyFunSuite {
  test("Buffet") {
    SimConfig.withWave.withIVerilog
      .addSimulatorFlag("-g2012")
      .doSim(
        new Buffet(3, 32)
      ) { dut =>
        dut.clockDomain.forkStimulus(period = 10)

        dut.io.fill.valid #= false
        dut.io.downstream.valid #= false
        dut.io.readData.ready #= true
        dut.clockDomain.waitRisingEdge()

        // read should block
        dut.io.downstream.valid #= true
        dut.io.downstream.action #= BuffetAction.Read
        dut.io.downstream.idxOrSize #= 0
        dut.clockDomain.waitSamplingWhere {
          dut.io.downstream.ready.toBoolean
        }
        dut.io.downstream.valid #= false
        dut.clockDomain.waitRisingEdge(10)
        assert(dut.state.toEnum == BuffetState.sWait)

        // fill
        dut.io.fill.valid #= true
        dut.io.fill.payload #= 1
        dut.clockDomain.waitSamplingWhere {
          dut.io.fill.ready.toBoolean
        }
        dut.io.fill.valid #= false

        // read should return
        dut.clockDomain.waitRisingEdge(1)
        assert(dut.io.readData.valid.toBoolean)
        assert(dut.io.readData.payload.toInt == 1)

        // read again should not block
        dut.io.downstream.valid #= true
        dut.io.downstream.action #= BuffetAction.Read
        dut.io.downstream.idxOrSize #= 0
        dut.clockDomain.waitRisingEdge()
        dut.io.downstream.valid #= false
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.readData.valid.toBoolean)
        assert(dut.io.readData.payload.toInt == 1)

        // fill more data
        dut.io.fill.valid #= true
        for (i <- 2 until 5) {
          dut.io.fill.payload #= i
          dut.clockDomain.waitSamplingWhere {
            dut.io.fill.ready.toBoolean
          }
        }
        dut.io.fill.valid #= false

        // read continuously should not block
        dut.io.downstream.valid #= true
        dut.io.downstream.action #= BuffetAction.Read
        dut.io.downstream.idxOrSize #= 0
        dut.clockDomain.waitRisingEdge()
        for (i <- 1 until 4) {
          dut.io.downstream.idxOrSize #= i
          dut.clockDomain.waitRisingEdge()
          assert(dut.io.readData.valid.toBoolean)
          assert(dut.io.readData.payload.toInt == i)
        }
        dut.io.downstream.valid #= false
        dut.clockDomain.waitRisingEdge()

        // test backpressure
        dut.io.downstream.valid #= true
        dut.io.downstream.action #= BuffetAction.Read
        dut.io.downstream.idxOrSize #= 0
        dut.io.readData.ready #= false
        dut.clockDomain.waitRisingEdge()
        dut.io.downstream.idxOrSize #= 1
        dut.clockDomain.waitRisingEdge()
        dut.io.downstream.idxOrSize #= 2
        dut.clockDomain.waitRisingEdge(3)
        dut.io.readData.ready #= true
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.readData.payload.toInt == 1)
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.readData.payload.toInt == 2)
        dut.clockDomain.waitRisingEdge()
        dut.io.downstream.valid #= false
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.readData.payload.toInt == 3)

        dut.clockDomain.waitRisingEdge(10)
      }
  }
}
