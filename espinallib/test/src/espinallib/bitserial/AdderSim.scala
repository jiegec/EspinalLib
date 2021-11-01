package espinallib.bitserial

import spinal.core.sim.{
  SimBitVectorPimper,
  SimConfig,
  SimClockDomainHandlePimper
}
import org.scalatest.funsuite.AnyFunSuite

class AdderSim extends AnyFunSuite {
  test("Adder") {
    SimConfig.withWave.withIVerilog
      .addSimulatorFlag("-g2012")
      .doSim(
        new Adder()
      ) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.a #= 0
        dut.io.b #= 0
        dut.clockDomain.waitRisingEdge()

        // c = 0
        dut.io.a #= 1
        dut.io.b #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 1)

        // c = 0
        dut.io.a #= 1
        dut.io.b #= 1
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)

        // c = 1
        dut.io.a #= 1
        dut.io.b #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)

        // c = 1
        dut.io.a #= 0
        dut.io.b #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 1)

        // c = 0
        dut.io.a #= 1
        dut.io.b #= 1
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)

        // c = 1
        dut.io.a #= 1
        dut.io.b #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)

        // c = 1
        dut.io.a #= 0
        dut.io.b #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 1)

        // c = 0
        dut.io.a #= 0
        dut.io.b #= 0
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.sum.toBigInt == 0)
      }
  }
}
