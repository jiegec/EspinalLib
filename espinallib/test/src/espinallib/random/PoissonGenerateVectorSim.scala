package espinallib.random

import spinal.core.sim.{
  SimBitVectorPimper,
  SimBoolPimper,
  SimConfig,
  SimClockDomainHandlePimper
}
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.ArrayBuffer

class PoissonGeneratorVectorSim extends AnyFunSuite {
  test("PoissonGeneratorVector") {
    val counterWidth = 16
    val outputWidth = 3
    val lanes = 4
    SimConfig.withWave.withIVerilog
      .addSimulatorFlag("-g2012")
      .doSim(
        new PoissonGeneratorVector(
          counterWidth,
          outputWidth,
          lanes
        )
      ) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.io.req.valid #= false
        dut.io.resp.ready #= false
        dut.clockDomain.waitRisingEdge()

        // threshold = exp(-lambda)*(2^counterWidth)
        val lambda = 1.0
        for (i <- 0 until lanes) {
          dut.io.req.payload(i).threshold #= ((1 << counterWidth) * Math.exp(
            -lambda
          )).toInt
        }

        val outputValues = 1 << outputWidth
        val counters = ArrayBuffer.fill(outputValues)(0)
        val samples = 2048
        for (_ <- 0 until samples) {
          dut.io.req.valid #= true
          dut.clockDomain.waitRisingEdgeWhere {
            dut.io.req.ready.toBoolean
          }
          dut.io.req.valid #= false

          dut.io.resp.ready #= true
          dut.clockDomain.waitRisingEdgeWhere {
            dut.io.resp.valid.toBoolean
          }
          for (i <- 0 until lanes) {
            val num = dut.io.resp.payload(i).res.toInt
            counters(num) += 1
          }
          dut.io.resp.ready #= false
        }

        var expected = 0.0
        // lambda=1
        // distribution: 0.368 0.368 0.184 0.061 0.015 0.003 0.0005
        for (i <- 0 until outputValues) {
          expected += i.toDouble * counters(i) / samples / lanes
          println(
            s"${i}: ${counters(i)} times (${counters(i).toDouble / samples / lanes})"
          )
        }
        println(s"Expected: ${expected}")
        assert(Math.abs(expected - lambda) / lambda < 0.1)
      }

  }
}
