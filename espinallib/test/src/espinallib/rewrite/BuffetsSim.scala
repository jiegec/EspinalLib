package espinallib.rewrite

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim.{
  SimBitVectorPimper,
  SimBoolPimper,
  SimClockDomainHandlePimper,
  SimConfig,
  SimEnumPimper,
  fork
}

import java.util
import java.util.concurrent.ConcurrentLinkedDeque
import java.util.concurrent.atomic.AtomicBoolean

class BuffetsSim extends AnyFunSuite {
  test("Buffets") {
    SimConfig.withWave.withIVerilog
      .addSimulatorFlag("-g2012")
      .doSim(
        new Buffets(3, Bits(32 bits))
      ) { dut =>
        dut.clockDomain.forkStimulus(period = 10)
        dut.clockDomain.waitRisingEdge(1)

        val done = new AtomicBoolean()
        val data = new util.ArrayList[Int]()
        val readAddress = new ConcurrentLinkedDeque[Int]()

        val fillMonitor = fork {
          while (!done.get()) {
            dut.clockDomain.waitRisingEdge()
            if (dut.io.fill.ready.toBoolean && dut.io.fill.valid.toBoolean) {
              val payload = dut.io.fill.payload.toInt
              data.synchronized {
                data.add(payload)
                printf(s"Fill: ${payload} -> ${data}\n")
              }
            }
          }
        }

        val downstreamMonitor = fork {
          while (!done.get()) {
            dut.clockDomain.waitRisingEdge()
            if (
              dut.io.downstream.ready.toBoolean && dut.io.downstream.valid.toBoolean
            ) {
              val idxOrSize = dut.io.downstream.idxOrSize.toInt
              if (dut.io.downstream.action.toEnum == BuffetsAction.Read) {
                printf(s"Read: ${idxOrSize}\n")
                readAddress.addLast(idxOrSize)
              } else if (
                dut.io.downstream.action.toEnum == BuffetsAction.Update
              ) {
                printf(
                  s"Update: ${dut.io.downstream.idxOrSize.toInt} ${dut.io.downstream.data.toInt}\n"
                )
                data.synchronized {
                  data.set(
                    dut.io.downstream.idxOrSize.toInt,
                    dut.io.downstream.data.toInt
                  )
                }
              } else if (
                dut.io.downstream.action.toEnum == BuffetsAction.Shrink
              ) {
                val shrink = dut.io.downstream.idxOrSize.toInt
                data.synchronized {
                  // remove first n elements
                  data.subList(0, shrink).clear()
                  printf(s"Shrink: ${shrink} -> ${data}\n")
                }
              }
            }
          }
        }

        val readDataMonitor = fork {
          while (!done.get()) {
            dut.clockDomain.waitRisingEdge()
            if (
              dut.io.readData.ready.toBoolean && dut.io.readData.valid.toBoolean
            ) {
              val read = dut.io.readData.payload.toInt
              printf(s"ReadData: ${read}\n")
              assert(!readAddress.isEmpty)
              val addr = readAddress.pollFirst()
              assert(data.get(addr) == read)
            }
          }
        }

        dut.io.fill.valid #= false
        dut.io.downstream.valid #= false
        dut.io.readData.ready #= true
        dut.clockDomain.waitRisingEdge()

        // read should block
        dut.io.downstream.valid #= true
        dut.io.downstream.action #= BuffetsAction.Read
        dut.io.downstream.idxOrSize #= 0
        dut.clockDomain.waitSamplingWhere {
          dut.io.downstream.ready.toBoolean
        }
        dut.io.downstream.valid #= false
        dut.clockDomain.waitRisingEdge(10)
        assert(dut.state.toEnum == BuffetsState.sWait)

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
        dut.io.downstream.action #= BuffetsAction.Read
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
        dut.io.downstream.action #= BuffetsAction.Read
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
        dut.io.downstream.action #= BuffetsAction.Read
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

        // shrink then read
        dut.io.downstream.valid #= true
        dut.io.downstream.action #= BuffetsAction.Shrink
        dut.io.downstream.idxOrSize #= 1
        dut.clockDomain.waitSamplingWhere {
          dut.io.downstream.ready.toBoolean
        }
        dut.io.downstream.action #= BuffetsAction.Read
        dut.io.downstream.idxOrSize #= 0
        dut.clockDomain.waitSamplingWhere {
          dut.io.downstream.ready.toBoolean
        }
        dut.io.downstream.valid #= false
        dut.io.readData.ready #= true
        dut.clockDomain.waitSamplingWhere {
          dut.io.readData.valid.toBoolean
        }
        assert(dut.io.readData.payload.toInt == 2)

        // test fill to full
        dut.io.fill.valid #= true
        for (i <- 5 until 10) {
          dut.io.fill.payload #= i
          dut.clockDomain.waitSamplingWhere {
            dut.io.fill.ready.toBoolean
          }
        }
        dut.io.fill.valid #= false
        dut.clockDomain.waitRisingEdge()
        assert(dut.io.credit.toInt == 0)

        // read last element
        dut.io.downstream.valid #= true
        dut.io.downstream.action #= BuffetsAction.Read
        dut.io.downstream.idxOrSize #= 7
        dut.io.readData.ready #= false
        dut.clockDomain.waitSamplingWhere {
          dut.io.downstream.ready.toBoolean
        }
        dut.io.downstream.valid #= false
        dut.io.readData.ready #= true
        dut.clockDomain.waitSamplingWhere {
          dut.io.readData.valid.toBoolean
        }
        assert(dut.io.readData.payload.toInt == 9)

        // update last element
        dut.io.downstream.valid #= true
        dut.io.downstream.action #= BuffetsAction.Update
        dut.io.downstream.idxOrSize #= 7
        dut.io.downstream.data #= 0
        dut.io.readData.ready #= false
        dut.clockDomain.waitSamplingWhere {
          dut.io.downstream.ready.toBoolean
        }

        // read last element
        dut.io.downstream.action #= BuffetsAction.Read
        dut.clockDomain.waitSamplingWhere {
          dut.io.downstream.ready.toBoolean
        }
        dut.io.readData.ready #= true
        dut.clockDomain.waitSamplingWhere {
          dut.io.readData.valid.toBoolean
        }
        assert(dut.io.readData.payload.toInt == 0)

        dut.io.fill.valid #= false
        dut.io.downstream.valid #= false
        done.set(true)
        fillMonitor.join()
        downstreamMonitor.join()
        readDataMonitor.join()

        dut.clockDomain.waitRisingEdge(10)
      }
  }
}
