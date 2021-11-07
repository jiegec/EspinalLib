package espinallib

import espinallib.formal.FormalUtils.doFormal
import espinallib.formal.FormalUtils.genPast
import espinallib.formal.FormalUtils.slaveAssume
import spinal.core.Formal.past
import spinal.core.Formal.stable
import spinal.core._

import scala.collection.mutable

package object formal {

  /** Helper class for Stream formal verification.
    *
    * @param stream
    *   Stream interface
    */
  implicit class StreamFormal[D <: Data](stream: spinal.lib.Stream[D]) {

    /** Assert that stream is valid until ready rises
      */
    def validBeforeReady(slave: Boolean = true, dataStable: Boolean = true) {
      implicit def isSlave: Boolean = slave

      doFormal { (outerReset, pastValid) =>
        when(pastValid && ~outerReset) {
          // When valid goes high, data is stable and valid stays high before ready
          when(past(stream.valid && ~stream.ready && ~outerReset)) {
            slaveAssume(stream.valid);
            if (dataStable) {
              slaveAssume(stable(stream.payload.asBits));
            }
          }
        }
      }
    }

    /** Assert that stream is not valid in the cycle right after reset falls
      */
    def notValidAfterReset(slave: Boolean = true) {
      implicit def isSlave: Boolean = slave

      doFormal { (outerReset, pastValid) =>
        // Valid is low in the first cycle after reset falls
        when(pastValid && past(outerReset) && ~outerReset) {
          slaveAssume(~stream.valid);
        }
      }
    }

    /** Add cover property that stream flows in several cycles continuously
      *
      * @param cycles
      *   The number of cycles
      */
    def flowContinuously(
        cycles: Int,
        forcePayloadDifferent: Boolean = true
    ): Unit = {
      doFormal { (outerReset, pastValid) =>
        // data flows in cycles continuously
        val payload: Data = if (forcePayloadDifferent) stream.payload else null
        cover(
          pastValid && genPast(pastValid, null, cycles) && genPast(
            ~outerReset,
            null,
            cycles
          ) && genPast(stream.fire, payload, cycles)
        )
      }
    }
  }

  object FormalUtils {

    val resetCache: mutable.HashMap[Component, Bool] =
      new mutable.HashMap[Component, Bool]()
    val validCache: mutable.HashMap[Component, Bool] =
      new mutable.HashMap[Component, Bool]()

    /** Do formal verification
      *
      * @param work
      *   a function of signature (outerReset, pastValid) => Unit
      */
    def doFormal(work: (Bool, Bool) => Unit) {
      val clockDomain = ClockDomain.current
      GenerationFlags.formal {
        val outerReset = resetCache.getOrElseUpdate(
          Component.current,
          clockDomain.readResetWire
        )
        new ClockingArea(FormalUtils.initialClockDomain) {
          // don't use $pass in first cycle
          val pastValid = validCache.get(Component.current) match {
            case Some(pastValid) => pastValid
            case None => {
              val pastValid =
                RegNext(True).init(False).setName("pastValid")
              validCache(Component.current) = pastValid

              // Reset properties
              // the reset signal is positive in the first cycle
              when(!pastValid) {
                assume(outerReset)
              }

              // if asserted, the reset signal must be asserted for a minimum of 8 cycles
              val resetCounter =
                Reg(UInt(3 bits)).init(0).setName("resetCounter")
              when(!outerReset) {
                resetCounter := 0
              } elsewhen (!resetCounter.andR) {
                resetCounter := resetCounter + 1
              }
              when(resetCounter =/= 0 && !resetCounter.andR) {
                assume(outerReset)
              }
              pastValid
            }
          }

          work(outerReset, pastValid)
        }
      }
    }

    /** Create clock domain with BOOT reset
      *
      * @return
      *   Initial clock domain
      */
    def initialClockDomain: ClockDomain = {
      ClockDomain(
        ClockDomain.current.clock,
        config = ClockDomainConfig(
          resetKind = BOOT
        )
      )
    }

    /** Generate condition for cover properties
      *
      * @param fire
      *   Fire signal to keep high for several cycles
      * @param data
      *   Data signal to change for several cycles
      * @param cycles
      *   the number of cycles
      * @return
      *   Condition
      */
    def genPast(fire: Bool, data: Data, cycles: Int): Bool = {
      var resFire = True
      var resData = True
      for (i <- 1 to cycles) {
        resFire = resFire && past(fire, i)
        // no duplicate data
        if (data != null) {
          for (j <- 1 until i) {
            resData = resData && past(data, i) =/= past(data, j)
          }
        }
      }
      resFire && resData
    }

    /** Do assume for slave, assert for master
      *
      * @param assertion
      *   The assertion or assumption
      * @param slave
      *   Whether this is slave
      * @return
      *   statement
      */
    def slaveAssume(assertion: Bool)(implicit slave: Boolean) = {
      if (slave) {
        assume(assertion)
      } else {
        assert(assertion)
      }
    }

    /** Do assert for slave, assume for master
      *
      * @param assertion
      *   The assertion or assumption
      * @param slave
      *   Whether this is slave
      * @return
      *   statement
      */
    def slaveAssert(assertion: Bool)(implicit slave: Boolean) = {
      if (slave) {
        assert(assertion)
      } else {
        assume(assertion)
      }
    }
  }

}
