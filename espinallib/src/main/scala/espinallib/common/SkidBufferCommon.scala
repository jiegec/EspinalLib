package espinallib.common

import espinallib.formal.StreamFormal
import spinal.core.Bundle
import spinal.core.Component
import spinal.core.Data
import spinal.lib.Stream
import spinal.lib.master
import spinal.lib.slave

// Related reading:
// https://electronics.stackexchange.com/questions/481603/understanding-skid-buffer-mechanism
// https://www.cs.upc.edu/~jordicf/gavina/BIB/files/PhD_Galceran.pdf

class SkidBufferCommon[T <: Data](
    gen: => T
) extends Component {
  val io = new Bundle {
    val s = slave(Stream(gen))
    val m = master(Stream(gen))
  }

  // formal properties
  io.s.validBeforeReady()
  io.m.validBeforeReady(slave = false)
  io.s.notValidAfterReset()
  io.m.notValidAfterReset(slave = false)
  io.s.flowContinuously(4)
  io.m.flowContinuously(4)
}
