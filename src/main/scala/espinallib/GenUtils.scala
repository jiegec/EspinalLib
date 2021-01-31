package espinallib

import spinal.core._

trait GenUtils extends App {
  def work[T <: Component](gen: => T, netlistName: String = null): Unit = {
    // verilog
    val verilog = SpinalConfig(
      netlistFileName = netlistName match {
        case null => null
        case s => s"${s}.v"
      }
    )
    verilog.generateVerilog(gen)

    // for formal verification
    val formal = SpinalConfig(
      defaultConfigForClockDomains = ClockDomainConfig(
        resetKind = SYNC
      ),
      netlistFileName = netlistName match {
        case null => null
        case s => s"${s}.sv"
      }
    )
    formal.includeFormal.generateSystemVerilog(gen)
  }
}
