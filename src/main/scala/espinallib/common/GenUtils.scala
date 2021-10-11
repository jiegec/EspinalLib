package espinallib.common

import spinal.core.{ClockDomainConfig, Component, SYNC, SpinalConfig}
import java.nio.file.Paths
import java.nio.file.Files

trait GenUtils extends App {
  def work[T <: Component](gen: => T, netlistName: String = null): Unit = {
    // verilog
    val verilog = SpinalConfig(
      netlistFileName = netlistName match {
        case null => null
        case s    => s"${s}.v"
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
        case s    => s"${s}.sv"
      }
    )
    val report = formal.includeFormal.generateSystemVerilog(gen)
    report.mergeRTLSource()
  }
}

object Resource {
  def path(name: String) = {
    val tmp = Files.createTempDirectory("resource");
    val path = tmp.resolve(Paths.get(name).getFileName())

    val is = getClass().getResourceAsStream(name)
    Files.copy(is, path)
    path.toFile().getAbsolutePath()
  }
}
