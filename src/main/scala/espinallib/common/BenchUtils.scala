package espinallib.common

import spinal.core._
import spinal.lib.eda.bench._
import spinal.lib.eda.xilinx.VivadoFlow
import scala.collection.mutable.ArrayBuffer

trait VerilogBench extends App {
  def bench[T <: Component](gen: => T) {
    val targets = ArrayBuffer[Target]()
    val vivadoPath = "/opt/Xilinx/Vivado/2019.2/bin"

    for (
      (family, device) <- Seq(
        ("Artix 7", "xc7a200tfbg676-3"),
        ("Kintex 7", "xc7k325tiffg900-2L")
      )
    ) {
      for (
        (freq, name) <- Seq(
          (50 MHz, "area"),
          (400 MHz, "fmax")
        )
      ) {
        targets += new Target {
          override def getFamilyName(): String = family
          override def synthesise(rtl: Rtl, workspace: String): Report = {
            VivadoFlow(
              frequencyTarget = freq,
              vivadoPath = vivadoPath,
              workspacePath = s"${workspace}_${name}",
              rtl = rtl,
              family = getFamilyName(),
              device = device
            )
          }
        }
      }
    }

    Bench(Seq(Rtl(SpinalVerilog(gen))), targets, "/tmp/")
  }
}
