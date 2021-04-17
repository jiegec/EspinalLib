package espinallib.zipcpu.blackbox

import espinallib.common.GenUtils
import spinal.core._

// blackbox of https://github.com/ZipCPU/wb2axip/blob/master/rtl/axi_addr.v
class AxiAddrBlackBox(
    addressWidth: Int = 8,
    dataWidth: Int = 8,
    axi3: Boolean = false
) extends BlackBox {

  val io = new Bundle {
    val i_last_addr = in(UInt(addressWidth bits))
    val i_size = in(UInt(3 bits))
    val i_burst = in(Bits(2 bits))
    val i_len = in(UInt(8 bits))

    val o_next_addr = out(UInt(addressWidth bits))
  }

  setDefinitionName("axi_addr")

  addGeneric("AW", addressWidth)
  addGeneric("DW", dataWidth)
  addGeneric("OPT_AXI3", axi3)

  noIoPrefix()

  addRTLPath("./submodules/wb2axip/rtl/axi_addr.v")
}

// wrapper for axi_addr
class AxiAddr(addrWidth: Int, dataWidth: Int, axi3: Boolean = false)
    extends Component {
  val io = new Bundle {

    /** previous address */
    val lastAddr = in(UInt(addrWidth bits))

    /** AxSIZE */
    val size = in(UInt(3 bits))

    /** AxBURST */
    val burst = in(Bits(2 bits))

    /** AxLEN */
    val len = in(UInt(8 bits))

    /** next address */
    val nextAddr = out(UInt(addrWidth bits))
  }

  val axiAddr =
    new AxiAddrBlackBox(addrWidth, dataWidth, axi3)
  axiAddr.io.i_last_addr := io.lastAddr
  axiAddr.io.i_size := io.size
  axiAddr.io.i_burst := io.burst
  axiAddr.io.i_len := io.len
  io.nextAddr := axiAddr.io.o_next_addr
}

object AxiAddrVerilog extends GenUtils {
  work(
    new AxiAddr(
      32,
      32
    )
  )
}
