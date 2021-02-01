package espinallib.zipcpu.equiv

import espinallib.GenUtils
import espinallib.formal.FormalUtils.doFormal
import espinallib.zipcpu.{blackbox, rewrite}
import spinal.core._

// verify the equivalence between two impls
class AxiAddrEquiv(
                    addressWidth: Int = 8,
                    dataWidth: Int = 8
                  ) extends Component {
  val io = new Bundle {
    val lastAddr = in(UInt(addressWidth bits))
    val size = in(UInt(3 bits))
    val burst = in(Bits(2 bits))
    val len = in(UInt(8 bits))
  }

  val theirs =
    new blackbox.AxiAddr(addressWidth, dataWidth)
  theirs.io.lastAddr := io.lastAddr
  theirs.io.size := io.size
  theirs.io.burst := io.burst
  theirs.io.len := io.len

  val ours = new rewrite.AxiAddr(
    addressWidth,
    dataWidth
  )

  ours.io.lastAddr := io.lastAddr
  ours.io.size := io.size
  ours.io.burst := io.burst
  ours.io.len := io.len

  doFormal { (outerReset, pastValid) =>
    when(~outerReset) {
      assert(theirs.io.nextAddr === ours.io.nextAddr)
    }
  }
}

object AxiAddrEquivVerilog extends GenUtils {
  work(
    new AxiAddrEquiv(
      32, 32
    )
  )
}
