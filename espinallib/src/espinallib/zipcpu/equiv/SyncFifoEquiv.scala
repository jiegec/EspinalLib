package espinallib.zipcpu.equiv

import espinallib.common.GenUtils
import espinallib.formal.FormalUtils.doFormal
import espinallib.zipcpu.blackbox
import espinallib.zipcpu.rewrite
import spinal.core._

// verify the equivalence between two impls
class SyncFifoEquiv(
    dataWidth: Int = 8,
    log2Size: Int = 4,
    asyncRead: Boolean = true,
    writeOnFull: Boolean = false,
    readOnEmpty: Boolean = false
) extends Component {
  val io = new Bundle {
    // write interface
    val write = in(Bool)
    val wData = in(Bits(dataWidth bits))

    // read interface
    val read = in(Bool)
  }

  val theirs =
    new blackbox.SyncFifo(
      Bits(dataWidth bits),
      log2Size,
      asyncRead,
      writeOnFull,
      readOnEmpty
    )
  theirs.io.write := io.write
  theirs.io.wData := io.wData
  theirs.io.read := io.read

  val ours = new rewrite.SyncFifo(
    Bits(dataWidth bits),
    log2Size,
    asyncRead,
    writeOnFull,
    readOnEmpty
  )
  ours.io.write := io.write
  ours.io.wData := io.wData
  ours.io.read := io.read

  doFormal { (outerReset, _) =>
    when(~outerReset) {
      when(!Bool(readOnEmpty) && theirs.io.empty) {
        assume(!io.read)
      }
      when(!Bool(writeOnFull) && theirs.io.full) {
        assume(!io.write)
      }

      assert(theirs.io.full === ours.io.full)
      assert(theirs.io.fill === ours.io.fill)
      when(io.read) {
        assert(theirs.io.rData === ours.io.rData)
      }
      assert(theirs.io.empty === ours.io.empty)
    }
  }
}

object SyncFifoEquivVerilog extends GenUtils {
  work(
    new SyncFifoEquiv(
      32
    )
  )
}
