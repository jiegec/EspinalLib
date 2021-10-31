package espinallib.zipcpu.rewrite

import espinallib.common.GenUtils
import espinallib.formal.FormalUtils.doFormal
import spinal.core.Formal.past
import spinal.core._

/** A synchronous fifo
  *
  * ref: https://github.com/ZipCPU/wb2axip/blob/master/rtl/sfifo.v
  *
  * @param ty
  *   data type
  * @param log2Size
  *   the log2 of fifo size
  * @param asyncRead
  *   whether read is async
  * @param writeOnFull
  *   support write on full
  * @param readOnEmpty
  *   support write on empty
  * @tparam T
  *   generic data type parameter
  */
class SyncFifo[T <: Data](
    ty: HardType[T],
    log2Size: Int = 4,
    asyncRead: Boolean = true,
    writeOnFull: Boolean = false,
    readOnEmpty: Boolean = false
) extends Component {
  val io = new Bundle {

    /** write enable */
    val write = in(Bool)

    /** write data */
    val wData = in(ty())

    /** whether the fifo is full */
    val full = out(Bool)

    /** the number of items in fifo currently */
    val fill = out(UInt((log2Size + 1) bits))

    /** read enable */
    val read = in(Bool)

    /** the data read */
    val rData = out(ty())

    /** whether the fifo is empty */
    val empty = out(Bool)
  }

  val size = 1 << log2Size

  // registers
  val rFull = Reg(Bool) init (False)
  val rEmpty = Reg(Bool) init (True)
  val mem = Mem(ty, size)
  val wrAddr = Reg(UInt((log2Size + 1) bits)) init (0)
  val rdAddr = Reg(UInt((log2Size + 1) bits)) init (0)
  val rdNext = UInt(log2Size bits)

  val doWrite = io.write && !io.full
  val doRead = io.read && !io.empty

  val oFill = Reg(UInt(log2Size + 1 bits)) init (0)
  io.fill := oFill
  when(doWrite && !doRead) {
    oFill := oFill + 1
  } elsewhen (!doWrite && doRead) {
    oFill := oFill - 1
  } otherwise {
    oFill := wrAddr - rdAddr
  }

  when(doWrite && !doRead) {
    rFull := oFill === U(size - 1)
  } elsewhen (!doWrite && doRead) {
    rFull := False
  } otherwise {
    rFull := oFill === U(size)
  }

  if (writeOnFull) {
    when(io.read) {
      io.full := False
    } otherwise {
      io.full := rFull
    }
  } else {
    io.full := rFull
  }

  when(doWrite) {
    wrAddr := wrAddr + 1
  }

  mem.write(wrAddr(log2Size - 1 downto 0), io.wData, enable = doWrite)

  when(doRead) {
    rdAddr := rdAddr + 1
  }

  rdNext := rdAddr(log2Size - 1 downto 0) + 1

  when(doWrite && !doRead) {
    rEmpty := False
  } elsewhen (!doWrite && doRead) {
    rEmpty := oFill <= 1
  }

  if (readOnEmpty) {
    when(Bool(readOnEmpty) && io.write) {
      io.empty := False
    } otherwise {
      io.empty := rEmpty
    }
  } else {
    io.empty := rEmpty
  }

  if (asyncRead && readOnEmpty) {
    io.rData := mem.readAsync(rdAddr(log2Size - 1 downto 0))
    when(rEmpty) {
      io.rData := io.wData
    }
  } else if (asyncRead) {
    io.rData := mem.readAsync(rdAddr(log2Size - 1 downto 0))
  } else {
    val bypassValid = Reg(Bool) init (False)
    when(!io.write) {
      bypassValid := False
    } elsewhen (rEmpty || (io.read && oFill === 1)) {
      bypassValid := True
    } otherwise {
      bypassValid := False
    }

    val bypassData = RegNext(io.rData)
    val rdData = Reg(ty())
    val addr = UInt(log2Size bits)
    when(doRead) {
      addr := rdNext
    } otherwise {
      addr := rdAddr(log2Size - 1 downto 0)
    }
    rdData := mem.readSync(addr)

    when(Bool(readOnEmpty) && rEmpty) {
      io.rData := io.wData
    } elsewhen (bypassValid) {
      io.rData := bypassData
    } otherwise {
      io.rData := rdData
    }
  }

  doFormal { (outerReset, pastValid) =>
    when(!outerReset) {
      val fFill = wrAddr - rdAddr
      val fEmpty = wrAddr === rdAddr
      val fNext = rdAddr + 1

      assert(fFill <= size)
      assert(io.fill === fFill)

      assert(rFull === (fFill === size))
      assert(rEmpty === (fFill === 0))
      assert(rdNext === fNext(log2Size - 1 downto 0))

      if (!writeOnFull) {
        assert(io.full === rFull)
      } else {
        assert(io.full === rFull && !io.read)
      }

      if (!readOnEmpty) {
        assert(io.empty === rEmpty)
      } else {
        assert(io.empty === rEmpty && !io.write)
      }

      when(!Bool(asyncRead) && pastValid) {
        when(fFill === 0) {
          assert(rEmpty)
          assert(io.empty || (Bool(readOnEmpty) && io.write))
        } elsewhen (past(fFill) > 1) {
          assert(!rEmpty)
        } elsewhen (past(!io.read && fFill > 0)) {
          assert(!rEmpty)
        }
      }

      when(!rEmpty) {
        assert(mem(rdAddr(log2Size - 1 downto 0)) === io.rData)
      } elsewhen (Bool(readOnEmpty)) {
        assert(io.rData === io.wData)
      }
    }

  }
}

object SyncFifoVerilog extends GenUtils {
  work(
    new SyncFifo(
      UInt(32 bits)
    )
  )
}
