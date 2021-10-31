package espinallib.zipcpu.blackbox

import espinallib.common.GenUtils
import espinallib.common.Resource
import spinal.core._

// blackbox of https://github.com/ZipCPU/wb2axip/blob/master/rtl/sfifo.v
class SFifoBlackBox(
    dataWidth: Int = 8,
    log2Size: Int = 4,
    asyncRead: Boolean = true,
    writeOnFull: Boolean = false,
    readOnEmpty: Boolean = false
) extends BlackBox {

  val io = new Bundle {
    val i_clk = in(Bool)
    val i_reset = in(Bool)
    // write interface
    val i_wr = in(Bool)
    val i_data = in(Bits(dataWidth bits))
    val o_full = out(Bool)
    val o_fill = out(UInt(log2Size + 1 bits))

    // read interface
    val i_rd = in(Bool)
    val o_data = out(Bits(dataWidth bits))
    val o_empty = out(Bool)
  }

  setBlackBoxName("sfifo")

  addGeneric("BW", dataWidth)
  addGeneric("LGFLEN", log2Size)
  addGeneric("OPT_ASYNC_READ", asyncRead)
  addGeneric("OPT_WRITE_ON_FULL", writeOnFull)
  addGeneric("OPT_READ_ON_EMPTY", readOnEmpty)

  // Map the clk
  mapCurrentClockDomain(clock = io.i_clk, reset = io.i_reset)

  noIoPrefix()

  addRTLPath(Resource.path("/wb2axip/rtl/sfifo.v"))
}

// wrapper for sfifo
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

  val dataWidth = widthOf(ty())

  val sFifo =
    new SFifoBlackBox(dataWidth, log2Size, asyncRead, writeOnFull, readOnEmpty)

  sFifo.io.i_wr := io.write
  sFifo.io.i_data := io.wData.asBits
  io.full := sFifo.io.o_full
  io.fill := sFifo.io.o_fill

  sFifo.io.i_rd := io.read
  io.rData.assignFromBits(sFifo.io.o_data)
  io.empty := sFifo.io.o_empty

}

object SyncFifoVerilog extends GenUtils {
  work(
    new SyncFifo(
      UInt(32 bits)
    )
  )
}
