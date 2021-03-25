package espinallib.rewrite

import espinallib.common.GenUtils
import espinallib.rewrite
import espinallib.rewrite.SkidBuffer2Verilog.work
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._

object BuffetAction extends SpinalEnum {
  val Read, Update, Shrink = newElement()
}

object BuffetState extends SpinalEnum {
  val sReady, sWait, sStall = newElement()
}

class BuffetDownstream(idxWidth: Int, dataWidth: Int) extends Bundle {
  val action = BuffetAction()
  val idxOrSize = UInt(idxWidth bits)
  val data = Bits(dataWidth bits)
}

// simplified version of buffet
// https://github.com/cwfletcher/buffets/blob/master/dut/buffet_control.v
class Buffet(idxWidth: Int, dataWidth: Int)
    extends Component {
  val io = new Bundle {
    val fill = slave(Stream(Bits(dataWidth bits)))

    // elements left
    val credit = out(UInt((idxWidth + 1) bits))

    val downstream = slave(Stream(new BuffetDownstream(idxWidth, dataWidth)))

    val readData = master(Stream(Bits(dataWidth bits)))
  }

  // pipeline:
  // downstream -> readIdx -> readData

  val readDataStream = Stream(Bits(dataWidth bits))
  readDataStream.s2mPipe() <> io.readData

  val readIdxStream = Stream(UInt(idxWidth bits))

  val count = 1 << idxWidth
  val memory = Mem(Bits(dataWidth bits), count)

  val head = RegInit(U(0, idxWidth bits))
  val tail = RegInit(U(0, idxWidth bits))
  val occupancy = tail - head
  io.credit := U(count, (idxWidth + 1) bits) - occupancy

  val empty = occupancy === 0

  val readEvent =
    ~empty & io.downstream.valid && io.downstream.action === BuffetAction.Read
  val shrinkEvent =
    ~empty & io.downstream.valid && io.downstream.action === BuffetAction.Shrink
  val fillEvent = io.fill.valid
  val updateEvent =
    ~empty & io.downstream.valid && io.downstream.action === BuffetAction.Update

  val idxValid = False
  when(head <= tail) {
    idxValid := io.downstream.idxOrSize >= head && io.downstream.idxOrSize < tail
  }.otherwise {
    // the original version seems wrong?
    idxValid := io.downstream.idxOrSize >= head || io.downstream.idxOrSize < tail
  }

  io.fill.ready := True
  when(io.fill.valid) {
    memory.write(tail, io.fill.payload)
    tail := tail + 1
  }

  val state = RegInit(BuffetState.sReady)
  state.simPublic()

  val readIdx = UInt(idxWidth bits)
  readIdx := 0
  val memRead = memory.readSync(readIdx)
  val readIdxStage = RegInit(U(0, idxWidth bits))

  readIdxStream.payload := 0
  readIdxStream.valid := False

  io.downstream.ready := False
  switch(io.downstream.action) {
    is(BuffetAction.Update) {
      io.downstream.ready := True
      when(io.downstream.valid) {
        memory.write(io.downstream.idxOrSize + head, io.downstream.data)
      }
    }
    is(BuffetAction.Shrink) {
      io.downstream.ready := True
      when(io.downstream.valid) {
        head := head + io.downstream.idxOrSize
      }
    }
    is(BuffetAction.Read) {
      switch(state) {
        is(BuffetState.sReady) {
          when(~readIdxStream.ready) {
            // back pressure
            io.downstream.ready := False
          } otherwise {
            io.downstream.ready := True
            when(io.downstream.valid) {
              when(idxValid) {
                // no stall
                readIdxStream.payload := io.downstream.idxOrSize + head
                readIdxStream.valid := True
              } otherwise {
                // stall, wait for data
                state := BuffetState.sWait
                readIdxStage := io.downstream.idxOrSize + head
              }
            }
          }
        }
        is(BuffetState.sWait) {
          io.downstream.ready := False
          when(io.fill.fire && readIdxStage === tail) {
            // data is available
            readIdxStream.payload := readIdxStage
            readIdxStream.valid := True
            when(readIdxStream.ready) {
              state := BuffetState.sReady
            }
          }
        }
      }
    }
  }

  // readIdx -> readData
  val readDataStage = RegInit(B(0, dataWidth bits))
  val readDataValid = RegInit(False)
  val fireStage = RegNext(readIdxStream.fire)
  val fillStage = RegNext(io.fill.fire && readIdxStream.payload === tail)
  val fillDataStage = RegNext(io.fill.payload)

  readIdxStream.ready := False
  readDataStream.payload := 0
  // backpressure
  when(readDataStream.ready && ~readDataValid) {
    readIdxStream.ready := True
  }

  when(readIdxStream.fire) {
    readIdx := readIdxStream.payload
  }

  // handle fill and read on the same cycle
  val readOut = memRead
  when(fillStage) {
    readOut := fillDataStage
  }

  readDataStream.valid := fireStage || readDataValid

  when(readDataValid) {
    readDataStream.payload := readDataStage
  } elsewhen(fireStage) {
    readDataStream.payload := memRead
  }

  when(fireStage && ~readDataStream.ready) {
    readDataValid := True
    readDataStage := readOut
  }

  when(readDataStream.fire) {
    readDataValid := False
  }
}

object BuffetVerilog extends GenUtils {
  work(
    new Buffet(3, 32)
  )
}