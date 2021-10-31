package espinallib.rewrite

import espinallib.common.GenUtils
import espinallib.common.VerilogBench
import spinal.core._
import spinal.core.sim.SimDataPimper
import spinal.lib._
import spinal.lib.eda.bench.Bench

object BuffetsAction extends SpinalEnum {
  val Read, Update, Shrink = newElement()
}

object BuffetsState extends SpinalEnum {
  val sReady, sWait, sStall = newElement()
}

class BuffetsDownstream(idxWidth: Int, dataWidth: Int) extends Bundle {
  val action = BuffetsAction()
  val idxOrSize = UInt(idxWidth bits)
  val data = Bits(dataWidth bits)
}

// simplified version of buffet
// https://github.com/cwfletcher/buffets/blob/master/dut/buffet_control.v
class Buffets(idxWidth: Int, dataWidth: Int) extends Component {
  val io = new Bundle {
    val fill = slave(Stream(Bits(dataWidth bits)))

    // elements left
    val credit = out(UInt((idxWidth + 1) bits))

    val downstream = slave(Stream(new BuffetsDownstream(idxWidth, dataWidth)))

    val readData = master(Stream(Bits(dataWidth bits)))
  }

  // pipeline:
  // downstream -> readIdx -> readData

  val readDataStream = Stream(Bits(dataWidth bits))
  readDataStream.s2mPipe() <> io.readData

  val readIdxStream = Stream(UInt(idxWidth bits))

  val count = 1 << idxWidth
  val memory = Mem(Bits(dataWidth bits), count)

  // An extra bit to tell difference between empty and full
  val head = RegInit(U(0, (idxWidth + 1) bits))
  val tail = RegInit(U(0, (idxWidth + 1) bits))
  val occupancy = RegInit(U(0, (idxWidth + 1) bits))
  io.credit := U(count, (idxWidth + 1) bits) - occupancy

  val empty = ~occupancy.orR

  val readEvent =
    ~empty & io.downstream.valid && io.downstream.action === BuffetsAction.Read
  val shrinkEvent =
    ~empty & io.downstream.valid && io.downstream.action === BuffetsAction.Shrink
  val fillEvent = io.fill.valid
  val updateEvent =
    ~empty & io.downstream.valid && io.downstream.action === BuffetsAction.Update

  val idxValid = io.downstream.idxOrSize < occupancy

  // non zero
  io.fill.ready := io.credit.orR
  when(io.fill.valid) {
    memory.write(tail.resize(idxWidth bits), io.fill.payload)
    tail := tail + 1
  }

  val occupancyAdd = U(0, (idxWidth + 1) bits)
  val occupancySub = U(0, (idxWidth + 1) bits)
  when(io.fill.valid) {
    occupancyAdd := 1
  }
  when(io.downstream.valid && io.downstream.action === BuffetsAction.Shrink) {
    occupancySub := io.downstream.idxOrSize.resized
  }
  occupancy := occupancy + occupancyAdd - occupancySub

  val state = RegInit(BuffetsState.sReady)
  state.simPublic()

  val readWriteIdx = UInt(idxWidth bits)
  readWriteIdx.assignDontCare()
  val writeEnable = False
  val memRead =
    memory.readWriteSync(readWriteIdx, io.downstream.data, True, writeEnable)
  val readIdxStage = RegInit(U(0, idxWidth + 1 bits))

  // improve timing
  readIdxStream.payload.assignDontCare()
  readIdxStream.valid := False
  readWriteIdx := readIdxStream.payload

  io.downstream.ready := False
  switch(io.downstream.action) {
    is(BuffetsAction.Update) {
      io.downstream.ready := True
      when(io.downstream.valid) {
        writeEnable := True
        readWriteIdx := (io.downstream.idxOrSize + head).resize(idxWidth bits)
      }
    }
    is(BuffetsAction.Shrink) {
      io.downstream.ready := True
      when(io.downstream.valid) {
        head := head + io.downstream.idxOrSize
      }
    }
    is(BuffetsAction.Read) {
      switch(state) {
        is(BuffetsState.sReady) {
          // calculate address early to improve timing
          readIdxStream.payload := (io.downstream.idxOrSize + head)
            .resize(idxWidth bits)
          when(~readIdxStream.ready) {
            // back pressure
            io.downstream.ready := False
          } otherwise {
            io.downstream.ready := True
            when(io.downstream.valid) {
              when(
                idxValid || (io.fill.fire && io.downstream.idxOrSize === occupancy)
              ) {
                // no stall
                readIdxStream.valid := True
              } otherwise {
                // stall, wait for data
                state := BuffetsState.sWait
                readIdxStage := (io.downstream.idxOrSize + head).resized
              }
            }
          }
        }
      }
    }
  }

  switch(state) {
    is(BuffetsState.sWait) {
      io.downstream.ready := False
      // improve timing
      readIdxStream.payload := readIdxStage.resized
      when(io.fill.fire && readIdxStage === tail) {
        // data is available
        readIdxStream.valid := True
        when(readIdxStream.ready) {
          state := BuffetsState.sReady
        }
      }
    }
  }

  // readIdx -> readData
  val readDataStage = Reg(Bits(dataWidth bits))
  val readDataValid = RegInit(False)
  val fireStage = RegNext(readIdxStream.fire)
  val fillStage = RegNext(io.fill.fire && readIdxStream.payload === tail)
  val fillDataStage = RegNext(io.fill.payload)

  readIdxStream.ready := False
  // backpressure
  when(readDataStream.ready && ~readDataValid) {
    readIdxStream.ready := True
  }

  // handle fill and read on the same cycle
  val readOut = memRead
  when(fillStage) {
    readOut := fillDataStage
  }

  readDataStream.payload.assignDontCare()
  readDataStream.valid := fireStage || readDataValid
  when(readDataValid) {
    readDataStream.payload := readDataStage
  } elsewhen (fireStage) {
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

object BuffetsVerilog extends GenUtils {
  work(
    new Buffets(3, 32)
  )
}

object BuffetsBench extends VerilogBench {
  bench(
    new Buffets(3, 32)
  )
}

object BuffetsLargeBench extends VerilogBench {
  bench(
    Bench.compressIo(
      new Buffets(6, 128)
    )
  )
}
