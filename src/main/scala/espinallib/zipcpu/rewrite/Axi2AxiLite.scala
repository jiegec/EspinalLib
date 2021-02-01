package espinallib.zipcpu.rewrite

import espinallib.common.GenUtils
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4.resp.SLVERR
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SpecRenamer}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SpecRenamer, AxiLite4W}

case class Axi2AxiLiteFifoBundle(idWidth: Int) extends Bundle {
  val id = UInt(idWidth bits)
  val len = UInt(8 bits)
}

/** Convert from AXI4(slave) to AXI4-Lite(master)
 */
class Axi2AxiLite(axiCfg: Axi4Config, axiLiteCfg: AxiLite4Config)
  extends Component {
  val s = slave(Axi4(axiCfg))
  val m = master(AxiLite4(axiLiteCfg))

  Axi4SpecRenamer(s)
  AxiLite4SpecRenamer(m)
  assert(axiCfg.dataWidth == axiLiteCfg.dataWidth, "Data width must match")

  val write = new Area {
    // pipes
    val sAwPipe = s.aw.s2mPipe()
    val sWPipe = s.w.s2mPipe()
    val mWPipe = Stream(AxiLite4W(axiLiteCfg))
    mWPipe.m2sPipe() >> m.w
    val mBPipe = m.b.s2mPipe()

    // registers
    val mAxiAwValid = Reg(Bool) init (False)
    val sAxiWReady = Reg(Bool) init (False)
    // track current aw info
    val axiAwAddr = Reg(cloneOf(s.aw.addr))
    val axiAwLen = Reg(cloneOf(s.aw.len))
    val axiBLen = Reg(cloneOf(s.aw.len))
    val axiAwBurst = Reg(cloneOf(s.aw.burst))
    val axiAwSize = Reg(cloneOf(s.aw.size))
    // next write addr
    val nextWriteAddr = Reg(cloneOf(s.aw.addr))
    val wFifoCount = UInt(5 bits)
    val wFifoFull = Bool()
    val wFifoEmpty = Bool()
    val wFifoBCount = UInt(8 bits)
    val wFifoBId = UInt(axiCfg.idWidth bits)
    val bCounts = Reg(UInt(9 bits)) init (0)
    val axiBId = Reg(UInt(axiCfg.idWidth bits)) init (0)
    val bId = Reg(UInt(axiCfg.idWidth bits)) init (0)
    val axiBResp = Reg(Bits(2 bits)) init (0)
    val sAxiBValid = Reg(Bool) init (False)
    val readFromWrFifo = Bool()

    // m.aw.valid
    when(sAwPipe.fire) {
      mAxiAwValid := True
    } elsewhen (m.aw.ready && axiAwLen === 0) {
      mAxiAwValid := False
    }

    sAwPipe.ready := (!m.aw.valid || (axiAwLen === 0 && m.aw.ready)) && !wFifoFull && (!s.w.ready || (sWPipe.valid && sWPipe.last && sWPipe.ready))

    when(sAwPipe.fire) {
      // save aw info when s.aw.fire
      axiAwAddr := sAwPipe.addr
      axiBLen := sAwPipe.len
      axiAwBurst := sAwPipe.burst
      axiAwSize := sAwPipe.size
    } elsewhen (m.aw.fire) {
      // advance to next addr when m.aw.fire
      axiAwAddr := nextWriteAddr
    }

    when(sAwPipe.fire) {
      axiAwLen := sAwPipe.len
    } elsewhen (m.aw.fire && axiAwLen > 0) {
      // count beats down
      axiAwLen := axiAwLen - 1
    }

    val calcWrAddr = new AxiAddr(axiCfg.addressWidth, axiCfg.dataWidth)
    calcWrAddr.io.lastAddr := axiAwAddr
    calcWrAddr.io.size := axiAwSize
    calcWrAddr.io.burst := axiAwBurst
    calcWrAddr.io.len := axiBLen
    nextWriteAddr := calcWrAddr.io.nextAddr

    when(s.aw.fire) {
      sAxiWReady := True
    } elsewhen (s.w.fire && s.w.last) {
      sAxiWReady := False
    }

    mWPipe.payload.data := sWPipe.payload.data
    mWPipe.payload.strb := sWPipe.payload.strb
    mWPipe.valid := sWPipe.valid && sAxiWReady
    sWPipe.ready := sAxiWReady && mWPipe.ready

    readFromWrFifo := (bCounts <= 1) && !wFifoEmpty && mBPipe.fire

    var wrFifo = new SyncFifo(Axi2AxiLiteFifoBundle(axiCfg.idWidth))
    wrFifo.io.write := sAwPipe.fire
    wrFifo.io.wData.id := sAwPipe.id
    wrFifo.io.wData.len := sAwPipe.len
    wFifoFull := wrFifo.io.full
    wFifoCount := wrFifo.io.fill
    wrFifo.io.read := readFromWrFifo
    wFifoBId := wrFifo.io.rData.id
    wFifoBCount := wrFifo.io.rData.len
    wFifoEmpty := wrFifo.io.empty

    when(readFromWrFifo) {
      bCounts := wFifoBCount + bCounts
    } elsewhen (mBPipe.fire) {
      bCounts := bCounts - 1
    }

    when(readFromWrFifo) {
      bId := wFifoBId
    }

    when(!s.b.valid || s.b.ready) {
      axiBId := Mux(readFromWrFifo && bCounts === 0, wFifoBId, bId)
    }

    when(mBPipe.fire) {
      sAxiBValid := (bCounts === 1) || (bCounts === 0 && (!wFifoEmpty) && wFifoBCount === 0)
    } elsewhen (s.b.ready) {
      sAxiBValid := False
    }

    when(s.b.fire) {
      when(mBPipe.fire) {
        axiBResp := mBPipe.resp
      } otherwise {
        axiBResp := 0
      }
    } elsewhen (mBPipe.fire) {
      axiBResp := SpinalMap(
        s.b.resp ## mBPipe.resp,
        M"--0-" -> s.b.resp,
        M"0-1-" -> mBPipe.resp,
        M"1-10" -> SLVERR,
        B"1011" -> SLVERR,
        B"1111" -> mBPipe.resp,
        default -> SLVERR
      )
    }

    m.aw.valid := mAxiAwValid
    m.aw.addr := axiAwAddr
    m.aw.prot := 0

    mBPipe.ready := (bCounts > 0) || !wFifoEmpty && (!s.b.valid || s.b.ready)
    s.b.id := axiBId
    s.b.resp := axiBResp
    s.b.valid := sAxiBValid
  }

  val read = new Area {
    // read
    // registers
    val mAxiArValid = Reg(Bool) init (False)
    val rFifoCount = UInt(5 bits)
    val rFifoFull = Bool()
    val rFifoEmpty = Bool()
    val rFifoRCount = UInt(8 bits)
    val sAxiRValid = Reg(Bool) init (False)
    val sAxiRResp = Reg(Bits(2 bits)) init (0)
    val rCounts = Reg(UInt(9 bits)) init (0)
    val axiArAddr = Reg(cloneOf(s.ar.addr)) init (0)
    val axiArLen = Reg(cloneOf(s.ar.len)) init (0)
    val axiRLen = Reg(cloneOf(s.ar.len)) init (0)
    val axiArBurst = Reg(cloneOf(s.ar.burst)) init (0)
    val axiArSize = Reg(cloneOf(s.ar.size)) init (0)
    val nextReadAddr = cloneOf(s.ar.addr)
    val sAxiRId = Reg(cloneOf(s.ar.id)) init (0)
    val rFifoRId = cloneOf(s.ar.id)
    val sAxiRData = Reg(cloneOf(s.r.data)) init (0)
    val sAxiRLast = Reg(Bool) init (False)
    val rId = Reg(cloneOf(s.r.id)) init (0)
    val readFromRdFifo = Bool()
    // pipes
    val sArPipe = s.ar.s2mPipe()
    val mRPipe = m.r.s2mPipe()

    when(sArPipe.fire) {
      mAxiArValid := True
    } elsewhen (m.ar.ready && axiArLen === 0) {
      mAxiArValid := False
    }

    when(sArPipe.fire) {
      axiArAddr := sArPipe.addr
      axiArBurst := sArPipe.burst
      axiArSize := sArPipe.size
      axiRLen := sArPipe.len
    } elsewhen (m.ar.ready) {
      axiArAddr := nextReadAddr
    }

    val calcRdAddr = new AxiAddr(axiCfg.addressWidth, axiCfg.dataWidth)
    calcRdAddr.io.lastAddr := axiArAddr
    calcRdAddr.io.size := axiArSize
    calcRdAddr.io.burst := axiArBurst
    calcRdAddr.io.len := axiRLen
    nextReadAddr := calcRdAddr.io.nextAddr

    when(sArPipe.fire) {
      axiArLen := sArPipe.len
    } elsewhen (m.ar.fire && axiArLen > 0) {
      axiArLen := axiArLen - 1
    }

    sArPipe.ready := (!m.ar.valid || (axiArLen === 0 && m.ar.ready)) && !rFifoFull

    readFromRdFifo := mRPipe.fire && (rCounts <= 1) && !rFifoEmpty

    var rdFifo = new SyncFifo(Axi2AxiLiteFifoBundle(axiCfg.idWidth))
    rdFifo.io.write := sArPipe.fire
    rdFifo.io.wData.id := sArPipe.id
    rdFifo.io.wData.len := sArPipe.len
    rFifoFull := rdFifo.io.full
    rFifoCount := rdFifo.io.fill
    rdFifo.io.read := readFromRdFifo
    rFifoRId := rdFifo.io.rData.id
    rFifoRCount := rdFifo.io.rData.len
    rFifoEmpty := rdFifo.io.empty

    mRPipe.ready := (!s.r.valid || s.r.ready)

    when(mRPipe.fire) {
      sAxiRValid := True
    } elsewhen (s.r.ready) {
      sAxiRValid := False
    }

    when(mRPipe.fire) {
      sAxiRResp := mRPipe.resp
      sAxiRData := mRPipe.data
    } elsewhen (s.r.ready) {
      sAxiRResp := 0
      sAxiRData := 0
    }

    when(readFromRdFifo) {
      rCounts <= rFifoRCount + rCounts
    } elsewhen (mRPipe.fire) {
      rCounts := rCounts - 1
    }

    when(readFromRdFifo) {
      rId := rFifoRId
    }

    when(!s.r.valid || s.r.ready) {
      when(readFromRdFifo) {
        sAxiRLast := rFifoRCount === 0
      } otherwise {
        sAxiRLast := False
      }

      when(rCounts === 1) {
        sAxiRLast := True
      }
    }

    when(s.r.fire && s.r.last || (!s.r.valid && rCounts === 0)) {
      sAxiRId := Mux(readFromRdFifo && rCounts === 0, rFifoRId, rId)
    }

    m.ar.valid := mAxiArValid
    m.ar.addr := axiArAddr
    m.ar.prot := 0

    s.r.valid := sAxiRValid
    s.r.data := sAxiRData
    s.r.resp := sAxiRResp
    s.r.last := sAxiRLast
    s.r.id := sAxiRId
  }
}

object Axi2AxiLiteVerilog extends GenUtils {
  work(
    new Axi2AxiLite(
      Axi4Config(addressWidth = 32, dataWidth = 32, idWidth = 5),
      AxiLite4Config(addressWidth = 32, dataWidth = 32)
    )
  )
}
