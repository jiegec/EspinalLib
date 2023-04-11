package espinallib.zipcpu.blackbox

import espinallib.common.GenUtils
import espinallib.common.Resource
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi.Axi4
import spinal.lib.bus.amba4.axi.Axi4Config
import spinal.lib.bus.amba4.axi.Axi4SpecRenamer
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axilite.AxiLite4Config
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer

// blackbox of https://github.com/ZipCPU/wb2axip/blob/master/rtl/axi2axilite.v
class Axi2AxiLiteBlackBox(
    idWidth: Int,
    addrWidth: Int,
    dataWidth: Int,
    writes: Boolean,
    reads: Boolean,
    log2Size: Int
) extends BlackBox {

  val io = new Bundle {
    val S_AXI_ACLK = in(Bool())
    val S_AXI_ARESETN = in(Bool())

    // AXI4 slave interface
    val S_AXI_AWVALID = in(Bool())
    val S_AXI_AWREADY = out(Bool())
    val S_AXI_AWID = in(UInt(idWidth bits))
    val S_AXI_AWADDR = in(UInt(addrWidth bits))
    val S_AXI_AWLEN = in(UInt(8 bits))
    val S_AXI_AWSIZE = in(UInt(3 bits))
    val S_AXI_AWBURST = in(Bits(2 bits))
    val S_AXI_AWLOCK = in(Bits(1 bits))
    val S_AXI_AWCACHE = in(Bits(4 bits))
    val S_AXI_AWPROT = in(Bits(3 bits))
    val S_AXI_AWQOS = in(Bits(4 bits))

    val S_AXI_WVALID = in(Bool())
    val S_AXI_WREADY = out(Bool())
    val S_AXI_WDATA = in(Bits(dataWidth bits))
    val S_AXI_WSTRB = in(Bits(dataWidth / 8 bits))
    val S_AXI_WLAST = in(Bool())

    val S_AXI_BVALID = out(Bool())
    val S_AXI_BREADY = in(Bool())
    val S_AXI_BID = out(UInt(idWidth bits))
    val S_AXI_BRESP = out(Bits(2 bits))

    val S_AXI_ARVALID = in(Bool())
    val S_AXI_ARREADY = out(Bool())
    val S_AXI_ARID = in(UInt(idWidth bits))
    val S_AXI_ARADDR = in(UInt(addrWidth bits))
    val S_AXI_ARLEN = in(UInt(8 bits))
    val S_AXI_ARSIZE = in(UInt(3 bits))
    val S_AXI_ARBURST = in(Bits(2 bits))
    val S_AXI_ARLOCK = in(Bits(1 bits))
    val S_AXI_ARCACHE = in(Bits(4 bits))
    val S_AXI_ARPROT = in(Bits(3 bits))
    val S_AXI_ARQOS = in(Bits(4 bits))

    val S_AXI_RVALID = out(Bool())
    val S_AXI_RREADY = in(Bool())
    val S_AXI_RID = out(UInt(idWidth bits))
    val S_AXI_RDATA = out(Bits(dataWidth bits))
    val S_AXI_RRESP = out(Bits(2 bits))
    val S_AXI_RLAST = out(Bool())

    // AXI4-Lite master interface
    val M_AXI_AWVALID = out(Bool())
    val M_AXI_AWREADY = in(Bool())
    val M_AXI_AWADDR = out(UInt(addrWidth bits))
    val M_AXI_AWPROT = out(Bits(3 bits))

    val M_AXI_WVALID = out(Bool())
    val M_AXI_WREADY = in(Bool())
    val M_AXI_WDATA = out(Bits(dataWidth bits))
    val M_AXI_WSTRB = out(Bits(dataWidth / 8 bits))

    val M_AXI_BVALID = in(Bool())
    val M_AXI_BREADY = out(Bool())
    val M_AXI_BRESP = in(Bits(2 bits))

    val M_AXI_ARVALID = out(Bool())
    val M_AXI_ARREADY = in(Bool())
    val M_AXI_ARADDR = out(UInt(addrWidth bits))
    val M_AXI_ARPROT = out(Bits(3 bits))

    val M_AXI_RVALID = in(Bool())
    val M_AXI_RREADY = out(Bool())
    val M_AXI_RDATA = in(Bits(dataWidth bits))
    val M_AXI_RRESP = in(Bits(2 bits))
  }

  setDefinitionName("axi2axilite")

  addGeneric("C_AXI_ID_WIDTH", idWidth)
  addGeneric("C_AXI_DATA_WIDTH", dataWidth)
  addGeneric("C_AXI_ADDR_WIDTH", addrWidth)
  addGeneric("OPT_WRITES", writes)
  addGeneric("OPT_READS", reads)
  addGeneric("LGFIFO", log2Size)

  // Map the clk
  mapCurrentClockDomain(
    clock = io.S_AXI_ACLK,
    reset = io.S_AXI_ARESETN,
    resetActiveLevel = LOW
  )

  noIoPrefix()

  addRTLPath(Resource.path("/wb2axip/rtl/axi2axilite.v"))
  addRTLPath(Resource.path("/wb2axip/rtl/skidbuffer.v"))
  addRTLPath(Resource.path("/wb2axip/rtl/sfifo.v"))
  addRTLPath(Resource.path("/wb2axip/rtl/axi_addr.v"))
  GenerationFlags.formal {
    addRTLPath(Resource.path("/wb2axip/bench/formal/faxil_master.v"))
    addRTLPath(Resource.path("/wb2axip/bench/formal/faxi_slave.v"))
  }
}

// wrapper
class Axi2AxiLite(
    idWidth: Int = 2,
    addrWidth: Int = 6,
    dataWidth: Int = 32,
    writes: Boolean = true,
    reads: Boolean = true,
    log2Size: Int = 4
) extends Component {
  val axiCfg = Axi4Config(
    idWidth = idWidth,
    addressWidth = addrWidth,
    dataWidth = dataWidth
  )
  val axiLiteCfg =
    AxiLite4Config(addressWidth = addrWidth, dataWidth = dataWidth)

  val s = slave(Axi4(axiCfg))
  val m = master(AxiLite4(axiLiteCfg))

  Axi4SpecRenamer(s)
  AxiLite4SpecRenamer(m)

  val ip = new Axi2AxiLiteBlackBox(
    idWidth,
    addrWidth,
    dataWidth,
    writes,
    reads,
    log2Size
  )

  ip.io.S_AXI_AWVALID := s.aw.valid
  s.aw.ready := ip.io.S_AXI_AWREADY
  ip.io.S_AXI_AWID := s.aw.id
  ip.io.S_AXI_AWADDR := s.aw.addr
  ip.io.S_AXI_AWLEN := s.aw.len
  ip.io.S_AXI_AWSIZE := s.aw.size
  ip.io.S_AXI_AWBURST := s.aw.burst
  ip.io.S_AXI_AWLOCK := s.aw.lock
  ip.io.S_AXI_AWCACHE := s.aw.cache
  ip.io.S_AXI_AWPROT := s.aw.prot
  ip.io.S_AXI_AWQOS := s.aw.qos

  ip.io.S_AXI_WVALID := s.w.valid
  s.w.ready := ip.io.S_AXI_WREADY
  ip.io.S_AXI_WDATA := s.w.data
  ip.io.S_AXI_WSTRB := s.w.strb
  ip.io.S_AXI_WLAST := s.w.last

  s.b.valid := ip.io.S_AXI_BVALID
  ip.io.S_AXI_BREADY := s.b.ready
  s.b.id := ip.io.S_AXI_BID
  s.b.resp := ip.io.S_AXI_BRESP

  ip.io.S_AXI_ARVALID := s.ar.valid
  s.ar.ready := ip.io.S_AXI_ARREADY
  ip.io.S_AXI_ARID := s.ar.id
  ip.io.S_AXI_ARADDR := s.ar.addr
  ip.io.S_AXI_ARLEN := s.ar.len
  ip.io.S_AXI_ARSIZE := s.ar.size
  ip.io.S_AXI_ARBURST := s.ar.burst
  ip.io.S_AXI_ARLOCK := s.ar.lock
  ip.io.S_AXI_ARCACHE := s.ar.cache
  ip.io.S_AXI_ARPROT := s.ar.prot
  ip.io.S_AXI_ARQOS := s.ar.qos

  s.r.valid := ip.io.S_AXI_RVALID
  ip.io.S_AXI_RREADY := s.r.ready
  s.r.id := ip.io.S_AXI_RID
  s.r.data := ip.io.S_AXI_RDATA
  s.r.resp := ip.io.S_AXI_RRESP
  s.r.last := ip.io.S_AXI_RLAST

  m.aw.valid := ip.io.M_AXI_AWVALID
  ip.io.M_AXI_AWREADY := m.aw.ready
  m.aw.addr := ip.io.M_AXI_AWADDR
  m.aw.prot := ip.io.M_AXI_AWPROT

  m.w.valid := ip.io.M_AXI_WVALID
  ip.io.M_AXI_WREADY := m.w.ready
  m.w.data := ip.io.M_AXI_WDATA
  m.w.strb := ip.io.M_AXI_WSTRB

  ip.io.M_AXI_BVALID := m.b.valid
  m.b.ready := ip.io.M_AXI_BREADY
  ip.io.M_AXI_BRESP := m.b.resp

  m.ar.valid := ip.io.M_AXI_ARVALID
  ip.io.M_AXI_ARREADY := m.ar.ready
  m.ar.addr := ip.io.M_AXI_ARADDR
  m.ar.prot := ip.io.M_AXI_ARPROT

  ip.io.M_AXI_RVALID := m.r.valid
  m.r.ready := ip.io.M_AXI_RREADY
  ip.io.M_AXI_RDATA := m.r.data
  ip.io.M_AXI_RRESP := m.r.resp

}

object Axi2AxiLiteVerilog extends GenUtils {
  work(
    new Axi2AxiLite()
  )
}
