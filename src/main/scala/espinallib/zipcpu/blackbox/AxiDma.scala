package espinallib.zipcpu.blackbox

import espinallib.common.GenUtils
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SpecRenamer}
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SpecRenamer}
import spinal.lib.{master, slave}

// blackbox of https://github.com/ZipCPU/wb2axip/blob/master/rtl/axidma.v
class AxiDmaBlackBox(
                      idWidth: Int,
                      addrWidth: Int,
                      dataWidth: Int,
                      unaligned: Boolean,
                      wrapMem: Boolean
                    ) extends BlackBox {

  val io = new Bundle {
    val S_AXI_ACLK = in(Bool)
    val S_AXI_ARESETN = in(Bool)

    // AXI4-Lite slave interface
    val S_AXIL_AWVALID = in(Bool)
    val S_AXIL_AWREADY = out(Bool)
    val S_AXIL_AWADDR = in(UInt(5 bits))
    val S_AXIL_AWPROT = in(Bits(3 bits))

    val S_AXIL_WVALID = in(Bool)
    val S_AXIL_WREADY = out(Bool)
    val S_AXIL_WDATA = in(Bits(32 bits))
    val S_AXIL_WSTRB = in(Bits(32 / 8 bits))

    val S_AXIL_BVALID = out(Bool)
    val S_AXIL_BREADY = in(Bool)
    val S_AXIL_BRESP = out(Bits(2 bits))

    val S_AXIL_ARVALID = in(Bool)
    val S_AXIL_ARREADY = out(Bool)
    val S_AXIL_ARADDR = in(UInt(5 bits))
    val S_AXIL_ARPROT = in(Bits(3 bits))

    val S_AXIL_RVALID = out(Bool)
    val S_AXIL_RREADY = in(Bool)
    val S_AXIL_RDATA = out(Bits(32 bits))
    val S_AXIL_RRESP = out(Bits(2 bits))

    // AXI4 master interface
    val M_AXI_AWVALID = out(Bool)
    val M_AXI_AWREADY = in(Bool)
    val M_AXI_AWID = out(UInt(idWidth bits))
    val M_AXI_AWADDR = out(UInt(addrWidth bits))
    val M_AXI_AWLEN = out(UInt(8 bits))
    val M_AXI_AWSIZE = out(UInt(3 bits))
    val M_AXI_AWBURST = out(Bits(2 bits))
    val M_AXI_AWLOCK = out(Bits(1 bits))
    val M_AXI_AWCACHE = out(Bits(4 bits))
    val M_AXI_AWPROT = out(Bits(3 bits))
    val M_AXI_AWQOS = out(Bits(4 bits))

    val M_AXI_WVALID = out(Bool)
    val M_AXI_WREADY = in(Bool)
    val M_AXI_WDATA = out(Bits(dataWidth bits))
    val M_AXI_WSTRB = out(Bits(dataWidth / 8 bits))
    val M_AXI_WLAST = out(Bool)

    val M_AXI_BVALID = in(Bool)
    val M_AXI_BREADY = out(Bool)
    val M_AXI_BID = in(UInt(idWidth bits))
    val M_AXI_BRESP = in(Bits(2 bits))

    val M_AXI_ARVALID = out(Bool)
    val M_AXI_ARREADY = in(Bool)
    val M_AXI_ARID = out(UInt(idWidth bits))
    val M_AXI_ARADDR = out(UInt(addrWidth bits))
    val M_AXI_ARLEN = out(UInt(8 bits))
    val M_AXI_ARSIZE = out(UInt(3 bits))
    val M_AXI_ARBURST = out(Bits(2 bits))
    val M_AXI_ARLOCK = out(Bits(1 bits))
    val M_AXI_ARCACHE = out(Bits(4 bits))
    val M_AXI_ARPROT = out(Bits(3 bits))
    val M_AXI_ARQOS = out(Bits(4 bits))

    val M_AXI_RVALID = in(Bool)
    val M_AXI_RREADY = out(Bool)
    val M_AXI_RID = in(UInt(idWidth bits))
    val M_AXI_RDATA = in(Bits(dataWidth bits))
    val M_AXI_RRESP = in(Bits(2 bits))
    val M_AXI_RLAST = in(Bool)

    val o_int = out(Bool)
  }

  setDefinitionName("axidma")

  addGeneric("C_AXI_ID_WIDTH", idWidth)
  addGeneric("C_AXI_DATA_WIDTH", dataWidth)
  addGeneric("C_AXI_ADDR_WIDTH", addrWidth)
  addGeneric("OPT_UNALIGNED", unaligned)
  addGeneric("OPT_WRAPMEM", wrapMem)

  // Map the clk
  mapCurrentClockDomain(
    clock = io.S_AXI_ACLK,
    reset = io.S_AXI_ARESETN,
    resetActiveLevel = LOW
  )

  noIoPrefix()

  addRTLPath("./submodules/wb2axip/rtl/axidma.v")
  addRTLPath("./submodules/wb2axip/rtl/axi_addr.v")
  addRTLPath("./submodules/wb2axip/rtl/sfifo.v")
  addRTLPath("./submodules/wb2axip/bench/formal/faxil_slave.v")
}

// wrapper
class AxiDma(
              idWidth: Int = 1,
              addrWidth: Int = 32,
              dataWidth: Int = 32,
              unaligned: Boolean = true,
              wrapMem: Boolean = true
            ) extends Component {
  val axiCfg = Axi4Config(idWidth = idWidth, addressWidth = addrWidth, dataWidth = dataWidth)
  val axiLiteCfg = AxiLite4Config(addressWidth = addrWidth, dataWidth = dataWidth)

  val s = slave(AxiLite4(axiLiteCfg))
  val m = master(Axi4(axiCfg))
  val intr = out(Bool)

  AxiLite4SpecRenamer(s)
  Axi4SpecRenamer(m)

  val ip = new AxiDmaBlackBox(
    idWidth,
    addrWidth,
    dataWidth,
    unaligned,
    wrapMem
  )

  // slave
  ip.io.S_AXIL_AWVALID := s.aw.valid
  s.aw.ready := ip.io.S_AXIL_AWREADY
  ip.io.S_AXIL_AWADDR := s.aw.addr.resized
  ip.io.S_AXIL_AWPROT := s.aw.prot

  ip.io.S_AXIL_WVALID := s.w.valid
  s.w.ready := ip.io.S_AXIL_WREADY
  ip.io.S_AXIL_WDATA := s.w.data
  ip.io.S_AXIL_WSTRB := s.w.strb

  s.b.valid := ip.io.S_AXIL_BVALID
  ip.io.S_AXIL_BREADY := s.b.ready
  s.b.resp := ip.io.S_AXIL_BRESP

  ip.io.S_AXIL_ARVALID := s.ar.valid
  s.ar.ready := ip.io.S_AXIL_ARREADY
  ip.io.S_AXIL_ARADDR := s.ar.addr.resized
  ip.io.S_AXIL_ARPROT := s.ar.prot

  s.r.valid := ip.io.S_AXIL_RVALID
  ip.io.S_AXIL_RREADY := s.r.ready
  s.r.data := ip.io.S_AXIL_RDATA
  s.r.resp := ip.io.S_AXIL_RRESP

  // master
  m.aw.valid := ip.io.M_AXI_AWVALID
  ip.io.M_AXI_AWREADY := m.aw.ready
  m.aw.id := ip.io.M_AXI_AWID
  m.aw.addr := ip.io.M_AXI_AWADDR
  m.aw.len := ip.io.M_AXI_AWLEN
  m.aw.size := ip.io.M_AXI_AWSIZE
  m.aw.burst := ip.io.M_AXI_AWBURST
  m.aw.lock := ip.io.M_AXI_AWLOCK
  m.aw.cache := ip.io.M_AXI_AWCACHE
  m.aw.prot := ip.io.M_AXI_AWPROT
  m.aw.qos := ip.io.M_AXI_AWQOS
  m.aw.region := 0

  m.w.valid := ip.io.M_AXI_WVALID
  ip.io.M_AXI_WREADY := m.w.ready
  m.w.data := ip.io.M_AXI_WDATA
  m.w.strb := ip.io.M_AXI_WSTRB
  m.w.last := ip.io.M_AXI_WLAST

  ip.io.M_AXI_BVALID := m.b.valid
  m.b.ready := ip.io.M_AXI_BREADY
  ip.io.M_AXI_BID := m.b.id
  ip.io.M_AXI_BRESP := m.b.resp

  m.ar.valid := ip.io.M_AXI_ARVALID
  ip.io.M_AXI_ARREADY := m.ar.ready
  m.ar.id := ip.io.M_AXI_ARID
  m.ar.addr := ip.io.M_AXI_ARADDR
  m.ar.len := ip.io.M_AXI_ARLEN
  m.ar.size := ip.io.M_AXI_ARSIZE
  m.ar.burst := ip.io.M_AXI_ARBURST
  m.ar.lock := ip.io.M_AXI_ARLOCK
  m.ar.cache := ip.io.M_AXI_ARCACHE
  m.ar.prot := ip.io.M_AXI_ARPROT
  m.ar.qos := ip.io.M_AXI_ARQOS
  m.ar.region := 0

  ip.io.M_AXI_RVALID := m.r.valid
  m.r.ready := ip.io.M_AXI_RREADY
  ip.io.M_AXI_RID := m.r.id
  ip.io.M_AXI_RDATA := m.r.data
  ip.io.M_AXI_RLAST := m.r.last
  ip.io.M_AXI_RRESP := m.r.resp

  intr := ip.io.o_int
}

object AxiDmaVerilog extends GenUtils {
  work(
    new AxiDma()
  )
}
