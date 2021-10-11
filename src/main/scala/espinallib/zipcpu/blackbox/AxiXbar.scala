package espinallib.zipcpu.blackbox

import espinallib.common.GenUtils
import spinal.core._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SpecRenamer}
import spinal.lib.{master, slave}
import espinallib.common.Resource

// blackbox of https://github.com/ZipCPU/wb2axip/blob/master/rtl/axixbar.v
class AxiXbarBlackBox(
    addrWidth: Int,
    dataWidth: Int,
    idWidth: Int,
    numMasters: Int,
    numSlaves: Int,
    slaveAddr: Seq[BigInt],
    slaveMask: Seq[BigInt],
    lowPower: Boolean,
    linger: Int,
    log2MaxBurst: Int
) extends BlackBox {

  val io = new Bundle {
    val S_AXI_ACLK = in(Bool)
    val S_AXI_ARESETN = in(Bool)

    // AXI4 slave interface
    val S_AXI_AWID = in(Bits(idWidth * numMasters bits))
    val S_AXI_AWADDR = in(Bits(addrWidth * numMasters bits))
    val S_AXI_AWLEN = in(Bits(8 * numMasters bits))
    val S_AXI_AWSIZE = in(Bits(3 * numMasters bits))
    val S_AXI_AWBURST = in(Bits(2 * numMasters bits))
    val S_AXI_AWLOCK = in(Bits(numMasters bits))
    val S_AXI_AWCACHE = in(Bits(4 * numMasters bits))
    val S_AXI_AWPROT = in(Bits(3 * numMasters bits))
    val S_AXI_AWQOS = in(Bits(4 * numMasters bits))
    val S_AXI_AWVALID = in(Bits(numMasters bits))
    val S_AXI_AWREADY = out(Bits(numMasters bits))

    val S_AXI_WDATA = in(Bits(dataWidth * numMasters bits))
    val S_AXI_WSTRB = in(Bits(dataWidth / 8 * numMasters bits))
    val S_AXI_WLAST = in(Bits(numMasters bits))
    val S_AXI_WVALID = in(Bits(numMasters bits))
    val S_AXI_WREADY = out(Bits(numMasters bits))

    val S_AXI_BID = out(Bits(idWidth * numMasters bits))
    val S_AXI_BRESP = out(Bits(2 * numMasters bits))
    val S_AXI_BVALID = out(Bits(numMasters bits))
    val S_AXI_BREADY = in(Bits(numMasters bits))

    val S_AXI_ARID = in(Bits(idWidth * numMasters bits))
    val S_AXI_ARADDR = in(Bits(addrWidth * numMasters bits))
    val S_AXI_ARLEN = in(Bits(8 * numMasters bits))
    val S_AXI_ARSIZE = in(Bits(3 * numMasters bits))
    val S_AXI_ARBURST = in(Bits(2 * numMasters bits))
    val S_AXI_ARLOCK = in(Bits(numMasters bits))
    val S_AXI_ARCACHE = in(Bits(4 * numMasters bits))
    val S_AXI_ARPROT = in(Bits(3 * numMasters bits))
    val S_AXI_ARQOS = in(Bits(4 * numMasters bits))
    val S_AXI_ARVALID = in(Bits(numMasters bits))
    val S_AXI_ARREADY = out(Bits(numMasters bits))

    val S_AXI_RID = out(Bits(idWidth * numMasters bits))
    val S_AXI_RDATA = out(Bits(dataWidth * numMasters bits))
    val S_AXI_RRESP = out(Bits(2 * numMasters bits))
    val S_AXI_RLAST = out(Bits(numMasters bits))
    val S_AXI_RVALID = out(Bits(numMasters bits))
    val S_AXI_RREADY = in(Bits(numMasters bits))

    // AXI4 master interface
    val M_AXI_AWID = out(Bits(idWidth * numSlaves bits))
    val M_AXI_AWADDR = out(Bits(addrWidth * numSlaves bits))
    val M_AXI_AWLEN = out(Bits(8 * numSlaves bits))
    val M_AXI_AWSIZE = out(Bits(3 * numSlaves bits))
    val M_AXI_AWBURST = out(Bits(2 * numSlaves bits))
    val M_AXI_AWLOCK = out(Bits(numSlaves bits))
    val M_AXI_AWCACHE = out(Bits(4 * numSlaves bits))
    val M_AXI_AWPROT = out(Bits(3 * numSlaves bits))
    val M_AXI_AWQOS = out(Bits(4 * numSlaves bits))
    val M_AXI_AWVALID = out(Bits(numSlaves bits))
    val M_AXI_AWREADY = in(Bits(numSlaves bits))

    val M_AXI_WDATA = out(Bits(dataWidth * numSlaves bits))
    val M_AXI_WSTRB = out(Bits(dataWidth / 8 * numSlaves bits))
    val M_AXI_WLAST = out(Bits(numSlaves bits))
    val M_AXI_WVALID = out(Bits(numSlaves bits))
    val M_AXI_WREADY = in(Bits(numSlaves bits))

    val M_AXI_BID = in(Bits(idWidth * numSlaves bits))
    val M_AXI_BRESP = in(Bits(2 * numSlaves bits))
    val M_AXI_BVALID = in(Bits(numSlaves bits))
    val M_AXI_BREADY = out(Bits(numSlaves bits))

    val M_AXI_ARID = out(Bits(idWidth * numSlaves bits))
    val M_AXI_ARADDR = out(Bits(addrWidth * numSlaves bits))
    val M_AXI_ARLEN = out(Bits(8 * numSlaves bits))
    val M_AXI_ARSIZE = out(Bits(3 * numSlaves bits))
    val M_AXI_ARBURST = out(Bits(2 * numSlaves bits))
    val M_AXI_ARLOCK = out(Bits(numSlaves bits))
    val M_AXI_ARCACHE = out(Bits(4 * numSlaves bits))
    val M_AXI_ARQOS = out(Bits(4 * numSlaves bits))
    val M_AXI_ARPROT = out(Bits(3 * numSlaves bits))
    val M_AXI_ARVALID = out(Bits(numSlaves bits))
    val M_AXI_ARREADY = in(Bits(numSlaves bits))

    val M_AXI_RID = in(Bits(idWidth * numSlaves bits))
    val M_AXI_RDATA = in(Bits(dataWidth * numSlaves bits))
    val M_AXI_RRESP = in(Bits(2 * numSlaves bits))
    val M_AXI_RLAST = in(Bits(numSlaves bits))
    val M_AXI_RVALID = in(Bits(numSlaves bits))
    val M_AXI_RREADY = out(Bits(numSlaves bits))
  }

  setDefinitionName("axixbar")

  addGeneric("C_AXI_DATA_WIDTH", dataWidth)
  addGeneric("C_AXI_ADDR_WIDTH", addrWidth)
  addGeneric("NM", numMasters)
  addGeneric("NS", numSlaves)

  var addr = BigInt(0)
  for (a <- slaveAddr.reverse) {
    addr = (addr << addrWidth) | a
  }
  addGeneric("SLAVE_ADDR", B(addr, numSlaves * addrWidth bits))

  var mask = BigInt(0)
  for (m <- slaveMask.reverse) {
    mask = (mask << addrWidth) | m
  }
  addGeneric("SLAVE_MASK", B(mask, numSlaves * addrWidth bits))

  addGeneric("OPT_LOWPOWER", lowPower)
  addGeneric("OPT_LINGER", linger)
  addGeneric("LGMAXBURST", log2MaxBurst)

  // Map the clk
  mapCurrentClockDomain(
    clock = io.S_AXI_ACLK,
    reset = io.S_AXI_ARESETN,
    resetActiveLevel = LOW
  )

  noIoPrefix()

  addRTLPath(Resource.path("/wb2axip/rtl/axixbar.v"))
  addRTLPath(Resource.path("/wb2axip/rtl/addrdecode.v"))
  addRTLPath(Resource.path("/wb2axip/rtl/skidbuffer.v"))
  GenerationFlags.formal {
    addRTLPath(Resource.path("/wb2axip/bench/formal/faxi_master.v"))
    addRTLPath(Resource.path("/wb2axip/bench/formal/faxi_slave.v"))
  }
}

class AxiXbar(
    addrWidth: Int = 32,
    dataWidth: Int = 32,
    idWidth: Int = 2,
    numMasters: Int = 4,
    numSlaves: Int = 8,
    slaveAddr: Seq[BigInt] =
      Seq(0x00000000L, 0x10000000L, 0x40000000L, 0x60000000L, 0x80000000L,
        0xa0000000L, 0xc0000000L, 0xe0000000L).map(BigInt(_)),
    slaveMask: Seq[BigInt] =
      Seq(0xf0000000L, 0xf0000000L, 0xe0000000L, 0xe0000000L, 0xe0000000L,
        0xe0000000L, 0xe0000000L, 0xe0000000L).map(BigInt(_)),
    slaveAddrWidth: Seq[Int] = List.fill(8)(32),
    lowPower: Boolean = true,
    linger: Int = 4,
    log2MaxBurst: Int = 5
) extends Component {

  val axiCfg = Axi4Config(
    addressWidth = addrWidth,
    dataWidth = dataWidth,
    idWidth = idWidth
  )

  val s = Vec(slave(Axi4(axiCfg)), numMasters)
  val m = for (i <- 0 until numSlaves) yield {
    master(
      Axi4(
        Axi4Config(
          addressWidth = slaveAddrWidth(i),
          dataWidth = dataWidth,
          idWidth = idWidth
        )
      )
    )
  }
  val ip = new AxiXbarBlackBox(
    addrWidth,
    dataWidth,
    idWidth,
    numMasters,
    numSlaves,
    slaveAddr,
    slaveMask,
    lowPower,
    linger,
    log2MaxBurst
  )

  for ((sl, i) <- s.zipWithIndex) {
    Axi4SpecRenamer(sl)

    ip.io.S_AXI_AWID.assignFromBits(sl.aw.id.asBits, i * idWidth, idWidth bits)
    ip.io.S_AXI_AWADDR.assignFromBits(
      sl.aw.addr.asBits,
      i * addrWidth,
      addrWidth bits
    )
    ip.io.S_AXI_AWLEN.assignFromBits(sl.aw.len.asBits, i * 8, 8 bits)
    ip.io.S_AXI_AWSIZE.assignFromBits(sl.aw.size.asBits, i * 3, 3 bits)
    ip.io.S_AXI_AWBURST.assignFromBits(sl.aw.burst, i * 2, 2 bits)
    ip.io.S_AXI_AWLOCK.assignFromBits(sl.aw.lock, i, 1 bits)
    ip.io.S_AXI_AWCACHE.assignFromBits(sl.aw.cache, i * 4, 4 bits)
    ip.io.S_AXI_AWPROT.assignFromBits(sl.aw.prot, i * 3, 3 bits)
    ip.io.S_AXI_AWQOS.assignFromBits(sl.aw.qos, i * 4, 4 bits)
    ip.io.S_AXI_AWVALID(i) := sl.aw.valid
    sl.aw.ready := ip.io.S_AXI_AWREADY(i)

    ip.io.S_AXI_WDATA.assignFromBits(sl.w.data, i * dataWidth, dataWidth bits)
    ip.io.S_AXI_WSTRB.assignFromBits(
      sl.w.strb,
      i * dataWidth / 8,
      dataWidth / 8 bits
    )
    ip.io.S_AXI_WLAST(i) := sl.w.last
    ip.io.S_AXI_WVALID(i) := sl.w.valid
    sl.w.ready := ip.io.S_AXI_WREADY(i)

    sl.b.id.assignFromBits(ip.io.S_AXI_BID(i * idWidth, idWidth bits))
    sl.b.resp := ip.io.S_AXI_BRESP(i * 2, 2 bits)
    sl.b.valid := ip.io.S_AXI_BVALID(i)
    ip.io.S_AXI_BREADY(i) := sl.b.ready

    ip.io.S_AXI_ARID.assignFromBits(
      sl.ar.id.asBits,
      i * idWidth,
      idWidth bits
    )
    ip.io.S_AXI_ARADDR.assignFromBits(
      sl.ar.addr.asBits,
      i * addrWidth,
      addrWidth bits
    )
    ip.io.S_AXI_ARLEN.assignFromBits(sl.ar.len.asBits, i * 8, 8 bits)
    ip.io.S_AXI_ARSIZE.assignFromBits(sl.ar.size.asBits, i * 3, 3 bits)
    ip.io.S_AXI_ARBURST.assignFromBits(sl.ar.burst, i * 2, 2 bits)
    ip.io.S_AXI_ARLOCK.assignFromBits(sl.ar.lock, i, 1 bits)
    ip.io.S_AXI_ARCACHE.assignFromBits(sl.ar.cache, i * 4, 4 bits)
    ip.io.S_AXI_ARQOS.assignFromBits(sl.ar.qos, i * 4, 4 bits)
    ip.io.S_AXI_ARPROT.assignFromBits(sl.ar.prot, i * 3, 3 bits)
    ip.io.S_AXI_ARVALID(i) := sl.ar.valid
    sl.ar.ready := ip.io.S_AXI_ARREADY(i)

    sl.r.id.assignFromBits(ip.io.S_AXI_RID(i * idWidth, idWidth bits))
    sl.r.data := ip.io.S_AXI_RDATA(i * dataWidth, dataWidth bits)
    sl.r.resp := ip.io.S_AXI_RRESP(i * 2, 2 bits)
    sl.r.last := ip.io.S_AXI_RLAST(i)
    sl.r.valid := ip.io.S_AXI_RVALID(i)
    ip.io.S_AXI_RREADY(i) := sl.r.ready
  }

  for ((ma, i) <- m.zipWithIndex) {
    Axi4SpecRenamer(ma)

    ma.aw.id.assignFromBits(ip.io.M_AXI_AWID(i * idWidth, idWidth bits))
    ma.aw.addr := U(ip.io.M_AXI_AWADDR(i * addrWidth, addrWidth bits)).resized
    ma.aw.len := U(ip.io.M_AXI_AWLEN(i * 8, 8 bits))
    ma.aw.size := U(ip.io.M_AXI_AWSIZE(i * 3, 3 bits))
    ma.aw.burst := ip.io.M_AXI_AWBURST(i * 2, 2 bits)
    ma.aw.lock := ip.io.M_AXI_AWLOCK(i, 1 bits)
    ma.aw.cache := ip.io.M_AXI_AWCACHE(i * 4, 4 bits)
    ma.aw.prot := ip.io.M_AXI_AWPROT(i * 3, 3 bits)
    ma.aw.qos := ip.io.M_AXI_AWQOS(i * 4, 4 bits)
    ma.aw.valid := ip.io.M_AXI_AWVALID(i)
    ip.io.M_AXI_AWREADY(i) := ma.aw.ready

    ma.w.data := ip.io.M_AXI_WDATA(i * dataWidth, dataWidth bits)
    ma.w.strb := ip.io.M_AXI_WSTRB(i * dataWidth / 8, dataWidth / 8 bits)
    ma.w.last := ip.io.M_AXI_WLAST(i)
    ma.w.valid := ip.io.M_AXI_WVALID(i)
    ip.io.M_AXI_WREADY(i) := ma.w.ready

    ip.io.M_AXI_BID.assignFromBits(ma.b.id.asBits, i * idWidth, idWidth bits)
    ip.io.M_AXI_BRESP.assignFromBits(ma.b.resp, i * 2, 2 bits)
    ip.io.M_AXI_BVALID(i) := ma.b.valid
    ma.b.ready := ip.io.M_AXI_BREADY(i)

    ma.ar.id := U(ip.io.M_AXI_ARID(i * idWidth, idWidth bits))
    ma.ar.addr := U(ip.io.M_AXI_ARADDR(i * addrWidth, addrWidth bits)).resized
    ma.ar.len := U(ip.io.M_AXI_ARLEN(i * 8, 8 bits))
    ma.ar.size := U(ip.io.M_AXI_ARSIZE(i * 3, 3 bits))
    ma.ar.burst := ip.io.M_AXI_ARBURST(i * 2, 2 bits)
    ma.ar.lock := ip.io.M_AXI_ARLOCK(i, 1 bits)
    ma.ar.cache := ip.io.M_AXI_ARCACHE(i * 4, 4 bits)
    ma.ar.prot := ip.io.M_AXI_ARPROT(i * 3, 3 bits)
    ma.ar.qos := ip.io.M_AXI_ARQOS(i * 4, 4 bits)
    ma.ar.valid := ip.io.M_AXI_ARVALID(i)
    ip.io.M_AXI_ARREADY(i) := ma.ar.ready

    ip.io.M_AXI_RID.assignFromBits(ma.r.id.asBits, i * idWidth, idWidth bits)
    ip.io.M_AXI_RDATA.assignFromBits(ma.r.data, i * dataWidth, dataWidth bits)
    ip.io.M_AXI_RRESP.assignFromBits(ma.r.resp, i * 2, 2 bits)
    ip.io.M_AXI_RLAST(i) := ma.r.last
    ip.io.M_AXI_RVALID(i) := ma.r.valid
    ma.r.ready := ip.io.M_AXI_RREADY(i)

    // unimplemented wires
    ma.ar.region := 0
    ma.aw.region := 0
  }
}

object AxiXbarVerilog extends GenUtils {
  work(
    new AxiXbar(slaveAddrWidth = Seq(8, 16, 24, 32, 32, 32, 32, 32))
  )
}
