package espinallib.zipcpu.blackbox

import espinallib.common.GenUtils
import spinal.core._
import spinal.lib.bus.amba4.axilite.{AxiLite4, AxiLite4Config, AxiLite4SpecRenamer}
import spinal.lib.{master, slave}

// blackbox of https://github.com/ZipCPU/wb2axip/blob/master/rtl/axilxbar.v
class AxiLiteXbarBlackBox(
                           addrWidth: Int,
                           dataWidth: Int,
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

    // AXI4-Lite slave interface
    val S_AXI_AWVALID = in(Bits(numMasters bits))
    val S_AXI_AWREADY = out(Bits(numMasters bits))
    val S_AXI_AWADDR = in(Bits(addrWidth * numMasters bits))
    val S_AXI_AWPROT = in(Bits(3 * numMasters bits))

    val S_AXI_WVALID = in(Bits(numMasters bits))
    val S_AXI_WREADY = out(Bits(numMasters bits))
    val S_AXI_WDATA = in(Bits(dataWidth * numMasters bits))
    val S_AXI_WSTRB = in(Bits(dataWidth / 8 * numMasters bits))

    val S_AXI_BVALID = out(Bits(numMasters bits))
    val S_AXI_BREADY = in(Bits(numMasters bits))
    val S_AXI_BRESP = out(Bits(2 * numMasters bits))

    val S_AXI_ARVALID = in(Bits(numMasters bits))
    val S_AXI_ARREADY = out(Bits(numMasters bits))
    val S_AXI_ARADDR = in(Bits(addrWidth * numMasters bits))
    val S_AXI_ARPROT = in(Bits(3 * numMasters bits))

    val S_AXI_RVALID = out(Bits(numMasters bits))
    val S_AXI_RREADY = in(Bits(numMasters bits))
    val S_AXI_RDATA = out(Bits(dataWidth * numMasters bits))
    val S_AXI_RRESP = out(Bits(2 * numMasters bits))

    // AXI4-Lite master interface
    val M_AXI_AWVALID = out(Bits(numSlaves bits))
    val M_AXI_AWREADY = in(Bits(numSlaves bits))
    val M_AXI_AWADDR = out(Bits(addrWidth * numSlaves bits))
    val M_AXI_AWPROT = out(Bits(3 * numSlaves bits))

    val M_AXI_WVALID = out(Bits(numSlaves bits))
    val M_AXI_WREADY = in(Bits(numSlaves bits))
    val M_AXI_WDATA = out(Bits(dataWidth * numSlaves bits))
    val M_AXI_WSTRB = out(Bits(dataWidth / 8 * numSlaves bits))

    val M_AXI_BVALID = in(Bits(numSlaves bits))
    val M_AXI_BREADY = out(Bits(numSlaves bits))
    val M_AXI_BRESP = in(Bits(2 * numSlaves bits))

    val M_AXI_ARVALID = out(Bits(numSlaves bits))
    val M_AXI_ARREADY = in(Bits(numSlaves bits))
    val M_AXI_ARADDR = out(Bits(addrWidth * numSlaves bits))
    val M_AXI_ARPROT = out(Bits(3 * numSlaves bits))

    val M_AXI_RVALID = in(Bits(numSlaves bits))
    val M_AXI_RREADY = out(Bits(numSlaves bits))
    val M_AXI_RDATA = in(Bits(dataWidth * numSlaves bits))
    val M_AXI_RRESP = in(Bits(2 * numSlaves bits))
  }

  setDefinitionName("axilxbar")

  addGeneric("C_AXI_DATA_WIDTH", dataWidth)
  addGeneric("C_AXI_ADDR_WIDTH", addrWidth)
  addGeneric("NM", numMasters)
  addGeneric("NS", numSlaves)

  var addr = BigInt(0)
  for (a <- slaveAddr) {
    addr = (addr << addrWidth) | a
  }
  addGeneric("SLAVE_ADDR", B(addr, numSlaves * addrWidth bits))

  var mask = BigInt(0)
  for (m <- slaveMask) {
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

  addRTLPath("./submodules/wb2axip/rtl/axilxbar.v")
  addRTLPath("./submodules/wb2axip/rtl/addrdecode.v")
  addRTLPath("./submodules/wb2axip/rtl/skidbuffer.v")
  addRTLPath("./submodules/wb2axip/bench/formal/faxil_master.v")
  addRTLPath("./submodules/wb2axip/bench/formal/faxil_slave.v")
}

class AxiLiteXbar(
                   addrWidth: Int = 32,
                   dataWidth: Int = 32,
                   numMasters: Int = 4,
                   numSlaves: Int = 8,
                   slaveAddr: Seq[BigInt] = Seq(0xE0000000L, 0xC0000000L, 0xA0000000L, 0x80000000L, 0x60000000L, 0x40000000L, 0x10000000L, 0x00000000L).map(BigInt(_)),
                   slaveMask: Seq[BigInt] = Seq(0xE0000000L, 0xE0000000L, 0xE0000000L, 0xE0000000L, 0xE0000000L, 0xE0000000L, 0xF0000000L, 0xF0000000L).map(BigInt(_)),
                   lowPower: Boolean = true,
                   linger: Int = 4,
                   log2MaxBurst: Int = 5
                 ) extends Component {

  val axiLiteCfg = AxiLite4Config(addressWidth = addrWidth, dataWidth = dataWidth)

  val s = Vec(slave(AxiLite4(axiLiteCfg)), numMasters)
  val m = Vec(master(AxiLite4(axiLiteCfg)), numSlaves)
  val ip = new AxiLiteXbarBlackBox(addrWidth, dataWidth, numMasters, numSlaves, slaveAddr, slaveMask, lowPower, linger, log2MaxBurst)

  for ((sl, i) <- s.zipWithIndex) {
    AxiLite4SpecRenamer(sl)

    ip.io.S_AXI_AWVALID(i) := sl.aw.valid
    sl.aw.ready := ip.io.S_AXI_AWREADY(i)
    ip.io.S_AXI_AWADDR.assignFromBits(sl.aw.addr.asBits, i * addrWidth, addrWidth bits)
    ip.io.S_AXI_AWPROT.assignFromBits(sl.aw.prot, i * 3, 3 bits)

    ip.io.S_AXI_WVALID(i) := sl.w.valid
    sl.w.ready := ip.io.S_AXI_WREADY(i)
    ip.io.S_AXI_WDATA.assignFromBits(sl.w.data, i * dataWidth, dataWidth bits)
    ip.io.S_AXI_WSTRB.assignFromBits(sl.w.strb, i * dataWidth / 8, dataWidth / 8 bits)

    sl.b.valid := ip.io.S_AXI_BVALID(i)
    ip.io.S_AXI_BREADY(i) := sl.b.ready
    sl.b.resp := ip.io.S_AXI_BRESP(i * 2, 2 bits)

    ip.io.S_AXI_ARVALID(i) := sl.ar.valid
    sl.ar.ready := ip.io.S_AXI_ARREADY(i)
    ip.io.S_AXI_ARADDR.assignFromBits(sl.ar.addr.asBits, i * addrWidth, addrWidth bits)
    ip.io.S_AXI_ARPROT.assignFromBits(sl.ar.prot, i * 3, 3 bits)

    sl.r.valid := ip.io.S_AXI_RVALID(i)
    ip.io.S_AXI_RREADY(i) := sl.r.ready
    sl.r.data := ip.io.S_AXI_RDATA(i * dataWidth, dataWidth bits)
    sl.r.resp := ip.io.S_AXI_RRESP(i * 2, 2 bits)
  }

  for ((ma, i) <- m.zipWithIndex) {
    AxiLite4SpecRenamer(ma)

    ma.aw.valid := ip.io.M_AXI_AWVALID(i)
    ip.io.M_AXI_AWREADY(i) := ma.aw.ready
    ma.aw.addr := U(ip.io.M_AXI_AWADDR(i * addrWidth, addrWidth bits))
    ma.aw.prot := ip.io.M_AXI_AWPROT(i * 3, 3 bits)

    ma.w.valid := ip.io.M_AXI_WVALID(i)
    ip.io.M_AXI_WREADY(i) := ma.w.ready
    ma.w.data := ip.io.M_AXI_WDATA(i * dataWidth, dataWidth bits)
    ma.w.strb := ip.io.M_AXI_WSTRB(i * dataWidth / 8, dataWidth / 8 bits)

    ip.io.M_AXI_BVALID(i) := ma.b.valid
    ma.b.ready := ip.io.M_AXI_BREADY(i)
    ip.io.M_AXI_BRESP.assignFromBits(ma.b.resp, i * 2, 2 bits)

    ma.ar.valid := ip.io.M_AXI_ARVALID(i)
    ip.io.M_AXI_ARREADY(i) := ma.ar.ready
    ma.ar.addr := U(ip.io.M_AXI_ARADDR(i * addrWidth, addrWidth bits))
    ma.ar.prot := ip.io.M_AXI_ARPROT(i * 3, 3 bits)

    ip.io.M_AXI_RVALID(i) := ma.r.valid
    ma.r.ready := ip.io.M_AXI_RREADY(i)
    ip.io.M_AXI_RDATA.assignFromBits(ma.r.data, i * dataWidth, dataWidth bits)
    ip.io.M_AXI_RRESP.assignFromBits(ma.r.resp, i * 2, 2 bits)
  }
}

object AxiLiteXbarVerilog extends GenUtils {
  work(
    new AxiLiteXbar()
  )
}
