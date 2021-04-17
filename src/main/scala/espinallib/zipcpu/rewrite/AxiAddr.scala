package espinallib.zipcpu.rewrite

import espinallib.common.GenUtils
import espinallib.formal.FormalUtils.doFormal
import spinal.core._
import spinal.lib.bus.amba4.axi.Axi4.burst.{FIXED, INCR, RESERVED, WRAP}

/** AXI4 burst transfer addr calculation
  *
  * ref: https://github.com/ZipCPU/wb2axip/blob/master/rtl/axi_addr.v
  * blog post: https://zipcpu.com/blog/2019/04/27/axi-addr.html
  * standard: ARM IHI 0022E A3.4.1 Address structure
  *
  * @param addrWidth address width
  * @param dataWidth data width
  */
class AxiAddr(addrWidth: Int, dataWidth: Int) extends Component {
  val io = new Bundle {

    /** previous address */
    val lastAddr = in(UInt(addrWidth bits))

    /** AxSIZE */
    val size = in(UInt(3 bits))

    /** AxBURST */
    val burst = in(Bits(2 bits))

    /** AxLEN */
    val len = in(UInt(8 bits))

    /** next address */
    val nextAddr = out(UInt(addrWidth bits))
  }

  assert(dataWidth % 8 == 0, "Data width must be divisible by 8")

  val lowAddrBits = log2Up(dataWidth / 8)

  // address increment
  val increment = UInt(addrWidth bits)
  when(io.burst =/= FIXED) {
    lowAddrBits match {
      case 0 => increment := 1
      case 1 =>
        increment := SpinalMap(io.size(0), U"0" -> U(1), U"1" -> U(2)).resized
      case 2 =>
        increment := SpinalMap(
          io.size(1 downto 0),
          U"00" -> U(1),
          U"01" -> U(2),
          U"10" -> U(4),
          U"11" -> U(4)
        ).resized
      case 3 =>
        increment := SpinalMap(
          io.size(1 downto 0),
          U"00" -> U(1),
          U"01" -> U(2),
          U"10" -> U(4),
          U"11" -> U(8)
        ).resized
      case _ =>
        increment := (U(1) << io.size).resized
    }
  } otherwise {
    increment := 0
  }

  // address mask for WRAP burst
  // "for wrapping bursts, the burst length must be 2, 4, 8, or 16"
  val wrapMask = UInt(addrWidth bits)
  switch(io.len) {
    is(1) {
      // burst length = 2
      wrapMask := ((U(1) << (io.size + 1)) - 1).resized
    }
    is(3) {
      // burst length = 4
      wrapMask := ((U(1) << (io.size + 2)) - 1).resized
    }
    is(7) {
      // burst length = 8
      wrapMask := ((U(1) << (io.size + 3)) - 1).resized
    }
    is(15) {
      // burst length = 16
      wrapMask := ((U(1) << (io.size + 4)) - 1).resized
    }
    default {
      wrapMask := 1
    }
  }

  val incAddr = UInt(addrWidth bits)
  incAddr := io.lastAddr + increment
  when(io.burst =/= FIXED) {
    if (lowAddrBits < 2) {
      when(io.size(0)) {
        incAddr(0) := False
      }
    } else if (lowAddrBits < 4) {
      switch(io.size(1 downto 0)) {
        is(U"01") {
          incAddr(0 downto 0) := 0
        }
        is(U"10") {
          incAddr(1 downto 0) := 0
        }
        is(U"11") {
          incAddr(2 downto 0) := 0
        }
      }
    } else {
      switch(io.size) {
        is(U"001") {
          incAddr(0 downto 0) := 0
        }
        is(U"010") {
          incAddr(1 downto 0) := 0
        }
        is(U"011") {
          incAddr(2 downto 0) := 0
        }
        is(U"100") {
          incAddr(3 downto 0) := 0
        }
        is(U"101") {
          incAddr(4 downto 0) := 0
        }
        is(U"110") {
          incAddr(5 downto 0) := 0
        }
        is(U"111") {
          incAddr(6 downto 0) := 0
        }
      }
    }
  }

  val nextAddr = UInt(addrWidth bits)
  switch(io.burst) {
    is(WRAP) {
      nextAddr := (io.lastAddr & ~wrapMask) | (incAddr & wrapMask)
    }
    default {
      nextAddr := incAddr
    }
  }
  if (addrWidth > 12) {
    io.nextAddr(addrWidth - 1 downto 12) := io.lastAddr(addrWidth - 1 downto 12)
    io.nextAddr(11 downto 0) := nextAddr(11 downto 0)
  } else {
    io.nextAddr := nextAddr
  }

  doFormal { (outerReset, pastValid) =>
    // The size of any transfer must not exceed the data bus width of either agent in the transaction.
    assume(
      (U(1) << io.size) <= dataWidth / 8
    )

    // burst type must not be RESERVED
    assume(io.burst =/= RESERVED)

    // AXI4 extends burst length support for the INCR burst type to 1 to 256 transfers. Support for all other burst types in AXI4 remains at 1 to 16 transfers.
    when(io.burst === INCR) {
      assume(0 <= io.len && io.len <= 255)
    } otherwise {
      assume(0 <= io.len && io.len <= 15)
    }

    // for wrapping bursts
    // 1. the start address must be aligned to the size of each transfer
    // 2. the length of the burst must be 2, 4, 8, or 16 transfers.
    when(io.burst === WRAP) {
      assume((io.lastAddr & ((U(1) << io.size) - 1).resized) === 0)
      assume(
        io.len === 1 || io.len === 3 || io.len === 7 || io.len === 15
      )
    }

    // a burst must not cross a 4KB address boundary
    if (addrWidth > 12) {
      assert(
        io.nextAddr(addrWidth - 1 downto 12) === io.lastAddr(
          addrWidth - 1 downto 12
        )
      )
    }

    // check increment calculation
    when(io.burst =/= FIXED) {
      assert(increment === (U(1) << io.size))
    } otherwise {
      assert(increment === 0)
    }
  }
}

object AxiAddrCalcVerilog extends GenUtils {
  work(
    new AxiAddr(32, 32)
  )
}
