package espinallib.zipcpu.rewrite

import espinallib.common.{GenUtils, SkidBufferCommon}
import espinallib.formal.FormalUtils.doFormal
import spinal.core._

// Implement skid buffer
// ZipCPU's blog post: https://zipcpu.com/blog/2019/05/22/skidbuffer.html
// ZipCPU's source code: https://github.com/ZipCPU/wb2axip/blob/master/rtl/skidbuffer.v
// Has one buffer
// When outputReg = true, all output signals(io.s.ready, io.m.valid and io.m.payload) are registered
// When outputReg = false, only io.s.ready is registered
class SkidBuffer[T <: Data](
    gen: => T,
    lowPower: Boolean = false,
    outputReg: Boolean = true
) extends SkidBufferCommon(gen) {
  val rValid = Reg(Bool) init (False)
  val dataWidth = widthOf(gen)
  val rData = Reg(Bits(dataWidth bits))

  when(io.s.fire && io.m.valid && !io.m.ready) {
    // data incoming, but output is stalled
    rValid := True
  } elsewhen (io.m.ready) {
    // pass through
    rValid := False
  }

  when(Bool(lowPower) && clockDomain.readResetWire) {
    // clear on reset
    rData := 0
  } elsewhen (Bool(lowPower) && (!io.m.valid || io.m.ready)) {
    // if no data or passthrough
    rData := 0
  } elsewhen ((!Bool(lowPower) || !Bool(
    outputReg
  ) || io.s.valid) && io.s.ready) {
    rData := io.s.payload.asBits
  }

  io.s.ready := !rValid

  if (outputReg) {
    val oValid = Reg(Bool).init(False).setName("oValid")
    io.m.valid := oValid
    when(!io.m.valid || io.m.ready) {
      oValid := io.s.valid || rValid;
    }

    val oData = Reg(Bits(dataWidth bits)).setName("oData")
    io.m.payload.assignFromBits(oData)
    when(Bool(lowPower) && clockDomain.readResetWire) {
      // clear on reset
      oData := 0
    } elsewhen (!oValid || io.m.ready) {
      when(rValid) {
        oData := rData
      } elsewhen (!Bool(lowPower) || io.s.valid) {
        oData := io.s.payload.asBits
      } otherwise {
        oData := 0
      }
    }
  } else {
    io.m.valid := !clockDomain.readResetWire && (io.s.valid || rValid)
    when(rValid) {
      io.m.payload.assignFromBits(rData)
    } elsewhen (!Bool(lowPower) || io.s.valid) {
      io.m.payload := io.s.payload
    } otherwise {
      io.m.payload.assignFromBits(U(0, dataWidth bits).asBits)
    }
  }

  doFormal { (outerReset, pastValid) =>
    when(pastValid && ~outerReset) {
      if (lowPower) {
        // m.payload is zero when !m.valid
        when(~io.m.valid) {
          assert(io.m.payload.asBits === 0)
        }
      }
    }
  }
}

object SkidBufferVerilog extends GenUtils {
  for (lowPower <- Seq(true, false)) {
    for (outputReg <- Seq(true, false)) {
      var name = "SkidBuffer"
      if (lowPower) {
        name += "LowPower"
      }
      if (outputReg) {
        name += "OutputReg"
      }

      work(
        new SkidBuffer(UInt(32 bits), lowPower, outputReg),
        name
      )
    }
  }
}
