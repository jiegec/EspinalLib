package espinallib.hardfloat.rewrite

import spinal.core._
import spinal.lib.Reverse

// https://github.com/ucb-bar/berkeley-hardfloat/blob/master/src/main/scala/primitives.scala
object LowMask {
  def apply(in: UInt, topBound: BigInt, bottomBound: BigInt): UInt = {
    require(topBound != bottomBound)
    val numInVals = BigInt(1) << in.getWidth
    if (topBound < bottomBound) {
      LowMask(~in, numInVals - 1 - topBound, numInVals - 1 - bottomBound)
    } else if (numInVals > 64 /* Empirical */ ) {
      // For simulation performance, we should avoid generating
      // extremely wide shifters, so we divide and conquer.
      // Empirically, this does not impact synthesis QoR.
      val mid = numInVals / 2
      val msb = in(in.getWidth - 1)
      val lsbs = in(in.getWidth - 2 downto 0)
      if (mid < topBound) {
        if (mid <= bottomBound) {
          Mux(msb, LowMask(lsbs, topBound - mid, bottomBound - mid), U(0))
        } else {
          Mux(
            msb,
            Cat(
              LowMask(lsbs, topBound - mid, 0),
              U((BigInt(1) << (mid - bottomBound).toInt) - 1)
            ).asUInt,
            LowMask(lsbs, mid, bottomBound)
          )
        }
      } else {
        ~Mux(msb, U(0), ~LowMask(lsbs, topBound, bottomBound))
      }
    } else {
      val shift = S(BigInt(-1) << numInVals.toInt) >> in
      Reverse(
        shift(
          (numInVals - 1 - bottomBound).toInt downto
            (numInVals - topBound).toInt
        )
      ).asUInt
    }
  }
}
