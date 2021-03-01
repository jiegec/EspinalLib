package espinallib.hardfloat.rewrite

import spinal.core._

object OrReduceBy4 {
  def apply(in: UInt): UInt = {
    val reducedWidth = (in.getWidth + 3) >> 2
    val reducedVec = Vec(Bool(), reducedWidth)
    val inBits: Bits = in.asBits
    for (ix <- 0 until reducedWidth - 1) {
      reducedVec(ix) := inBits(ix * 4 + 3 downto ix * 4).orR
    }
    reducedVec(reducedWidth - 1) :=
      in(in.getWidth - 1 downto (reducedWidth - 1) * 4).orR
    Cat(reducedVec).asUInt
  }
}
