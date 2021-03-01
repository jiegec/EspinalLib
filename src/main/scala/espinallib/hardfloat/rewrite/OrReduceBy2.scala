package espinallib.hardfloat.rewrite

import spinal.core._

object OrReduceBy2 {
  def apply(in: UInt): UInt = {
    val reducedWidth = (in.getWidth + 1) >> 1
    val reducedVec = Vec(Bool(), reducedWidth)
    val inBits: Bits = in.asBits
    for (ix <- 0 until reducedWidth - 1) {
      reducedVec(ix) := inBits(ix * 2 + 1 downto ix * 2).orR
    }
    reducedVec(reducedWidth - 1) :=
      in(in.getWidth - 1 downto (reducedWidth - 1) * 2).orR
    Cat(reducedVec).asUInt
  }
}
