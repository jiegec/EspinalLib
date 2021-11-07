package espinallib.hardfloat.rewrite

import spinal.core.B
import spinal.core.Bits

object Consts {
  // rounding modes in RISC-V

  // Round to Nearest, ties to Even
  def roundNearestEven: Bits = B("b000")

  // Round towards Zero
  def roundTowardsZero: Bits = B("b001")

  // Round Down
  def roundDown: Bits = B("b010")

  // Round Up
  def roundUp: Bits = B("b011")

  // Round to Nearest, ties to Max Magnitude
  def roundNearestMaxMag: Bits = B("b100")
}
