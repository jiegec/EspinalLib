package espinallib.hardfloat.rewrite

import spinal.core._

class RawFloat(
    expWidth: Int,
    sigWidth: Int
) extends Bundle {
    val isNaN = Bool()
    val isInf = Bool()
    val isZero = Bool()
    val sign = Bool()
    val sExp = SInt(expWidth + 2 bits)
    val sig = UInt(sigWidth + 1 bits)

    def isSigNan: Bool = isNaN && !sig(sigWidth - 2)
}
