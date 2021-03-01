package espinallib.hardfloat.rewrite

import espinallib.hardfloat.rewrite.Consts.roundDown
import spinal.core._

// https://github.com/ucb-bar/berkeley-hardfloat/blob/master/src/main/scala/AddRecFN.scala
class AddRawFN(
                expWidth: Int,
                sigWidth: Int
              ) extends Component {
  val io = new Bundle {
    // add or sub
    val subOp = in(Bool())

    // input values
    val a = in(new RawFloat(expWidth, sigWidth))
    val b = in(new RawFloat(expWidth, sigWidth))

    // flags
    val roundingMode = in(Bits(3 bits))

    // output
    val invalidExc = out(Bool())
    val rawOut = out(new RawFloat(expWidth, sigWidth + 2))
  }

  val alignDistWidth: Int = log2Up(sigWidth)

  // effective sign of b
  val effSignB: Bool = io.b.sign ^ io.subOp
  val eqSigns: Bool = io.a.sign === effSignB
  val notEqSigns_signZero: Bool = io.roundingMode === roundDown
  val sDiffExps: SInt = io.a.sExp - io.b.sExp
  val modNatAlignDist: UInt = Mux(sDiffExps < S(0), io.b.sExp - io.a.sExp, sDiffExps).asUInt(0, alignDistWidth - 1 bits)
  val isMaxAlign: Bool =
    (sDiffExps >> alignDistWidth) =/= S(0) &&
      ((sDiffExps >> alignDistWidth) =/= S(-1) || sDiffExps.asBits(0, alignDistWidth - 1 bits) === B(0))
  val alignDist: UInt = Mux(isMaxAlign, U((BigInt(1) << alignDistWidth) - 1), modNatAlignDist)
  val closeSubMags: Bool = !eqSigns && !isMaxAlign && (modNatAlignDist <= U(1))
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val close_alignedSigA: UInt =
    Mux((S(0) <= sDiffExps) && sDiffExps(0), io.a.sig << 2, U(0)) |
      Mux((S(0) <= sDiffExps) && !sDiffExps(0), io.a.sig << 1, U(0)) |
      Mux((sDiffExps < S(0)), io.a.sig, U(0))
  val close_sSigSum: SInt = close_alignedSigA.asSInt - (io.b.sig << 1).asSInt
  val close_sigSum: UInt = Mux(close_sSigSum < S(0), -close_sSigSum, close_sSigSum).asUInt(0, sigWidth + 1 bits)
  val close_adjustedSigSum: UInt = close_sigSum << (sigWidth & 1)
  val close_reduced2SigSum: UInt = OrReduceBy2(close_adjustedSigSum)
  val close_normDistReduced2: UInt = CountLeadingZeros(close_reduced2SigSum)
  val close_nearNormDist: UInt = (close_normDistReduced2 << 1)(0, alignDistWidth - 1 bits)
  val close_sigOut: UInt = ((close_sigSum << close_nearNormDist) << 1)(0, sigWidth + 2 bits)
  val close_totalCancellation: Bool = !(close_sigOut((sigWidth + 2) downto (sigWidth + 1)).orR)
  val close_notTotalCancellation_signOut: Bool = io.a.sign ^ (close_sSigSum < S(0))
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val far_signOut: Bool = Mux(sDiffExps < S(0), effSignB, io.a.sign)
  val far_sigLarger: UInt = Mux(sDiffExps < S(0), io.b.sig, io.a.sig)(sigWidth - 1 downto 0)
  val far_sigSmaller: UInt = Mux(sDiffExps < S(0), io.a.sig, io.b.sig)(sigWidth - 1 downto 0)
  val far_mainAlignedSigSmaller: UInt = (far_sigSmaller << 5) >> alignDist
  val far_reduced4SigSmaller: UInt = OrReduceBy4(far_sigSmaller << 2)
  val far_roundExtraMask: UInt = LowMask(alignDist(alignDistWidth - 1 downto 2), (sigWidth + 5) / 4, 0)
  val far_alignedSigSmaller: Bits =
    Cat(far_mainAlignedSigSmaller >> 3,
      far_mainAlignedSigSmaller(2 downto 0).orR || (far_reduced4SigSmaller & far_roundExtraMask).orR)
  val far_subMags: Bool = !eqSigns
  val far_negAlignedSigSmaller: Bits = Mux(far_subMags, Cat(U(1), ~far_alignedSigSmaller), far_alignedSigSmaller)
  val far_sigSum: UInt = (far_sigLarger << 3) + far_negAlignedSigSmaller.asUInt + far_subMags.asUInt
  val far_sigOut: UInt = Mux(far_subMags, far_sigSum, (far_sigSum >> 1) | far_sigSum(0 downto 0))(sigWidth + 2 downto 0)
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  val notSigNaN_invalidExc: Bool = io.a.isInf && io.b.isInf && !eqSigns
  val notNaN_isInfOut: Bool = io.a.isInf || io.b.isInf
  val addZeros: Bool = io.a.isZero && io.b.isZero
  val notNaN_specialCase: Bool = notNaN_isInfOut || addZeros
  val notNaN_isZeroOut: Bool = addZeros || (!notNaN_isInfOut && closeSubMags && close_totalCancellation)
  val notNaN_signOut: Bool =
    (eqSigns && io.a.sign) ||
      (io.a.isInf && io.a.sign) ||
      (io.b.isInf && effSignB) ||
      (notNaN_isZeroOut && !eqSigns && notEqSigns_signZero) ||
      (!notNaN_specialCase && closeSubMags && !close_totalCancellation
        && close_notTotalCancellation_signOut) ||
      (!notNaN_specialCase && !closeSubMags && far_signOut)
  val common_sExpOut: SInt =
    (Mux(closeSubMags || (sDiffExps < S(0)), io.b.sExp, io.a.sExp)
      - Mux(closeSubMags, close_nearNormDist, far_subMags.asUInt).asSInt)
  val common_sigOut: UInt = Mux(closeSubMags, close_sigOut, far_sigOut)
  /*------------------------------------------------------------------------
  *------------------------------------------------------------------------*/
  io.invalidExc := io.a.isSigNan || io.b.isSigNan || notSigNaN_invalidExc
  io.rawOut.isInf := notNaN_isInfOut
  io.rawOut.isZero := notNaN_isZeroOut
  io.rawOut.sExp := common_sExpOut
  io.rawOut.isNaN := io.a.isNaN || io.b.isNaN
  io.rawOut.sign := notNaN_signOut
  io.rawOut.sig := common_sigOut
}
