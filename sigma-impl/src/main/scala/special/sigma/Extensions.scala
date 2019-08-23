package special.sigma

import org.bouncycastle.math.ec.ECPoint

/** This extensions are used from SigmaDsl.
  * If you add something here, make sure the corresponding syntax is supported by SigmaScript. */
class DslSyntaxExtensions(dsl: SigmaDslBuilder) {
  implicit class BooleanOps(source: Boolean) {
    /** Logical AND between Boolean on the left and SigmaProp value on the right. */
    def &&(prop: SigmaProp) = dsl.sigmaProp(source) && prop

    /** Logical AND between Boolean on the left and SigmaProp value on the right. */
    def ||(prop: SigmaProp) = dsl.sigmaProp(source) || prop
  }
}

object Extensions {

  def showECPoint(p: ECPoint): String = {
    if (p.isInfinity) {
      "INF"
    }
    else {
      val rawX = p.getRawXCoord.toString.substring(0, 6)
      val rawY = p.getRawYCoord.toString.substring(0, 6)
      s"ECPoint($rawX,$rawY,...)"
    }
  }

  implicit class GroupElementOps(val source: GroupElement) extends AnyVal {
    def showToString: String = showECPoint(source.value)
  }

}
