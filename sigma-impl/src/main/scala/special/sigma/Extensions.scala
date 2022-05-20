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
