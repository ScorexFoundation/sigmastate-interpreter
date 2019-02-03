package special.sigma

class ExtensionMethods(dsl: SigmaDslBuilder) {
  implicit class BooleanOps(source: Boolean) {
    /** Logical AND between Boolean on the left and SigmaProp value on the right. */
    def &&(prop: SigmaProp) = dsl.sigmaProp(source) && prop

    /** Logical AND between Boolean on the left and SigmaProp value on the right. */
    def ||(prop: SigmaProp) = dsl.sigmaProp(source) || prop
  }
}
