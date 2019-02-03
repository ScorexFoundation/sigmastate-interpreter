package special.sigma

class ExtensionMethods(dsl: SigmaDslBuilder) {
  implicit class BooleanOps(source: Boolean) {
    def &&(prop: SigmaProp) = dsl.sigmaProp(source) && prop
  }
}
