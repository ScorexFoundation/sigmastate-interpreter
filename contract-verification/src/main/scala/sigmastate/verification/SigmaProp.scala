package sigmastate.verification

import stainless.annotation.{extern, library, pure}

@library
case class SigmaProp(@extern sigmaProp: special.sigma.SigmaProp) {
  @extern @pure
  def isValid: Boolean = ???
}
