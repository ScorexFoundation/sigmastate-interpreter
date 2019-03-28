package sigmastate

import special.sigma.SigmaDslBuilder

package object eval {
  /** Shortcut to access Global object whenever using more meaningful name. */
  val SigmaDsl: SigmaDslBuilder = CostingSigmaDslBuilder

  /** Shortcut to access non-instance (aka static) methods of collection
    * (e.g. `fromArray`, `replicate`, `fromItems`, `unzip` etc.) */
  val Colls = SigmaDsl.Colls
}
