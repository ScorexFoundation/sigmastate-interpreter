package sigmastate

import special.sigma.SigmaDslBuilder

package object eval {
  val SigmaDsl: SigmaDslBuilder = CostingSigmaDslBuilder
  val Colls = SigmaDsl.Colls
}
