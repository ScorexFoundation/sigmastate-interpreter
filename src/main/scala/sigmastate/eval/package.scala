package sigmastate

import special.sigma.SigmaDslBuilder

package object eval {
  val SigmaDsl = CostingSigmaDslBuilder
  val Colls = SigmaDsl.Colls
}
