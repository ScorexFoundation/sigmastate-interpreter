package special.sigma

import org.scalacheck.{Arbitrary, Gen}
import sigmastate.serialization.generators.ObjectGenerators

trait SigmaTypeGens extends ObjectGenerators {
  import sigma.types._
}

