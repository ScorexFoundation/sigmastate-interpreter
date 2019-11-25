package sigmastate.verification.test

import org.scalacheck.Gen
import scalan.RType
import sigmastate.eval.Extensions._
import sigmastate.serialization.generators.ObjectGenerators
import special.collection.Coll
import stainless.annotation.ignore

@ignore
trait MiscGenerators extends ObjectGenerators {

  def collOfGen[A: RType](g: Gen[A], length: Int): Gen[Coll[A]] = Gen.listOfN(length, g).map(_.toColl)

}
