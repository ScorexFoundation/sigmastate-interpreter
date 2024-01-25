package sigmastate

import org.scalacheck.Gen
import sigma.interpreter.SigmaMap
import sigmastate.helpers.TestingCommons

// old representation scala.collection.Map[Byte, EvaluatedValue[_ <: SType]]
class SigmaMapSpecification extends TestingCommons {

  property ("SigmaMap.empty") {
    val empty = SigmaMap.empty
    empty.knownSize shouldBe 0
    empty.iterator.toSeq.isEmpty shouldBe true
  }

  property("traversal equivalence") {
    forAll(Gen.chooseNum(0, Byte.MaxValue)){ n =>
     // (0 until n) do {

     // }
    }
  }
}
