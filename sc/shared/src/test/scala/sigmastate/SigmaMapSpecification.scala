package sigmastate

import org.scalacheck.Gen
import sigma.ast.{EvaluatedValue, IntConstant, SType}
import sigma.interpreter.SigmaMap
import sigmastate.helpers.TestingCommons

import scala.util.Random

class SigmaMapSpecification extends TestingCommons {

  property("SigmaMap.empty") {
    val empty = SigmaMap.empty
    empty.knownSize shouldBe 0
    empty.iterator.toSeq.isEmpty shouldBe true
  }

  property("traversal equivalence") {
    val fullArr = (0 to Byte.MaxValue).map { b => b.toByte -> IntConstant(b) }
    forAll(Gen.chooseNum(0, Byte.MaxValue)) { n =>
      val rnd = Random.shuffle(fullArr).take(n)
      val oldRepr: scala.collection.Map[Byte, EvaluatedValue[_ <: SType]] = rnd.toMap
      val sm = SigmaMap(oldRepr)

      oldRepr.iterator.toList shouldBe sm.iterator.toList
    }
  }

}
