package sigmastate.serialization

import sigmastate.Values.AvlTreeConstant
import sigmastate.AvlTreeFlags

class AvlTreeSpecification extends SerializationSpecification {

  private val flags = Array(
    AvlTreeFlags(false, false, false),
    AvlTreeFlags(false, false, true),
    AvlTreeFlags(false, true, false),
    AvlTreeFlags(false, true, true),
    AvlTreeFlags(true, false, false),
    AvlTreeFlags(true, false, true),
    AvlTreeFlags(true, true, false),
    AvlTreeFlags(true, true, true)
  )

  property("roundtrip for all the possible AVL tree flags") {
    flags.foreach { f =>
      AvlTreeFlags(AvlTreeFlags.serializeFlags(f)) shouldBe f
    }
  }

  property("roundtrip for an AVL tree") {
    forAll(avlTreeGen) { t =>
      val v = AvlTreeConstant(t)
      roundTripTest(v)
    }
  }
}
