package sigmastate.serialization

import sigmastate.lang.SigmaTyper

class UpcastOnDeserializationSpecification extends SerializationSpecification {

  property("Upcast deserialization round trip") {
    forAll(comparisonExprTreeNodeGen, minSuccessful(500)) { tree =>
      val typedTree = (new SigmaTyper).typecheck(tree)
      roundTripTest(typedTree)
    }
  }
}
