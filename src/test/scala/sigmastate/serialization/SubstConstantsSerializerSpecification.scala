package sigmastate.serialization

import sigmastate.Values.{ConcreteCollection, IntArrayConstant, IntValue}
import sigmastate.{SInt, SubstConstants}

class SubstConstantsSerializerSpecification extends SerializationSpecification {

  property("SubstConstant deserialization round trip") {
    forAll(numExprTreeNodeGen) { tree =>
      val bytes = ErgoTreeSerializer.serialize(tree)
      val newVals = ConcreteCollection(Vector[IntValue](1), SInt)
      val expr = SubstConstants(bytes, IntArrayConstant(Array(0)), newVals)
      roundTripTest(expr)
    }
  }

}
