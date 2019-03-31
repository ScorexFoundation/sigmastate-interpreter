package sigmastate.serialization

import sigmastate.Values.{ConcreteCollection, IntArrayConstant, IntConstant, IntValue}
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.{EQ, SInt, SubstConstants}

class SubstConstantsSerializerSpecification extends SerializationSpecification {

  property("SubstConstant deserialization round trip") {
    forAll(numExprTreeNodeGen) { tree =>
      val bytes = DefaultSerializer.serializeWithSegregation(EQ(tree, IntConstant(1)).toSigmaProp)
      val newVals = ConcreteCollection(Vector[IntValue](1), SInt)
      val expr = SubstConstants(bytes, IntArrayConstant(Array(0)), newVals)
      roundTripTest(expr)
    }
  }

}
