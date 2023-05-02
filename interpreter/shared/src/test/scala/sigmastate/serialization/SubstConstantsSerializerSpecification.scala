package sigmastate.serialization

import sigmastate.Values.{ConcreteCollection, IntConstant, IntArrayConstant, IntValue}
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.{CrossVersionProps, SInt, EQ, SubstConstants}

class SubstConstantsSerializerSpecification extends SerializationSpecification
  with CrossVersionProps {

  property("SubstConstant deserialization round trip") {
    forAll(numExprTreeNodeGen) { prop =>
      val tree = mkTestErgoTree(EQ(prop, IntConstant(1)).toSigmaProp)
      val bytes = DefaultSerializer.serializeErgoTree(tree)
      val newVals = ConcreteCollection(Array[IntValue](1), SInt)
      val expr = SubstConstants(bytes, IntArrayConstant(Array(0)), newVals)
      roundTripTest(expr)
    }
  }

}
