package sigmastate.serialization

import sigmastate.Values.{ConcreteCollection, IntArrayConstant, IntConstant, IntValue}
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.{EQ, SInt, SubstConstants}

class SubstConstantsSerializerSpecification extends SerializationSpecification {

  property("SubstConstant deserialization round trip") {
    forAll(numExprTreeNodeGen) { prop =>
      val tree = EQ(prop, IntConstant(1)).toSigmaProp.treeWithSegregation
      val bytes = DefaultSerializer.serializeErgoTree(tree)
      val newVals = ConcreteCollection(Array[IntValue](1), SInt)
      val expr = SubstConstants(bytes, IntArrayConstant(Array(0)), newVals)
      roundTripTest(expr)
    }
  }

}
