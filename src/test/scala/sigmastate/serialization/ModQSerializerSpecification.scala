package sigmastate.serialization

import sigmastate.Values.BigIntConstant
import sigmastate._

class ModQSerializerSpecification extends SerializationSpecification {

  property("ModQ: Serializer round trip") {
    forAll(bigIntConstGen) { x: BigIntConstant =>
      roundTripTest(ModQ(x))
    }
  }

  property("PlusModQ: Serializer round trip") {
    forAll(bigIntConstGen, bigIntConstGen) { (x1: BigIntConstant, x2: BigIntConstant) =>
      roundTripTest(PlusModQ(x1, x2))
    }
  }

  property("MinusModQ: Serializer round trip") {
    forAll(bigIntConstGen, bigIntConstGen) { (x1: BigIntConstant, x2: BigIntConstant) =>
      roundTripTest(MinusModQ(x1, x2))
    }
  }
}
