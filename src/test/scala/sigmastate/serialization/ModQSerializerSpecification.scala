package sigmastate.serialization

import sigmastate.Values.BigIntConstant
import sigmastate._

class ModQSerializerSpecification extends SerializationSpecification {

  // TODO https://github.com/ScorexFoundation/sigmastate-interpreter/issues/327
  ignore("ModQ: Serializer round trip") {
    forAll(bigIntConstGen) { x: BigIntConstant =>
      roundTripTest(ModQ(x))
    }
  }

  // TODO https://github.com/ScorexFoundation/sigmastate-interpreter/issues/327
  ignore("PlusModQ: Serializer round trip") {
    forAll(bigIntConstGen, bigIntConstGen) { (x1: BigIntConstant, x2: BigIntConstant) =>
      roundTripTest(PlusModQ(x1, x2))
    }
  }

  // TODO https://github.com/ScorexFoundation/sigmastate-interpreter/issues/327
  ignore("MinusModQ: Serializer round trip") {
    forAll(bigIntConstGen, bigIntConstGen) { (x1: BigIntConstant, x2: BigIntConstant) =>
      roundTripTest(MinusModQ(x1, x2))
    }
  }
}
