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

}
