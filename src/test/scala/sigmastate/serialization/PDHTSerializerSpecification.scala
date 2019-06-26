package sigmastate.serialization

import sigmastate.basics.ProveDHTuple
import sigmastate.utils.GenSerializers
import sigmastate.utxo.{CostTableStat, ComplexityTableStat}

class PDHTSerializerSpecification extends SerializationSpecification {

  property("ProveDiffieHellmanTupleSerializer: Serializer round trip") {
    forAll { i: ProveDHTuple =>
      roundTripTest(i.toSigmaProp)
    }
    // In IntelliJ IDEA this test is executed last, at this point all statistics has been collected
    // We output it here in the console
//    println(CostTableStat.costTableString)
//    println(ValueSerializer.printSerInfo())
//    GenSerializers.generateSerSpec()
  }

}
