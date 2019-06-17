package sigmastate.serialization

import sigmastate.basics.ProveDHTuple
import sigmastate.utils.GenSerializers
import sigmastate.utxo.CostTableStat

class PDHTSerializerSpecification extends SerializationSpecification {

  property("ProveDiffieHellmanTupleSerializer: Serializer round trip") {
    forAll { i: ProveDHTuple =>
      roundTripTest(i.toSigmaProp)
    }

    // In IntelliJ IDEA this test is executed last, at this point all statistics has been collected
    // uncomment to output it in the console
    // println(CostTableStat.costTableString)

    // uncomment to output info about serializers
    // println(ValueSerializer.printSerInfo())

    // ValueSerializer.serializerInfo is ready to be printed after all serializers are executed
    // at least once during test execution.
    // uncomment to save serialization info to the file
    // GenSerializers.generateSerSpec()
  }

}
