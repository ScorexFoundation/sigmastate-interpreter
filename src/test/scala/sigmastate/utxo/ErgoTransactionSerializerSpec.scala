package sigmastate.utxo

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import sigmastate.serialization.generators.ValueGenerators

import scala.util.Success

class ErgoTransactionSerializerSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with SerializationRoundTripSpec
  with ValueGenerators {

  private val ergoTransactionSerializer = ErgoTransaction.serializer

  property("ErgoTransaction: Serializer round trip") {
    forAll { t: ErgoTransaction =>
      ergoTransactionSerializer.parseBytes(ergoTransactionSerializer.toBytes(t)) shouldEqual Success(t)
    }
  }
}
