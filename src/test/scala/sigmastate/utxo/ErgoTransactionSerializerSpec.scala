package sigmastate.utxo

import org.ergoplatform.ErgoTransaction
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import sigmastate.serialization.generators.ValueGenerators

class ErgoTransactionSerializerSpec extends PropSpec
  with GeneratorDrivenPropertyChecks
  with SerializationRoundTripSpec
  with ValueGenerators {

  private val ergoTransactionSerializer = ErgoTransaction.serializer

  property("ErgoTransaction: Serializer round trip") {
    forAll { t: ErgoTransaction =>
      ergoTransactionSerializer.parseBytes(ergoTransactionSerializer.toBytes(t)).get shouldEqual t
    }
  }

  property("ErgoTransaction: start pos and consumed bytes") {
    forAll { t: ErgoTransaction =>
      val randomBytesCount = Gen.chooseNum(1, 20).sample.get
      val randomBytes = Gen.listOfN(randomBytesCount, arbByte.arbitrary).sample.get.toArray
      val bytes = ergoTransactionSerializer.toBytes(t)
      ergoTransactionSerializer.parseBody(randomBytes ++ bytes, randomBytesCount) shouldEqual (t, bytes.length)
    }
  }
}
