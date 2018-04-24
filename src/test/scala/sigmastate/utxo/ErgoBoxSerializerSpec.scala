package sigmastate.utxo

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scorex.crypto.hash.Digest32
import sigmastate.SType
import sigmastate.Values.{FalseLeaf, TrueLeaf, Value}
import sigmastate.utxo.ErgoBox.NonMandatoryIdentifier
import collection.JavaConverters._

class ErgoBoxSerializerSpec extends PropSpec with GeneratorDrivenPropertyChecks with SerializationRoundTripSpec {

  implicit val ergoBoxSerializer = ErgoBox.serializer

  def arGen(cnt: Byte): Seq[Gen[(NonMandatoryIdentifier, Value[SType])]] = {
    (0 until cnt).map(_ + ErgoBox.startingNonMandatoryIndex)
      .map(rI => ErgoBox.registerByIndex(rI.toByte).asInstanceOf[NonMandatoryIdentifier])
      .map(r => Gen.oneOf(TrueLeaf, FalseLeaf).map(v => r -> v))
  }

  val ergoBoxGen: Gen[ErgoBox] = for {
    l <- arbLong.arbitrary
    b <- Gen.oneOf(TrueLeaf, FalseLeaf)
    tId <- Gen.listOfN(32, arbByte.arbitrary)
    boxId <- arbShort.arbitrary
    regNum <- Gen.chooseNum[Byte](0, 7)
    ar <- Gen.sequence(arGen(regNum))
  } yield ErgoBox(l, b, ar.asScala.toMap, Digest32 @@ tId.toArray, boxId)

  implicit val arbBox: Arbitrary[ErgoBox] = Arbitrary(ergoBoxGen)

  property("ErgoBox: Serializer round trip") {
    forAll { b: ErgoBox =>
      roundTripTest(b)
    }
  }
}
