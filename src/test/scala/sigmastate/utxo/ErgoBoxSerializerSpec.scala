package sigmastate.utxo

import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scorex.crypto.hash.Digest32
import sigmastate.SType
import sigmastate.Values.{FalseLeaf, TrueLeaf, Value}
import sigmastate.utxo.ErgoBox.{NonMandatoryIdentifier, R3}

class ErgoBoxSerializerSpec extends PropSpec with GeneratorDrivenPropertyChecks with SerializationRoundTripSpec {

  implicit val ergoBoxSerializer = ErgoBox.serializer

  val arGen: Gen[(NonMandatoryIdentifier, Value[SType])] = for {
    rIndex <- Gen.chooseNum[Byte](3, 9)
    r = ErgoBox.registerByIndex(rIndex).asInstanceOf[NonMandatoryIdentifier]
    v <- Gen.oneOf(TrueLeaf, FalseLeaf)
  } yield (r, v)

  val arMapGen: Gen[Map[NonMandatoryIdentifier, Value[SType]]] = for {
    length <- Gen.chooseNum(0, 3)
    list <- Gen.listOfN(length, arGen)
  } yield list.toMap


  val ergoBoxGen: Gen[ErgoBox] = for {
    l <- arbLong.arbitrary
    b <- Gen.oneOf(TrueLeaf, FalseLeaf)
    tId <- Gen.listOfN(32, arbByte.arbitrary)
    boxId <- arbShort.arbitrary
    ar <- arMapGen
  } yield ErgoBox(l, b, ar, Digest32 @@ tId.toArray, boxId)

  implicit val arbBox: Arbitrary[ErgoBox] = Arbitrary(ergoBoxGen)

  property("ErgoBox: Serializer round trip") {
    forAll { b: ErgoBox =>
      roundTripTest(b)
    }
  }

}
