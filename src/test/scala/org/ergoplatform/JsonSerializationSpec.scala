package org.ergoplatform

import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, R4, R5, R6, R7, R8}
import org.ergoplatform.validation.ValidationRules
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigmastate.{AvlTreeData, SType}
import sigmastate.Values.{ByteArrayConstant, ByteConstant, ErgoTree, EvaluatedValue, IntConstant, LongArrayConstant, SigmaPropConstant}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.{ContextExtension, CryptoConstants, ProverResult}
import sigmastate.serialization.SerializationSpecification
import special.collection.Coll
import special.sigma.{Header, PreHeader}

class JsonSerializationSpec extends SigmaTestingCommons with SerializationSpecification with JsonCodecs {

  def jsonRoundTrip[T](v: T)(implicit encoder: Encoder[T], decoder: Decoder[T]): Unit = {
    val json = v.asJson
    withClue(s"\n for JSON: ${json.spaces2}") { json.as[T].toTry.get shouldEqual v }
  }

  property("ErgoLikeContext should be encoded into JSON and decoded back correctly") {
    forAll(ergoLikeContextGen) { v: ErgoLikeContext => jsonRoundTrip(v) }
  }

  property("sigma.BigInt should be encoded into JSON and decoded back correctly") {
    forAll { v: special.sigma.BigInt => jsonRoundTrip(v) }
  }

  property("byte array should be encoded into JSON and decoded back correctly") {
    forAll(byteArrayGen(0, 1000)) { v: Array[Byte] => jsonRoundTrip(v) }
  }

  property("byte coll should be encoded into JSON and decoded back correctly") {
    forAll(byteCollGen(0, 1000)) { v: Coll[Byte] => jsonRoundTrip(v) }
  }

  property("ADKey should be encoded into JSON and decoded back correctly") {
    forAll(boxIdGen) { v: ADKey => jsonRoundTrip(v) }
  }

  property("ADDigest should be encoded into JSON and decoded back correctly") {
    forAll(aDDigestGen) { v: ADDigest => jsonRoundTrip(v) }
  }

  property("Digest32 should be encoded into JSON and decoded back correctly") {
    forAll(digest32Gen) { v: Digest32 => jsonRoundTrip(v) }
  }

  property("ModifierId should be encoded into JSON and decoded back correctly") {
    forAll(modifierIdGen) { v: ModifierId => jsonRoundTrip(v) }
  }

  property("Header should be encoded into JSON and decoded back correctly") {
    forAll(headerGen) { v: Header => jsonRoundTrip(v) }
  }

  property("PreHeader should be encoded into JSON and decoded back correctly") {
    forAll(preHeaderGen) { v: PreHeader => jsonRoundTrip(v) }
  }

  property("EvaluatedValue[SType] should be encoded into JSON and decoded back correctly") {
    forAll(evaluatedValueGen) { v: EvaluatedValue[SType] => jsonRoundTrip(v) }
  }

  property("DataInput should be encoded into JSON and decoded back correctly") {
    forAll(dataInputGen) { v: DataInput => jsonRoundTrip(v) }
  }

  property("Input should be encoded into JSON and decoded back correctly") {
    forAll(inputGen) { v: Input => jsonRoundTrip(v) }
  }

  property("UnsignedInput should be encoded into JSON and decoded back correctly") {
    forAll(unsignedInputGen) { v: UnsignedInput => jsonRoundTrip(v) }
  }

  property("ContextExtension should be encoded into JSON and decoded back correctly") {
    forAll(contextExtensionGen) { v: ContextExtension => jsonRoundTrip(v) }
  }

  property("ProverResult should be encoded into JSON and decoded back correctly") {
    forAll(serializedProverResultGen) { v: ProverResult => jsonRoundTrip(v) }
  }

  property("AvlTreeData should be encoded into JSON and decoded back correctly") {
    forAll(avlTreeDataGen) { v: AvlTreeData => jsonRoundTrip(v) }
  }

  property("ErgoTree should be encoded into JSON and decoded back correctly") {
    forAll(ergoTreeGen) { v: ErgoTree => jsonRoundTrip(v) }
  }

  property("ErgoBox should be encoded into JSON and decoded back correctly") {
    forAll(ergoBoxGen) { v: ErgoBox => jsonRoundTrip(v) }
  }

  property("ErgoLikeTransaction should be encoded into JSON and decoded back correctly") {
    forAll(ergoLikeTransactionGen) { v: ErgoLikeTransaction => jsonRoundTrip(v) }
  }

  property("UnsignedErgoLikeTransaction should be encoded into JSON and decoded back correctly") {
    forAll(unsignedErgoLikeTransactionGen) { v: UnsignedErgoLikeTransaction => jsonRoundTrip(v) }
  }

  property("ErgoLikeTransactionTemplate should be encoded into JSON and decoded back correctly") {
    forAll(ergoLikeTransactionTemplateGen) { v: ErgoLikeTransactionTemplate[_ <: UnsignedInput] =>
      v.asJson.as(ergoLikeTransactionTemplateDecoder).toTry.get shouldEqual v
    }
  }

  property("SigmaValidationSettingsEncoder should be encoded into JSON and decoded back correctly") {
    jsonRoundTrip(ValidationRules.currentSettings)
  }

  property("ErgoBox.additionalRegisters should be encoded into JSON in certain order") {
    val minerPkHex = "0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942"
    val minerPk = Base16.decode(minerPkHex).map { point =>
      ProveDlog(
        CryptoConstants.dlogGroup.curve.decodePoint(point).asInstanceOf[CryptoConstants.EcPointType]
      )
    }.get
    val regs = Map(
      R7 -> LongArrayConstant(Array(1L, 2L, 1234123L)),
      R4 -> ByteConstant(1),
      R6 -> IntConstant(10),
      R5 -> SigmaPropConstant(minerPk),
      R8 -> ByteArrayConstant(Base16.decode("123456123456123456123456123456123456123456123456123456123456123456").get),
    )
    // registers should be ordered by their number in Json
    regs.asJson.spaces2 shouldEqual
      """{
        |  "R4" : "0201",
        |  "R5" : "08cd0326df75ea615c18acc6bb4b517ac82795872f388d5d180aac90eaa84de750b942",
        |  "R6" : "0414",
        |  "R7" : "1103020496d39601",
        |  "R8" : "0e21123456123456123456123456123456123456123456123456123456123456123456"
        |}""".stripMargin
  }
}
