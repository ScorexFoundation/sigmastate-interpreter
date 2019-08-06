package org.ergoplatform

import io.circe._
import io.circe.syntax._
import org.ergoplatform.validation.ValidationRules
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigmastate.{AvlTreeData, SType}
import sigmastate.Values.{ErgoTree, EvaluatedValue}
import sigmastate.helpers.SigmaTestingCommons
import sigmastate.interpreter.{ContextExtension, ProverResult}
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
}
