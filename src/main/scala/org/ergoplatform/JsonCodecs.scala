package org.ergoplatform

import cats.syntax.either._
import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import org.ergoplatform.settings.Algos
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigmastate.Values.{ErgoTree, EvaluatedValue}
import sigmastate.eval.{CGroupElement, CPreHeader, WrapperOf, _}
import sigmastate.serialization.{ErgoTreeSerializer, ValueSerializer}
import sigmastate.{AvlTreeData, SType}
import special.collection.Coll
import special.sigma.{Header, PreHeader}

import scala.util.Try

trait JsonCodecs {
  //TODO: remove in ergo

  def fromTry[T](tryResult: Try[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    tryResult.fold(e => Left(DecodingFailure(e.toString, cursor.history)), Right.apply)
  }

  def fromOption[T](maybeResult: Option[T])(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    maybeResult.fold[Either[DecodingFailure, T]](Left(DecodingFailure("No value found", cursor.history)))(Right.apply)
  }

  def fromThrows[T](throwsBlock: => T)(implicit cursor: ACursor): Either[DecodingFailure, T] = {
    Either.catchNonFatal(throwsBlock).leftMap(e => DecodingFailure(e.toString, cursor.history))
  }

  private def bytesDecoder[T](transform: Array[Byte] => T): Decoder[T] = { implicit cursor =>
    for {
      str <- cursor.as[String]
      bytes <- fromTry(Algos.decode(str))
    } yield transform(bytes)
  }

  implicit val sigmaBigIntEncoder: Encoder[special.sigma.BigInt] = { bigInt =>
    JsonNumber.fromDecimalStringUnsafe(bigInt.asInstanceOf[WrapperOf[BigInt]].wrappedValue.toString).asJson
  }

  implicit val sigmaBigIntDecoder: Decoder[special.sigma.BigInt] = { implicit cursor =>
    for {
      str <- cursor.as[String]
      bigInt <- fromOption(JsonNumber.fromString(str).flatMap(_.toBigInt))
    } yield CBigInt(bigInt.bigInteger)
  }

  implicit val arrayBytesEncoder: Encoder[Array[Byte]] = Algos.encode(_).asJson
  implicit val arrayBytesDecoder: Decoder[Array[Byte]] = bytesDecoder(x => x)

  implicit val collBytesEncoder: Encoder[Coll[Byte]] = Algos.encode(_).asJson
  implicit val collBytesDecoder: Decoder[Coll[Byte]] = bytesDecoder(Colls.fromArray(_))

  implicit val adKeyEncoder: Encoder[ADKey] = _.array.asJson
  implicit val adKeyDecoder: Decoder[ADKey] = bytesDecoder(ADKey @@ _)

  implicit val digest32Encoder: Encoder[Digest32] = _.array.asJson
  implicit val digest32Decoder: Decoder[Digest32] = bytesDecoder(Digest32 @@ _)

  implicit val modifierIdEncoder: Encoder[ModifierId] = _.asInstanceOf[String].asJson

  implicit val registerIdEncoder: KeyEncoder[NonMandatoryRegisterId] = { regId =>
    s"R${regId.number}"
  }

  implicit val registerIdDecoder: KeyDecoder[NonMandatoryRegisterId] = { key =>
    ErgoBox.registerByName.get(key).collect {
      case nonMandatoryId: NonMandatoryRegisterId => nonMandatoryId
    }
  }

  implicit val headerEncoder: Encoder[Header] = { h: Header =>
    Map(
      "id" -> h.id.asJson,
      "version" -> h.version.asJson,
      "parentId" -> h.parentId.asJson,
      "adProofsRoot" -> h.ADProofsRoot.asJson,
      // TODO: fix
      "stateRoot" -> h.stateRoot.asInstanceOf[WrapperOf[AvlTreeData]].wrappedValue.asJson,
      "transactionsRoot" -> h.transactionsRoot.asJson,
      "timestamp" -> h.timestamp.asJson,
      "nBits" -> h.nBits.asJson,
      "height" -> h.height.asJson,
      "extensionRoot" -> h.extensionRoot.asJson,
      "minerPk" -> h.minerPk.getEncoded.asJson,
      "powOnetimePk" -> h.powOnetimePk.getEncoded.asJson,
      "powNonce" -> h.powNonce.asJson,
      "powDistance" -> h.powDistance.asJson,
      "votes" -> h.votes.asJson
    ).asJson
  }

  implicit val headerDecoder: Decoder[Header] = { cursor =>
    for {
      id <- cursor.downField("id").as[Coll[Byte]]
      version <- cursor.downField("version").as[Byte]
      parentId <- cursor.downField("parentId").as[Coll[Byte]]
      adProofsRoot <- cursor.downField("adProofsRoot").as[Coll[Byte]]
      stateRoot <- cursor.downField("stateRoot").as[AvlTreeData]
      transactionsRoot <- cursor.downField("transactionsRoot").as[Coll[Byte]]
      timestamp <- cursor.downField("timestamp").as[Long]
      nBits <- cursor.downField("nBits").as[Long]
      height <- cursor.downField("height").as[Int]
      extensionRoot <- cursor.downField("extensionRoot").as[Coll[Byte]]
      minerPk <- cursor.downField("minerPk").as[Coll[Byte]]
      powOnetimePk <- cursor.downField("powOnetimePk").as[Coll[Byte]]
      powNonce <- cursor.downField("powNonce").as[Coll[Byte]]
      powDistance <- cursor.downField("powDistance").as[special.sigma.BigInt]
      votes <- cursor.downField("votes").as[Coll[Byte]]
    } yield new CHeader(id, version, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp, nBits,
      height, extensionRoot, CGroupElement.decode(minerPk), CGroupElement.decode(powOnetimePk), powNonce, powDistance, votes)
  }

  implicit val preHeaderEncoder: Encoder[PreHeader] = { v: PreHeader =>
    Map(
      "version" -> v.version.asJson,
      "parentId" -> v.parentId.asJson,
      "timestamp" -> v.timestamp.asJson,
      "nBits" -> v.nBits.asJson,
      "height" -> v.height.asJson,
      "minerPk" -> v.minerPk.getEncoded.asJson,
      "votes" -> v.votes.asJson
    ).asJson
  }

  implicit val preHeaderDecoder: Decoder[PreHeader] = { cursor =>
    for {
      versionId <- cursor.downField("versionId").as[Byte]
      parentId <- cursor.downField("parentId").as[Coll[Byte]]
      timeStamp <- cursor.downField("timeStamp").as[Long]
      nBits <- cursor.downField("nBits").as[Long]
      height <- cursor.downField("height").as[Int]
      minerPk <- cursor.downField("minerPk").as[Coll[Byte]]
      votes <- cursor.downField("votes").as[Coll[Byte]]
    } yield CPreHeader(versionId, parentId, timeStamp, nBits, height, CGroupElement.decode(minerPk), votes)
  }

  implicit val evaluatedValueEncoder: Encoder[EvaluatedValue[SType]] = { value =>
    ValueSerializer.serialize(value).asJson
  }

  implicit val evaluatedValueDecoder: Decoder[EvaluatedValue[SType]] = {
    decodeEvaluatedValue(_.asInstanceOf[EvaluatedValue[SType]])
  }

  def decodeEvaluatedValue[T](transform: EvaluatedValue[SType] => T): Decoder[T] = { implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(transform(ValueSerializer.deserialize(bytes).asInstanceOf[EvaluatedValue[SType]]))
    }
  }

  implicit val ergoTreeEncoder: Encoder[ErgoTree] = { value =>
    ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(value).asJson
  }

  def decodeErgoTree[T](transform: ErgoTree => T): Decoder[T] = { implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(transform(ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)))
    }
  }

  implicit val ergoTreeDecoder: Decoder[ErgoTree] = {
    decodeErgoTree(_.asInstanceOf[ErgoTree])
  }
}
