package org.ergoplatform

import java.math.BigInteger

import cats.syntax.either._
import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId, TokenId}
import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.validation.{SigmaValidationSettings, SigmaValidationSettingsSerializer}
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigmastate.Values.{ErgoTree, EvaluatedValue}
import sigmastate.eval.Extensions._
import sigmastate.eval.{CGroupElement, CPreHeader, WrapperOf, _}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.lang.exceptions.SigmaException
import sigmastate.serialization.{DataJsonEncoder, ErgoTreeSerializer, ValueSerializer}
import sigmastate.{AvlTreeData, AvlTreeFlags, SType}
import special.collection.Coll
import special.sigma.{AnyValue, Header, PreHeader}

import scala.util.Try

trait JsonCodecs {

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
      bytes <- fromTry(ErgoAlgos.decode(str))
    } yield transform(bytes)
  }

  implicit val anyValueEncoder: Encoder[AnyValue] = { anyval => DataJsonEncoder.encode(anyval) }

  implicit val sigmaBigIntEncoder: Encoder[special.sigma.BigInt] = { bigInt =>
    JsonNumber.fromDecimalStringUnsafe(bigInt.asInstanceOf[WrapperOf[BigInteger]].wrappedValue.toString).asJson
  }

  implicit val sigmaBigIntDecoder: Decoder[special.sigma.BigInt] = { implicit cursor =>
    for {
      jsonNumber <- cursor.as[JsonNumber]
      bigInt <- fromOption(jsonNumber.toBigInt)
    } yield CBigInt(bigInt.bigInteger)
  }

  implicit val arrayBytesEncoder: Encoder[Array[Byte]] = ErgoAlgos.encode(_).asJson
  implicit val arrayBytesDecoder: Decoder[Array[Byte]] = bytesDecoder(x => x)

  implicit val collBytesEncoder: Encoder[Coll[Byte]] = ErgoAlgos.encode(_).asJson
  implicit val collBytesDecoder: Decoder[Coll[Byte]] = bytesDecoder(Colls.fromArray(_))

  implicit val adKeyEncoder: Encoder[ADKey] = _.array.asJson
  implicit val adKeyDecoder: Decoder[ADKey] = bytesDecoder(ADKey @@ _)

  implicit val adDigestEncoder: Encoder[ADDigest] = _.array.asJson
  implicit val adDigestDecoder: Decoder[ADDigest] = bytesDecoder(ADDigest @@ _)

  implicit val digest32Encoder: Encoder[Digest32] = _.array.asJson
  implicit val digest32Decoder: Decoder[Digest32] = bytesDecoder(Digest32 @@ _)

  implicit val assetEncoder: Encoder[(TokenId, Long)] = { asset =>
    Json.obj(
      "tokenId" -> asset._1.asJson,
      "amount" -> asset._2.asJson
    )
  }

  implicit val assetDecoder: Decoder[(TokenId, Long)] = { cursor =>
    for {
      tokenId <- cursor.downField("tokenId").as[TokenId]
      amount <- cursor.downField("amount").as[Long]
    } yield (tokenId, amount)
  }

  implicit val modifierIdEncoder: Encoder[ModifierId] = _.asInstanceOf[String].asJson
  implicit val modifierIdDecoder: Decoder[ModifierId] = ModifierId @@ _.as[String]

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
      "stateRoot" -> SigmaDsl.toAvlTreeData(h.stateRoot).asJson,
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
      height, extensionRoot, SigmaDsl.decodePoint(minerPk), SigmaDsl.decodePoint(powOnetimePk), powNonce, powDistance, votes)
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
      version <- cursor.downField("version").as[Byte]
      parentId <- cursor.downField("parentId").as[Coll[Byte]]
      timestamp <- cursor.downField("timestamp").as[Long]
      nBits <- cursor.downField("nBits").as[Long]
      height <- cursor.downField("height").as[Int]
      minerPk <- cursor.downField("minerPk").as[Coll[Byte]]
      votes <- cursor.downField("votes").as[Coll[Byte]]
    } yield CPreHeader(version, parentId, timestamp, nBits, height, SigmaDsl.decodePoint(minerPk), votes)
  }

  implicit val evaluatedValueEncoder: Encoder[EvaluatedValue[_ <: SType]] = { value =>
    ValueSerializer.serialize(value).asJson
  }

  implicit val evaluatedValueDecoder: Decoder[EvaluatedValue[_ <: SType]] = {
    decodeEvaluatedValue(_.asInstanceOf[EvaluatedValue[SType]])
  }

  def decodeEvaluatedValue[T](transform: EvaluatedValue[SType] => T): Decoder[T] = { implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(transform(ValueSerializer.deserialize(bytes).asInstanceOf[EvaluatedValue[SType]]))
    }
  }

  implicit val dataInputEncoder: Encoder[DataInput] = { input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
    )
  }

  implicit val dataInputDecoder: Decoder[DataInput] = { cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
    } yield DataInput(boxId)
  }

  implicit val inputEncoder: Encoder[Input] = { input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
      "spendingProof" -> input.spendingProof.asJson
    )
  }


  implicit val inputDecoder: Decoder[Input] = { cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
      proof <- cursor.downField("spendingProof").as[ProverResult]
    } yield Input(boxId, proof)
  }

  implicit val unsignedInputEncoder: Encoder[UnsignedInput] = { input =>
    Json.obj(
      "boxId" -> input.boxId.asJson,
      "extension" -> input.extension.asJson
    )
  }

  implicit val unsignedInputDecoder: Decoder[UnsignedInput] = { cursor =>
    for {
      boxId <- cursor.downField("boxId").as[ADKey]
      extension <- cursor.downField("extension").as[ContextExtension]
    } yield new UnsignedInput(boxId, extension)
  }

  implicit val contextExtensionEncoder: Encoder[ContextExtension] = { extension =>
    extension.values.map { case (key, value) =>
      key -> evaluatedValueEncoder(value)
    }.asJson
  }

  implicit val contextExtensionDecoder: Decoder[ContextExtension] = { cursor =>
    for {
      values <- cursor.as[Map[Byte, EvaluatedValue[SType]]]
    } yield ContextExtension(values)
  }

  implicit val proverResultEncoder: Encoder[ProverResult] = { v =>
    Json.obj(
      "proofBytes" -> v.proof.asJson,
      "extension" -> v.extension.asJson
    )
  }

  implicit val proverResultDecoder: Decoder[ProverResult] = { cursor =>
    for {
      proofBytes <- cursor.downField("proofBytes").as[Array[Byte]]
      extMap <- cursor.downField("extension").as[Map[Byte, EvaluatedValue[SType]]]
    } yield ProverResult(proofBytes, ContextExtension(extMap))
  }


  implicit val avlTreeDataEncoder: Encoder[AvlTreeData] = { v =>
    Json.obj(
      "digest" -> v.digest.asJson,
      "treeFlags" -> v.treeFlags.serializeToByte.asJson,
      "keyLength" -> v.keyLength.asJson,
      "valueLength" -> v.valueLengthOpt.asJson
    )
  }

  implicit val avlTreeDataDecoder: Decoder[AvlTreeData] = { cursor =>
    for {
      digest <- cursor.downField("digest").as[ADDigest]
      treeFlagsByte <- cursor.downField("treeFlags").as[Byte]
      keyLength <- cursor.downField("keyLength").as[Int]
      valueLength <- cursor.downField("valueLength").as[Option[Int]]
    } yield new AvlTreeData(digest, AvlTreeFlags(treeFlagsByte), keyLength, valueLength)
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

  implicit def registersEncoder[T <: EvaluatedValue[_ <: SType]]: Encoder[Map[NonMandatoryRegisterId, T]] = { m =>
    Json.obj(
      m.toSeq
        .sortBy(_._1.number)
        .map { case (k, v) => registerIdEncoder(k) -> evaluatedValueEncoder(v) }: _*)
  }

  implicit val ergoBoxEncoder: Encoder[ErgoBox] = { box =>
    Json.obj(
      "boxId" -> box.id.asJson,
      "value" -> box.value.asJson,
      "ergoTree" -> ErgoTreeSerializer.DefaultSerializer.serializeErgoTree(box.ergoTree).asJson,
      "assets" -> box.additionalTokens.toArray.toSeq.asJson,
      "creationHeight" -> box.creationHeight.asJson,
      "additionalRegisters" -> box.additionalRegisters.asJson,
      "transactionId" -> box.transactionId.asJson,
      "index" -> box.index.asJson
    )
  }

  implicit val ergoBoxDecoder: Decoder[ErgoBox] = { cursor =>
    for {
      value <- cursor.downField("value").as[Long]
      ergoTreeBytes <- cursor.downField("ergoTree").as[Array[Byte]]
      additionalTokens <- cursor.downField("assets").as(Decoder.decodeSeq(assetDecoder))
      creationHeight <- cursor.downField("creationHeight").as[Int]
      additionalRegisters <- cursor.downField("additionalRegisters").as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
      transactionId <- cursor.downField("transactionId").as[ModifierId]
      index <- cursor.downField("index").as[Short]
    } yield new ErgoBox(
      value = value,
      ergoTree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(ergoTreeBytes),
      additionalTokens = additionalTokens.toColl,
      additionalRegisters = additionalRegisters,
      transactionId = transactionId,
      index = index,
      creationHeight = creationHeight
    )
  }

  implicit val ergoLikeTransactionEncoder: Encoder[ErgoLikeTransaction] = { tx =>
    Json.obj(
      "id" -> tx.id.asJson,
      "inputs" -> tx.inputs.asJson,
      "dataInputs" -> tx.dataInputs.asJson,
      "outputs" -> tx.outputs.asJson
    )
  }

  implicit val unsignedErgoLikeTransactionEncoder: Encoder[UnsignedErgoLikeTransaction] = { tx =>
    Json.obj(
      "id" -> tx.id.asJson,
      "inputs" -> tx.inputs.asJson,
      "dataInputs" -> tx.dataInputs.asJson,
      "outputs" -> tx.outputs.asJson
    )
  }

  implicit def ergoLikeTransactionTemplateEncoder[T <: UnsignedInput]: Encoder[ErgoLikeTransactionTemplate[T]] = {
    case transaction: ErgoLikeTransaction => ergoLikeTransactionEncoder(transaction)
    case transaction: UnsignedErgoLikeTransaction => unsignedErgoLikeTransactionEncoder(transaction)
    case t => throw new SigmaException(s"Don't know how to encode transaction $t")
  }

  implicit val transactionOutputsDecoder: Decoder[(ErgoBoxCandidate, Option[BoxId])] = { cursor =>
    for {
      maybeId <- cursor.downField("boxId").as[Option[BoxId]]
      value <- cursor.downField("value").as[Long]
      creationHeight <- cursor.downField("creationHeight").as[Int]
      ergoTree <- cursor.downField("ergoTree").as[ErgoTree]
      assets <- cursor.downField("assets").as[Seq[(ErgoBox.TokenId, Long)]] // TODO optimize: encode directly into Coll avoiding allocation of Tuple2 for each element
      registers <- cursor.downField("additionalRegisters").as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
    } yield (new ErgoBoxCandidate(value, ergoTree, creationHeight, assets.toColl, registers), maybeId)
  }

  implicit val ergoLikeTransactionDecoder: Decoder[ErgoLikeTransaction] = { implicit cursor =>
    for {
      inputs <- cursor.downField("inputs").as[IndexedSeq[Input]]
      dataInputs <- cursor.downField("dataInputs").as[IndexedSeq[DataInput]]
      outputsWithIndex <- cursor.downField("outputs").as[IndexedSeq[(ErgoBoxCandidate, Option[BoxId])]]
    } yield new ErgoLikeTransaction(inputs, dataInputs, outputsWithIndex.map(_._1))
  }

  implicit val unsignedErgoLikeTransactionDecoder: Decoder[UnsignedErgoLikeTransaction] = { implicit cursor =>
    for {
      inputs <- cursor.downField("inputs").as[IndexedSeq[UnsignedInput]]
      dataInputs <- cursor.downField("dataInputs").as[IndexedSeq[DataInput]]
      outputsWithIndex <- cursor.downField("outputs").as[IndexedSeq[(ErgoBoxCandidate, Option[BoxId])]]
    } yield new UnsignedErgoLikeTransaction(inputs, dataInputs, outputsWithIndex.map(_._1))
  }

  implicit val ergoLikeTransactionTemplateDecoder: Decoder[ErgoLikeTransactionTemplate[_ <: UnsignedInput]] = {
    ergoLikeTransactionDecoder.asInstanceOf[Decoder[ErgoLikeTransactionTemplate[_ <: UnsignedInput]]] or
    unsignedErgoLikeTransactionDecoder.asInstanceOf[Decoder[ErgoLikeTransactionTemplate[_ <: UnsignedInput]]]
  }

  implicit val sigmaValidationSettingsEncoder: Encoder[SigmaValidationSettings] = { v =>
    SigmaValidationSettingsSerializer.toBytes(v).asJson
  }

  implicit val sigmaValidationSettingsDecoder: Decoder[SigmaValidationSettings] = { implicit cursor: ACursor =>
    cursor.as[Array[Byte]] flatMap { bytes =>
      fromThrows(SigmaValidationSettingsSerializer.fromBytes(bytes))
    }
  }

  implicit val ergoLikeContextEncoder: Encoder[ErgoLikeContext] = { ctx =>
    Json.obj(
      "lastBlockUtxoRoot" -> ctx.lastBlockUtxoRoot.asJson,
      "headers" -> ctx.headers.toArray.toSeq.asJson,
      "preHeader" -> ctx.preHeader.asJson,
      "dataBoxes" -> ctx.dataBoxes.asJson,
      "boxesToSpend" -> ctx.boxesToSpend.asJson,
      "spendingTransaction" -> ctx.spendingTransaction.asJson,
      "selfIndex" -> ctx.selfIndex.asJson,
      "extension" -> ctx.extension.asJson,
      "validationSettings" -> ctx.validationSettings.asJson,
      "costLimit" -> ctx.costLimit.asJson,
      "initCost" -> ctx.initCost.asJson
    )
  }

  implicit val ergoLikeContextDecoder: Decoder[ErgoLikeContext] = { cursor =>
    for {
      lastBlockUtxoRoot <- cursor.downField("lastBlockUtxoRoot").as[AvlTreeData]
      headers <- cursor.downField("headers").as[Seq[Header]]
      preHeader <- cursor.downField("preHeader").as[PreHeader]
      dataBoxes <- cursor.downField("dataBoxes").as[IndexedSeq[ErgoBox]]
      boxesToSpend <- cursor.downField("boxesToSpend").as[IndexedSeq[ErgoBox]]
      spendingTransaction <- cursor.downField("spendingTransaction").as(ergoLikeTransactionTemplateDecoder)
      selfIndex <- cursor.downField("selfIndex").as[Int]
      extension <- cursor.downField("extension").as[ContextExtension]
      validationSettings <- cursor.downField("validationSettings").as[SigmaValidationSettings]
      costLimit <- cursor.downField("costLimit").as[Long]
      initCost <- cursor.downField("initCost").as[Long]
    } yield new ErgoLikeContext(lastBlockUtxoRoot, Colls.fromArray(headers.toArray), preHeader,
      dataBoxes, boxesToSpend, spendingTransaction, selfIndex, extension, validationSettings, costLimit, initCost)
  }
}
