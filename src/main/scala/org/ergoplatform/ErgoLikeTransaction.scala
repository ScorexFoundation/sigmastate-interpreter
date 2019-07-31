package org.ergoplatform

import java.util

import io.circe._
import io.circe.syntax._
import org.ergoplatform.ErgoBox.{BoxId, NonMandatoryRegisterId, TokenId}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util._
import sigmastate.SType
import sigmastate.interpreter.ProverResult
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import special.collection.ExtensionMethods._
import sigmastate.eval.Extensions._
import spire.syntax.all.cfor

import scala.collection.mutable
import scala.util.Try
import sigmastate.SType._
import sigmastate.Values.{ErgoTree, EvaluatedValue}
import sigmastate.eval._

trait ErgoBoxReader {
  def byId(boxId: ADKey): Try[ErgoBox]
}

/**
  * Base trait of a real transaction to be used in Ergo network.
  * May be in unsigned (`UnsignedErgoLikeTransaction`) or in signed (`ErgoLikeTransaction`) version.
  *
  * Consists of:
  *
  * @param inputs           - inputs, that will be spent by this transaction.
  * @param dataInputs       - inputs, that are not going to be spent by transaction, but will be
  *                         reachable from inputs scripts. `dataInputs` scripts will not be executed,
  *                         thus their scripts costs are not included in transaction cost and
  *                         they do not contain spending proofs.
  * @param outputCandidates - box candidates to be created by this transaction.
  *                         Differ from ordinary ones in that they do not include transaction id and index
  */
trait ErgoLikeTransactionTemplate[IT <: UnsignedInput] {
  val dataInputs: IndexedSeq[DataInput]
  val inputs: IndexedSeq[IT]
  val outputCandidates: IndexedSeq[ErgoBoxCandidate]

  require(outputCandidates.size <= Short.MaxValue)

  val id: ModifierId

  lazy val outputs: IndexedSeq[ErgoBox] =
    outputCandidates.indices.map(idx => outputCandidates(idx).toBox(id, idx.toShort))

  lazy val messageToSign: Array[Byte] = ErgoLikeTransaction.bytesToSign(this)

  lazy val inputIds: IndexedSeq[ADKey] = inputs.map(_.boxId)

  override def toString = s"ErgoLikeTransactionTemplate(dataInputs=$dataInputs, inputs=$inputs, outputCandidates=$outputCandidates)"
}


/**
  * Unsigned version of `ErgoLikeTransactionTemplate`
  */
class UnsignedErgoLikeTransaction(override val inputs: IndexedSeq[UnsignedInput],
                                  override val dataInputs: IndexedSeq[DataInput],
                                  override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoLikeTransactionTemplate[UnsignedInput] {

  override lazy val id: ModifierId = Blake2b256.hash(messageToSign).toModifierId

  def toSigned(proofs: IndexedSeq[ProverResult]): ErgoLikeTransaction = {
    require(proofs.size == inputs.size)
    val ins = inputs.zip(proofs).map { case (ui, proof) => Input(ui.boxId, proof) }
    new ErgoLikeTransaction(ins, dataInputs, outputCandidates)
  }
}

object UnsignedErgoLikeTransaction {
  def apply(inputs: IndexedSeq[UnsignedInput], dataInputs: IndexedSeq[DataInput], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new UnsignedErgoLikeTransaction(inputs, dataInputs, outputCandidates)

  def apply(inputs: IndexedSeq[UnsignedInput], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new UnsignedErgoLikeTransaction(inputs, IndexedSeq(), outputCandidates)
}

/**
  * Signed version of `ErgoLikeTransactionTemplate`
  */
class ErgoLikeTransaction(override val inputs: IndexedSeq[Input],
                          override val dataInputs: IndexedSeq[DataInput],
                          override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoLikeTransactionTemplate[Input] {

  require(outputCandidates.length <= Short.MaxValue, s"${Short.MaxValue} is the maximum number of outputs")

  override lazy val id: ModifierId = Blake2b256.hash(messageToSign).toModifierId

  override def equals(obj: Any): Boolean = obj match {
    //we're ignoring spending proofs here
    case tx: ErgoLikeTransaction => this.id == tx.id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()
}

object ErgoLikeTransactionSerializer extends SigmaSerializer[ErgoLikeTransaction, ErgoLikeTransaction] {

  override def serialize(tx: ErgoLikeTransaction, w: SigmaByteWriter): Unit = {
    // serialize transaction inputs
    w.putUShort(tx.inputs.length)
    for (input <- tx.inputs) {
      Input.serializer.serialize(input, w)
    }
    // serialize transaction data inputs
    w.putUShort(tx.dataInputs.length)
    for (input <- tx.dataInputs) {
      w.putBytes(input.boxId)
    }
    // serialize distinct ids of tokens in transaction outputs
    val tokenIds = tx.outputCandidates.toColl
      .flatMap(box => box.additionalTokens.map(t => t._1))

    val distinctTokenIds = tokenIds.map(_.toColl).distinct.map(_.toArray.asInstanceOf[TokenId])

    w.putUInt(distinctTokenIds.length)
    cfor(0)(_ < distinctTokenIds.length, _ + 1) { i =>
      val tokenId = distinctTokenIds(i)
      w.putBytes(tokenId)
    }
    // serialize outputs
    val outs = tx.outputCandidates
    w.putUShort(outs.length)
    cfor(0)(_ < outs.length, _ + 1) { i =>
      val out = outs(i)
      ErgoBoxCandidate.serializer.serializeBodyWithIndexedDigests(out, Some(distinctTokenIds), w)
    }
  }

  override def parse(r: SigmaByteReader): ErgoLikeTransaction = {
    // parse transaction inputs
    val inputsCount = r.getUShort()
    val inputsBuilder = mutable.ArrayBuilder.make[Input]()
    for (_ <- 0 until inputsCount) {
      inputsBuilder += Input.serializer.parse(r)
    }
    // parse transaction data inputs
    val dataInputsCount = r.getUShort()
    val dataInputsBuilder = mutable.ArrayBuilder.make[DataInput]()
    for (_ <- 0 until dataInputsCount) {
      dataInputsBuilder += DataInput(ADKey @@ r.getBytes(ErgoBox.BoxId.size))
    }
    // parse distinct ids of tokens in transaction outputs
    val tokensCount = r.getUInt().toInt
    val tokensBuilder = mutable.ArrayBuilder.make[TokenId]()
    for (_ <- 0 until tokensCount) {
      tokensBuilder += Digest32 @@ r.getBytes(TokenId.size)
    }
    val tokens = tokensBuilder.result().toColl
    // parse outputs

    val outsCount = r.getUShort()
    val outputCandidatesBuilder = mutable.ArrayBuilder.make[ErgoBoxCandidate]()
    for (_ <- 0 until outsCount) {
      outputCandidatesBuilder += ErgoBoxCandidate.serializer.parseBodyWithIndexedDigests(Some(tokens), r)
    }
    new ErgoLikeTransaction(inputsBuilder.result(), dataInputsBuilder.result(), outputCandidatesBuilder.result())
  }

}


object ErgoLikeTransaction extends JsonCodecs {

  val TransactionIdBytesSize: Short = 32

  val dummy = ErgoLikeTransaction(IndexedSeq(), IndexedSeq())

  /**
    * Bytes that should be signed by provers.
    * Contains all the transaction bytes except of signatures
    */
  def bytesToSign[IT <: UnsignedInput](tx: ErgoLikeTransactionTemplate[IT]): Array[Byte] = {
    val emptyProofInputs = tx.inputs.map(_.inputToSign)
    val w = SigmaSerializer.startWriter()
    val txWithoutProofs = new ErgoLikeTransaction(emptyProofInputs, tx.dataInputs, tx.outputCandidates)
    ErgoLikeTransactionSerializer.serialize(txWithoutProofs, w)
    w.toBytes
  }

  /**
    * Creates ErgoLikeTransaction without data inputs
    */
  def apply(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new ErgoLikeTransaction(inputs, IndexedSeq(), outputCandidates)

  // TODO unify serialization approach in Ergo/sigma with BytesSerializable
  val serializer: SigmaSerializer[ErgoLikeTransaction, ErgoLikeTransaction] = ErgoLikeTransactionSerializer

  implicit val jsonEncoder: Encoder[ErgoLikeTransaction] = { tx =>
    Json.obj(
      "id" -> tx.id.asJson,
      "inputs" -> tx.inputs.asJson,
      "dataInputs" -> tx.dataInputs.asJson,
      "outputs" -> tx.outputs.asJson
    )
  }

  private implicit val outputDecoder: Decoder[(ErgoBoxCandidate, Option[BoxId])] = { cursor =>
    for {
      maybeId <- cursor.downField("boxId").as[Option[BoxId]]
      value <- cursor.downField("value").as[Long]
      creationHeight <- cursor.downField("creationHeight").as[Int]
      ergoTree <- cursor.downField("ergoTree").as[ErgoTree]
      assets <- cursor.downField("assets").as[Seq[(ErgoBox.TokenId, Long)]] // TODO optimize: encode directly into Coll avoiding allocation of Tuple2 for each element
      registers <- cursor.downField("additionalRegisters").as[Map[NonMandatoryRegisterId, EvaluatedValue[SType]]]
    } yield (new ErgoBoxCandidate(value, ergoTree, creationHeight, assets.toColl, registers), maybeId)
  }

  implicit val jsonDecoder: Decoder[ErgoLikeTransaction] = { implicit cursor =>
    for {
      inputs <- cursor.downField("inputs").as[IndexedSeq[Input]]
      dataInputs <- cursor.downField("dataInputs").as[IndexedSeq[DataInput]]
      outputsWithIndex <- cursor.downField("outputs").as[IndexedSeq[(ErgoBoxCandidate, Option[BoxId])]]
    } yield new ErgoLikeTransaction(inputs, dataInputs, outputsWithIndex.map(_._1))
  }
}
