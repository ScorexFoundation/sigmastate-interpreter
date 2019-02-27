package org.ergoplatform

import org.ergoplatform.ErgoBox.TokenId
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util._
import sigmastate.interpreter.ProverResult
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable
import scala.util.Try


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
    val distinctTokenIds = tx.outputCandidates
      .flatMap(_.additionalTokens.map(t => new mutable.WrappedArray.ofByte(t._1)))
      .distinct
      .toArray
    w.putUInt(distinctTokenIds.length)
    distinctTokenIds.foreach { tokenId =>
      w.putBytes(tokenId.array)
    }
    // serialize outputs
    w.putUShort(tx.outputCandidates.length)
    for (out <- tx.outputCandidates) {
      ErgoBoxCandidate.serializer.serializeBodyWithIndexedDigests(out, Some(distinctTokenIds), w)
    }
  }

  override def parse(r: SigmaByteReader): ErgoLikeTransaction = {
    val inputsCount = r.getUShort()
    val inputsBuilder = mutable.ArrayBuilder.make[Input]()
    for (_ <- 0 until inputsCount) {
      inputsBuilder += Input.serializer.parse(r)
    }
    val dataInputsCount = r.getUShort()
    val dataInputsBuilder = mutable.ArrayBuilder.make[DataInput]()
    for (_ <- 0 until dataInputsCount) {
      dataInputsBuilder += DataInput(ADKey @@ r.getBytes(ErgoBox.BoxId.size))
    }
    val digestsCount = r.getUInt().toInt
    val digestsBuilder = mutable.ArrayBuilder.make[Digest32]()
    for (_ <- 0 until digestsCount) {
      digestsBuilder += Digest32 @@ r.getBytes(TokenId.size)
    }
    val digests = digestsBuilder.result()
    val outsCount = r.getUShort()
    val outputCandidatesBuilder = mutable.ArrayBuilder.make[ErgoBoxCandidate]()
    for (_ <- 0 until outsCount) {
      outputCandidatesBuilder += ErgoBoxCandidate.serializer.parseBodyWithIndexedDigests(Some(digests), r)
    }
    new ErgoLikeTransaction(inputsBuilder.result(), dataInputsBuilder.result(), outputCandidatesBuilder.result())
  }

}


object ErgoLikeTransaction {

  val TransactionIdBytesSize: Short = 32

  /**
    * Bytes that should be signed by provers.
    * Contains all the transaction bytes except of signatures itself
    */
  def bytesToSign[IT <: UnsignedInput](tx: ErgoLikeTransactionTemplate[IT]): Array[Byte] = {
    val emptyProofInputs = tx.inputs.map(_.inputToSign)
    val w = SigmaSerializer.startWriter()
    val txWithoutProofs = ErgoLikeTransaction(emptyProofInputs, tx.dataInputs, tx.outputCandidates)
    ErgoLikeTransactionSerializer.serialize(txWithoutProofs, w)
    w.toBytes
  }

  def apply(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new ErgoLikeTransaction(inputs, IndexedSeq(), outputCandidates)

  def apply(inputs: IndexedSeq[Input], dataInputs: IndexedSeq[DataInput], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new ErgoLikeTransaction(inputs, dataInputs, outputCandidates)

  val serializer: SigmaSerializer[ErgoLikeTransaction, ErgoLikeTransaction] = ErgoLikeTransactionSerializer

}
