package org.ergoplatform

import org.ergoplatform.ErgoBox.TokenId
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util._
import sigmastate.interpreter.ProverResult
import sigmastate.serialization.Serializer

import scala.util.Try
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable



trait ErgoBoxReader {
  def byId(boxId: ADKey): Try[ErgoBox]
}


trait ErgoLikeTransactionTemplate[IT <: UnsignedInput] {
  val inputs: IndexedSeq[IT]
  val outputCandidates: IndexedSeq[ErgoBoxCandidate]

  require(outputCandidates.size <= Short.MaxValue)

  val id: ModifierId

  lazy val outputs: IndexedSeq[ErgoBox] =
    outputCandidates.indices.map(idx => outputCandidates(idx).toBox(id, idx.toShort))

  lazy val messageToSign: Array[Byte] =
    ErgoLikeTransaction.flattenedTxSerializer.bytesToSign(this)

  lazy val inputIds: IndexedSeq[ADKey] = inputs.map(_.boxId)
}


class UnsignedErgoLikeTransaction(override val inputs: IndexedSeq[UnsignedInput],
                                  override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoLikeTransactionTemplate[UnsignedInput] {

  override lazy val id: ModifierId = Blake2b256.hash(messageToSign).toModifierId

  def toSigned(proofs: IndexedSeq[ProverResult]): ErgoLikeTransaction = {
    require(proofs.size == inputs.size)
    val ins = inputs.zip(proofs).map { case (ui, proof) => Input(ui.boxId, proof) }
    new ErgoLikeTransaction(ins, outputCandidates)
  }
}

object UnsignedErgoLikeTransaction {
  def apply(inputs: IndexedSeq[UnsignedInput], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new UnsignedErgoLikeTransaction(inputs, outputCandidates)
}

/**
  * Fully signed transaction
  *
  * @param inputs
  * @param outputCandidates
  */
class ErgoLikeTransaction(override val inputs: IndexedSeq[Input],
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


object ErgoLikeTransaction {

  val TransactionIdBytesSize: Short = 32

  def apply(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new ErgoLikeTransaction(inputs, outputCandidates)

  def apply(ftx: FlattenedTransaction): ErgoLikeTransaction =
    new ErgoLikeTransaction(ftx.inputs, ftx.outputCandidates)

  case class FlattenedTransaction(inputs: Array[Input],
                                  outputCandidates: Array[ErgoBoxCandidate])

  object FlattenedTransaction {
    def apply(tx: ErgoLikeTransaction): FlattenedTransaction =
      FlattenedTransaction(tx.inputs.toArray, tx.outputCandidates.toArray)
  }

  object flattenedTxSerializer extends Serializer[FlattenedTransaction, FlattenedTransaction] {

    def bytesToSign(inputs: IndexedSeq[ADKey],
                    outputCandidates: IndexedSeq[ErgoBoxCandidate]): Array[Byte] = {
      //todo: set initial capacity
      val w = Serializer.startWriter()

      w.putUShort(inputs.length)
      inputs.foreach { i =>
        w.putBytes(i)
      }
      w.putUShort(outputCandidates.length)
      outputCandidates.foreach { c =>
        ErgoBoxCandidate.serializer.serializeBody(c, w)
      }

      w.toBytes
    }

    def bytesToSign[IT <: UnsignedInput](tx: ErgoLikeTransactionTemplate[IT]): Array[Byte] =
      bytesToSign(tx.inputs.map(_.boxId), tx.outputCandidates)

    override def serializeBody(ftx: FlattenedTransaction, w: SigmaByteWriter): Unit = {
      SerializeLog.logPrintf(true, true, false, "FlattenedTransaction")

      SerializeLog.logPrintf(true, true, false, "inputs.length")
      w.putUShort(ftx.inputs.length)
      SerializeLog.logPrintf(false, true, false, "inputs.length")

      SerializeLog.logPrintf(true, true, false, "inputs*")
      for (input <- ftx.inputs) {
        Input.serializer.serializeBody(input, w)
      }
      SerializeLog.logPrintf(false, true, false, "inputs*")

      val digests = ftx.outputCandidates.flatMap(_.additionalTokens.map(_._1)).distinct

      SerializeLog.logPrintf(true, true, false, "digests.length")
      w.putUInt(digests.length)
      SerializeLog.logPrintf(false, true, false, "digests.length")

      SerializeLog.logPrintf(true, true, false, "digests*")
      digests.foreach { digest =>
        w.putBytes(digest)
      }
      SerializeLog.logPrintf(false, true, false, "digests*")

      SerializeLog.logPrintf(true, true, false, "outputCandidates.length")
      w.putUShort(ftx.outputCandidates.length)
      SerializeLog.logPrintf(false, true, false, "outputCandidates.length")

      SerializeLog.logPrintf(true, true, false, "outputCandidates*")
      for (out <- ftx.outputCandidates) {
        ErgoBoxCandidate.serializer.serializeBodyWithIndexedDigests(out, Some(digests), w)
      }
      SerializeLog.logPrintf(false, true, false, "outputCandidates*")

      SerializeLog.logPrintf(false, true, false, "FlattenedTransaction")
    }

    override def parseBody(r: SigmaByteReader): FlattenedTransaction = {
      val inputsCount = r.getUShort()
      val inputsBuilder = mutable.ArrayBuilder.make[Input]()
      for (_ <- 0 until inputsCount) {
        inputsBuilder += Input.serializer.parseBody(r)
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
      FlattenedTransaction(inputsBuilder.result(), outputCandidatesBuilder.result())
    }
  }

  object serializer extends Serializer[ErgoLikeTransaction, ErgoLikeTransaction] {

    override def serializeBody(tx: ErgoLikeTransaction, w: SigmaByteWriter): Unit = {
      SerializeLog.logPrintf(true, true, false, "ErgoLikeTransaction")

      flattenedTxSerializer.serializeBody(FlattenedTransaction(tx), w)

      SerializeLog.logPrintf(false, true, false, "ErgoLikeTransaction")
    }

    override def parseBody(r: SigmaByteReader): ErgoLikeTransaction =
      ErgoLikeTransaction(flattenedTxSerializer.parseBody(r))
  }
}
