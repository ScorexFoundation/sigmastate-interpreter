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

trait ErgoLikeTransactionTemplate[IT <: UnsignedInput] {
  val dataInputs: IndexedSeq[UnsignedInput]
  val inputs: IndexedSeq[IT]
  val outputCandidates: IndexedSeq[ErgoBoxCandidate]

  require(outputCandidates.size <= Short.MaxValue)

  val id: ModifierId

  lazy val outputs: IndexedSeq[ErgoBox] =
    outputCandidates.indices.map(idx => outputCandidates(idx).toBox(id, idx.toShort))

  lazy val messageToSign: Array[Byte] =
    ErgoLikeTransaction.FlattenedTransaction.sigmaSerializer.bytesToSign(this)

  lazy val inputIds: IndexedSeq[ADKey] = inputs.map(_.boxId)
}


class UnsignedErgoLikeTransaction(override val inputs: IndexedSeq[UnsignedInput],
                                  override val dataInputs: IndexedSeq[UnsignedInput],
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
  def apply(inputs: IndexedSeq[UnsignedInput], dataInputs: IndexedSeq[UnsignedInput], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new UnsignedErgoLikeTransaction(inputs, dataInputs, outputCandidates)

  def apply(inputs: IndexedSeq[UnsignedInput], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new UnsignedErgoLikeTransaction(inputs, IndexedSeq(), outputCandidates)
}

/**
  * Fully signed transaction
  *
  * @param inputs           - signed inputs, that will be spent by this transaction
  * @param dataInputs       - unsigned inputs, that are not going to be spent by transaction, but will be
  *                         reachable from inputs scripts.
  * @param outputCandidates - box candidates to be created by this transaction.
  *                         Differ from ordinary ones in that they do not include transaction id and index
  */
class ErgoLikeTransaction(override val inputs: IndexedSeq[Input],
                          override val dataInputs: IndexedSeq[UnsignedInput],
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
    new ErgoLikeTransaction(inputs, IndexedSeq(), outputCandidates)

  def apply(inputs: IndexedSeq[Input], dataInputs: IndexedSeq[UnsignedInput], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new ErgoLikeTransaction(inputs, dataInputs, outputCandidates)

  def apply(ftx: FlattenedTransaction): ErgoLikeTransaction =
    new ErgoLikeTransaction(ftx.inputs, ftx.dataInputs, ftx.outputCandidates)

  case class FlattenedTransaction(inputs: Array[Input],
                                  dataInputs: Array[UnsignedInput],
                                  outputCandidates: Array[ErgoBoxCandidate])

  object FlattenedTransaction {
    def apply(tx: ErgoLikeTransaction): FlattenedTransaction =
      FlattenedTransaction(tx.inputs.toArray, tx.dataInputs.toArray, tx.outputCandidates.toArray)

    object sigmaSerializer extends SigmaSerializer[FlattenedTransaction, FlattenedTransaction] {

      def bytesToSign[IT <: UnsignedInput](tx: ErgoLikeTransactionTemplate[IT]): Array[Byte] = {
        val emptyProofInputs = tx.inputs.map(i => new Input(i.boxId, ProverResult.empty))
        val w = SigmaSerializer.startWriter()
        serialize(FlattenedTransaction(emptyProofInputs.toArray, tx.dataInputs.toArray, tx.outputCandidates.toArray), w)
        w.toBytes
      }

      override def serialize(ftx: FlattenedTransaction, w: SigmaByteWriter): Unit = {
        w.putUShort(ftx.inputs.length)
        for (input <- ftx.inputs) {
          Input.serializer.serialize(input, w)
        }
        w.putUShort(ftx.dataInputs.length)
        for (input <- ftx.dataInputs) {
          w.putBytes(input.boxId)
        }
        val digests = ftx.outputCandidates.flatMap(_.additionalTokens.map(t => new mutable.WrappedArray.ofByte(t._1))).distinct
        w.putUInt(digests.length)
        digests.foreach { digest =>
          w.putBytes(digest.array)
        }
        w.putUShort(ftx.outputCandidates.length)
        for (out <- ftx.outputCandidates) {
          ErgoBoxCandidate.serializer.serializeBodyWithIndexedDigests(out, Some(digests), w)
        }
      }

      override def parse(r: SigmaByteReader): FlattenedTransaction = {
        val inputsCount = r.getUShort()
        val inputsBuilder = mutable.ArrayBuilder.make[Input]()
        for (_ <- 0 until inputsCount) {
          inputsBuilder += Input.serializer.parse(r)
        }
        val dataInputsCount = r.getUShort()
        val dataInputsBuilder = mutable.ArrayBuilder.make[UnsignedInput]()
        for (_ <- 0 until dataInputsCount) {
          dataInputsBuilder += new UnsignedInput(ADKey @@ r.getBytes(ErgoBox.BoxId.size))
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
        FlattenedTransaction(inputsBuilder.result(), dataInputsBuilder.result(), outputCandidatesBuilder.result())
      }
    }

  }

  object serializer extends SigmaSerializer[ErgoLikeTransaction, ErgoLikeTransaction] {

    override def serialize(tx: ErgoLikeTransaction, w: SigmaByteWriter): Unit =
      FlattenedTransaction.sigmaSerializer.serialize(FlattenedTransaction(tx), w)

    override def parse(r: SigmaByteReader): ErgoLikeTransaction =
      ErgoLikeTransaction(FlattenedTransaction.sigmaSerializer.parse(r))
  }

}
