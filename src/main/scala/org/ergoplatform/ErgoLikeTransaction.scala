package org.ergoplatform

import com.google.common.primitives.Shorts
import org.ergoplatform.ErgoBox.BoxId
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.interpreter.{ProverResult, SerializedProverResult}
import sigmastate.serialization.Serializer
import sigmastate.serialization.Serializer.{Consumed, Position}

import scala.util.Try
import org.ergoplatform.ErgoBox.BoxId


trait ErgoBoxReader {
  def byId(boxId: ADKey): Try[ErgoBox]
}


sealed trait ErgoLikeTransactionTemplate[IT <: UnsignedInput] {
  val inputs: IndexedSeq[IT]
  val outputCandidates: IndexedSeq[ErgoBoxCandidate]

  require(outputCandidates.size <= Short.MaxValue)

  lazy val outputs: IndexedSeq[ErgoBox] =
    outputCandidates.indices.map(idx => outputCandidates(idx).toBox(id, idx.toShort))

  lazy val messageToSign: Array[Byte] =
    ErgoLikeTransaction.flattenedTxSerializer.bytesToSign(inputs.map(_.boxId), outputCandidates)

  lazy val id: Digest32 = Blake2b256.hash(messageToSign)

  lazy val inputIds: IndexedSeq[ADKey] = inputs.map(_.boxId)
}


class UnsignedErgoLikeTransaction(override val inputs: IndexedSeq[UnsignedInput],
                                  override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoLikeTransactionTemplate[UnsignedInput] {

  def toSigned(proofs: IndexedSeq[ProverResult]): ErgoLikeTransaction = {
    require(proofs.size == inputs.size)
    val ins = inputs.zip(proofs).map { case (ui, proof) => Input(ui.boxId, proof.toSerialized) }
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

  override def equals(obj: Any): Boolean = obj match {
    case tx: ErgoLikeTransaction => this.id sameElements tx.id  //we're ignoring spending proofs here
    case _ => false
  }
}


object ErgoLikeTransaction {
  def apply(inputs: IndexedSeq[Input], outputCandidates: IndexedSeq[ErgoBoxCandidate]) =
    new ErgoLikeTransaction(inputs, outputCandidates)

  type FlattenedTransaction = (IndexedSeq[Input], IndexedSeq[ErgoBoxCandidate])

  object flattenedTxSerializer extends Serializer[FlattenedTransaction, FlattenedTransaction] {

    def bytesToSign(inputs: IndexedSeq[ADKey],
                    outputCandidates: IndexedSeq[ErgoBoxCandidate]): Array[Byte] = {
      val inputsCount = inputs.size.toShort
      val inputBytes = new Array[Byte](inputsCount * ErgoBox.BoxId.size)
      (0 until inputsCount).foreach { i =>
        System.arraycopy(inputs(i), 0, inputBytes, i * BoxId.size, BoxId.size)
      }

      val outputsCount = outputCandidates.size.toShort

      val outputBytes = outputCandidates.foldLeft(Array[Byte]()) { case (ba, c) =>
        ba ++ ErgoBoxCandidate.serializer.toBytes(c)
      }

      Shorts.toByteArray(inputsCount) ++
        inputBytes ++
        Shorts.toByteArray(outputsCount) ++
        outputBytes
    }

    def bytesToSign(tx: UnsignedErgoLikeTransaction): Array[Byte] =
      bytesToSign(tx.inputs.map(_.boxId), tx.outputCandidates)

    def bytesToSign(tx: ErgoLikeTransaction): Array[Byte] =
      bytesToSign(tx.inputs.map(_.boxId), tx.outputCandidates)


    override def toBytes(ftx: FlattenedTransaction): Array[Byte] = {
      ftx._1.map(_.spendingProof).foldLeft(bytesToSign(ftx._1.map(_.boxId), ftx._2)) { case (bytes, proof) =>
        bytes ++ SerializedProverResult.serializer.toBytes(proof)
      }
    }

    override def parseBody(bytes: Array[Byte], pos: Position): (FlattenedTransaction, Consumed) = {
      val posBeforeInputs = pos + 2
      val inputsCount = Shorts.fromByteArray(bytes.slice(pos, pos + 2))
      val inputs = (0 until inputsCount).foldLeft(Seq[ADKey]()) { case (ins, i) =>
        val boxId = ADKey @@ bytes.slice(posBeforeInputs + i * BoxId.size, posBeforeInputs + (i + 1) * BoxId.size)
        ins :+ boxId
      }

      val posBeforeOuts = posBeforeInputs + inputsCount * BoxId.size

      val outsCount = Shorts.fromByteArray(bytes.slice(posBeforeOuts, posBeforeOuts + 2))
      val (outputs, posBeforeProofs) = (0 until outsCount).foldLeft(Seq[ErgoBoxCandidate]() -> (posBeforeOuts + 2)) { case ((outs, p), _) =>
        val (bc, cs) = ErgoBoxCandidate.serializer.parseBody(bytes, p)
        (outs :+ bc) -> (p + cs)
      }

      val (proofs, finalPos) = (0 until inputsCount).foldLeft(Seq[SerializedProverResult]() -> posBeforeProofs) { case ((prs, p), _) =>
        val (pr, cs) = SerializedProverResult.serializer.parseBody(bytes, p)
        (prs :+ pr) -> (p + cs)
      }

      val signedInputs = inputs.zip(proofs).map { case (inp, pr) =>
        Input(inp, pr)
      }

      (signedInputs.toIndexedSeq, outputs.toIndexedSeq) -> (finalPos - pos)
    }
  }

  object serializer extends Serializer[ErgoLikeTransaction, ErgoLikeTransaction] {
    override def toBytes(tx: ErgoLikeTransaction): Array[Byte] =
      flattenedTxSerializer.toBytes(tx.inputs, tx.outputCandidates)

    override def parseBody(bytes: Array[Byte], pos: Position): (ErgoLikeTransaction, Consumed) = {
      val ((inputs, outputCandidates), consumed) = flattenedTxSerializer.parseBody(bytes, pos)
      ErgoLikeTransaction(inputs, outputCandidates) -> consumed
    }
  }
}
