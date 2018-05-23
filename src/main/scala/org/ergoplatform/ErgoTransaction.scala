package org.ergoplatform

import com.google.common.primitives.Shorts
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.interpreter.{ProverResult, SerializedProverResult}
import sigmastate.serialization.Serializer
import sigmastate.serialization.Serializer.{Consumed, Position}

import scala.util.Try


trait ErgoBoxReader {
  def byId(boxId: ADKey): Try[ErgoBox]
}


sealed trait ErgoTransactionTemplate[IT <: UnsignedInput] {
  val inputs: IndexedSeq[IT]
  val outputCandidates: IndexedSeq[ErgoBoxCandidate]

  require(outputCandidates.size <= Short.MaxValue)

  lazy val outputs: IndexedSeq[ErgoBox] = outputCandidates.indices.map(idx => outputCandidates(idx).toBox(id, idx.toShort))

  lazy val messageToSign: Array[Byte] = ErgoTransaction.serializer.bytesToSign(inputs.map(_.boxId), outputCandidates)

  lazy val id: Digest32 = Blake2b256.hash(messageToSign)
}


case class UnsignedErgoTransaction(override val inputs: IndexedSeq[UnsignedInput],
                                   override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoTransactionTemplate[UnsignedInput] {

  def toSigned(proofs: IndexedSeq[ProverResult]): ErgoTransaction = {
    require(proofs.size == inputs.size)
    val ins = inputs.zip(proofs).map { case (ui, proof) => Input(ui.boxId, proof.toSerialized) }
    ErgoTransaction(ins, outputCandidates)
  }
}

/**
  * Fully signed transaction
  *
  * @param inputs
  * @param outputCandidates
  */
case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoTransactionTemplate[Input]


object ErgoTransaction {

  object serializer extends Serializer[ErgoTransaction, ErgoTransaction] {
    def bytesToSign(inputs: IndexedSeq[ADKey],
                    outputCandidates: IndexedSeq[ErgoBoxCandidate]): Array[Byte] = {
      val inputsCount = inputs.size.toShort
      val inputBytes = new Array[Byte](inputsCount * 32)
      (0 until inputsCount).foreach{i =>
        System.arraycopy(inputs(i), 0, inputBytes, i*32, 32)
      }

      val outputsCount = outputCandidates.size.toShort

      val outputBytes = outputCandidates.foldLeft(Array[Byte]()){case (ba, c) =>
        ba ++ ErgoBoxCandidate.serializer.toBytes(c)
      }

      Shorts.toByteArray(inputsCount) ++
        inputBytes ++
        Shorts.toByteArray(outputsCount) ++
        outputBytes
    }

    def bytesToSign(tx: UnsignedErgoTransaction): Array[Byte] =
      bytesToSign(tx.inputs.map(_.boxId), tx.outputCandidates)

    def bytesToSign(tx: ErgoTransaction): Array[Byte] =
      bytesToSign(tx.inputs.map(_.boxId), tx.outputCandidates)


    override def toBytes(tx: ErgoTransaction): Array[Byte] = {
      tx.inputs.map(_.spendingProof).foldLeft(bytesToSign(tx)){case (bytes, proof) =>
          bytes ++ SerializedProverResult.serializer.toBytes(proof)
      }
    }

    override def parseBody(bytes: Array[Byte], pos: Position): (ErgoTransaction, Consumed) = {
      val inputsCount = Shorts.fromByteArray(bytes.slice(pos, pos + 2))
      val inputs = (0 until inputsCount).foldLeft(Seq[ADKey]()) {case (ins, i) =>
        val boxId = ADKey @@ bytes.slice(pos + 2 + i*32, pos + 2 + (i + 1)*32)
        ins :+ boxId
      }

      val posBeforeOuts = pos + 2 + inputsCount * 32

      val outsCount = Shorts.fromByteArray(bytes.slice(posBeforeOuts, posBeforeOuts + 2))
      val (outputs, posBeforeProofs) = (0 until outsCount).foldLeft(Seq[ErgoBoxCandidate]() -> (posBeforeOuts + 2)){case ((outs, p), _) =>
        val (bc, cs) = ErgoBoxCandidate.serializer.parseBody(bytes, p)
        (outs :+ bc) -> (p + cs)
      }

      val (proofs, finalPos) = (0 until inputsCount).foldLeft(Seq[SerializedProverResult]() -> posBeforeProofs){case ((prs, p), _) =>
        val (pr, cs) = SerializedProverResult.serializer.parseBody(bytes, p)
        (prs :+ pr) -> (p + cs)
      }

      val signedInputs = inputs.zip(proofs).map {case (inp, pr) =>
        Input(inp, pr)
      }

      ErgoTransaction(signedInputs.toIndexedSeq, outputs.toIndexedSeq) -> (finalPos - pos)
    }
  }
}