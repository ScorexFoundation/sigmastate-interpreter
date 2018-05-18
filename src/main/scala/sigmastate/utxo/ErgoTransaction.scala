package sigmastate.utxo

import com.google.common.primitives.Shorts
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.UncheckedTree
import sigmastate.interpreter.ProverResult
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
      val inputsCount = tx.inputs.size
      val outputCandidatesCount = tx.outputCandidates.size
      ???
    }

    override def parseBytes(bytes: Array[Byte]): Try[ErgoTransaction] = ???

    override def parseBody(bytes: Array[Byte], pos: Position): (ErgoTransaction, Consumed) = ???

    override def serializeBody(obj: ErgoTransaction): Array[Byte] = ???
  }
}