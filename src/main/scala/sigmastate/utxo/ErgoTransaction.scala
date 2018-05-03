package sigmastate.utxo

import com.google.common.primitives.Bytes
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.UncheckedTree
import sigmastate.interpreter.ProverResult
import sigmastate.utils.Helpers._

import scala.collection.immutable
import scala.util.Try


trait ErgoBoxReader {
  def byId(boxId: ADKey): Try[ErgoBox]
}

trait InputTemplate {
  val boxId: ADKey
}


case class UnsignedInput(override val boxId: ADKey) extends InputTemplate

case class Input(override val boxId: ADKey, spendingProof: ProverResult[UncheckedTree])
  extends InputTemplate {
  def bytes: Array[Byte] = Array()
}

trait ErgoTransactionTemplate[IT <: InputTemplate] {
  val inputs: IndexedSeq[IT]
  val outputCandidates: IndexedSeq[ErgoBoxCandidate]

  require(outputCandidates.size <= Short.MaxValue)

  lazy val outputs: IndexedSeq[ErgoBox] = outputCandidates.indices.map(idx => outputCandidates(idx).toBox(id, idx.toShort))

  lazy val messageToSign: Array[Byte] = {
    val outBytes = if (outputCandidates.nonEmpty)
        outputCandidates.map(_.bytesWithNoRef)
      else
        Vector(Array[Byte]())
    concatBytes(outBytes ++ inputs.map(_.boxId))
  }

  lazy val id: Digest32 = Blake2b256.hash(messageToSign)
}

case class UnsignedErgoTransaction(override val inputs: IndexedSeq[UnsignedInput],
                                   override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoTransactionTemplate[UnsignedInput] {

  def toSigned(proofs: IndexedSeq[ProverResult[UncheckedTree]]): ErgoTransaction = {
    require(proofs.size == inputs.size)
    val ins = inputs.zip(proofs).map{case (ui, proof) => Input(ui.boxId, proof)}
    ErgoTransaction(ins, outputCandidates)
  }
}

/**
  * Fully signed transaction
  * @param inputs
  * @param outputCandidates
  */
case class ErgoTransaction(override val inputs: IndexedSeq[Input],
                           override val outputCandidates: IndexedSeq[ErgoBoxCandidate])
  extends ErgoTransactionTemplate[Input]