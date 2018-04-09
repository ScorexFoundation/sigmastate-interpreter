package sigmastate.utxo

import com.google.common.primitives.Bytes
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.UncheckedTree
import sigmastate.interpreter.ProverResult

import scala.util.Try


trait ErgoBoxReader {
  def byId(boxId: ADKey): Try[ErgoBox]
}

case class Input(boxId: ADKey, spendingProof: ProverResult[UncheckedTree]) {
  def bytes: Array[Byte] = Array()
}


case class ErgoTransaction(inputs: IndexedSeq[Input], outputs: IndexedSeq[ErgoBox]) {
  def concatBytes(seq: Traversable[Array[Byte]]): Array[Byte] = {
    val length: Int = seq.map(_.length).sum
    val result: Array[Byte] = new Array[Byte](length)
    var pos: Int = 0
    seq.foreach{ array =>
      System.arraycopy(array, 0, result, pos, array.length)
      pos += array.length
    }
    result
  }

  lazy val messageToSign: Array[Byte] =
    Bytes.concat(if (outputs.nonEmpty) concatBytes(outputs.map(_.bytes)) else Array[Byte](),
      concatBytes(inputs.map(_.boxId)))

  lazy val id: Digest32 = Blake2b256.hash(messageToSign)
}