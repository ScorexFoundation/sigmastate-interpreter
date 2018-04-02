package sigmastate.utxo

import com.google.common.primitives.Bytes


case class ErgoTransaction(inputs: IndexedSeq[ErgoBox], outputs: IndexedSeq[ErgoBox]) {
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
      concatBytes(inputs.map(_.bytes)))
}