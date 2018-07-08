package sigmastate.serialization

import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position
import sigmastate.utxo.{ProofBytes}

object ProofBytesSerializer extends ValueSerializer[ProofBytes] {
  override val opCode: Byte = ProofBytesCode

  override def serializeBody(node: ProofBytes): Array[Byte] = {
    val w = Serializer.startWriter()
        .putValue(node.input)
    w.toBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (ProofBytes, Position) = {
    val r = Serializer.startReader(bytes, pos)
    val p = r.getValue().asProof
    (ProofBytes(p), r.consumed)
  }
}
