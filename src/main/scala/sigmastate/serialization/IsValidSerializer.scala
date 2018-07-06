package sigmastate.serialization

import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position
import sigmastate.utxo.IsValid

object IsValidSerializer extends ValueSerializer[IsValid] {

  override val opCode: Byte = ProofIsValidCode

  override def serializeBody(node: IsValid): Array[Byte] = {
    val w = Serializer.startWriter()
        .putValue(node.input)
    w.toBytes
  }

  override def parseBody(bytes: Array[Byte], pos: Position): (IsValid, Position) = {
    val r = Serializer.startReader(bytes, pos)
    val p = r.getValue().asProof
    (IsValid(p), r.consumed)
  }
}
