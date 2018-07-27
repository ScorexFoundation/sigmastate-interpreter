package sigmastate.serialization

import sigmastate.{Values, SType}
import sigmastate.lang.Terms._
import sigmastate.utils.Extensions._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteWriter, ByteReader}
import sigmastate.utxo.ProofBytes

object ProofBytesSerializer extends ValueSerializer[ProofBytes] {
  override val opCode: Byte = ProofBytesCode

  def serializeBody(obj: ProofBytes, w: ByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parseBody(r: ByteReader): Values.Value[SType] = {
    val p = r.getValue().asProof
    ProofBytes(p)
  }
}
