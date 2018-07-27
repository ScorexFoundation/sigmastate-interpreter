package sigmastate.serialization

import sigmastate.{Values, SType}
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{ByteWriter, ByteReader}
import sigmastate.utxo.IsValid
import sigmastate.utils.Extensions._

object IsValidSerializer extends ValueSerializer[IsValid] {

  override val opCode: Byte = ProofIsValidCode

  def serializeBody(obj: IsValid, w: ByteWriter): Unit = {
    w.putValue(obj.input)
  }

  def parseBody(r: ByteReader): Values.Value[SType] = {
    val p = r.getValue().asProof
    IsValid(p)
  }
}
