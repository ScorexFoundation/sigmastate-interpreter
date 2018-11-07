package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class ConstantPlaceholderSerializer(cons: (Int, SType) => Value[SType])
  extends ValueSerializer[ConstantPlaceholder[SType]] {

  override val opCode: OpCode = ConstantPlaceholderIndexCode

  override def serializeBody(obj: ConstantPlaceholder[SType], w: SigmaByteWriter): Unit = {
    w.putUInt(obj.id)
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val constant = r.constantStore match {
      case Some(store) => store.get(id)
      case None => sys.error("cannot deserialize Constant without a ConstantStore")
    }
    constant
  }
}

