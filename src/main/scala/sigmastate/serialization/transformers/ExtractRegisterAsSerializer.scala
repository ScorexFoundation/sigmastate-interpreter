package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.ExtractRegisterAs
import sigmastate.{SBox, SOption, SType}

case class ExtractRegisterAsSerializer(cons: (Value[SBox.type], RegisterId, SOption[SType]) => Value[SType])
  extends ValueSerializer[ExtractRegisterAs[SType]] {
  override val opCode: OpCode = OpCodes.ExtractRegisterAs

  override def serializeBody(obj: ExtractRegisterAs[SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "ExtractRegisterAs")

    SerializeLog.logPrintf(true, true, false, "input")
    w.putValue(obj.input)
    SerializeLog.logPrintf(false, true, false, "input")

    SerializeLog.logPrintf(true, true, false, "registerId.number")
    w.put(obj.registerId.number)
    SerializeLog.logPrintf(false, true, false, "registerId.number")

    SerializeLog.logPrintf(true, true, false, "tpe.elemType")
    w.putType(obj.tpe.elemType)
    SerializeLog.logPrintf(false, true, false, "tpe.elemType")

    SerializeLog.logPrintf(false, true, false, "ExtractRegisterAs")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val input = r.getValue()
    val regId = r.getByte()
    val register = ErgoBox.findRegisterByIndex(regId).get
    val tpe = r.getType()
    cons(input.asInstanceOf[Value[SBox.type]], register, SOption(tpe))
  }
}
