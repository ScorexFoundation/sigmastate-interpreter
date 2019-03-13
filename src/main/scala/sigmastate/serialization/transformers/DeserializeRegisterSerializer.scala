package sigmastate.serialization.transformers

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import sigmastate.SType
import sigmastate.Values.Value
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.DeserializeRegister

case class DeserializeRegisterSerializer(cons: (RegisterId, SType, Option[Value[SType]]) => Value[SType])
  extends ValueSerializer[DeserializeRegister[SType]] {

  override val opCode: OpCode = OpCodes.DeserializeRegisterCode

  override def serializeBody(obj: DeserializeRegister[SType], w: SigmaByteWriter): Unit = {

    SerializeLog.logPrintf(true, true, false, "DeserializeRegisterSerializer")

    SerializeLog.logPrintf(true, true, false, "Register number")

    w.put(obj.reg.number)

    SerializeLog.logPrintf(false, true, false, "Register number")

    SerializeLog.logPrintf(true, true, false, "Object type")

    w.putType(obj.tpe)

    SerializeLog.logPrintf(false, true, false, "Object type")

    SerializeLog.logPrintf(true, true, false, "Object.default")

    //andruiman: why obj.default?, also changed from _.putValue(_).
    w.putOption(obj.default)((_,x) => w.putValue(x))

    SerializeLog.logPrintf(false, true, false, "Object.default")

    SerializeLog.logPrintf(false, true, false, "DeserializeRegisterSerializer")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val registerId = ErgoBox.findRegisterByIndex(r.getByte()).get
    val tpe = r.getType()
    val dv = r.getOption(r.getValue())
    cons(registerId, tpe, dv)
  }

}
