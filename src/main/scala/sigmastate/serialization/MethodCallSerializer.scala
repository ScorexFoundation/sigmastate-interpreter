package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms.MethodCall
import sigmastate.serialization.OpCodes._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class MethodCallSerializer(opCode: Byte, cons: (Value[SType], SMethod, IndexedSeq[Value[SType]]) => Value[SType])
  extends ValueSerializer[MethodCall] {

  override def serializeBody(mc: MethodCall, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "MethodCall")

    SerializeLog.logPrintf(true, true, false, "method.objType.typeId")
    w.put(mc.method.objType.typeId)
    SerializeLog.logPrintf(false, true, false, "method.objType.typeId")

    SerializeLog.logPrintf(true, true, false, "method.methodId")
    w.put(mc.method.methodId)
    SerializeLog.logPrintf(false, true, false, "method.methodId")

    SerializeLog.logPrintf(true, true, false, "obj")
    w.putValue(mc.obj)
    SerializeLog.logPrintf(false, true, false, "obj")

    if (opCode == OpCodes.MethodCallCode) {
      assert(mc.args.nonEmpty)

      SerializeLog.logPrintf(true, true, false, "opCode == OpCodes.MethodCallCode")

      SerializeLog.logPrintf(true, true, false, "args")
      w.putValues(mc.args)
      SerializeLog.logPrintf(false, true, false, "args")

      SerializeLog.logPrintf(false, true, false, "opCode == OpCodes.MethodCallCode")
    }

    SerializeLog.logPrintf(false, true, false, "MethodCall")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
    val typeId = r.getByte()
    val methodId = r.getByte()
    val obj = r.getValue()
    val args = if (opCode == OpCodes.MethodCallCode) {
      r.getValues()
    }
    else IndexedSeq()
    val method = MethodCall.fromIds(typeId, methodId)
    cons(obj, method, args)
  }
}
