package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms.MethodCall
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class MethodCallSerializer(opCode: Byte, cons: (Value[SType], SMethod, IndexedSeq[Value[SType]]) => Value[SType])
  extends ValueSerializer[MethodCall] {

  override def serialize(mc: MethodCall, w: SigmaByteWriter): Unit = {
    w.put(mc.method.objType.typeId)
    w.put(mc.method.methodId)
    w.putValue(mc.obj)
    if (opCode == OpCodes.MethodCallCode) {
      assert(mc.args.nonEmpty)
      w.putValues(mc.args)
    }
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
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
