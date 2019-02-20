package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.SigmaTyper.STypeSubst
import sigmastate.lang.Terms.MethodCall
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class MethodCallSerializer(opCode: Byte, cons: (Value[SType], SMethod, IndexedSeq[Value[SType]], STypeSubst) => Value[SType])
  extends ValueSerializer[MethodCall] {

  override def serialize(mc: MethodCall, w: SigmaByteWriter): Unit = {
    w.put(mc.method.objType.typeId)
    w.put(mc.method.methodId)
    w.putValue(mc.obj)
    if (opCode == OpCodes.MethodCallCode) {
      assert(mc.args.nonEmpty)
      w.putValues(mc.args)
    }
    w.putUByte(mc.typeSubst.size)
    mc.typeSubst.foreach { case (typeIdent, tpe) => w.putType(typeIdent).putType(tpe) }
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
    val typeSubstSize = r.getUByte()
    val xs = new Array[(STypeIdent, SType)](typeSubstSize)
    for (i <- 0 until typeSubstSize) {
      xs(i) = (r.getType().asInstanceOf[STypeIdent], r.getType())
    }
    val typeSubst = xs.toMap
    cons(obj, method, args, typeSubst)
  }
}
