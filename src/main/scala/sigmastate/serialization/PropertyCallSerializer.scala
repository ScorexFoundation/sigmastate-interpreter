package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.SigmaTyper.STypeSubst
import sigmastate.lang.Terms.{MethodCall, PropertyCall}
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

case class PropertyCallSerializer(cons: (Value[SType], SMethod, IndexedSeq[Value[SType]], STypeSubst) => Value[SType])
  extends ValueSerializer[MethodCall] {
  override def opDesc: ValueCompanion = PropertyCall

  override def serialize(mc: MethodCall, w: SigmaByteWriter): Unit = {
    w.put(mc.method.objType.typeId, ArgInfo("typeCode", "type of the method (see Table~\\ref{table:predeftypes})"))
    w.put(mc.method.methodId, ArgInfo("methodCode", "a code of the proprty"))
    w.putValue(mc.obj, ArgInfo("obj", "receiver object of this property call"))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val typeId = r.getByte()
    val methodId = r.getByte()
    val obj = r.getValue()
    val args = IndexedSeq()
    val method = SMethod.fromIds(typeId, methodId)
    val specMethod = method.specializeFor(obj.tpe, args)
    cons(obj, specMethod, args, Map())
  }
}
