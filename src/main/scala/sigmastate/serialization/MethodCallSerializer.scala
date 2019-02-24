package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.SigmaTyper.STypeSubst
import sigmastate.lang.Terms.{MethodCall, STypeParam}
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
    mc.method.stype match {
      case genType: SGenericType =>
        w.putUByte(mc.typeSubst.size)
        mc.typeSubst.foreach { case (ti, tpe) =>
          val tpIndex = genType.substitutedTypeParams.indexOf(STypeParam(ti))
          w.putUByte(tpIndex)
            .putType(tpe)
        }
      case _ => w.putUByte(0)
    }
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val typeId = r.getByte()
    val methodId = r.getByte()
    val obj = r.getValue()
    val args = if (opCode == OpCodes.MethodCallCode) r.getValues() else IndexedSeq()
    val method = MethodCall.fromIds(typeId, methodId)
    val typeSubst: STypeSubst = method.stype match {
      case genType: SGenericType =>
        val typeSubstSize = r.getUByte()
        val xs = new Array[(STypeIdent, SType)](typeSubstSize)
        for (i <- 0 until typeSubstSize) {
          val tpIndex = r.getUByte()
          val ti = genType.substitutedTypeParams.apply(tpIndex).ident
          xs(i) = (ti, r.getType())
        }
        xs.toMap
      case _ =>
        r.getUByte() // read 0
        Map()
    }
    cons(obj, method, args, typeSubst)
  }
}
