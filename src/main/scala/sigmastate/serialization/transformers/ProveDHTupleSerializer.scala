package sigmastate.serialization.transformers

import sigmastate.SGroupElement
import sigmastate.Values.{Value, GroupElementConstant, SigmaBoolean, Constant}
import sigmastate.basics.ProveDHTuple
import sigmastate.lang.Terms._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.serialization.{DataSerializer, Serializer}

case class ProveDHTupleSerializer(cons: (Value[SGroupElement.type],
                                         Value[SGroupElement.type],
                                         Value[SGroupElement.type],
                                         Value[SGroupElement.type]) => SigmaBoolean)
  extends Serializer[ProveDHTuple, ProveDHTuple] {

  private val constCodePrefix: Byte = 0

  override def serializeBody(obj: ProveDHTuple, w: SigmaByteWriter): Unit = obj match {
    case ProveDHTuple(
    gv @ Constant(_, SGroupElement),
    hv @ Constant(_, SGroupElement),
    uv @ Constant(_, SGroupElement),
    vv @ Constant(_, SGroupElement)) =>
      w.put(constCodePrefix)
      w.putType(SGroupElement)
      DataSerializer.serialize(gv.value, gv.tpe, w)
      DataSerializer.serialize(hv.value, hv.tpe, w)
      DataSerializer.serialize(uv.value, uv.tpe, w)
      DataSerializer.serialize(vv.value, vv.tpe, w)
    case _ =>
      w.putValue(obj.gv)
      w.putValue(obj.hv)
      w.putValue(obj.uv)
      w.putValue(obj.vv)
  }

  override def parseBody(r: SigmaByteReader): ProveDHTuple = {
    val res = if (r.peekByte() == constCodePrefix) {
      val _ = r.getByte() // skip prefix code
      r.getType() match {
        case SGroupElement =>
          val gv = GroupElementConstant(DataSerializer.deserialize(SGroupElement, r))
          val hv = GroupElementConstant(DataSerializer.deserialize(SGroupElement, r))
          val uv = GroupElementConstant(DataSerializer.deserialize(SGroupElement, r))
          val vv = GroupElementConstant(DataSerializer.deserialize(SGroupElement, r))
          cons(gv, hv, uv, vv)
        case _ =>
          error("only SGroupElement tpe are supported")
      }
    } else {
      val gv = r.getValue().asValue[SGroupElement.type]
      val hv = r.getValue().asValue[SGroupElement.type]
      val uv = r.getValue().asValue[SGroupElement.type]
      val vv = r.getValue().asValue[SGroupElement.type]
      cons(gv, hv, uv, vv)
    }
    res.asInstanceOf[ProveDHTuple]
  }
}
