package sigmastate.serialization.transformers

import sigmastate.SGroupElement
import sigmastate.Values.{Constant, GroupElementConstant, SigmaBoolean, Value}
import sigmastate.basics.ProveDHTuple
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{DataSerializer, OpCodes, ValueSerializer}
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

case class ProveDHTupleSerializer(cons: (Value[SGroupElement.type],
                                         Value[SGroupElement.type],
                                         Value[SGroupElement.type],
                                         Value[SGroupElement.type]) => SigmaBoolean)
  extends ValueSerializer[ProveDHTuple] {

  override val opCode: OpCode = OpCodes.ProveDiffieHellmanTupleCode

  private val constCodePrefix: Byte = 0

  override def serializeBody(obj: ProveDHTuple, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false,"ProveDHTuple")

    obj match {
      case ProveDHTuple(
      gv @ Constant(_, SGroupElement),
      hv @ Constant(_, SGroupElement),
      uv @ Constant(_, SGroupElement),
      vv @ Constant(_, SGroupElement)) =>
        SerializeLog.logPrintf(true, true, false,"SGroupElementConstants")

        SerializeLog.logPrintf(true, true, false,"constCodePrefix")
        w.put(constCodePrefix)
        SerializeLog.logPrintf(false, true, false,"constCodePrefix")

        SerializeLog.logPrintf(true, true, false,"SGroupElement type")
        w.putType(SGroupElement)
        SerializeLog.logPrintf(false, true, false,"SGroupElement type")

        SerializeLog.logPrintf(true, true, false,"gv data")
        DataSerializer.serialize(gv.value, gv.tpe, w)
        SerializeLog.logPrintf(false, true, false,"gv data")

        SerializeLog.logPrintf(true, true, false,"hv data")
        DataSerializer.serialize(hv.value, hv.tpe, w)
        SerializeLog.logPrintf(false, true, false,"hv data")

        SerializeLog.logPrintf(true, true, false,"uv data")
        DataSerializer.serialize(uv.value, uv.tpe, w)
        SerializeLog.logPrintf(false, true, false,"uv data")

        SerializeLog.logPrintf(true, true, false,"vv data")
        DataSerializer.serialize(vv.value, vv.tpe, w)
        SerializeLog.logPrintf(false, true, false,"vv data")

        SerializeLog.logPrintf(false, true, false,"SGroupElementConstants")
      case _ =>
        SerializeLog.logPrintf(true, true, false,"Not SGroupElementConstants")

        SerializeLog.logPrintf(true, true, false,"gv")
        w.putValue(obj.gv)
        SerializeLog.logPrintf(false, true, false,"gv")

        SerializeLog.logPrintf(true, true, false,"hv")
        w.putValue(obj.hv)
        SerializeLog.logPrintf(false, true, false,"hv")

        SerializeLog.logPrintf(true, true, false,"uv")
        w.putValue(obj.uv)
        SerializeLog.logPrintf(false, true, false,"uv")

        SerializeLog.logPrintf(true, true, false,"vv")
        w.putValue(obj.vv)
        SerializeLog.logPrintf(false, true, false,"vv")

        SerializeLog.logPrintf(false, true, false,"Not SGroupElementConstants")
    }

    SerializeLog.logPrintf(false, true, false,"ProveDHTuple")
  }

  override def parseBody(r: SigmaByteReader): SigmaBoolean = {
    if (r.peekByte() == constCodePrefix) {
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
  }
}
