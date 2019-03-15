package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.lang.SigmaBuilder
import sigmastate.lang.Terms.OperationId
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.SerializeLog
import sigmastate.utils.{SigmaByteWriter, SigmaByteReader}
import sigmastate.utxo.CostTable.Cost

/** This works in tandem with DataSerializer, if you change one make sure to check the other.*/
case class ConstantSerializer(builder: SigmaBuilder)
  extends ByteBufferSerializer[Constant[SType]] with ValueSerializer[Constant[SType]] {

  val opCode: OpCode = OpCodes.ConstantCode

  override def opCost(opId: OperationId) = Cost.ConstantNode

  def parseBody(r: SigmaByteReader): Value[SType] = deserialize(r)

  def serializeBody(obj: Constant[SType], w: SigmaByteWriter): Unit = serialize(obj, w)

  override def serialize(c: Constant[SType], w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false,"Constant")

    SerializeLog.logPrintf(true, true, false,"tpe")
    w.putType(c.tpe)
    SerializeLog.logPrintf(false, true, false,"tpe")

    SerializeLog.logPrintf(true, true, false,"value")
    DataSerializer.serialize(c.value, c.tpe, w)
    SerializeLog.logPrintf(false, true, false,"value")

    SerializeLog.logPrintf(false, true, false, "Constant")
  }

  override def deserialize(r: SigmaByteReader): Constant[SType] = {
    val tpe = r.getType()
    val obj = DataSerializer.deserialize(tpe, r)
    builder.mkConstant(obj, tpe)
  }
}

