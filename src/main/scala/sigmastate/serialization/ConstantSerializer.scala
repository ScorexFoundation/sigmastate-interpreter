package sigmastate.serialization

import sigmastate.SType
import sigmastate.Values._
import sigmastate.lang.SigmaBuilder
import sigmastate.lang.Terms.OperationId
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.CostTable.Cost

/** This works in tandem with DataSerializer, if you change one make sure to check the other.*/
case class ConstantSerializer(builder: SigmaBuilder)
  extends ByteBufferSerializer[Constant[SType]] with ValueSerializer[Constant[SType]] {
  override def opDesc = Constant

  override def opCost(opId: OperationId): Int = Cost.ConstantNode

  override def parse(r: SigmaByteReader): Value[SType] = deserialize(r)

  override def serialize(c: Constant[SType], w: SigmaByteWriter): Unit = {
    w.putType(c.tpe)
    DataSerializer.serialize(c.value, c.tpe, w)
  }

  override def deserialize(r: SigmaByteReader): Constant[SType] = {
    val tpe = r.getType()
    val obj = DataSerializer.deserialize(tpe, r)
    builder.mkConstant(obj, tpe)
  }

}

