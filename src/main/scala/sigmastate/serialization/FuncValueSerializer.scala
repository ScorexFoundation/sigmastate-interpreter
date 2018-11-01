package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}

case class FuncValueSerializer(cons: (IndexedSeq[(Int, SType)], Value[SType]) => Value[SType])
  extends ValueSerializer[FuncValue] {

  override val opCode: OpCode = FuncValueCode

  override def serializeBody(obj: FuncValue, w: ByteWriter): Unit = {
    w.putUInt(obj.args.length)
    obj.args.foreach{ case (idx, tpe) => w.putUInt(idx).putType(tpe) }
    w.putValue(obj.body)
  }

  override def parseBody(r: ByteReader): Value[SType] = {
    val argsSize = r.getUInt().toIntExact
    val args = (1 to argsSize).map(_ => (r.getUInt().toInt, r.getType()))
    val body = r.getValue()
    cons(args, body)
  }
}
