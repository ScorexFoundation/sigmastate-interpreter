package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable

case class FuncValueSerializer(cons: (IndexedSeq[(Int, SType)], Value[SType]) => Value[SType])
  extends ValueSerializer[FuncValue] {

  override val opCode: OpCode = FuncValueCode

  override def serialize(obj: FuncValue, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.args.length)
    obj.args.foreach{ case (idx, tpe) => w.putUInt(idx).putType(tpe) }
    w.putValue(obj.body)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val argsSize = r.getUInt().toIntExact
    val argsBuilder = mutable.ArrayBuilder.make[(Int, SType)]()
    for (_ <- 0 until argsSize) {
      val id = r.getUInt().toInt
      val tpe = r.getType()
      r.valDefTypeStore(id) = tpe
      argsBuilder += ((id, tpe))
    }
    val body = r.getValue()
    cons(argsBuilder.result(), body)
  }
}
