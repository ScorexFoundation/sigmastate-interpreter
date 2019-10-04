package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._

import scala.collection.mutable

case class FuncValueSerializer(cons: (IndexedSeq[(Int, SType)], Value[SType]) => Value[SType])
  extends ValueSerializer[FuncValue] {
  override def opDesc = FuncValue

  override def serialize(obj: FuncValue, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.args.length, ArgInfo("numArgs", "number of function arguments"))
    foreach("numArgs", obj.args) { case (idx, tpe) =>
      w.putUInt(idx, ArgInfo("id_i", "identifier of the i-th argument"))
        .putType(tpe, ArgInfo("type_i", "type of the i-th argument"))
    }
    w.putValue(obj.body, ArgInfo("body", "function body, which is parameterized by arguments"))
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val argsSize = r.getUInt().toIntExact
    val argsBuilder = mutable.ArrayBuilder.make[(Int, SType)]()
    for (_ <- 0 until argsSize) {
      val id = r.getUInt().toIntExact   // HF change: was r.getUInt().toInt
      val tpe = r.getType()
      r.valDefTypeStore(id) = tpe
      argsBuilder += ((id, tpe))
    }
    val body = r.getValue()
    cons(argsBuilder.result(), body)
  }
}
