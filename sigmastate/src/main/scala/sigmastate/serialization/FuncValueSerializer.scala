package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._
import sigmastate.utils.SigmaByteWriter.{DataInfo, U, Vlq}

import scala.collection.mutable

case class FuncValueSerializer(cons: (IndexedSeq[(Int, SType)], Value[SType]) => Value[SType])
  extends ValueSerializer[FuncValue] {
  override def opDesc = FuncValue
  val numArgsInfo: DataInfo[Vlq[U[Int]]] = ArgInfo("numArgs", "number of function arguments")
  val idInfo: DataInfo[Vlq[U[Int]]] = ArgInfo("id_i", "identifier of the i-th argument")
  val typeInfo: DataInfo[SType] = ArgInfo("type_i", "type of the i-th argument")
  val bodyInfo: DataInfo[SValue] = ArgInfo("body", "function body, which is parameterized by arguments")

  override def serialize(obj: FuncValue, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.args.length, numArgsInfo)
    foreach(numArgsInfo.info.name, obj.args) { case (idx, tpe) =>
      w.putUInt(idx, idInfo)
        .putType(tpe, typeInfo)
    }
    w.putValue(obj.body, bodyInfo)
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
