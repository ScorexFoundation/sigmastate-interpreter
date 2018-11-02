package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Extensions._
import sigmastate.utils.{ByteReader, ByteWriter}

import scala.collection.mutable

case class ValDefSerializer(override val opCode: OpCode) extends ValueSerializer[ValDef] {

  override def serializeBody(obj: ValDef, w: ByteWriter): Unit = {
    w.putUInt(obj.id)
    if (opCode == FunDefCode) {
      require(!obj.isValDef, s"expected FunDef, got $obj")
      require(obj.tpeArgs.nonEmpty, s"expected FunDef with type args, got $obj")
      w.put(obj.tpeArgs.length.toByteExact)
      obj.tpeArgs.foreach(w.putType)
    }
    w.putValue(obj.rhs)
  }

  override def parseBody(r: ByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val tpeArgs: Seq[STypeIdent] = opCode match {
      case FunDefCode =>
        val tpeArgsCount = r.getByte()
        val inputsBuilder = mutable.ArrayBuilder.make[STypeIdent]()
        for (_ <- 0 until tpeArgsCount) {
          inputsBuilder += r.getType().asInstanceOf[STypeIdent]
        }
        inputsBuilder.result()
      case ValDefCode =>
        Seq()
    }
    val rhs = r.getValue()
    ValDef(id, tpeArgs, rhs)
  }
}
