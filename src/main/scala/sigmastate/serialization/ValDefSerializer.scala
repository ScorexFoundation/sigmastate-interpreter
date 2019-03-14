package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigma.util.Extensions._
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}

import scala.collection.mutable

case class ValDefSerializer(override val opCode: OpCode) extends ValueSerializer[ValDef] {

  override def serializeBody(obj: ValDef, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false, "ValDef")

    SerializeLog.logPrintf(true, true, false, "id")
    w.putUInt(obj.id)
    SerializeLog.logPrintf(false, true, false, "id")

    if (opCode == FunDefCode) {
      require(!obj.isValDef, s"expected FunDef, got $obj")
      require(obj.tpeArgs.nonEmpty, s"expected FunDef with type args, got $obj")

      SerializeLog.logPrintf(true, true, false, "opCode==FunDefCode")

      SerializeLog.logPrintf(true, true, false, "tpeArgs.length.toByteExact")
      w.put(obj.tpeArgs.length.toByteExact)
      SerializeLog.logPrintf(false, true, false, "tpeArgs.length.toByteExact")

      SerializeLog.logPrintf(true, true, false, "tpeArgs*")
      obj.tpeArgs.foreach(w.putType)
      SerializeLog.logPrintf(false, true, false, "tpeArgs*")

      SerializeLog.logPrintf(false, true, false, "opCode==FunDefCode")
    }

    SerializeLog.logPrintf(true, true, false, "rhs")
    w.putValue(obj.rhs)
    SerializeLog.logPrintf(false, true, false, "rhs")

    SerializeLog.logPrintf(false, true, false, "ValDef")
  }

  override def parseBody(r: SigmaByteReader): Value[SType] = {
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
    r.valDefTypeStore(id) = rhs.tpe
    ValDef(id, tpeArgs, rhs)
  }
}
