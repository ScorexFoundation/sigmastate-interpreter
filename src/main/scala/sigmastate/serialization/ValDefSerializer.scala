package sigmastate.serialization

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import scorex.util.Extensions._
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import ValueSerializer._
import spire.syntax.all.cfor

case class ValDefSerializer(override val opDesc: ValueCompanion) extends ValueSerializer[ValDef] {

  override def serialize(obj: ValDef, w: SigmaByteWriter): Unit = {
    w.putUInt(obj.id)
    optional("type arguments") {
      if (opCode == FunDefCode) {
        require(!obj.isValDef, s"expected FunDef, got $obj")
        require(obj.tpeArgs.nonEmpty, s"expected FunDef with type args, got $obj")
        w.put(obj.tpeArgs.length.toByteExact)
        obj.tpeArgs.foreach(w.putType(_))
      }
    }
    w.putValue(obj.rhs)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt().toInt
    val tpeArgs: Seq[STypeVar] = opCode match {
      case FunDefCode =>
        val nTpeArgs = r.getByte()
        val inputs = new Array[STypeVar](nTpeArgs)
        cfor(0)(_ < nTpeArgs, _ + 1) { i =>
          inputs(i) = r.getType().asInstanceOf[STypeVar]
        }
        inputs
      case ValDefCode =>
        Nil
    }
    val rhs = r.getValue()
    r.valDefTypeStore(id) = rhs.tpe
    ValDef(id, tpeArgs, rhs)
  }
}
