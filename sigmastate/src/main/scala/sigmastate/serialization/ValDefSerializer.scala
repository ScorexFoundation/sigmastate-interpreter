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
        val args = obj.tpeArgs
        val len = args.length
        require(!obj.isValDef, s"expected FunDef, got $obj")
        require(len > 0, s"expected FunDef with type args, got $obj")
        w.put(len.toByteExact)
        cfor(0)(_ < len, _ + 1) { i =>
          val arg = args(i)
          w.putType(arg)
        }
      }
    }
    w.putValue(obj.rhs)
  }

  override def parse(r: SigmaByteReader): Value[SType] = {
    val id = r.getUInt().toIntExact   // HF change: was r.getUInt().toInt
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
