package sigmastate.js

import sigmastate.{Iso, SType}
import sigmastate.Values.Constant
import sigmastate.eval.Evaluation

object Isos {
  implicit val isoValueToConstant: Iso[Value, Constant[SType]] = new Iso[Value, Constant[SType]] {
    override def to(x: Value): Constant[SType] =
      Constant(x.runtimeValue.asInstanceOf[SType#WrappedType], Evaluation.rtypeToSType(x.tpe.rtype))

    override def from(x: Constant[SType]): Value = {
      val rtype = Evaluation.stypeToRType(x.tpe)
      val jsvalue = Value.fromRuntimeValue(x.value, rtype)
      new Value(jsvalue, new Type(rtype))
    }
  }
}
