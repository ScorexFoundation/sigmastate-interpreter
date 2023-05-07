package org.ergoplatform.sdk.js

import org.ergoplatform.sdk.Iso
import sigmastate.SType
import sigmastate.Values.Constant
import sigmastate.eval.Evaluation

/** Definitions of isomorphisms. */
object Isos {
  /** Conversion between `Value` and `Constant[SType]`. */
  implicit val isoValueToConstant: Iso[Value, Constant[SType]] = new Iso[Value, Constant[SType]] {
    override def to(x: Value): Constant[SType] =
      Constant(x.runtimeData.asInstanceOf[SType#WrappedType], Evaluation.rtypeToSType(x.tpe.rtype))

    override def from(x: Constant[SType]): Value = {
      val rtype = Evaluation.stypeToRType(x.tpe)
      val jsvalue = Value.fromRuntimeData(x.value, rtype)
      new Value(jsvalue, new Type(rtype))
    }
  }
}
