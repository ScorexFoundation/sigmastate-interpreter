package org.ergoplatform.sdk.js

import sigma.js.SigmaProp
import sigma.util.Extensions.LongOps
import sigmastate.interpreter.Interpreter

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[sigmastate.interpreter.Interpreter.ReductionResult]] available from JS. */
@JSExportTopLevel("ReductionResult")
class ReductionResult(
  val value: sigma.js.SigmaProp,
  val cost: Int
) extends js.Object

@JSExportTopLevel("ReductionResult$")
object ReductionResult extends js.Object {
  val isoToSdk = new sigma.data.Iso[ReductionResult, Interpreter.ReductionResult] {
    override def to(a: ReductionResult): Interpreter.ReductionResult = {
      Interpreter.ReductionResult(a.value.sigmaBoolean, a.cost)
    }
    override def from(b: Interpreter.ReductionResult): ReductionResult = {
      new ReductionResult(new SigmaProp(b.value), b.cost.toIntExact)
    }
  }
}