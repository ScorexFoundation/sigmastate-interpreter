package sigmastate.interpreter

import sigmastate.SType
import sigmastate.Values.EvaluatedValue

/**
  * Variables to be put into context
  */
case class ContextExtension(values: Map[Byte, EvaluatedValue[_ <: SType]]) {

  //context values should not use context to determine their cost
  //todo: getOrElse(0L) branch triggers for local variables in Where/ForAll/Exists/Fold etc.
  //todo: Should the cost be 0 in this case?
  def cost(id: Byte): Long = values.get(id).map(_.cost(null)).getOrElse(0L)
}

object ContextExtension {
  val empty = ContextExtension(Map())
}


trait Context[C <: Context[C]] {
  val extension: ContextExtension

  def withExtension(newExtension: ContextExtension): C
}
