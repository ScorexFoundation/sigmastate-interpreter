package sigmastate.interpreter

import sigmastate.SType
import sigmastate.Values.EvaluatedValue

/**
  * Variables to be put into context
  */
case class ContextExtension(values: Map[Byte, EvaluatedValue[_ <: SType]]) {

  //context values should not use context to determine their cost
  def cost(id: Byte) = values(id).cost(null)
}

object ContextExtension {
  val empty = ContextExtension(Map())
}


trait Context[C <: Context[C]] {
  val extension: ContextExtension

  def withExtension(newExtension: ContextExtension): C
}
