package sigmastate.interpreter

import sigmastate.EvaluatedValue

/**
  * Variables to be put into context
  */
case class ContextExtension(values: Map[Byte, _ <: EvaluatedValue[_]])


trait Context[C <: Context[C]] {
  val extension: ContextExtension

  def withExtension(newExtension: ContextExtension): C
}
