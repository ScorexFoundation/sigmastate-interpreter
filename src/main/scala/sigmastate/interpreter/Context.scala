package sigmastate.interpreter

import sigmastate.Value

/**
  * Variables to be put into context
  */
case class ContextExtension(values: Map[Int, _ <: Value])


trait Context[C <: Context[C]] {
  val extension: ContextExtension

  def withExtension(newExtension: ContextExtension): C
}
