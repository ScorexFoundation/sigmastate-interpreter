package sigmastate.exceptions

import sigmastate.JitCost

class SigmaException(val message: String, val cause: Option[Throwable] = None)
    extends Exception(message, cause.orNull)

case class SerializerException(
    override val message: String,
    override val cause: Option[Throwable] = None)
    extends SigmaException(message, cause)

class InterpreterException(message: String, cause: Option[Throwable] = None)
    extends SigmaException(message, cause)

class CostLimitException(
    val estimatedCost: Long,
    message: String,
    cause: Option[Throwable] = None)
    extends SigmaException(message, cause)

object CostLimitException {
  def msgCostLimitError(
      cost: JitCost,
      limit: JitCost) = s"Estimated execution cost $cost exceeds the limit $limit"
}