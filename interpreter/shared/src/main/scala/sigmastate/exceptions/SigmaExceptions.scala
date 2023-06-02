package sigmastate.exceptions

import sigmastate.JitCost

/** Base class for Sigma-related exceptions.
  *
  * @param message the error message
  * @param cause an optional cause for the exception
  */
class SigmaException(val message: String, val cause: Option[Throwable] = None)
    extends Exception(message, cause.orNull)

/** Exception thrown during serialization.
  *
  * @param message the error message
  * @param cause an optional cause for the exception
  */
case class SerializerException(
    override val message: String,
    override val cause: Option[Throwable] = None)
    extends SigmaException(message, cause)

/** Exception thrown by [[sigmastate.interpreter.Interpreter]].
  *
  * @param message the error message
  * @param cause an optional cause for the exception
  */
class InterpreterException(message: String, cause: Option[Throwable] = None)
    extends SigmaException(message, cause)

/** Exception thrown when the estimated cost exceeds the allowed cost limit.
  *
  * @param estimatedCost the estimated cost of execution
  * @param message the error message
  * @param cause an optional cause for the exception
  */
class CostLimitException(
    val estimatedCost: Long,
    message: String,
    cause: Option[Throwable] = None)
    extends SigmaException(message, cause)

object CostLimitException {
  /** Generates a cost limit error message.
    *
    * @param cost  the estimated cost of execution
    * @param limit the allowed cost limit
    */
  def msgCostLimitError(
      cost: JitCost,
      limit: JitCost) = s"Estimated execution cost $cost exceeds the limit $limit"
}