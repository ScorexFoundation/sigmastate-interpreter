package sigmastate.lang.exceptions

import sigmastate.JitCost
import sigmastate.lang.SourceContext

/** Base class of all exceptions thrown from various Sigma components. */
class SigmaException(val message: String, val source: Option[SourceContext] = None, val cause: Option[Throwable] = None)
    extends Exception(message, cause.orNull) {

  override def getMessage: String = source.map { srcCtx =>
    val lineNumberStrPrefix = s"line ${srcCtx.line}: "
    "\n" + lineNumberStrPrefix +
      s"${srcCtx.sourceLine}\n${" " * (lineNumberStrPrefix.length + srcCtx.column - 1)}^\n" + message
  }.getOrElse(message)
}

/** Exception thrown by the [[sigmastate.lang.SigmaBinder]]. */
class BinderException(message: String, source: Option[SourceContext] = None)
    extends SigmaException(message, source)

/** Exception thrown by the [[sigmastate.lang.SigmaTyper]]. */
class TyperException(message: String, source: Option[SourceContext] = None)
    extends SigmaException(message, source)

/** Exception thrown by the [[sigmastate.lang.SigmaSpecializer]]. */
class SpecializerException(message: String, source: Option[SourceContext] = None)
    extends SigmaException(message, source)

/** Exception thrown by the [[sigmastate.serialization.SigmaSerializer]]. */
case class SerializerException(
  override val message: String,
  override val source: Option[SourceContext] = None,
  override val cause: Option[Throwable] = None)
  extends SigmaException(message, source, cause)

/** Exception thrown by the [[sigmastate.lang.SigmaBuilder]]. */
class BuilderException(message: String, source: Option[SourceContext] = None)
  extends SigmaException(message, source)

/** Exception thrown by interpreter during cost estimation. */
class CosterException(message: String, source: Option[SourceContext], cause: Option[Throwable] = None)
    extends SigmaException(message, source, cause)

/** Exception thrown by [[sigmastate.interpreter.Interpreter]]. */
class InterpreterException(message: String, source: Option[SourceContext] = None, cause: Option[Throwable] = None)
  extends SigmaException(message, source, cause)

/** Exception thrown by [[sigmastate.interpreter.Interpreter]] when cost limit is exceeded. */
class CostLimitException(val estimatedCost: Long, message: String, cause: Option[Throwable] = None)
    extends SigmaException(message, None, cause)

object CostLimitException {
  /** Formats the error message with the given parameters. */
  def msgCostLimitError(cost: JitCost, limit: JitCost) = s"Estimated execution cost $cost exceeds the limit $limit"
}
