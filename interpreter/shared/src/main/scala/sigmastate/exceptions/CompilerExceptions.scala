package sigmastate.exceptions

import sigmastate.lang.SourceContext

/** Base class for exceptions thrown by the compiler.
  *
  * @param message the error message
  * @param source an optional source context with location information
  * @param cause an optional cause for the exception
  */
class CompilerException(message: String, val source: Option[SourceContext] = None, cause: Option[Throwable] = None)
    extends SigmaException(message, cause) {

  override def getMessage: String = source.map { srcCtx =>
    val lineNumberStrPrefix = s"line ${srcCtx.line}: "
    "\n" + lineNumberStrPrefix +
      s"${srcCtx.sourceLine}\n${" " * (lineNumberStrPrefix.length + srcCtx.column - 1)}^\n" + message
  }.getOrElse(message)
}

/** Exception thrown during the binding phase of the compiler.
  *
  * @param message the error message
  * @param source an optional source context with location information
  */
class BinderException(message: String, source: Option[SourceContext] = None)
    extends CompilerException(message, source)

/** Exception thrown during the type checking phase of the compiler.
  *
  * @param message the error message
  * @param source an optional source context with location information
  */
class TyperException(message: String, source: Option[SourceContext] = None)
    extends CompilerException(message, source)

/** Exception thrown during the building phase of the compiler.
  *
  * @param message the error message
  * @param source an optional source context with location information
  */
class BuilderException(message: String, source: Option[SourceContext] = None)
  extends CompilerException(message, source)

// TODO v5.x: remove this exception
class CosterException(message: String, source: Option[SourceContext], cause: Option[Throwable] = None)
    extends CompilerException(message, source, cause)






