package sigmastate.exceptions

import sigmastate.lang.SourceContext

class CompilerException(message: String, val source: Option[SourceContext] = None, cause: Option[Throwable] = None)
    extends SigmaException(message, cause) {

  override def getMessage: String = source.map { srcCtx =>
    val lineNumberStrPrefix = s"line ${srcCtx.line}: "
    "\n" + lineNumberStrPrefix +
      s"${srcCtx.sourceLine}\n${" " * (lineNumberStrPrefix.length + srcCtx.column - 1)}^\n" + message
  }.getOrElse(message)
}

class BinderException(message: String, source: Option[SourceContext] = None)
    extends CompilerException(message, source)

class TyperException(message: String, source: Option[SourceContext] = None)
    extends CompilerException(message, source)

class BuilderException(message: String, source: Option[SourceContext] = None)
  extends CompilerException(message, source)

class CosterException(message: String, source: Option[SourceContext], cause: Option[Throwable] = None)
    extends CompilerException(message, source, cause)






