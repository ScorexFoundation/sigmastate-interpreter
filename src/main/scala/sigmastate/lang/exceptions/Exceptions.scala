package sigmastate.lang.exceptions

import sigmastate.lang.SourceContext

class SigmaException(val message: String, val source: Option[SourceContext] = None)
    extends Exception(message) {

  override def getMessage: String = source.map { srcCtx =>
    val lineNumberStrPrefix = s"line ${srcCtx.line}: "
    "\n" + lineNumberStrPrefix +
      s"${srcCtx.sourceLine}\n${" " * (lineNumberStrPrefix.length + srcCtx.column - 1)}^\n" + message
  }.getOrElse(message)
}

class BinderException(message: String, source: Option[SourceContext] = None)
    extends SigmaException(message, source)

class TyperException(message: String, source: Option[SourceContext] = None)
    extends SigmaException(message, source)

class SpecializerException(message: String, source: Option[SourceContext] = None)
    extends SigmaException(message, source)

class SerializerException(message: String, source: Option[SourceContext] = None)
  extends SigmaException(message, source)

class BuilderException(message: String, source: Option[SourceContext] = None)
  extends SigmaException(message, source)

class CosterException(message: String, source: Option[SourceContext])
    extends SigmaException(message, source)

class InterpreterException(message: String, source: Option[SourceContext] = None)
  extends SigmaException(message, source)
