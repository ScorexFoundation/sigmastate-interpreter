package sigmastate.lang.exceptions

case class SourceContext(index: Int)

class SigmaException(val message: String, val source: Option[SourceContext] = None)
    extends Exception(message)

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
