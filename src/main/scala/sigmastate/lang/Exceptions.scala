package sigmastate.lang

case class SourceContext(index: Int)

class SigmaException(val message: String, val source: Option[SourceContext])
    extends Exception(message)

class BinderException(message: String, source: Option[SourceContext])
    extends SigmaException(message, source)

class TyperException(message: String, source: Option[SourceContext])
    extends SigmaException(message, source)

class SpecializerException(message: String, source: Option[SourceContext])
    extends SigmaException(message, source)

