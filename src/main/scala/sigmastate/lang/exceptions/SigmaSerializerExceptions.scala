package sigmastate.lang.exceptions

final class InvalidTypePrefix(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)
