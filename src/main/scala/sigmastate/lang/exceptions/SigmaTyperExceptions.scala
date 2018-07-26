package sigmastate.lang.exceptions

final class InvalidBinaryOperationParameters(message: String, source: Option[SourceContext] = None)
  extends TyperException(message, source)

final class MethodNotFound(message: String, source: Option[SourceContext] = None)
  extends TyperException(message, source)
