package sigmastate.exceptions

import sigma.ast.SourceContext

final class InvalidBinaryOperationParameters(message: String, source: Option[SourceContext] = None)
  extends TyperException(message, source)

final class InvalidUnaryOperationParameters(message: String, source: Option[SourceContext] = None)
  extends TyperException(message, source)

final class MethodNotFound(message: String, source: Option[SourceContext] = None)
  extends TyperException(message, source)

final class NonApplicableMethod(message: String, source: Option[SourceContext] = None)
  extends TyperException(message, source)
