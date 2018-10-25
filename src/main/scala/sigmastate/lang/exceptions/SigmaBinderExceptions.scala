package sigmastate.lang.exceptions

final class InvalidArguments(message: String, source: Option[SourceContext] = None)
  extends BinderException(message, source)

final class InvalidTypeArguments(message: String, source: Option[SourceContext] = None)
  extends BinderException(message, source)
