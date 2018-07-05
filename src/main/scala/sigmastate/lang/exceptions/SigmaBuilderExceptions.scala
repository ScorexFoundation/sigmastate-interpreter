package sigmastate.lang.exceptions

final class ConstraintFailed(message: String, source: Option[SourceContext] = None)
  extends BuilderException(message, source)

