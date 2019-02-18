package sigmastate.lang.exceptions

import sigmastate.lang.SourceContext

final class ConstraintFailed(message: String, source: Option[SourceContext] = None)
  extends BuilderException(message, source)

final class ArithException(message: String, source: Option[SourceContext] = None)
  extends BuilderException(message, source)

