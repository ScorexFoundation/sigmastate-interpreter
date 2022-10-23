package sigmastate.exceptions

import sigmastate.lang.SourceContext

final class ConstraintFailed(message: String, source: Option[SourceContext] = None)
  extends BuilderException(message, source)

