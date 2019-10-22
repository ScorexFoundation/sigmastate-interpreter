package sigmastate.lang.exceptions

import sigmastate.lang.SourceContext

final class OptionUnwrapNone(message: String, source: Option[SourceContext] = None)
  extends InterpreterException(message, source)


