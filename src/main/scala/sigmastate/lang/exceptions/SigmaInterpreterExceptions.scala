package sigmastate.lang.exceptions

final class OptionUnwrapNone(message: String, source: Option[SourceContext] = None)
  extends InterpreterException(message, source)
