package sigmastate.lang.exceptions

import sigmastate.lang.SourceContext

final class InvalidTypePrefix(message: String, source: Option[SourceContext] = None, cause: Option[Throwable] = None)
  extends SerializerException(message, source, cause)

final class InputSizeLimitExceeded(message: String, source: Option[SourceContext] = None, cause: Option[Throwable] = None)
  extends SerializerException(message, source, cause)

final class DeserializeCallDepthExceeded(message: String, source: Option[SourceContext] = None, cause: Option[Throwable] = None)
  extends SerializerException(message, source, cause)

final class InvalidOpCode(message: String, source: Option[SourceContext] = None, cause: Option[Throwable] = None)
  extends SerializerException(message, source, cause)
