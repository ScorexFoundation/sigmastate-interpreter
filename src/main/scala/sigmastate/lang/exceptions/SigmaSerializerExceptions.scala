package sigmastate.lang.exceptions

import sigmastate.lang.SourceContext

final class InvalidTypePrefix(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)

final class InputSizeLimitExceeded(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)

final class TypeDeserializeCallDepthExceeded(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)

final class DeserializeCallDepthExceeded(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)

final class InvalidOpCode(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)
