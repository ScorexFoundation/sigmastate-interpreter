package sigmastate.lang.exceptions

final class InvalidTypePrefix(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)

final class InputSizeLimitExceeded(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)

final class TypeDeserializeCallDepthExceeded(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)

final class ValueDeserializeCallDepthExceeded(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)

final class InvalidOpCode(message: String, source: Option[SourceContext] = None)
  extends SerializerException(message, source)
