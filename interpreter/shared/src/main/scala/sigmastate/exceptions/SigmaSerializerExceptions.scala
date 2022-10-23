package sigmastate.exceptions

/** Thrown by TypeSerializer when type prefix <= 0. */
final class InvalidTypePrefix(message: String, cause: Option[Throwable] = None)
  extends SerializerException(message, cause)

/** Thrown when the current reader position > positionLimit which is set in the Reader.
  * @see [[org.ergoplatform.validation.ValidationRules.CheckPositionLimit]]
  */
final class ReaderPositionLimitExceeded(
  message: String,
  val position: Int,
  val positionLimit: Int,
  cause: Option[Throwable] = None)
  extends SerializerException(message, cause)

/** Thrown when the current depth level > maxDepthLevel which is set in the Reader. */
final class DeserializeCallDepthExceeded(message: String, cause: Option[Throwable] = None)
  extends SerializerException(message, cause)

/** Thrown by [[org.ergoplatform.validation.ValidationRules.CheckValidOpCode]] validation rule. */
final class InvalidOpCode(message: String, cause: Option[Throwable] = None)
  extends SerializerException(message, cause)
