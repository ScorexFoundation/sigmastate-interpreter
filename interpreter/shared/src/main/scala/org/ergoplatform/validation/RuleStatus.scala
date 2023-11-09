import java.util
import scorex.utils.Ints

sealed trait RuleStatus {
  def statusCode: Byte
}

object RuleStatus {
  val EnabledRuleCode: Byte = 1.toByte
  val DisabledRuleCode: Byte = 2.toByte
  val ReplacedRuleCode: Byte = 3.toByte
  val ChangedRuleCode: Byte = 4.toByte
}

case object EnabledRule extends RuleStatus {
  val statusCode: Byte = RuleStatus.EnabledRuleCode
}

case object DisabledRule extends RuleStatus {
  val statusCode: Byte = RuleStatus.DisabledRuleCode
}

case class ReplacedRule(newRuleId: Short) extends RuleStatus {
  val statusCode: Byte = RuleStatus.ReplacedRuleCode
}

case class ChangedRule(newValue: Array[Byte]) extends RuleStatus {
  val statusCode: Byte = RuleStatus.ChangedRuleCode

  override def hashCode(): Int = Ints.fromByteArray(newValue)

  override def canEqual(that: Any): Boolean = that.isInstanceOf[ChangedRule]

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case that: ChangedRule => util.Arrays.equals(newValue, that.newValue)
  })
}
