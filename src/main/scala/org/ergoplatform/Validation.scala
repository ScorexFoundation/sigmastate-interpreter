package org.ergoplatform

class RuleStatus
case object EnabledRule extends RuleStatus
case object DisabledRule extends RuleStatus
case class  ReplacedRule(newRuleId: Short) extends RuleStatus

final case class InvalidResult(errors: Seq[Throwable])

case class ValidationRule(
  id: Short,
  errorReporter: String => InvalidResult,
  status: RuleStatus
)
object ValidationRule {
  def enabled(id: Short, errorReporter: String => InvalidResult): ValidationRule = {
    ValidationRule(id, errorReporter, EnabledRule)
  }
  def error(msg: String): InvalidResult = InvalidResult(Seq(new SigmaSoftForkException(msg)))
}

class SigmaSoftForkException(message: String) extends Exception(message)

/**
  * Configuration of validation.
  */
abstract class ValidationSettings {
  def get(id: Short): Option[ValidationRule]
  def getStatus(id: Short): Option[RuleStatus]
}

class MapValidationSettings(map: Map[Short, ValidationRule]) extends ValidationSettings {
  override def get(id: Short): Option[ValidationRule] = map.get(id)
  override def getStatus(id: Short): Option[RuleStatus] = map.get(id).map(_.status)
}

object ValidationRules {
  import org.ergoplatform.ValidationRule._

  val Rule1: Short = 1000

  lazy val rulesSpec: Seq[ValidationRule] = Seq(
    enabled(Rule1, s => error(s"Rule1. $s"))
  )

  lazy val initialSettings: ValidationSettings = new MapValidationSettings(
    rulesSpec.map(r => r.id -> r).toMap)

}
