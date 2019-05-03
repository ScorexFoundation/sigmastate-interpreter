package org.ergoplatform

import sigmastate.Values.SValue
import sigmastate.lang.exceptions.{InterpreterException, SerializerException}
import sigmastate.utxo.DeserializeContext

sealed trait RuleStatus
case object EnabledRule extends RuleStatus
case object DisabledRule extends RuleStatus
case class  ReplacedRule(newRuleId: Short) extends RuleStatus

final case class InvalidResult(errors: Seq[Throwable])

case class ValidationRule(
  id: Short,
  description: String,
  status: RuleStatus
) {
  protected def validate[T](vs: ValidationSettings, condition: => Boolean, error: => Throwable)(block: => T): T = {
    val status = vs.getStatus(this.id)
    status match {
      case Some(DisabledRule) => block
      case Some(EnabledRule) =>
        if (!condition) {
          throw error
        } else
          block
      case Some(r: ReplacedRule) =>
        throw new ReplacedRuleException(vs, this, r)
      case None =>
        throw new InterpreterException(
          s"ValidationRule $this not found in validation settings")
    }
  }
}
object ValidationRule {
  def error(msg: String): InvalidResult = InvalidResult(Seq(new SigmaSoftForkException(msg)))
}

class SigmaSoftForkException(message: String) extends Exception(message)

class ReplacedRuleException(vs: ValidationSettings, replacedRule: ValidationRule, replacement: ReplacedRule)
  extends SigmaSoftForkException(s"Rule ${replacedRule.id} was replaced with ${replacement.newRuleId}")

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

  val CheckDeserializedScriptType = new ValidationRule(1000,
    "Deserialized script should have expected type", EnabledRule) {
    def apply[T](vs: ValidationSettings, d: DeserializeContext[_], script: SValue)(block: => T): T = {
      validate(vs, d.tpe == script.tpe,
        new InterpreterException(s"Failed context deserialization of $d: \n" +
        s"expected deserialized script to have type ${d.tpe}; got ${script.tpe}"))(block)
    }
  }

  val CheckDeserializedScriptIsSigmaProp = new ValidationRule(1001,
    "Deserialized script should have SigmaProp type", EnabledRule) {
    def apply[T](vs: ValidationSettings, root: SValue)(block: => T): T = {
      validate(vs, root.tpe.isSigmaProp, new SerializerException(
        s"Failed deserialization, expected deserialized script to have type SigmaProp; got ${root.tpe}")
        )(block)
    }
  }

  val ruleSpecs: Seq[ValidationRule] = Seq(
    CheckDeserializedScriptType,
    CheckDeserializedScriptIsSigmaProp
  )

  /** Validation settings that correspond to the latest version of the ErgoScript. */
  val currentSettings: ValidationSettings = new MapValidationSettings(
    ruleSpecs.map(r => r.id -> r).toMap)

}
