package org.ergoplatform.validation

/** Base trait for rule status information. */
sealed trait RuleStatus

/** This is a default status of a rule which is registered in the table
  * and not yet altered by soft-forks.
  */
case object EnabledRule extends RuleStatus

/** This is a status of a rule which is disabled in current version
  * and not yet altered by soft-forks.
  * The rule can be disabled via block extensions and voting process.
  */
case object DisabledRule extends RuleStatus

/** The status of the rule which is replaced by a new rule via soft-fork extensions.
  * This is similar to DisabledRule, but in addition require the new rule to be enabled
  * at the same time (i.e. atomically)
  * @see `ValidationSettings.isSoftFork`
  * @param newRuleId  id of a new rule which replaces the rule marked with this status
  */
case class  ReplacedRule(newRuleId: Short) extends RuleStatus

/** The status of the rule whose parameters are changed via soft-fork extensions.
  * The same rule can be changed many times via voting.
  * @param newValue  new value of block extension value with key == rule.id
  */
case class ChangedRule(newValue: Array[Byte]) extends RuleStatus
