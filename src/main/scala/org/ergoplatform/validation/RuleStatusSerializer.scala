package org.ergoplatform.validation

import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}

object RuleStatusSerializer extends SigmaSerializer[RuleStatus, RuleStatus] {
  val DisabledRuleCode = 1
  val ReplacedRuleCode = 2
  val ChangedRuleCode = 3
  val FirstRuleId = 1000

  override def serialize(obj: RuleStatus, w: SigmaByteWriter): Unit = {
  }

  override def parse(r: SigmaByteReader): RuleStatus = {
    val numBytes = r.getUShort() // read number of bytes occupied by status data
    val statusType = r.getByte()
    statusType match {
      case DisabledRuleCode =>
        DisabledRule  // the rule is explicitly disabled
      case ReplacedRuleCode =>
        val newRule = (r.getUShort() + FirstRuleId).toShort // store small offsets using single byte
        ReplacedRule(newRule) // the rule is disabled, but we also have info about new rule
      case ChangedRuleCode =>
        val bytes = r.getBytes(numBytes - 1) // value bytes except statusType
        ChangedRule(bytes)
      case _ =>
        r.position += numBytes - 1 // skip status bytes which we don't understand
        ReplacedRule(0)  // unrecognized status code, the old code should process it as soft-fork
    }
  }
}

