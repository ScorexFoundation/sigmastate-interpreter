package org.ergoplatform

import sigmastate.Values.{Value, SValue, IntValue}
import sigmastate.lang.exceptions._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.serialization.{ValueSerializer, OpCodes}
import sigmastate.utxo.DeserializeContext
import sigma.util.Extensions.ByteOps
import sigmastate.eval.IRContext
import sigmastate.serialization.TypeSerializer.{CheckPrimitiveTypeCode, CheckTypeCode}
import sigmastate.{SCollection, SType}

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

/** Interface implemented by objects cable of checking soft-fork conditions. */
trait SoftForkChecker {
  /** Check soft-fork condition.
    * @param vs       ValidationSettings actualized from blockchain extension sections
    * @param ruleId   id of the rule which raised ValidationException
    * @param status   status of the rule in the blockchain (agreed upon via voting)
    * @param args     arguments of Validation rule with which the rule has risen the exception
    * @return         true if `args` and `status` can be interpreted as valid soft-fork condition.
    */
  def isSoftFork(vs: ValidationSettings, ruleId: Short, status: RuleStatus, args: Seq[Any]): Boolean = false
}

trait SoftForkWhenReplaced extends SoftForkChecker {
  override def isSoftFork(vs: ValidationSettings,
      ruleId: Short,
      status: RuleStatus,
      args: Seq[Any]): Boolean = (status, args) match {
    case (ReplacedRule(_), _) => true
    case _ => false
  }
}

trait SoftForkWhenCodeAdded extends SoftForkChecker {
  override def isSoftFork(vs: ValidationSettings,
      ruleId: Short,
      status: RuleStatus,
      args: Seq[Any]): Boolean = (status, args) match {
    case (ChangedRule(newValue), Seq(code: Byte)) => newValue.contains(code)
    case _ => false
  }
}

/** Base class for different validation rules registered in ValidationRules.currentSettings.
  * Each rule is identified by `id` and have a description.
  * Validation logic is implemented by `apply` methods of derived classes.
  */
case class ValidationRule(
  id: Short,
  description: String
) extends SoftForkChecker {
  /** Can be used in derived classes to implemented validation logic. */
  protected def validate[T](
        condition: => Boolean,
        cause: => Throwable, args: Seq[Any], block: => T): T = {
    val status = ValidationRules.currentSettings.getStatus(this.id)
    status match {
      case None =>
        throw new InterpreterException(s"ValidationRule $this not found in validation settings")
      case Some(DisabledRule) =>
        block  // if the rule is disabled we still need to execute the block of code
      case Some(status) =>
        if (condition)
          block
        else {
          throw new ValidationException(s"Validation failed on $this with args $args", this, args, Option(cause))
        }
    }
  }
}

/** Base class for all exception which may be thrown by validation rules. */
case class ValidationException(message: String, rule: ValidationRule, args: Seq[Any], cause: Option[Throwable] = None)
  extends Exception(message, cause.orNull)

/** Instances of this class are used as messages to communicate soft-fork information,
  * from the context where the soft-fork condition is detected (such as in ValidationRules),
  * up the stack to the point where it is clear how to handle it.
  * Some messages of this kind are not handled, it which case a new Exception is thrown
  * and this instance should be attached as `cause` parameter. */
class SoftForkException(message: String) extends Exception(message) {
  /** This override is required as an optimization to avoid spending time on recording stack trace.
    * This makes throwing exceptions almost as fast as usual return of a method.
    */
  override def fillInStackTrace(): Throwable = this
}

case class ReplacedRuleException(vs: ValidationSettings, replacedRule: ValidationRule, replacement: ReplacedRule)
  extends SoftForkException(s"Rule ${replacedRule.id} was replaced with ${replacement.newRuleId}")

case class ChangedRuleException(vs: ValidationSettings, changedRule: ValidationRule, change: ChangedRule)
  extends SoftForkException(s"Rule ${changedRule.id} was changed with ${change}")

/**
  * Configuration of validation.
  * A new `ValidationRule` should be implemented as an `object` in the code.
  * It then should be registered in `ValidationRules.currentSettings`.
  * Added to `currentSettings` the rule has EnabledRule status by default,
  * but only in a new version of the code. The value `currentSettings` represents
  * validation settings of the current version of the code.
  * Old versions of the code don't have access to these new rules.
  * However, the behavior of old rules can be altered by changing their parameters
  * in block extensions section via voting.
  *
  * These parameter changes are represented in ValidationSettings as RuleStatus.
  * Each descendant class represent a particular change in rule parameters.
  * Rule ids are used as the keys of the values in the block extension section.
  * RuleStatus instances are deserialized from block extension values that correspond
  * to rule ids.
  *
  * Each rule has associated check of soft-fork condition by implementing `isSoftFork`
  * method. If `isSoftFork` returns true, then ValidationException raised by the rule
  * is interpreted as *soft-fork condition*. Depending on the use case, soft-fork condition
  * allows some operations to succeed which otherwise would fail due to ValidationException
  * raised in an old version of code.
  * One notable use case is Box.ergoTree validation in which old code can skip
  * ValidationExceptions under soft-fork condition (i.e. when isSoftFork returns true)
  */
abstract class ValidationSettings {
  def get(id: Short): Option[(ValidationRule, RuleStatus)]
  def getStatus(id: Short): Option[RuleStatus]
  def updated(id: Short, newStatus: RuleStatus): ValidationSettings
  def isSoftFork(ve: ValidationException): Boolean = isSoftFork(ve.rule.id, ve)
  def isSoftFork(ruleId: Short, ve: ValidationException): Boolean = {
    val infoOpt = get(ruleId)
    infoOpt match {
      case Some((_, ReplacedRule(newRuleId))) => true
      case Some((rule, status)) => rule.isSoftFork(this, rule.id, status, ve.args)
      case None => false
    }
  }
}

sealed class MapValidationSettings(map: Map[Short, (ValidationRule, RuleStatus)]) extends ValidationSettings {
  override def get(id: Short): Option[(ValidationRule, RuleStatus)] = map.get(id)
  override def getStatus(id: Short): Option[RuleStatus] = map.get(id).map(_._2)
  override def updated(id: Short, newStatus: RuleStatus): MapValidationSettings = {
    val (rule,_) = map(id)
    new MapValidationSettings(map.updated(id, (rule, newStatus)))
  }
}

object ValidationRules {

  object CheckDeserializedScriptType extends ValidationRule(1000,
    "Deserialized script should have expected type") {
    def apply[T](d: DeserializeContext[_], script: SValue)(block: => T): T =
      validate(d.tpe == script.tpe,
        new InterpreterException(s"Failed context deserialization of $d: \n" +
        s"expected deserialized script to have type ${d.tpe}; got ${script.tpe}"),
        Seq[Any](d, script), block
      )
  }

  object CheckDeserializedScriptIsSigmaProp extends ValidationRule(1001,
    "Deserialized script should have SigmaProp type") {
    def apply[T](root: SValue)(block: => T): T =
      validate(root.tpe.isSigmaProp,
        new SerializerException(s"Failed deserialization, expected deserialized script to have type SigmaProp; got ${root.tpe}"),
        Seq(root), block
      )
  }

  object CheckValidOpCode extends ValidationRule(1002,
    "Check the opcode is supported by registered serializer or is added via soft-fork")
    with SoftForkWhenCodeAdded {
    def apply[T](ser: ValueSerializer[_], opCode: OpCode)(block: => T): T = {
      def msg = s"Cannot find serializer for Value with opCode = LastConstantCode + ${opCode.toUByte - OpCodes.LastConstantCode}"
      def args = Seq(opCode)
      validate(ser != null && ser.opCode == opCode, new InvalidOpCode(msg), args, block)
    }
  }

  object CheckIsSupportedIndexExpression extends ValidationRule(1003,
    "Check the index expression for accessing collection element is supported.") {
    def apply[Ctx <: IRContext, T](ctx: Ctx)(coll: Value[SCollection[_]], i: IntValue, iSym: ctx.Rep[Int])(block: => T): T = {
      def msg = s"Unsupported index expression $i when accessing collection $coll"
      def args = Seq(coll, i)
      validate(ctx.isSupportedIndexExpression(iSym),
        new SigmaException(msg, i.sourceContext.toOption),
        args, block)
    }
  }

  object CheckCostFunc extends ValidationRule(1004,
    "Cost function should contain only operations from specified list.") {
    def apply[Ctx <: IRContext, T](ctx: Ctx)(costF: ctx.Rep[Any => Int])(block: => T): T = {
      def msg = s"Invalid cost function $costF"
      def args = Seq(costF)
      lazy val verification = ctx.verifyCostFunc(ctx.asRep[Any => Int](costF))
      validate(verification.isSuccess,
        verification.toEither.left.get,
        args, block)
    }
  }

  object CheckCalcFunc extends ValidationRule(1005,
    "If SigmaProp.isProven method calls exists in the given function,\n then it is the last operation") {
    def apply[Ctx <: IRContext, T](ctx: Ctx)(calcF: ctx.Rep[ctx.Context => Any])(block: => T): T = {
      def msg = s"Invalid calc function $calcF"
      def args = Seq(calcF)
      lazy val verification = ctx.verifyIsProven(calcF)
      validate(verification.isSuccess,
        verification.toEither.left.get,
        args, block)
    }
  }

  object CheckCostWithContext extends ValidationRule(1006,
    "Contract execution cost in a given context is limited by given maximum value.") {
    def apply[Ctx <: IRContext, T](ctx: Ctx)
        (costingCtx: ctx.Context.SContext, exp: Value[SType],
            costF: ctx.Rep[((ctx.Context, (Int, ctx.Size[ctx.Context]))) => Int], maxCost: Long): Int = {
      def args = Seq(costingCtx, exp, costF, maxCost)
      lazy val estimatedCostTry = ctx.checkCostWithContext(costingCtx, exp, costF, maxCost)
      validate(estimatedCostTry.isSuccess,
        {
          val t = estimatedCostTry.toEither.left.get
          new CosterException(s"Script cannot be executed due to high cost $exp: ", exp.sourceContext.toList.headOption, Some(t))
        },
        args, estimatedCostTry.get)
    }
  }

  object CheckTupleType extends ValidationRule(1007,
    "Supported tuple type.") with SoftForkWhenReplaced {
    def apply[Ctx <: IRContext, T](ctx: Ctx)(e: ctx.Elem[_])(block: => T): T = {
      def msg = s"Invalid tuple type $e"
      lazy val condition = e match {
        case pe: ctx.PairElem[_,_] => true
        case _ => false
      }
      validate(condition, new SigmaException(msg), Seq[ctx.Elem[_]](e), block)
    }
  }

  val ruleSpecs: Seq[ValidationRule] = Seq(
    CheckDeserializedScriptType,
    CheckDeserializedScriptIsSigmaProp,
    CheckValidOpCode,
    CheckIsSupportedIndexExpression,
    CheckCostFunc,
    CheckCalcFunc,
    CheckCostWithContext,
    CheckTupleType,
    CheckPrimitiveTypeCode,
    CheckTypeCode,
  )

  /** Validation settings that correspond to the current version of the ErgoScript implementation.
    * Different version of the code will have a different set of rules here.
    * This variable is globally available and can be use wherever checking of the rules is necessary.
    * This is immutable data structure, it can be augmented with RuleStates from block extension
    * sections of the blockchain, but that augmentation is only available in stateful context.
    */
  val currentSettings: ValidationSettings = new MapValidationSettings({
    val map = ruleSpecs.map(r => r.id -> (r, EnabledRule)).toMap
    assert(map.size == ruleSpecs.size, s"Duplicate ruleIds ${ruleSpecs.groupBy(_.id).filter(g => g._2.length > 1)}")
    map
  })

  def trySoftForkable[T](whenSoftFork: => T)(block: => T)(implicit vs: ValidationSettings): T = {
    try block
    catch {
      case ve: ValidationException =>
        if (vs.isSoftFork(ve)) whenSoftFork
        else throw ve
    }
  }
}
