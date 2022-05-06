package org.ergoplatform.validation

import java.nio.ByteBuffer
import java.util

import org.ergoplatform.SigmaConstants.MaxLoopLevelInCostFunction
import scorex.util.ByteArrayBuilder
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}
import scalan.util.Extensions.toUByte
import sigmastate.Values.{ErgoTree, IntValue, SValue, Value}
import sigmastate._
import sigmastate.eval.IRContext
import sigmastate.lang.exceptions._
import sigmastate.serialization.OpCodes.{OpCode, OpCodeExtra}
import sigmastate.serialization.TypeSerializer.embeddableIdToType
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utxo.DeserializeContext
import sigmastate.utils.Helpers._  // required for Scala 2.11
import scala.collection.mutable

/** Base class for different validation rules registered in ValidationRules.currentSettings.
  * Each rule is identified by `id` and have a description.
  * Validation logic is implemented by `apply` methods of derived classes.
  */
case class ValidationRule(
    id: Short,
    description: String
) extends SoftForkChecker {

  /** Whether the status of this rule was checked on first invocation. */
  private var _checked: Boolean = false

  /** Check the rule is registered and enabled.
    * Since it is easy to forget to register new rule, we need to do this check.
    * But because it is hotspot, we do this check only once for each rule.
    * HOTSPOT: executed on every typeCode and opCode during script deserialization
    */
  @inline protected final def checkRule(): Unit = {
    if (!_checked) {
      if (ValidationRules.currentSettings.getStatus(this.id).isEmpty) {
        throw new SigmaException(s"ValidationRule $this not found in validation settings")
      }
      _checked = true  // prevent this check on every call (only first call is checked)
    }
    // upon successful return we know the rule is registered with EnabledRule status
  }

  /** Throws ValidationException with the given cause and args.
    * Should be used in all validation rules to unify ValidationException instances
    * which can be thrown (to simplify handling).
    */
  def throwValidationException(cause: Throwable, args: Seq[Any]): Nothing = {
    if (cause.isInstanceOf[ValidationException]) {
      throw cause
    }
    else {
      throw ValidationException(s"Validation failed on $this with args $args", this, args, Option(cause))
    }
  }

}

/** Base class for all exceptions which may be thrown by validation rules.
  * Instances of this class are used as messages to communicate soft-fork information,
  * from the context where the soft-fork condition is detected (such as in ValidationRules),
  * up the stack to the point where it is clear how to handle it.
  * Some messages of this kind are not handled, in which case a new Exception is thrown
  * and this instance should be attached as a `cause` parameter.
  *
  * This exception should typically always come with Some(cause). As result, stack trace is not
  * filled in when this instance is create. The `cause` parameter should be examined for the source
  * of the exception.
  */
case class ValidationException(message: String, rule: ValidationRule, args: Seq[Any], cause: Option[Throwable] = None)
    extends Exception(message, cause.orNull) {
  override def fillInStackTrace(): Throwable = this  // to avoid spending time on recording stack trace
}

/** All validation rules which are used to check soft-forkable conditions. Each validation
  * rule throws a [[org.ergoplatform.validation.ValidationException]]. Each
  * ValidationException can be caught and handled with respect to
  * [[SigmaValidationSettings]], which can be changed by miners via voting.
  * Thus, the behavior of the rules can be overridden without breaking consensus.
  */
object ValidationRules {
  /** The id of the first validation rule. Can be used as the beginning of the rules id range. */
  val FirstRuleId = 1000.toShort

  object CheckDeserializedScriptType extends ValidationRule(FirstRuleId,
    "Deserialized script should have expected type") {
    final def apply[T](d: DeserializeContext[_], script: SValue): Unit = {
      checkRule()
      if (d.tpe != script.tpe) {
        throwValidationException(
          new InterpreterException(s"Failed context deserialization of $d: \n" +
              s"expected deserialized script to have type ${d.tpe}; got ${script.tpe}"),
          Array[Any](d, script))
      }
    }
  }

  object CheckDeserializedScriptIsSigmaProp extends ValidationRule(1001,
    "Deserialized script should have SigmaProp type") {
    final def apply[T](root: SValue): Unit = {
      checkRule()
      if (!root.tpe.isSigmaProp) {
        throwValidationException(
          new SerializerException(s"Failed deserialization, expected deserialized script to have type SigmaProp; got ${root.tpe}"),
          Array(root))
      }
    }
  }

  object CheckValidOpCode extends ValidationRule(1002,
    "Check the opcode is supported by registered serializer or is added via soft-fork")
    with SoftForkWhenCodeAdded {
    final def apply[T](ser: ValueSerializer[_], opCode: OpCode): Unit = {
      checkRule()
      if (ser == null || ser.opCode != opCode) {
        throwValidationException(
          new InvalidOpCode(s"Cannot find serializer for Value with opCode = LastConstantCode + ${toUByte(opCode) - OpCodes.LastConstantCode}"),
          Array(opCode))
      }
    }
  }

  object CheckIsSupportedIndexExpression extends ValidationRule(1003,
    "Check the index expression for accessing collection element is supported.") {
    final def apply[Ctx <: IRContext, T](ctx: Ctx)(coll: Value[SCollection[_]], i: IntValue, iSym: ctx.Ref[Int]): Unit = {
      checkRule()
      if (!ctx.isSupportedIndexExpression(iSym)) {
        // TODO consensus: cover with tests
        throwValidationException(
          new SigmaException(s"Unsupported index expression $i when accessing collection $coll", i.sourceContext.toOption),
          Array(coll, i))
      }
    }
  }

  object CheckCostFunc extends ValidationRule(1004,
    "Cost function should contain only operations from specified list.") {
    final def apply[Ctx <: IRContext, T](ctx: Ctx)(costF: ctx.Ref[Any => Int]): Unit = {
      checkRule()
      val verification = ctx.verifyCostFunc(ctx.asRep[Any => Int](costF))
      if (!verification.isSuccess) {
        throwValidationException(verification.toEither.left.get, Array(costF))
      }
    }
  }

  object CheckCalcFunc extends ValidationRule(1005,
    "If SigmaProp.isProven method calls exists in the given function,\n then it is the last operation") {
    final def apply[Ctx <: IRContext, T](ctx: Ctx)(calcF: ctx.Ref[ctx.Context => Any]): Unit = {
      checkRule()
      val verification = ctx.verifyIsValid(calcF)
      if (!verification.isSuccess) {
        throwValidationException(verification.toEither.left.get, Array(calcF))
      }
    }
  }

  object CheckTupleType extends ValidationRule(1006,
    "Supported tuple type.") with SoftForkWhenReplaced {
    final def apply[Ctx <: IRContext, T](ctx: Ctx)(e: ctx.Elem[_]): Unit = {
      checkRule()
      val condition = e match {
        case _: ctx.PairElem[_,_] => true
        case _ => false
      }
      if (!condition) {
        throwValidationException(new SigmaException(s"Invalid tuple type $e"), Array[ctx.Elem[_]](e))
      }
    }
  }

  object CheckPrimitiveTypeCode extends ValidationRule(1007,
    "Check the primitive type code is supported or is added via soft-fork")
      with SoftForkWhenCodeAdded {
    final def apply[T](code: Byte): Unit = {
      checkRule()
      val ucode = toUByte(code)
      if (ucode <= 0 || ucode >= embeddableIdToType.length) {
        throwValidationException(
          new SerializerException(s"Cannot deserialize primitive type with code $ucode"),
          Array(code))
      }
    }
  }

  object CheckTypeCode extends ValidationRule(1008,
    "Check the non-primitive type code is supported or is added via soft-fork")
      with SoftForkWhenCodeAdded {
    final def apply[T](typeCode: Byte): Unit = {
      checkRule()
      val ucode = toUByte(typeCode)
      if (ucode > toUByte(SGlobal.typeCode)) {
        throwValidationException(
          new SerializerException(s"Cannot deserialize the non-primitive type with code $ucode"),
          Array(typeCode))
      }
    }
  }

  object CheckSerializableTypeCode extends ValidationRule(1009,
    "Check the data values of the type (given by type code) can be serialized")
      with SoftForkWhenReplaced {

    /** Creates an exception which is used as a cause when throwing a ValidationException. */
    def throwValidationException(typeCode: Byte): Nothing = {
      val ex = new SerializerException(
        s"Data value of the type with the code ${toUByte(typeCode)} cannot be deserialized.")
      throwValidationException(ex, Array(typeCode))
    }

    final def apply[T](typeCode: Byte): Unit = {
      checkRule()
      val ucode = toUByte(typeCode)
      if (typeCode == SOption.OptionTypeCode || ucode > toUByte(OpCodes.LastDataType)) {
        // the `typeCode == SOption.OptionTypeCode` condition is added in v5.0 and we
        // throw ValidationException for Option type as well in order to be able to
        // interpret it as soft-fork condition.
        // This will allow to add Option serialization in DataSerializer via v6.0 soft-fork.
        // This is in contrast to v4.x of this rule where Option type is not checked and
        // ordinary SigmaSerializer exception is thrown by the fallback case of DataSerializer.
        // This change is consensus-safe as v4.x and v5.x nodes will both throw exceptions
        // (albeit different ones) while attempting deserialization of an Option value.
        throwValidationException(typeCode)
      }
    }
  }

  object CheckTypeWithMethods extends ValidationRule(1010,
    "Check the type (given by type code) supports methods")
      with SoftForkWhenCodeAdded {
    final def apply[T](typeCode: Byte, cond: Boolean): Unit = {
      checkRule()
      val ucode = toUByte(typeCode)
      if (!cond) {
        throwValidationException(
          new SerializerException(s"Type with code $ucode doesn't support methods."),
          Array(typeCode))
      }
    }
  }

  object CheckAndGetMethod extends ValidationRule(1011,
    "Check the type has the declared method.") {
    final def apply[T](objType: STypeCompanion, methodId: Byte): SMethod = {
      checkRule()
      val methodOpt = objType.getMethodById(methodId)
      if (methodOpt.isDefined) methodOpt.get
      else {
        throwValidationException(
          new SerializerException(s"The method with code $methodId doesn't declared in the type $objType."),
          Array(objType, methodId))
      }
    }

    override def isSoftFork(vs: SigmaValidationSettings,
                            ruleId: Short,
                            status: RuleStatus,
                            args: Seq[Any]): Boolean = (status, args) match {
      case (ChangedRule(newValue), Seq(objType: STypeCompanion, methodId: Byte)) =>
        val key = Array(objType.typeId, methodId)
        newValue.grouped(2).exists(util.Arrays.equals(_, key))
      case _ => false
    }
  }

  object CheckHeaderSizeBit extends ValidationRule(1012,
    "For version greater then 0, size bit should be set.") with SoftForkWhenReplaced {
    final def apply(header: Byte): Unit = {
      checkRule()
      val version = ErgoTree.getVersion(header)
      if (version != 0 && !ErgoTree.hasSize(header)) {
        throwValidationException(
          new SigmaException(s"Invalid ErgoTreeHeader $header, size bit is expected for version $version"),
          Array(header))
      }
    }
  }

  /** For CheckCostFuncOperation we use 1-511 range op codes. Thus
   * `ChangedRule.newValue` should be parsed as a sequence of `getUShort`
   * values and then the exOpCode should be checked against that parsed
   * sequence.
   * Note, we don't need to store a number of items in a sequence,
   * because at the time of parsing we may assume that `ChangedRule.newValue`
   * has correct length, so we just parse it until end of bytes (of cause
   * checking consistency). */
  object CheckCostFuncOperation extends ValidationRule(1013,
    "Check the opcode is allowed in cost function") with SoftForkWhenCodeAdded {
    final def apply[Ctx <: IRContext, T](ctx: Ctx)(opCode: OpCodeExtra): Unit = {
      checkRule()
      if (!ctx.isAllowedOpCodeInCosting(opCode)) {
        throwValidationException(
          new CosterException(s"Not allowed opCode $opCode in cost function", None),
          Array(opCode))
      }
    }

    override def isSoftFork(vs: SigmaValidationSettings,
                            ruleId: Short,
                            status: RuleStatus, args: Seq[Any]): Boolean = (status, args) match {
      case (ChangedRule(newValue), Seq(code: Short)) =>
        decodeVLQUShort(newValue).contains(code)
      case _ => false
    }

    def encodeVLQUShort(opCodes: Seq[OpCodeExtra]): Array[Byte] = {
      val w = new VLQByteBufferWriter(new ByteArrayBuilder())
      opCodes.foreach(w.putUShort(_))
      w.toBytes
    }

    def decodeVLQUShort(bytes: Array[Byte]): Seq[OpCodeExtra] = {
      val r = new VLQByteBufferReader(ByteBuffer.wrap(bytes))
      val builder = mutable.ArrayBuilder.make[OpCodeExtra]()
      while(r.remaining > 0) {
        builder += OpCodeExtra @@ r.getUShort().toShort
      }
      builder.result()
    }
  }

  /** Introduced in v5.0, this rule it is used in creation of ValidationExceptions, which
    * in turn can be checked for soft-fork condition using `this.isSoftFork`. Thus, this
    * rule can be replaced with a new rule and the limit can be increased.
    */
  object CheckPositionLimit extends ValidationRule(1014,
    "Check that the Reader has not exceeded the position limit.") with SoftForkWhenReplaced {

    /** Wraps the given cause into [[ValidationException]] and throws it. */
    def throwValidationException(cause: ReaderPositionLimitExceeded): Nothing = {
      throwValidationException(cause, args = Nil)
    }

    /** Throws a [[ValidationException]] with the given parameters. */
    def throwValidationException(position: Int, positionLimit: Int): Nothing = {
      throwValidationException(
        new ReaderPositionLimitExceeded(
          s"SigmaByteReader position limit $positionLimit is reached at position $position",
          position, positionLimit))
    }

    final def apply(position: Int, positionLimit: Int): Unit = {
      checkRule()
      if (position > positionLimit) {
        throwValidationException(position, positionLimit)
      }
    }
  }

  object CheckLoopLevelInCostFunction extends ValidationRule(1015,
    "Check that loop level is not exceeded.") with SoftForkWhenReplaced {
    final def apply(level: Int): Unit = {
      checkRule()
      val max = MaxLoopLevelInCostFunction.value
      if (level > max) {
        throwValidationException(
          new CosterException(s"The loop level $level exceeds maximum $max", None),
          Array(level))
      }
    }
  }

  val ruleSpecs: Seq[ValidationRule] = Seq(
    CheckDeserializedScriptType,
    CheckDeserializedScriptIsSigmaProp,
    CheckValidOpCode,
    CheckIsSupportedIndexExpression,
    CheckCostFunc,
    CheckCalcFunc,
    CheckTupleType,
    CheckPrimitiveTypeCode,
    CheckTypeCode,
    CheckSerializableTypeCode,
    CheckTypeWithMethods,
    CheckAndGetMethod,
    CheckHeaderSizeBit,
    CheckCostFuncOperation,
    CheckPositionLimit,
    CheckLoopLevelInCostFunction
  )

  /** Validation settings that correspond to the current version of the ErgoScript implementation.
    * Different version of the code will have a different set of rules here.
    * This variable is globally available and can be use wherever checking of the rules is necessary.
    * This is immutable data structure, it can be augmented with RuleStates from block extension
    * sections of the blockchain, but that augmentation is only available in stateful context.
    */
  val currentSettings: SigmaValidationSettings = new MapSigmaValidationSettings({
    val map = ruleSpecs.map(r => r.id -> (r, EnabledRule)).toMap
    assert(map.size == ruleSpecs.size, s"Duplicate ruleIds ${ruleSpecs.groupBy(_.id).filter(g => g._2.length > 1)}")
    map
  })

  /** Executes the given `block` catching [[ValidationException]] and checking possible
    * soft-fork condition in the context of the given [[SigmaValidationSettings]].
    * If soft-fork condition is recognized the `whenSoftFork` is executed and its result
    * is returned.
    *
    * @param whenSoftFork executed when soft-fork condition is detected
    * @param block        block of code, which may throw [[ValidationException]]
    * @param vs           set of [[SigmaValidationSettings]] which can be used to recognize soft-fork conditions.
    * @return result of `block` if no ValidationException was thrown, or the result of
    * `whenSoftFork` if soft-fork condition is detected.
    * @throws ValidationException if soft-fork condition is not recognized by the given `vs`
    */
  def trySoftForkable[T](whenSoftFork: => T)(block: => T)(implicit vs: SigmaValidationSettings): T = {
    try block
    catch {
      case ve: ValidationException =>
        if (vs.isSoftFork(ve)) whenSoftFork
        else throw ve
    }
  }
}
