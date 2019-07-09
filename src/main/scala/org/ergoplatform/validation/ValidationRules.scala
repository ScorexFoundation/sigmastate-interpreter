package org.ergoplatform.validation

import java.nio.ByteBuffer
import java.util

import org.ergoplatform.ErgoConstants.MaxLoopLevelInCostFunction
import scorex.util.ByteArrayBuilder
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}
import sigma.util.Extensions.ByteOps
import sigmastate.eval.IRContext
import sigmastate.serialization.OpCodes.{OpCodeExtra, OpCode}
import sigmastate.Values.{Value, ErgoTree, SValue, IntValue}
import sigmastate.serialization.{ValueSerializer, OpCodes}
import sigmastate.utxo.DeserializeContext
import sigmastate.lang.exceptions._
import sigmastate.serialization.TypeSerializer.embeddableIdToType
import sigmastate._

import scala.collection.mutable

/** Base class for different validation rules registered in ValidationRules.currentSettings.
  * Each rule is identified by `id` and have a description.
  * Validation logic is implemented by `apply` methods of derived classes.
  */
case class ValidationRule(
    id: Short,
    description: String
) extends SoftForkChecker {
  private var _checked: Boolean = false

  /** Generic helper method to implement validation rules.
    * It executes the given `block` only when this rule is disabled of `condition` is satisfied.
    * Should be used in derived classes to implemented validation logic.
    *
    * @tparam  T  type of the result produced by `block`
    * @param   condition  executes condition to be checked and returns its result
    * @param   cause      executed only when condition returns false, attached as `cause` parameter when Validation exception
    * @param   args       parameters which should be attached to ValidationException
    * @param   block      executed only when condition returns true, its result become a result of `validate` call.
    * @return    result produced by the `block` if condition is true
    * @throws    SigmaException if this rule is not found in ValidationRules.currentSettings
    * @throws    ValidationException if the `condition` is not true.
    *
    * @see ValidationRules
    */
  protected def validate[T](
        condition: => Boolean,
        cause: => Throwable, args: Seq[Any], block: => T): T = {
    if (!_checked) {
      if (ValidationRules.currentSettings.getStatus(this.id).isEmpty)
        throw new SigmaException(s"ValidationRule $this not found in validation settings")
      _checked = true  // prevent this check on every call (only first call is checked)
    }
    // here we assume the rule is registered with EnabledRule status
    if (condition) {
      block
    }
    else if (cause.isInstanceOf[ValidationException]) {
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

object ValidationRules {
  /** The id of the first validation rule. Can be used as the beginning of the rules id range. */
  val FirstRuleId = 1000.toShort

  object CheckDeserializedScriptType extends ValidationRule(FirstRuleId,
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
      def args = Seq(calcF)
      lazy val verification = ctx.verifyIsProven(calcF)
      validate(verification.isSuccess,
        verification.toEither.left.get,
        args, block)
    }
  }

  object CheckTupleType extends ValidationRule(1006,
    "Supported tuple type.") with SoftForkWhenReplaced {
    def apply[Ctx <: IRContext, T](ctx: Ctx)(e: ctx.Elem[_])(block: => T): T = {
      def msg = s"Invalid tuple type $e"
      lazy val condition = e match {
        case _: ctx.PairElem[_,_] => true
        case _ => false
      }
      validate(condition, new SigmaException(msg), Seq[ctx.Elem[_]](e), block)
    }
  }

  object CheckPrimitiveTypeCode extends ValidationRule(1007,
    "Check the primitive type code is supported or is added via soft-fork")
      with SoftForkWhenCodeAdded {
    def apply[T](code: Byte)(block: => T): T = {
      val ucode = code.toUByte
      validate(ucode > 0 && ucode < embeddableIdToType.length,
        new SerializerException(s"Cannot deserialize primitive type with code $ucode"), Array(code), block)
    }
  }

  object CheckTypeCode extends ValidationRule(1008,
    "Check the non-primitive type code is supported or is added via soft-fork")
      with SoftForkWhenCodeAdded {
    def apply[T](typeCode: Byte)(block: => T): T = {
      val ucode = typeCode.toUByte
      def msg = s"Cannot deserialize the non-primitive type with code $ucode"
      validate(ucode <= SGlobal.typeCode.toUByte, new SerializerException(msg), Seq(typeCode), block)
    }
  }

  object CheckSerializableTypeCode extends ValidationRule(1009,
    "Check the data values of the type (given by type code) can be serialized")
      with SoftForkWhenReplaced {
    def apply[T](typeCode: Byte)(block: => T): T = {
      val ucode = typeCode.toUByte
      def msg = s"Data value of the type with the code $ucode cannot be deserialized."
      validate(ucode <= OpCodes.LastDataType.toUByte, new SerializerException(msg), Seq(typeCode), block)
    }
  }

  object CheckTypeWithMethods extends ValidationRule(1010,
    "Check the type (given by type code) supports methods")
      with SoftForkWhenCodeAdded {
    def apply[T](typeCode: Byte, cond: => Boolean)(block: => T): T = {
      val ucode = typeCode.toUByte
      def msg = s"Type with code $ucode doesn't support methods."
      validate(cond, new SerializerException(msg), Seq(typeCode), block)
    }
  }

  object CheckAndGetMethod extends ValidationRule(1011,
    "Check the type has the declared method.") {
    def apply[T](objType: STypeCompanion, methodId: Byte)(block: SMethod => T): T = {
      def msg = s"The method with code $methodId doesn't declared in the type $objType."
      lazy val methodOpt = objType.getMethodById(methodId)
      validate(methodOpt.isDefined, new SerializerException(msg), Seq(objType, methodId), block(methodOpt.get))
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
    def apply(header: Byte): Unit = {
      validate(
        ErgoTree.getVersion(header) == 0 || ErgoTree.hasSize(header),
        new SigmaException(s"Invalid ErgoTreeHeader $header, size bit is expected"), Seq(header), {})
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
    def apply[Ctx <: IRContext, T](ctx: Ctx)(opCode: OpCodeExtra)(block: => T): T = {
      def msg = s"Not allowed opCode $opCode in cost function"
      def args = Seq(opCode)
      validate(ctx.isAllowedOpCodeInCosting(opCode), new CosterException(msg, None), args, block)
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

  /** This rule doesn't have it's own validation logic, however it is used in creation of
    * ValidationExceptions, which in turn can be checked for soft-fork condition using `this.isSoftFork`. */
  object CheckPositionLimit extends ValidationRule(1014,
    "Check that the Reader has not exceeded the position limit.") with SoftForkWhenReplaced {
  }

  object CheckLoopLevelInCostFunction extends ValidationRule(1015,
    "Check that loop level is not exceeded.") with SoftForkWhenReplaced {
    def apply(level: Int): Unit = {
      val max = MaxLoopLevelInCostFunction.value
      validate(level <= max,
      new CosterException(s"The loop level $level exceeds maximum $max", None), Seq(level), {})
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

  def trySoftForkable[T](whenSoftFork: => T)(block: => T)(implicit vs: SigmaValidationSettings): T = {
    try block
    catch {
      case ve: ValidationException =>
        if (vs.isSoftFork(ve)) whenSoftFork
        else throw ve
    }
  }
}
