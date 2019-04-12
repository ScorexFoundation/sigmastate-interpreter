package sigmastate

import scorex.crypto.hash.{Sha256, Blake2b256, CryptographicHash32}
import sigmastate.SCollection.{SIntArray, SByteArray}
import sigmastate.SOption.SIntOption
import sigmastate.Values._
import sigmastate.basics.{SigmaProtocol, SigmaProtocolPrivateInput, SigmaProtocolCommonInput}
import sigmastate.serialization.OpCodes._
import sigmastate.serialization._
import sigmastate.utxo.Transformer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * AND conjunction for sigma propositions
  */
case class CAND(sigmaBooleans: Seq[SigmaBoolean]) extends SigmaBoolean {
  /** The same code is used for AND operation, but they belong to different type hierarchies. */
  override val opCode: OpCode = OpCodes.AndCode
}
object CAND {
  import TrivialProp._
  def normalized(items: Seq[SigmaBoolean]): SigmaBoolean = {
    require(items.nonEmpty)
    val res = new ArrayBuffer[SigmaBoolean]()
    for (x <- items) {
      x match {
        case FalseProp => return FalseProp
        case TrueProp => // skip
        case _ => res += x
      }
    }
    if (res.isEmpty) TrueProp
    else if (res.length == 1) res(0)
    else CAND(res)
  }
}

/**
  * OR disjunction for sigma propositions
  */
case class COR(sigmaBooleans: Seq[SigmaBoolean]) extends SigmaBoolean {
  /** The same code is also used for OR operation, but they belong to different type hierarchies. */
  override val opCode: OpCode = OpCodes.OrCode
}
object COR {
  import TrivialProp._
  def normalized(items: Seq[SigmaBoolean]): SigmaBoolean = {
    require(items.nonEmpty)
    val res = new ArrayBuffer[SigmaBoolean]()
    for (x <- items) {
      x match {
        case FalseProp => // skip
        case TrueProp => return TrueProp
        case _ => res += x
      }
    }
    if (res.isEmpty) FalseProp
    else if (res.length == 1) res(0)
    else COR(res)
  }
}

/**
  * THRESHOLD connector for sigma propositions
  */
case class CTHRESHOLD(k: Int, sigmaBooleans: Seq[SigmaBoolean]) extends SigmaBoolean {
  // Our polynomial arithmetic can take only byte inputs
  require(k >= 0 && k <= sigmaBooleans.length && sigmaBooleans.length <= 255)

  override val opCode: OpCode = OpCodes.AtLeastCode
}

trait SigmaProofOfKnowledgeTree[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP, _]]
  extends SigmaBoolean with SigmaProtocolCommonInput[SP]

/** Represents boolean values (true/false) in SigmaBoolean tree.
  * Participates in evaluation of CAND, COR, THRESHOLD connectives over SigmaBoolean values.
  * See CAND.normalized, COR.normalized and AtLeast.reduce. */
abstract class TrivialProp(val condition: Boolean) extends SigmaBoolean with Product1[Boolean] {
  override def _1: Boolean = condition
  override def canEqual(that: Any): Boolean = that != null && that.isInstanceOf[TrivialProp]
}
object TrivialProp {
  // NOTE: the corresponding unapply is missing because any implementation (even using Nullable)
  // will lead to Boolean boxing, which we want to avoid
  // So, instead of `case TrivialProp(b) => ... b ...` use more efficient
  // `case p: TrivialProp => ... p.condition ...

  def apply(b: Boolean): TrivialProp = if (b) TrueProp else FalseProp

  val FalseProp = new TrivialProp(false) {
    override val opCode: OpCode = OpCodes.TrivialPropFalseCode
    override def toString = "FalseProp"
  }
  val TrueProp = new TrivialProp(true) {
    override val opCode: OpCode = OpCodes.TrivialPropTrueCode
    override def toString = "TrueProp"
  }
}

/** Embedding of Boolean values to SigmaProp values. As an example, this operation allows boolean experesions
  * to be used as arguments of `atLeast(..., sigmaProp(boolExpr), ...)` operation.
  * During execution results to either `TrueProp` or `FalseProp` values of SigmaBoolean type.
  */
case class BoolToSigmaProp(value: BoolValue) extends SigmaPropValue {
  override def companion = BoolToSigmaProp
  def tpe = SSigmaProp
  val opType = SFunc(SBoolean, SSigmaProp)
}
object BoolToSigmaProp extends ValueCompanion {
  override val opCode: OpCode = OpCodes.BoolToSigmaPropCode
}

/** ErgoTree operation to create a new SigmaProp value representing public key
  * of discrete logarithm signature protocol. */
case class CreateProveDlog(value: Value[SGroupElement.type]) extends SigmaPropValue {
  override def companion = CreateProveDlog
  override def tpe = SSigmaProp
  override def opType = SFunc(SGroupElement, SSigmaProp)
}
object CreateProveDlog extends ValueCompanion {
  override val opCode: OpCode = OpCodes.ProveDlogCode
}

/** ErgoTree operation to create a new SigmaProp value representing public key
  * of discrete logarithm signature protocol. */
case class CreateAvlTree(operationFlags: ByteValue,
    digest: Value[SByteArray],
    keyLength: IntValue,
    valueLengthOpt: Value[SIntOption]) extends AvlTreeValue {
  override def companion = CreateAvlTree
  override def tpe = SAvlTree
  override def opType = CreateAvlTree.opType
}
object CreateAvlTree extends ValueCompanion {
  override val opCode: OpCode = OpCodes.AvlTreeCode
  val opType = SFunc(IndexedSeq(SByte, SByteArray, SInt, SIntOption), SAvlTree)
}

/** ErgoTree operation to create a new SigmaProp value representing public key
  * of Diffie Hellman signature protocol.
  * Common input: (g,h,u,v)*/
case class CreateProveDHTuple(gv: Value[SGroupElement.type],
    hv: Value[SGroupElement.type],
    uv: Value[SGroupElement.type],
    vv: Value[SGroupElement.type]) extends SigmaPropValue {
  override def companion = CreateProveDHTuple
  override def tpe = SSigmaProp
  override def opType = SFunc(IndexedSeq(SGroupElement, SGroupElement, SGroupElement, SGroupElement), SSigmaProp)
}
object CreateProveDHTuple extends ValueCompanion {
  override val opCode: OpCode = OpCodes.ProveDHTupleCode
}

trait SigmaTransformer[IV <: SigmaPropValue, OV <: SigmaPropValue] extends SigmaPropValue {
  val items: Seq[IV]
}

/**
  * AND conjunction for sigma propositions
  */
case class SigmaAnd(items: Seq[SigmaPropValue]) extends SigmaTransformer[SigmaPropValue, SigmaPropValue] {
  override def companion = SigmaAnd
  def tpe = SSigmaProp
  val opType = SFunc(SCollection.SSigmaPropArray, SSigmaProp)
}

object SigmaAnd extends ValueCompanion {
  override val opCode: OpCode = OpCodes.SigmaAndCode
  def apply(head: SigmaPropValue, tail: SigmaPropValue*): SigmaAnd = SigmaAnd(head +: tail)
}

/**
  * OR disjunction for sigma propositions
  */
case class SigmaOr(items: Seq[SigmaPropValue]) extends SigmaTransformer[SigmaPropValue, SigmaPropValue] {
  override def companion = SigmaOr
  def tpe = SSigmaProp
  val opType = SFunc(SCollection.SSigmaPropArray, SSigmaProp)
}

object SigmaOr extends ValueCompanion {
  override val opCode: OpCode = OpCodes.SigmaOrCode
  def apply(head: SigmaPropValue, tail: SigmaPropValue*): SigmaOr = SigmaOr(head +: tail)
}


/**
  * OR logical conjunction
  */
case class OR(input: Value[SCollection[SBoolean.type]])
  extends Transformer[SCollection[SBoolean.type], SBoolean.type] with NotReadyValueBoolean {
  override def companion = OR
  override val opType = SFunc(SCollection.SBooleanArray, SBoolean)
}

object OR extends ValueCompanion {
  override val opCode: OpCode = OrCode
  def apply(children: Seq[Value[SBoolean.type]]): OR =
    OR(ConcreteCollection(children.toIndexedSeq))

  def apply(head: Value[SBoolean.type], tail: Value[SBoolean.type]*): OR = apply(head +: tail)
}

/** Similar to allOf, but performing logical XOR operation instead of `&&`
  */
case class XorOf(input: Value[SCollection[SBoolean.type]])
  extends Transformer[SCollection[SBoolean.type], SBoolean.type] with NotReadyValueBoolean {
  override def companion = XorOf
  override val opType = SFunc(SCollection.SBooleanArray, SBoolean)
}

object XorOf extends ValueCompanion {
  override val opCode: OpCode = XorOfCode
  def apply(children: Seq[Value[SBoolean.type]]): XorOf =
    XorOf(ConcreteCollection(children.toIndexedSeq))

  def apply(head: Value[SBoolean.type], tail: Value[SBoolean.type]*): XorOf = apply(head +: tail)
}

/**
  * AND logical conjunction
  */
case class AND(input: Value[SCollection[SBoolean.type]])
  extends Transformer[SCollection[SBoolean.type], SBoolean.type]
    with NotReadyValueBoolean {
  override def companion = AND
  override val opType = SFunc(SCollection.SBooleanArray, SBoolean)
}

object AND extends ValueCompanion {
  override val opCode: OpCode = AndCode
  def apply(children: Seq[Value[SBoolean.type]]): AND =
    AND(ConcreteCollection(children.toIndexedSeq))

  def apply(head: Value[SBoolean.type], tail: Value[SBoolean.type]*): AND = apply(head +: tail)
}

/**
  * Logical threshold.
  * AtLeast has two inputs: integer bound and children same as in AND/OR. The result is true if at least bound children are true.
  */
case class AtLeast(bound: Value[SInt.type], input: Value[SCollection[SSigmaProp.type]])
  extends Transformer[SCollection[SSigmaProp.type], SSigmaProp.type]
    with NotReadyValue[SSigmaProp.type] {
  override def companion = AtLeast
  override def tpe: SSigmaProp.type = SSigmaProp
  override def opType: SFunc = SFunc(IndexedSeq(SInt, SCollection.SBooleanArray), SBoolean)

}

object AtLeast extends ValueCompanion {
  override val opCode: OpCode = AtLeastCode
  val MaxChildrenCount = 255

  def apply(bound: Value[SInt.type], children: Seq[SigmaPropValue]): AtLeast =
    AtLeast(bound, ConcreteCollection(children.toIndexedSeq))

  def apply(bound: Value[SInt.type], head: SigmaPropValue, tail: SigmaPropValue*): AtLeast = apply(bound, head +: tail)

  def reduce(bound: Int, children: Seq[SigmaBoolean]): SigmaBoolean = {
    import sigmastate.TrivialProp._
    if (bound <= 0) return TrueProp
    if (bound > children.length) return FalseProp

    var curBound = bound
    var childrenLeft = children.length
    // invariant due to the two if statements above: 0<curBound<=childrenLeft

    val sigmas = mutable.Buffer[SigmaBoolean]()

    // we should make sure that number of children doesn't exceed 255, because CTHRESHOLD cannot handle
    // more than 255 children, because of the way polynomial arithmetic is implemented (single-byte inputs only
    // are allowed to polynomials)
    //
    // (this will ensure bound is between 2 and 254, because otherwise one of the conditions above will apply and it will
    // be converted to one of true, false, and, or)
    require(children.length <= MaxChildrenCount)
    // My preferred method: if (children.length>=255) return FalseLeaf

    for (iChild <- children.indices) {
      if (curBound == 1)
        return COR.normalized(sigmas ++ children.slice(iChild, children.length))
      // If at any point bound == number of children, convert to AND.
      if (curBound == childrenLeft)
        return CAND.normalized(sigmas ++ children.slice(iChild, children.length))
      // at this point 1<curBound<childrenLeft
      children(iChild) match {
        case TrueProp => // If child is true, remove child and reduce bound.
          childrenLeft -= 1
          curBound -= 1
        case FalseProp => // If child is false, remove child, leave bound unchanged.
          childrenLeft -= 1
        case sigma => sigmas += sigma
      }
      // at this point 1<=curBound<=childrenLeft
    }
    if (curBound == 1) return COR.normalized(sigmas)
    if (curBound == childrenLeft) return CAND.normalized(sigmas)
    CTHRESHOLD(curBound, sigmas)
  }
}

/**
  * Up cast for Numeric types
  */
case class Upcast[T <: SNumericType, R <: SNumericType](input: Value[T], tpe: R)
  extends Transformer[T, R] {
  import Upcast._
  require(input.tpe.isInstanceOf[SNumericType], s"Cannot create Upcast node for non-numeric type ${input.tpe}")
  override def companion = Upcast
  override val opType = SFunc(Vector(tT), tR)
}

object Upcast extends ValueCompanion {
  override val opCode: OpCode = OpCodes.UpcastCode
  val tT = STypeIdent("T")
  val tR = STypeIdent("R")
}

/**
  * Down cast for Numeric types
  */
case class Downcast[T <: SNumericType, R <: SNumericType](input: Value[T], tpe: R)
  extends Transformer[T, R] {
  import Downcast._
  require(input.tpe.isInstanceOf[SNumericType], s"Cannot create Downcast node for non-numeric type ${input.tpe}")
  override def companion = Downcast
  override val opType = SFunc(Vector(tT), tR)
}

object Downcast extends ValueCompanion {
  override val opCode: OpCode = OpCodes.DowncastCode
  val tT = STypeIdent("T")
  val tR = STypeIdent("R")
}

/**
  * Convert SLong to SByteArray
  */
case class LongToByteArray(input: Value[SLong.type])
  extends Transformer[SLong.type, SByteArray] with NotReadyValueByteArray {
  override def companion = LongToByteArray
  override val opType = SFunc(SLong, SByteArray)
}
object LongToByteArray extends ValueCompanion {
  override val opCode: OpCode = OpCodes.LongToByteArrayCode
}

/**
  * Convert SByteArray to SLong
  */
case class ByteArrayToLong(input: Value[SByteArray])
  extends Transformer[SByteArray, SLong.type] with NotReadyValueLong {
  override def companion = ByteArrayToLong
  override val opType = SFunc(SByteArray, SLong)
}
object ByteArrayToLong extends ValueCompanion {
  override val opCode: OpCode = OpCodes.ByteArrayToLongCode
}

/**
  * Convert SByteArray to SBigInt
  */
case class ByteArrayToBigInt(input: Value[SByteArray])
  extends Transformer[SByteArray, SBigInt.type] with NotReadyValueBigInt {
  override def companion = ByteArrayToBigInt
  override val opType = SFunc(SByteArray, SBigInt)
}
object ByteArrayToBigInt extends ValueCompanion {
  override val opCode: OpCode = OpCodes.ByteArrayToBigIntCode
}

/**
  * Convert SByteArray to SGroupElement using CryptoConstants.dlogGroup.curve.decodePoint(bytes)
  */
case class DecodePoint(input: Value[SByteArray])
  extends Transformer[SByteArray, SGroupElement.type] with NotReadyValueGroupElement {
  override def companion = DecodePoint
  override val opType = SFunc(SByteArray, SGroupElement)
}
object DecodePoint extends ValueCompanion {
  override val opCode: OpCode = OpCodes.DecodePointCode
}

trait CalcHash extends Transformer[SByteArray, SByteArray] with NotReadyValueByteArray {
  val input: Value[SByteArray]
  val hashFn: CryptographicHash32
  override val opType = SFunc(SByteArray, SByteArray)
}

/**
  * Calculate Blake2b hash from `input`
  */
case class CalcBlake2b256(override val input: Value[SByteArray]) extends CalcHash {
  override def companion = CalcBlake2b256
  override val hashFn: CryptographicHash32 = Blake2b256
}
object CalcBlake2b256 extends ValueCompanion {
  override val opCode: OpCode = OpCodes.CalcBlake2b256Code
}

/**
  * Calculate Sha256 hash from `input`
  */
case class CalcSha256(override val input: Value[SByteArray]) extends CalcHash {
  override def companion = CalcSha256
  override val hashFn: CryptographicHash32 = Sha256
}
object CalcSha256 extends ValueCompanion {
  override val opCode: OpCode = OpCodes.CalcSha256Code
}

/**
  * Transforms serialized bytes of ErgoTree with segregated constants by replacing constants
  * at given positions with new values. This operation allow to use serialized scripts as
  * pre-defined templates.
  * The typical usage is "check that output box have proposition equal to given script bytes,
  * where minerPk (constants(0)) is replaced with currentMinerPk".
  * Each constant in original scriptBytes have SType serialized before actual data (see ConstantSerializer).
  * During substitution each value from newValues is checked to be an instance of the corresponding type.
  * This means, the constants during substitution cannot change their types.
  *
  * @param scriptBytes serialized ErgoTree with ConstantSegregationFlag set to 1.
  * @param positions zero based indexes in ErgoTree.constants array which should be replaced with new values
  * @param newValues new values to be injected into the corresponding positions in ErgoTree.constants array
  * @return original scriptBytes array where only specified constants are replaced and all other bytes remain exactly the same
  */
case class SubstConstants[T <: SType](scriptBytes: Value[SByteArray], positions: Value[SIntArray], newValues: Value[SCollection[T]])
    extends NotReadyValueByteArray {
  import SubstConstants._
  override def companion = SubstConstants
  override val opType = SFunc(Vector(SByteArray, SIntArray, SCollection(tT)), SByteArray)
}

object SubstConstants extends ValueCompanion {
  override val opCode: OpCode = OpCodes.SubstConstantsCode
  val tT = STypeIdent("T")

  def eval(scriptBytes: Array[Byte],
           positions: Array[Int],
           newVals: Array[Value[SType]]): Array[Byte] =
    ErgoTreeSerializer.DefaultSerializer.substituteConstants(scriptBytes, positions, newVals)
}

/**
  * A tree node with left and right descendants
  */
sealed trait Triple[LIV <: SType, RIV <: SType, OV <: SType] extends NotReadyValue[OV] {
  val left: Value[LIV]
  val right: Value[RIV]
  override def opType = SFunc(Vector(left.tpe, right.tpe), tpe)
}

sealed trait OneArgumentOperation[IV <: SType, OV <: SType] extends NotReadyValue[OV] {
  val input: Value[IV]
  override def opType = SFunc(input.tpe, tpe)
}

// TwoArgumentsOperation
sealed trait TwoArgumentsOperation[LIV <: SType, RIV <: SType, OV <: SType]
  extends Triple[LIV, RIV, OV]

case class ArithOp[T <: SType](left: Value[T], right: Value[T], override val opCode: OpCode)
  extends TwoArgumentsOperation[T, T, T] with NotReadyValue[T] {
  override def companion: ValueCompanion = ArithOp.operations(opCode)
  override def tpe: T = left.tpe
  override def opName: String = ArithOp.opcodeToArithOpName(opCode)

  // TODO refactor: avoid such enumaration, use ArithOp.operations map instead
  override def toString: String = opCode match {
    case OpCodes.PlusCode     => s"Plus($left, $right)"
    case OpCodes.MinusCode    => s"Minus($left, $right)"
    case OpCodes.MultiplyCode => s"Multiply($left, $right)"
    case OpCodes.DivisionCode => s"Divide($left, $right)"
    case OpCodes.ModuloCode   => s"Modulo($left, $right)"
    case OpCodes.MinCode      => s"Min($left, $right)"
    case OpCodes.MaxCode      => s"Max($left, $right)"
  }
}
case class ArithOpCompanion(opCode: Byte, name: String) extends ValueCompanion {
}
object ArithOp {
  import OpCodes._
  val Plus     = ArithOpCompanion(PlusCode,     "+")
  val Minus    = ArithOpCompanion(MinusCode,    "-")
  val Multiply = ArithOpCompanion(MultiplyCode, "*")
  val Division = ArithOpCompanion(DivisionCode, "/")
  val Modulo   = ArithOpCompanion(ModuloCode,   "%")
  val Min      = ArithOpCompanion(MinCode,      "min")
  val Max      = ArithOpCompanion(MaxCode,      "max")

  val operations: Map[Byte, ArithOpCompanion] =
    Seq(Plus, Minus, Multiply, Division, Modulo, Min, Max).map(o => (o.opCode, o)).toMap

  def opcodeToArithOpName(opCode: Byte): String = operations.get(opCode) match {
    case Some(c)  => c.name
    case _ => sys.error(s"Cannot find ArithOpName for opcode $opCode")
  }
}

case class Negation[T <: SType](input: Value[T]) extends OneArgumentOperation[T, T] {
  override def companion = Negation
  override def tpe: T = input.tpe
}
object Negation extends ValueCompanion {
  override val opCode: OpCode = OpCodes.NegationCode
}

case class BitInversion[T <: SNumericType](input: Value[T]) extends OneArgumentOperation[T, T] {
  override def companion = BitInversion
  override def tpe: T = input.tpe
}
object BitInversion extends ValueCompanion {
  override val opCode: OpCode = OpCodes.BitInversionCode
}

case class BitOp[T <: SNumericType](left: Value[T], right: Value[T], override val opCode: OpCode)
  extends TwoArgumentsOperation[T, T, T] with NotReadyValue[T] {
  override def companion = BitOp.operations(opCode)
  override def tpe: T = left.tpe
}
case class BitOpCompanion(opCode: Byte, name: String) extends ValueCompanion {
}

object BitOp {
  import OpCodes._
  val Or     = BitOpCompanion(BitOrCode,     "|")
  val And    = BitOpCompanion(BitAndCode,    "&")
  val Xor = BitOpCompanion(BitXorCode, "^")
  val ShiftRight = BitOpCompanion(BitShiftRightCode, ">>")
  val ShiftLeft        = BitOpCompanion(BitShiftLeftCode,   "<<")
  val ShiftRightZeroed = BitOpCompanion(BitShiftRightZeroedCode, ">>>")

  val operations: Map[Byte, BitOpCompanion] =
    Seq(Or, And, Xor, ShiftRight, ShiftLeft, ShiftRightZeroed).map(o => (o.opCode, o)).toMap

  def opcodeToName(opCode: Byte): String = operations.get(opCode) match {
    case Some(c)  => c.name
    case _ => sys.error(s"Cannot find BitOpName for opcode $opCode")
  }
}

case class ModQ(input: Value[SBigInt.type])
  extends NotReadyValue[SBigInt.type] {
  override def companion = ModQ
  override def tpe: SBigInt.type = SBigInt
  override def opType: SFunc = SFunc(input.tpe, tpe)
}
object ModQ extends ValueCompanion {
  override val opCode: OpCode = OpCodes.ModQCode
}

case class ModQArithOp(left: Value[SBigInt.type], right: Value[SBigInt.type], override val opCode: OpCode)
  extends NotReadyValue[SBigInt.type] {
  override def companion = ModQArithOp.operations(opCode)
  override def tpe: SBigInt.type = SBigInt
  override def opType: SFunc = SFunc(Vector(left.tpe, right.tpe), tpe)
}
case class ModQArithOpCompanion(opCode: Byte, name: String) extends ValueCompanion {
}

trait OpGroup[C <: ValueCompanion] {
  def operations: Map[Byte, C]
}

object ModQArithOp extends OpGroup[ModQArithOpCompanion] {
  import OpCodes._
  val Plus  = ModQArithOpCompanion(BitOrCode,  "PlusModQ")
  val Minus = ModQArithOpCompanion(BitAndCode, "MinusModQ")

  val operations: Map[Byte, ModQArithOpCompanion] = Seq(Plus, Minus).map(o => (o.opCode, o)).toMap

  def opcodeToName(opCode: Byte): String = operations.get(opCode) match {
    case Some(c)  => c.name
    case _ => sys.error(s"Cannot find ModQArithOp operation name for opcode $opCode")
  }
}
/**
  * XOR for two SByteArray
  */
case class Xor(override val left: Value[SByteArray],
               override val right: Value[SByteArray])
  extends TwoArgumentsOperation[SByteArray, SByteArray, SByteArray]
    with NotReadyValueByteArray {
  override def companion = Xor
}
object Xor extends ValueCompanion {
  override val opCode: OpCode = XorCode
}

case class Exponentiate(override val left: Value[SGroupElement.type],
                        override val right: Value[SBigInt.type])
  extends TwoArgumentsOperation[SGroupElement.type, SBigInt.type, SGroupElement.type]
    with NotReadyValueGroupElement {
  override def companion = Exponentiate
}
object Exponentiate extends ValueCompanion {
  override val opCode: OpCode = ExponentiateCode
}

case class MultiplyGroup(override val left: Value[SGroupElement.type],
                         override val right: Value[SGroupElement.type])
  extends TwoArgumentsOperation[SGroupElement.type, SGroupElement.type, SGroupElement.type]
    with NotReadyValueGroupElement {
  override def companion = MultiplyGroup
}
object MultiplyGroup extends ValueCompanion {
  override val opCode: OpCode = MultiplyGroupCode
}
// Relation

sealed trait Relation[LIV <: SType, RIV <: SType] extends Triple[LIV, RIV, SBoolean.type]
  with NotReadyValueBoolean

trait SimpleRelation[T <: SType] extends Relation[T, T] {
  val tT = STypeIdent("T")
  override val opType = SFunc(Vector(tT, tT), SBoolean)
}

/**
  * Less operation for SInt
  */
case class LT[T <: SType](override val left: Value[T], override val right: Value[T]) extends SimpleRelation[T] {
  override def companion = LT
}
object LT extends ValueCompanion {
  override val opCode: OpCode = LtCode
}
/**
  * Less or equals operation for SInt
  */
case class LE[T <: SType](override val left: Value[T], override val right: Value[T]) extends SimpleRelation[T] {
  override def companion = LE
}
object LE extends ValueCompanion {
  override val opCode: OpCode = LeCode
}
/**
  * Greater operation for SInt
  */
case class GT[T <: SType](override val left: Value[T], override val right: Value[T]) extends SimpleRelation[T] {
  override def companion = GT
}
object GT extends ValueCompanion {
  override val opCode: OpCode = GtCode
}
/**
  * Greater or equals operation for SInt
  */
case class GE[T <: SType](override val left: Value[T], override val right: Value[T]) extends SimpleRelation[T] {
  override def companion = GE
}
object GE extends ValueCompanion {
  override val opCode: OpCode = GeCode
}

/**
  * Equals operation for SType
  * todo: make EQ to really accept only values of the same type, now EQ(TrueLeaf, IntConstant(5)) is valid
  */
case class EQ[S <: SType](override val left: Value[S], override val right: Value[S])
  extends SimpleRelation[S] {
  override def companion = EQ
}
object EQ extends ValueCompanion {
  override val opCode: OpCode = EqCode
}

/**
  * Non-Equals operation for SType
  */
case class NEQ[S <: SType](override val left: Value[S], override val right: Value[S])
  extends SimpleRelation[S] {
  override def companion = NEQ
}
object NEQ extends ValueCompanion {
  override val opCode: OpCode = NeqCode
}

/**
  * Logical OR with lazy right argument which is evaluated only if left == false.
  * If left argument is true, the right is guaranteed to NOT be evaluated
  */
case class BinOr(override val left: BoolValue, override val right: BoolValue)
  extends Relation[SBoolean.type, SBoolean.type] {
  override def companion = BinOr
}
object BinOr extends ValueCompanion {
  override val opCode: OpCode = BinOrCode
}

/**
  * Logical AND with lazy right argument which is evaluated only if left == true.
  * If left argument is false, the right is guaranteed to NOT be evaluated
  */
case class BinAnd(override val left: BoolValue, override val right: BoolValue)
  extends Relation[SBoolean.type, SBoolean.type] {
  override def companion = BinAnd
}
object BinAnd extends ValueCompanion {
  override val opCode: OpCode = BinAndCode
}

case class BinXor(override val left: BoolValue, override val right: BoolValue)
  extends Relation[SBoolean.type, SBoolean.type] {
  override def companion = BinXor
}
object BinXor extends ValueCompanion {
  override val opCode: OpCode = BinXorCode
}

/**
  * A tree node with three descendants
  */
sealed trait Quadruple[IV1 <: SType, IV2 <: SType, IV3 <: SType, OV <: SType] extends NotReadyValue[OV] {

  val first: Value[IV1]
  val second: Value[IV2]
  val third: Value[IV3]

  val opType = SFunc(Vector(first.tpe, second.tpe, third.tpe), tpe)
}

sealed trait Relation3[IV1 <: SType, IV2 <: SType, IV3 <: SType]
  extends Quadruple[IV1, IV2, IV3, SBoolean.type] with NotReadyValueBoolean

/**
  * Perform a lookup of key `key` in a tree with root `tree` using proof `proof`.
  * Throws exception if proof is incorrect
  * Return Some(bytes) of leaf with key `key` if it exists
  * Return None if leaf with provided key does not exist.
  */
case class TreeLookup(tree: Value[SAvlTree.type],
    key: Value[SByteArray],
    proof: Value[SByteArray]) extends Quadruple[SAvlTree.type, SByteArray, SByteArray, SOption[SByteArray]] {
  override def companion = TreeLookup
  override def tpe = SOption[SByteArray]
  override lazy val first = tree
  override lazy val second = key
  override lazy val third = proof
}
object TreeLookup extends ValueCompanion {
  override val opCode: OpCode = OpCodes.AvlTreeGetCode
}

/**
  * If conditional function.
  * Non-lazy - evaluate both branches.
  *
  * @param condition   - condition to check
  * @param trueBranch  - branch that will be used if condition is true
  * @param falseBranch - branch that will be used if condition is false
  */
case class If[T <: SType](condition: Value[SBoolean.type], trueBranch: Value[T], falseBranch: Value[T])
  extends Quadruple[SBoolean.type, T, T, T] {
  override def companion = If
  override def tpe = trueBranch.tpe
  override lazy val first = condition
  override lazy val second = trueBranch
  override lazy val third = falseBranch
}
object If extends ValueCompanion {
  override val opCode: OpCode = OpCodes.IfCode
  val tT = STypeIdent("T")
}

case class LogicalNot(input: Value[SBoolean.type]) extends NotReadyValueBoolean {
  override def companion = LogicalNot
  override val opType = SFunc(Vector(SBoolean), SBoolean)
}
object LogicalNot extends ValueCompanion {
  override val opCode: OpCode = OpCodes.LogicalNotCode
}


