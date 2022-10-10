package sigmastate

import org.ergoplatform.SigmaConstants
import org.ergoplatform.validation.SigmaValidationSettings
import scalan.{ExactIntegral, ExactNumeric, ExactOrdering, Nullable}
import scalan.OverloadHack.Overloaded1
import scorex.crypto.hash.{Blake2b256, CryptographicHash32, Sha256}
import sigmastate.Operations._
import sigmastate.SCollection.{SByteArray, SIntArray}
import sigmastate.SOption.SIntOption
import sigmastate.Values._
import sigmastate.basics.{SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import sigmastate.interpreter.ErgoTreeEvaluator
import sigmastate.interpreter.ErgoTreeEvaluator.DataEnv
import sigmastate.serialization.OpCodes._
import sigmastate.serialization._
import sigmastate.utxo.{SimpleTransformerCompanion, Transformer}
import debox.{Map => DMap}
import scalan.ExactIntegral._
import scalan.ExactOrdering._
import sigmastate.ArithOp.OperationImpl
import sigmastate.eval.NumericOps.{BigIntIsExactIntegral, BigIntIsExactOrdering}
import sigmastate.eval.{Colls, SigmaDsl}
import sigmastate.lang.TransformingSigmaBuilder
import special.collection.Coll
import special.sigma.{GroupElement, SigmaProp}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import spire.syntax.all.cfor

/**
  * Basic trait for inner nodes of crypto-trees, so AND/OR/THRESHOLD sigma-protocol connectives
  */
trait SigmaConjecture extends SigmaBoolean {
  def children: Seq[SigmaBoolean]
}

/**
  * Basic trait for leafs of crypto-trees, such as ProveDlog and ProveDiffieHellman instances
  */
trait SigmaProofOfKnowledgeLeaf[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP, _]]
  extends SigmaBoolean with SigmaProtocolCommonInput[SP]


/**
  * AND conjunction for sigma propositions
  */
case class CAND(override val children: Seq[SigmaBoolean]) extends SigmaConjecture {
  /** The same code is used for AND operation, but they belong to different type hierarchies. */
  override val opCode: OpCode = OpCodes.AndCode
  override val size: Int = SigmaBoolean.totalSize(children) + 1
}

object CAND {
  import TrivialProp._

  /** Connects the given sigma propositions into CAND proposition performing
    * partial evaluation when some of them are trivial propositioins.
    *
    * @param items propositions to combine into CAND
    * @return CAND, TrueProp, FalseProp or even one of the items depending on partial evaluation
    */
  def normalized(items: Seq[SigmaBoolean]): SigmaBoolean = {
    require(items.nonEmpty)
    val res = new ArrayBuffer[SigmaBoolean]()
    val nItems = items.length
    cfor(0)(_ < nItems, _ + 1) { i =>
      val x = items(i)
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
case class COR(children: Seq[SigmaBoolean]) extends SigmaConjecture {
  /** The same code is also used for OR operation, but they belong to different type hierarchies. */
  override val opCode: OpCode = OpCodes.OrCode
  override val size: Int = SigmaBoolean.totalSize(children) + 1
}

object COR {
  import TrivialProp._

  /** Connects the given sigma propositions into COR proposition performing
    * partial evaluation when some of them are trivial propositioins.
    *
    * @param items propositions to combine into COR
    * @return COR, TrueProp, FalseProp or even one of the items depending on partial evaluation
    */
  def normalized(items: Seq[SigmaBoolean]): SigmaBoolean = {
    require(items.nonEmpty)
    val res = new ArrayBuffer[SigmaBoolean]()
    val nItems = items.length
    cfor(0)(_ < nItems, _ + 1) { i =>
      val x = items(i)
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
case class CTHRESHOLD(k: Int, children: Seq[SigmaBoolean]) extends SigmaConjecture {
  // Our polynomial arithmetic can take only byte inputs
  require(k >= 0 && k <= children.length && children.length <= 255)

  override val opCode: OpCode = OpCodes.AtLeastCode
  override val size: Int = SigmaBoolean.totalSize(children) + 1
}


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
    override def size: Int = 1
    override def toString = "FalseProp"
  }
  val TrueProp = new TrivialProp(true) {
    override val opCode: OpCode = OpCodes.TrivialPropTrueCode
    override def size: Int = 1
    override def toString = "TrueProp"
  }
}

/** Embedding of Boolean values to SigmaProp values. As an example, this operation allows boolean experesions
  * to be used as arguments of `atLeast(..., sigmaProp(boolExpr), ...)` operation.
  * During execution results to either `TrueProp` or `FalseProp` values of SigmaProp type.
  */
case class BoolToSigmaProp(value: BoolValue) extends SigmaPropValue {
  override def companion = BoolToSigmaProp
  override def tpe = SSigmaProp
  override def opType = BoolToSigmaProp.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val v = value.evalTo[Boolean](env)
    addCost(BoolToSigmaProp.costKind)
    SigmaDsl.sigmaProp(v)
  }
}
object BoolToSigmaProp extends ValueCompanion {
  override def opCode: OpCode = OpCodes.BoolToSigmaPropCode
  override val costKind = FixedCost(15)
  val OpType = SFunc(SBoolean, SSigmaProp)
}

/** ErgoTree operation to create a new SigmaProp value representing public key
  * of discrete logarithm signature protocol. */
case class CreateProveDlog(value: Value[SGroupElement.type]) extends SigmaPropValue {
  override def companion = CreateProveDlog
  override def tpe = SSigmaProp
  override def opType = CreateProveDlog.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val v = value.evalTo[GroupElement](env)
    addCost(CreateProveDlog.costKind)
    SigmaDsl.proveDlog(v)
  }
}
object CreateProveDlog extends ValueCompanion {
  override def opCode: OpCode = OpCodes.ProveDlogCode
  override val costKind = FixedCost(10)
  val OpType = SFunc(SGroupElement, SSigmaProp)
}

// TODO refactor: remove not used class
/** Construct a new authenticated dictionary with given parameters and tree root digest.*/
case class CreateAvlTree(operationFlags: ByteValue,
    digest: Value[SByteArray],
    keyLength: IntValue,
    valueLengthOpt: Value[SIntOption]) extends AvlTreeValue {
  override def companion = CreateAvlTree
  override def tpe = SAvlTree
  override def opType = CreateAvlTree.OpType
}
object CreateAvlTree extends ValueCompanion {
  override def opCode: OpCode = OpCodes.AvlTreeCode
  override def costKind: CostKind = Value.notSupportedError(this, "costKind")
  val OpType = SFunc(Array(SByte, SByteArray, SInt, SIntOption), SAvlTree)
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
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val g = gv.evalTo[GroupElement](env)
    val h = hv.evalTo[GroupElement](env)
    val u = uv.evalTo[GroupElement](env)
    val v = vv.evalTo[GroupElement](env)
    addCost(CreateProveDHTuple.costKind)
    SigmaDsl.proveDHTuple(g, h, u, v)
  }
}
object CreateProveDHTuple extends ValueCompanion {
  override def opCode: OpCode = OpCodes.ProveDiffieHellmanTupleCode
  override val costKind = FixedCost(20)
}

sealed trait SigmaTransformer[IV <: SigmaPropValue, OV <: SigmaPropValue] extends SigmaPropValue {
  val items: Seq[IV]
}
trait SigmaTransformerCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
}

/**
  * AND conjunction for sigma propositions
  */
case class SigmaAnd(items: Seq[SigmaPropValue]) extends SigmaTransformer[SigmaPropValue, SigmaPropValue] {
  override def companion = SigmaAnd
  override def tpe = SSigmaProp
  override def opType = SigmaAnd.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val len = items.length
    val is = new Array[SigmaProp](len)
    cfor(0)(_ < len, _ + 1) { i =>
      is(i) = items(i).evalTo[SigmaProp](env)
    }
    addSeqCost(SigmaAnd.costKind, len) { () =>
      SigmaDsl.allZK(Colls.fromArray(is))
    }
  }
}
object SigmaAnd extends SigmaTransformerCompanion {
  val OpType = SFunc(SCollection.SSigmaPropArray, SSigmaProp)
  override def opCode: OpCode = OpCodes.SigmaAndCode
  /** BaseCost:
    * - constructing new CSigmaProp and allocation collection
    * - one iteration over collection of items
    */
  override val costKind = PerItemCost(baseCost = 10, perChunkCost = 2, chunkSize = 1)
  override def argInfos: Seq[ArgInfo] = SigmaAndInfo.argInfos
  def apply(first: SigmaPropValue, second: SigmaPropValue, tail: SigmaPropValue*): SigmaAnd = SigmaAnd(Array(first, second) ++ tail)
}

/**
  * OR disjunction for sigma propositions
  */
case class SigmaOr(items: Seq[SigmaPropValue]) extends SigmaTransformer[SigmaPropValue, SigmaPropValue] {
  override def companion = SigmaOr
  override def tpe = SSigmaProp
  override def opType = SigmaOr.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val len = items.length
    val is = new Array[SigmaProp](len)
    cfor(0)(_ < len, _ + 1) { i =>
      is(i) = items(i).evalTo[SigmaProp](env)
    }
    addSeqCost(SigmaOr.costKind, len) { () =>
      SigmaDsl.anyZK(Colls.fromArray(is))
    }
  }
}

object SigmaOr extends SigmaTransformerCompanion {
  val OpType = SFunc(SCollection.SSigmaPropArray, SSigmaProp)
  override def opCode: OpCode = OpCodes.SigmaOrCode
  /** BaseCost:
    * - constructing new CSigmaProp and allocation collection
    * - one iteration over collection of items */
  override val costKind = PerItemCost(baseCost = 10, perChunkCost = 2, chunkSize = 1)
  override def argInfos: Seq[ArgInfo] = SigmaOrInfo.argInfos
  def apply(head: SigmaPropValue, tail: SigmaPropValue*): SigmaOr = SigmaOr(head +: tail)
}

/** Base trait for companions of OR, AND and XorOf nodes. */
trait LogicalTransformerCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
  val OpType: SFunc = SFunc(SCollection.SBooleanArray, SBoolean)
}

/**
  * OR logical conjunction
  */
case class OR(input: Value[SCollection[SBoolean.type]])
  extends Transformer[SCollection[SBoolean.type], SBoolean.type] with NotReadyValueBoolean {
  override def companion = OR
  override def opType = OR.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Boolean]](env)
    var res = false
    val len = inputV.length
    var i = 0
    E.addSeqCost(OR.costKind, this.companion.opDesc) { () =>
      // this loop is bounded since ErgoTree is bounded by MaxBoxSize
      while (i < len && !res) {
        res ||= inputV(i)
        i += 1
      }
      i // return actual number of processed items
    }
    res
  }
}

object OR extends LogicalTransformerCompanion {
  override def opCode: OpCode = OrCode
  /** Base cost: operations factored out of reduction loop.
    * Per-chunk cost: cost of scala `||` operations amortized over a chunk of boolean values.
    * @see BinOr
    * @see AND */
  override val costKind = PerItemCost(5, 5, 64/*size of cache line in bytes*/)
  override def argInfos: Seq[ArgInfo] = Operations.ORInfo.argInfos

  def apply(children: Seq[Value[SBoolean.type]]): OR =
    OR(ConcreteCollection.fromSeq(children))

  def apply(items: Value[SBoolean.type]*)(implicit o: Overloaded1): OR = apply(items)
}

/** Similar to allOf, but performing logical XOR operation instead of `&&`
  */
case class XorOf(input: Value[SCollection[SBoolean.type]])
  extends Transformer[SCollection[SBoolean.type], SBoolean.type] with NotReadyValueBoolean {
  override def companion = XorOf
  override def opType = XorOf.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Boolean]](env)
    val len = inputV.length
    addSeqCost(XorOf.costKind, len) { () =>
      val res = if (E.context.activatedScriptVersion >= 2) {
        if (len == 0) false
        else if (len == 1) inputV(0)
        else {
          var res = inputV(0)
          cfor(1)(_ < len, _ + 1) { i =>
            res ^= inputV(i)
          }
          res
        }
      } else {
        SigmaDsl.xorOf(inputV)
      }
      res
    }
  }
}

object XorOf extends LogicalTransformerCompanion {
  override def opCode: OpCode = XorOfCode
  /** Base cost: operations factored out of reduction loop.
    * Per-chunk cost: cost of scala `||` operations amortized over a chunk of boolean values.
    * @see BinOr
    * @see AND */
  override val costKind = PerItemCost(20, 5, 32)
  override def argInfos: Seq[ArgInfo] = Operations.XorOfInfo.argInfos

  def apply(children: Seq[Value[SBoolean.type]]): XorOf =
    XorOf(ConcreteCollection.fromSeq(children))
}

/**
  * AND logical conjunction
  */
case class AND(input: Value[SCollection[SBoolean.type]])
  extends Transformer[SCollection[SBoolean.type], SBoolean.type]
    with NotReadyValueBoolean {
  override def companion = AND
  override def opType = AND.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Boolean]](env)
    var res = true
    val len = inputV.length
    var i = 0
    E.addSeqCost(AND.costKind, this.companion.opDesc) { () =>
      // this loop is bounded since ErgoTree is bounded by MaxBoxSize
      while (i < len && res) {
        res &&= inputV(i)
        i += 1
      }
      i // return actual number of processed items
    }
    res
  }
}

object AND extends LogicalTransformerCompanion {
  override def opCode: OpCode = AndCode
  /** Base cost: operations factored out of reduction loop.
    * Per-chunk cost: cost of scala `&&` operations amortized over a chunk of boolean values.
    * @see BinAnd
    * @see OR */
  override val costKind = PerItemCost(10, 5, 32/* half size of cache line in bytes */)
  override def argInfos: Seq[ArgInfo] = Operations.ANDInfo.argInfos

  def apply(children: Seq[Value[SBoolean.type]]): AND =
    AND(ConcreteCollection.fromSeq(children))

//  def apply(head: Value[SBoolean.type], tail: Value[SBoolean.type]*): AND = apply(head +: tail)
  def apply(items: Value[SBoolean.type]*)(implicit o1: Overloaded1): AND = apply(items)
}

/**
  * Logical threshold.
  * AtLeast has two inputs: integer bound and children same as in AND/OR.
  * The result is true if at least bound children are true.
  */
case class AtLeast(bound: Value[SInt.type], input: Value[SCollection[SSigmaProp.type]])
  extends Transformer[SCollection[SSigmaProp.type], SSigmaProp.type]
    with NotReadyValue[SSigmaProp.type] {
  override def companion = AtLeast
  override def tpe: SSigmaProp.type = SSigmaProp
  override def opType: SFunc = AtLeast.OpType

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val b = bound.evalTo[Int](env)
    val props = input.evalTo[Coll[SigmaProp]](env)
    addSeqCost(AtLeast.costKind, props.length) { () =>
      SigmaDsl.atLeast(b, props)
    }
  }
}

object AtLeast extends ValueCompanion {
  override def opCode: OpCode = AtLeastCode
  /** Base cost: constructing new CSigmaProp value
    * Per chunk cost: obtaining SigmaBooleans for each chunk in AtLeast
    */
  override val costKind = PerItemCost(20, 3, 5)
  val OpType: SFunc = SFunc(Array(SInt, SCollection.SBooleanArray), SBoolean)
  val MaxChildrenCount: Int = SigmaConstants.MaxChildrenCountForAtLeastOp.value

  def apply(bound: Value[SInt.type], children: Seq[SigmaPropValue]): AtLeast =
    AtLeast(bound, ConcreteCollection.fromSeq(children))

  def apply(bound: Value[SInt.type], head: SigmaPropValue, tail: SigmaPropValue*): AtLeast =
    apply(bound, head +: tail)

  /** HOTSPOT: don't beautify this code */
  def reduce(bound: Int, children: Seq[SigmaBoolean]): SigmaBoolean = {
    import sigmastate.TrivialProp._
    if (bound <= 0) return TrueProp
    val nChildren = children.length
    if (bound > nChildren) return FalseProp

    var curBound = bound
    var childrenLeft = nChildren
    // invariant due to the two if statements above: 0<curBound<=childrenLeft

    val sigmas = mutable.ArrayBuilder.make[SigmaBoolean]
    sigmas.sizeHint(nChildren)

    // we should make sure that number of children doesn't exceed 255, because CTHRESHOLD cannot handle
    // more than 255 children, because of the way polynomial arithmetic is implemented (single-byte inputs only
    // are allowed to polynomials)
    //
    // (this will ensure bound is between 2 and 254, because otherwise one of the conditions above will apply and it will
    // be converted to one of true, false, and, or)
    require(nChildren <= MaxChildrenCount)
    // My preferred method: if (children.length>=255) return FalseLeaf

    var iChild = 0
    while (iChild < nChildren) {
      if (curBound == 1) {
        sigmas ++= children.slice(iChild, nChildren)
        return COR.normalized(sigmas.result())
      }
      // If at any point bound == number of children, convert to AND.
      if (curBound == childrenLeft) {
        sigmas ++= children.slice(iChild, nChildren)
        return CAND.normalized(sigmas.result())
      }
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
      iChild += 1
    }

    val ch = sigmas.result()
    if (curBound == 1) return COR.normalized(ch)
    if (curBound == childrenLeft) return CAND.normalized(ch)
    CTHRESHOLD(curBound, ch)
  }
}

/**
  * Up cast for Numeric types
  */
case class Upcast[T <: SNumericType, R <: SNumericType](input: Value[T], tpe: R)
  extends Transformer[T, R] {
  require(input.tpe.isInstanceOf[SNumericType], s"Cannot create Upcast node for non-numeric type ${input.tpe}")
  override def companion = Upcast
  override def opType = Upcast.OpType

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[AnyVal](env)
    addCost(Upcast.costKind, tpe) { () =>
      tpe.upcast(inputV)
    }
  }
}

/** Base class for Upcast and Downcast companion objects. */
trait NumericCastCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
  val OpType = SFunc(Array(SType.tT), SType.tR)
  /** Returns cost descriptor of this operation. */
  def costKind: TypeBasedCost = NumericCastCostKind
}

/** Cost of:
  * 1) converting numeric value to the numeric value of the given type, i.e. Byte -> Int
  * NOTE: the cost of BigInt casting is the same in JITC (comparing to AOTC) to simplify
  * implementation.
  */
object NumericCastCostKind extends TypeBasedCost {
  override def costFunc(targetTpe: SType): Int = targetTpe match {
    case SBigInt => 30
    case _ => 10
  }
}

object Upcast extends NumericCastCompanion {
  override def opCode: OpCode = OpCodes.UpcastCode
  override def argInfos: Seq[ArgInfo] = UpcastInfo.argInfos
  def tT = SType.tT
  def tR = SType.tR
  val BigIntOpType = SFunc(tT, SBigInt) // TODO check usage
}

/**
  * Down cast for Numeric types
  */
case class Downcast[T <: SNumericType, R <: SNumericType](input: Value[T], tpe: R)
  extends Transformer[T, R] {
  require(input.tpe.isInstanceOf[SNumericType], s"Cannot create Downcast node for non-numeric type ${input.tpe}")
  override def companion = Downcast
  override def opType = Downcast.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[AnyVal](env)
    addCost(Downcast.costKind, tpe) { () =>
      tpe.downcast(inputV)
    }
  }
}

object Downcast extends NumericCastCompanion {
  override def opCode: OpCode = OpCodes.DowncastCode
  override def argInfos: Seq[ArgInfo] = DowncastInfo.argInfos
  def tT = SType.tT
  def tR = SType.tR
  val BigIntOpType = SFunc(SBigInt, tR) // TODO check usage
}

/**
  * Convert SLong to SByteArray
  */
case class LongToByteArray(input: Value[SLong.type])
  extends Transformer[SLong.type, SByteArray] with NotReadyValueByteArray {
  override def companion = LongToByteArray
  override def opType = LongToByteArray.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Long](env)
    addCost(LongToByteArray.costKind)
    SigmaDsl.longToByteArray(inputV)
  }
}
object LongToByteArray extends SimpleTransformerCompanion {
  val OpType = SFunc(SLong, SByteArray)
  override def opCode: OpCode = OpCodes.LongToByteArrayCode
  override val costKind = FixedCost(17)
  override def argInfos: Seq[ArgInfo] = LongToByteArrayInfo.argInfos
}

/**
  * Convert SByteArray to SLong
  */
case class ByteArrayToLong(input: Value[SByteArray])
  extends Transformer[SByteArray, SLong.type] with NotReadyValueLong {
  override def companion = ByteArrayToLong
  override def opType = ByteArrayToLong.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Byte]](env)
    addCost(ByteArrayToLong.costKind)
    SigmaDsl.byteArrayToLong(inputV)
  }
}
object ByteArrayToLong extends SimpleTransformerCompanion {
  val OpType = SFunc(SByteArray, SLong)
  override def opCode: OpCode = OpCodes.ByteArrayToLongCode
  override val costKind = FixedCost(16)
  override def argInfos: Seq[ArgInfo] = ByteArrayToLongInfo.argInfos
}

/**
  * Convert SByteArray to SBigInt
  */
case class ByteArrayToBigInt(input: Value[SByteArray])
  extends Transformer[SByteArray, SBigInt.type] with NotReadyValueBigInt {
  override def companion = ByteArrayToBigInt
  override val opType = ByteArrayToBigInt.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Byte]](env)
    addCost(ByteArrayToBigInt.costKind)
    SigmaDsl.byteArrayToBigInt(inputV)
  }
}
object ByteArrayToBigInt extends SimpleTransformerCompanion {
  val OpType = SFunc(SByteArray, SBigInt)
  override def opCode: OpCode = OpCodes.ByteArrayToBigIntCode
  override val costKind = FixedCost(30)
  override def argInfos: Seq[ArgInfo] = ByteArrayToBigIntInfo.argInfos
}

/**
  * Convert SByteArray to SGroupElement using CryptoConstants.dlogGroup.curve.decodePoint(bytes)
  */
case class DecodePoint(input: Value[SByteArray])
  extends Transformer[SByteArray, SGroupElement.type] with NotReadyValueGroupElement {
  override def companion = DecodePoint
  override def opType = DecodePoint.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Byte]](env)
    addCost(DecodePoint.costKind)
    SigmaDsl.decodePoint(inputV)
  }
}
object DecodePoint extends SimpleTransformerCompanion with FixedCostValueCompanion {
  val OpType = SFunc(SByteArray, SGroupElement)
  override def opCode: OpCode = OpCodes.DecodePointCode
  /** Cost of:
    * 1) create reader and read bytes in a new array
    * 2) calling curve.decodePoint and obtain EcPoint
    * 3) wrap EcPoint in GroupElement*/
  override val costKind = FixedCost(300)
  override def argInfos: Seq[ArgInfo] = DecodePointInfo.argInfos
}

sealed abstract class CalcHash extends Transformer[SByteArray, SByteArray] with NotReadyValueByteArray {
  def hashFn: CryptographicHash32
  override def opType = CalcHash.OpType
}
object CalcHash {
  val OpType = SFunc(SByteArray, SByteArray)
}

/**
  * Calculate Blake2b hash from `input`
  */
case class CalcBlake2b256(override val input: Value[SByteArray]) extends CalcHash {
  override def companion = CalcBlake2b256
  override val hashFn: CryptographicHash32 = Blake2b256
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Byte]](env)
    addSeqCost(CalcBlake2b256.costKind, inputV.length) { () =>
      SigmaDsl.blake2b256(inputV)
    }
  }
}
object CalcBlake2b256 extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.CalcBlake2b256Code

  /** Cost of: of hashing 1 block of data.
    *
    * This cost is used as a baseline to connect cost units with absolute time.
    * The block validation have 1000000 of cost units budget, and we want this to
    * correspond to 1 second. Thus we can assume 1 cost unit == 1 micro-second.
    *
    * It takes approximately 1 micro-seconds on average to compute hash of 128 bytes
    * block on MacBook Pro (16-inch, 2019) 2.3 GHz 8-Core Intel Core i9.
    *
    * Thus per block cost of Blake2b256 hashing can be limited by 1 cost units.
    * However, on a less powerful processor it may take much more time, so we add
    * a factor of 3 for that. Additionally, the interpreter has overhead so that
    * performing 1000 of hashes in a tight loop is 3-4 times faster than doing the same
    * via ErgoTreeEvaluator. Thus we should add another factor of 2 and this takes
    * place for all operations. So we will use a total factor of 10 to convert
    * actual operation micro-seconds time (obtained via benchmarking) to cost unit
    * estimation (used for cost prediction).
    *
    * Cost_in_units = time_in_micro-seconds * 7 = 7
    *
    * NOTE, 128 is the size of message chunk processed by Blake2b256 algorithm.
    *
    * @see [[sigmastate.interpreter.ErgoTreeEvaluator.DataBlockSize]]
    */
  override val costKind = PerItemCost(baseCost = 20, perChunkCost = 7, chunkSize = 128)

  override def argInfos: Seq[ArgInfo] = CalcBlake2b256Info.argInfos
}

/**
  * Calculate Sha256 hash from `input`
  */
case class CalcSha256(override val input: Value[SByteArray]) extends CalcHash {
  override def companion = CalcSha256
  override val hashFn: CryptographicHash32 = Sha256
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Coll[Byte]](env)
    addSeqCost(CalcSha256.costKind, inputV.length) { () =>
      SigmaDsl.sha256(inputV)
    }
  }
}
object CalcSha256 extends SimpleTransformerCompanion {
  override def opCode: OpCode = OpCodes.CalcSha256Code
  /** perChunkCost - cost of hashing 64 bytes of data (see also CalcBlake2b256). */
  override val costKind = PerItemCost(baseCost = 80, perChunkCost = 8, chunkSize = 64)
  override def argInfos: Seq[ArgInfo] = CalcSha256Info.argInfos
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
  override def companion = SubstConstants
  override val opType = SubstConstants.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val scriptBytesV = scriptBytes.evalTo[Coll[Byte]](env)
    val positionsV = positions.evalTo[Coll[Int]](env)
    val newValuesV = newValues.evalTo[Coll[T#WrappedType]](env)
    var res: Coll[Byte] = null
    E.addSeqCost(SubstConstants.costKind, SubstConstants.opDesc) { () =>
      val typedNewVals: Array[SValue] = newValuesV.toArray.map { v =>
        TransformingSigmaBuilder.liftAny(v) match {
          case Nullable(v) => v
          case _ => sys.error(s"Cannot evaluate substConstants($scriptBytesV, $positionsV, $newValuesV): cannot lift value $v")
        }
      }

      val (newBytes, nConstants) = SubstConstants.eval(
        scriptBytes = scriptBytesV.toArray,
        positions = positionsV.toArray,
        newVals = typedNewVals)(SigmaDsl.validationSettings)

      res = Colls.fromArray(newBytes)
      nConstants
    }
    res
  }
}

object SubstConstants extends ValueCompanion {
  override def opCode: OpCode = OpCodes.SubstConstantsCode
  override val costKind = PerItemCost(100, 100, 1)

  val OpType = SFunc(Array(SByteArray, SIntArray, SCollection(SType.tT)), SByteArray)

  /** Transforms serialized bytes of ErgoTree with segregated constants by
    * replacing constants at given positions with new values. This operation
    * allow to use serialized scripts as pre-defined templates.
    * See [[sigmastate.SubstConstants]] for details.
    *
    * @param scriptBytes serialized ErgoTree with ConstantSegregationFlag set to 1.
    * @param positions   zero based indexes in ErgoTree.constants array which
    *                    should be replaced with new values
    * @param newVals     new values to be injected into the corresponding
    *                    positions in ErgoTree.constants array
    * @return original scriptBytes array where only specified constants are
    *         replaced and all other bytes remain exactly the same
    */
  def eval(scriptBytes: Array[Byte],
           positions: Array[Int],
           newVals: Array[Value[SType]])(implicit vs: SigmaValidationSettings): (Array[Byte], Int) =
    ErgoTreeSerializer.DefaultSerializer.substituteConstants(scriptBytes, positions, newVals)
}

/**
  * A tree node with left and right descendants
  */
sealed trait Triple[LIV <: SType, RIV <: SType, OV <: SType] extends NotReadyValue[OV] {
  val left: Value[LIV]
  val right: Value[RIV]
}

sealed trait OneArgumentOperation[IV <: SType, OV <: SType] extends NotReadyValue[OV] {
  val input: Value[IV]
  override def opType = SFunc(input.tpe, tpe)
}
trait OneArgumentOperationCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
}

// TwoArgumentsOperation
sealed trait TwoArgumentsOperation[LIV <: SType, RIV <: SType, OV <: SType]
  extends Triple[LIV, RIV, OV]

trait TwoArgumentOperationCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
}

/** Represents binary operation with the given opCode. */
case class ArithOp[T <: SType](left: Value[T], right: Value[T], override val opCode: OpCode)
  extends TwoArgumentsOperation[T, T, T] with NotReadyValue[T] {
  override def companion: ArithOpCompanion = ArithOp.operations(opCode)
  override def tpe: T = left.tpe
  override val opType = SFunc(Array[SType](left.tpe, right.tpe), tpe)
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

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val x = left.evalTo[Any](env)
    val y = right.evalTo[Any](env)
    companion.eval(this, tpe.typeCode, x, y) // NOTE: cost is added as part of eval call
  }
}
/** NOTE: by-name argument is required for correct initialization order. */
abstract class ArithOpCompanion(val opCode: OpCode, val name: String, _argInfos: => Seq[ArgInfo])
  extends TwoArgumentOperationCompanion {
  override def argInfos: Seq[ArgInfo] = _argInfos
  override def costKind: TypeBasedCost
  @inline final def eval(node: SValue, typeCode: SType.TypeCode, x: Any, y: Any)(implicit E: ErgoTreeEvaluator): Any = {
    val impl = ArithOp.impls(typeCode)
    node.addCost(costKind, impl.argTpe) { () =>
      eval(impl, x, y)
    }
  }
  def eval(impl: OperationImpl, x: Any, y: Any): Any
}

object ArithOp {
  import OpCodes._

  /** Addition operation `x + y`. */
  object Plus     extends ArithOpCompanion(PlusCode,     "+", PlusInfo.argInfos) {
    override def eval(impl: OperationImpl, x: Any, y: Any): Any = impl.i.plus(x, y)
    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    override val costKind = new TypeBasedCost {
      override def costFunc(tpe: SType): Int = tpe match {
        case SBigInt => 20
        case _ => 15
      }
    }
  }

  /** Subtraction operation `x - y`. */
  object Minus    extends ArithOpCompanion(MinusCode,    "-", MinusInfo.argInfos) {
    override def eval(impl: OperationImpl, x: Any, y: Any): Any = impl.i.minus(x, y)
    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    override val costKind = new TypeBasedCost {
      override def costFunc(tpe: SType): Int = tpe match {
        case SBigInt => 20
        case _ => 15
      }
    }
  }

  /** Multiplication operation `x * y`. */
  object Multiply extends ArithOpCompanion(MultiplyCode, "*", MultiplyInfo.argInfos) {
    override def eval(impl: OperationImpl, x: Any, y: Any): Any = impl.i.times(x, y)
    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Numeric
      */
    override val costKind = new TypeBasedCost {
      override def costFunc(tpe: SType): Int = tpe match {
        case SBigInt => 25
        case _ => 15
      }
    }
  }

  /** Integer division operation `x / y`. */
  object Division extends ArithOpCompanion(DivisionCode, "/", DivisionInfo.argInfos) {
    override def eval(impl: OperationImpl, x: Any, y: Any): Any = impl.i.quot(x, y)
    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Integral
      */
    override val costKind = new TypeBasedCost {
      override def costFunc(tpe: SType): Int = tpe match {
        case SBigInt => 25
        case _ => 15
      }
    }
  }

  /** Operation which returns remainder from dividing x by y.
    * See ExactIntegral.divisionRemainder implementation for the concrete numeric type.
    */
  object Modulo extends ArithOpCompanion(ModuloCode,   "%", ModuloInfo.argInfos) {
    override def eval(impl: OperationImpl, x: Any, y: Any): Any = impl.i.divisionRemainder(x, y)
    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of Integral
      */
    override val costKind = new TypeBasedCost {
      override def costFunc(tpe: SType): Int = tpe match {
        case SBigInt => 25
        case _ => 15
      }
    }
  }

  /** Return `x` if `x` <= `y`, otherwise `y`. */
  object Min extends ArithOpCompanion(MinCode,      "min", MinInfo.argInfos) {
    override def eval(impl: OperationImpl, x: Any, y: Any): Any = impl.o.min(x, y)
    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of ExactOrdering
      */
    override val costKind = new TypeBasedCost {
      override def costFunc(tpe: SType): Int = tpe match {
        case SBigInt => 10
        case _ => 5
      }
    }
  }

  /** Return `x` if `x` >= `y`, otherwise `y`. */
  object Max extends ArithOpCompanion(MaxCode,      "max", MaxInfo.argInfos) {
    override def eval(impl: OperationImpl, x: Any, y: Any): Any = impl.o.max(x, y)
    /** Cost of:
      * 1) resolving ArithOpCompanion by typeCode
      * 2) calling method of ExactOrdering
      */
    override val costKind = new TypeBasedCost {
      override def costFunc(tpe: SType): Int = tpe match {
        case SBigInt => 10
        case _ => 5
      }
    }
  }

  private[sigmastate] val operations: DMap[Byte, ArithOpCompanion] =
    DMap.fromIterable(Seq(Plus, Minus, Multiply, Division, Modulo, Min, Max).map(o => (o.opCode, o)))

  /** Represents implementation of numeric Arith operations for the given type argTpe. */
  class OperationImpl(_i: ExactIntegral[_], _o: ExactOrdering[_], val argTpe: SType) {
    val i = _i.asInstanceOf[ExactIntegral[Any]]
    val o = _o.asInstanceOf[ExactOrdering[Any]]
  }

  private[sigmastate] val impls: DMap[SType.TypeCode, OperationImpl] =
    DMap.fromIterable(Seq(
      SByte   -> new OperationImpl(ByteIsExactIntegral,   ByteIsExactOrdering,   SByte),
      SShort  -> new OperationImpl(ShortIsExactIntegral,  ShortIsExactOrdering,  SShort),
      SInt    -> new OperationImpl(IntIsExactIntegral,    IntIsExactOrdering,    SInt),
      SLong   -> new OperationImpl(LongIsExactIntegral,   LongIsExactOrdering,   SLong),
      SBigInt -> new OperationImpl(BigIntIsExactIntegral, BigIntIsExactOrdering, SBigInt)
    ).map { case (t, n) => (t.typeCode, n) })

  /** Returns operation name for the given opCode. */
  def opcodeToArithOpName(opCode: Byte): String = operations.get(opCode) match {
    case Some(c)  => c.name
    case _ => sys.error(s"Cannot find ArithOpName for opcode $opCode")
  }
}

/** Negation operation on numeric type T.
  * See ExactNumeric instance for the corresponding type T.
  */
case class Negation[T <: SType](input: Value[T]) extends OneArgumentOperation[T, T] {
  require(input.tpe.isNumTypeOrNoType, s"invalid type ${input.tpe}")
  override def companion = Negation
  override def tpe: T = input.tpe
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[AnyVal](env)
    val i = ArithOp.impls(input.tpe.typeCode).i
    addCost(Negation.costKind)
    i.negate(inputV)
  }
}
object Negation extends OneArgumentOperationCompanion {
  override def opCode: OpCode = OpCodes.NegationCode
  override val costKind = FixedCost(30)
  override def argInfos: Seq[ArgInfo] = NegationInfo.argInfos
}

/** Not implemented in v4.x. */
case class BitInversion[T <: SType](input: Value[T]) extends OneArgumentOperation[T, T] {
  require(input.tpe.isNumTypeOrNoType, s"invalid type ${input.tpe}")
  override def companion = BitInversion
  override def tpe: T = input.tpe
}
object BitInversion extends OneArgumentOperationCompanion {
  override def opCode: OpCode = OpCodes.BitInversionCode
  override def costKind: CostKind = Value.notSupportedError(this, "costKind")
  override def argInfos: Seq[ArgInfo] = BitInversionInfo.argInfos
}

/** ErgoTree node which represents a binary bit-wise operation with the given opCode. */
case class BitOp[T <: SType](left: Value[T], right: Value[T], override val opCode: OpCode)
  extends TwoArgumentsOperation[T, T, T] with NotReadyValue[T] {
  require(left.tpe.isNumTypeOrNoType && right.tpe.isNumTypeOrNoType, s"invalid types left:${left.tpe}, right:${right.tpe}")
  override def companion = BitOp.operations(opCode)
  override def tpe: T = left.tpe
  override val opType = SFunc(Array[SType](left.tpe, right.tpe), tpe)
}
/** NOTE: by-name argument is required for correct initialization order. */
abstract class BitOpCompanion(val opCode: OpCode, val name: String, _argInfos: => Seq[ArgInfo]) extends TwoArgumentOperationCompanion {
  override def argInfos: Seq[ArgInfo] = _argInfos
}

object BitOp {
  import OpCodes._
  object BitOr     extends BitOpCompanion(BitOrCode,  "|", BitOrInfo.argInfos) {
    override val costKind = FixedCost(1)
  }
  object BitAnd    extends BitOpCompanion(BitAndCode, "&", BitAndInfo.argInfos) {
    override val costKind = FixedCost(1)
  }
  object BitXor    extends BitOpCompanion(BitXorCode, "^", BitXorInfo.argInfos) {
    override val costKind = FixedCost(1)
  }
  object BitShiftRight extends BitOpCompanion(BitShiftRightCode, ">>", BitShiftRightInfo.argInfos) {
    override val costKind = FixedCost(1)
  }
  object BitShiftLeft  extends BitOpCompanion(BitShiftLeftCode,   "<<", BitShiftLeftInfo.argInfos) {
    override val costKind = FixedCost(1)
  }
  object BitShiftRightZeroed extends BitOpCompanion(BitShiftRightZeroedCode, ">>>", BitShiftRightZeroedInfo.argInfos) {
    override val costKind = FixedCost(1)
  }

  val operations: Map[Byte, BitOpCompanion] =
    Seq(BitOr, BitAnd, BitXor, BitShiftRight, BitShiftLeft, BitShiftRightZeroed).map(o => (o.opCode, o)).toMap

  def opcodeToName(opCode: Byte): String = operations.get(opCode) match {
    case Some(c)  => c.name
    case _ => sys.error(s"Cannot find BitOpName for opcode $opCode")
  }
}

// TODO HF (24h): implement modular operations
case class ModQ(input: Value[SBigInt.type])
  extends NotReadyValue[SBigInt.type] {
  override def companion = ModQ
  override def tpe: SBigInt.type = SBigInt
  override def opType: SFunc = SFunc(input.tpe, tpe)
}
object ModQ extends ValueCompanion {
  override def opCode: OpCode = OpCodes.ModQCode
  override val costKind: CostKind = FixedCost(1)
}

case class ModQArithOp(left: Value[SBigInt.type], right: Value[SBigInt.type], override val opCode: OpCode)
  extends NotReadyValue[SBigInt.type] {
  override def companion = ModQArithOp.operations(opCode)
  override def tpe: SBigInt.type = SBigInt
  override def opType: SFunc = SFunc(Array(left.tpe, right.tpe), tpe)
}
abstract class ModQArithOpCompanion(val opCode: OpCode, val name: String) extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
  override val costKind: CostKind = FixedCost(1)
}

trait OpGroup[C <: ValueCompanion] {
  def operations: Map[Byte, C]
}

object ModQArithOp extends OpGroup[ModQArithOpCompanion] {
  import OpCodes._
  object PlusModQ extends ModQArithOpCompanion(PlusModQCode,  "PlusModQ") {
    // TODO soft-fork:
    // override def argInfos: Seq[ArgInfo] = PlusModQInfo.argInfos
    override def argInfos: Seq[ArgInfo] = Seq(ArgInfo("this", ""), ArgInfo("other", ""))
  }
  object MinusModQ extends ModQArithOpCompanion(MinusModQCode, "MinusModQ") {
    // TODO soft-fork:
    // override def argInfos: Seq[ArgInfo] = MinusModQInfo.argInfos
    override def argInfos: Seq[ArgInfo] = Seq(ArgInfo("this", ""), ArgInfo("other", ""))
  }

  val operations: Map[Byte, ModQArithOpCompanion] = Seq(PlusModQ, MinusModQ).map(o => (o.opCode, o)).toMap

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
  override def opType = Xor.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val lV = left.evalTo[Coll[Byte]](env)
    val rV = right.evalTo[Coll[Byte]](env)
    addSeqCost(Xor.costKind, lV.length) { () =>
      Colls.xor(lV, rV)
    }
  }
}
object Xor extends TwoArgumentOperationCompanion {
  val OpType = SFunc(Array(SByteArray, SByteArray), SByteArray)
  override def opCode: OpCode = XorCode
  override val costKind = PerItemCost(10, 2, 128)
  override def argInfos: Seq[ArgInfo] = XorInfo.argInfos
}

case class Exponentiate(override val left: Value[SGroupElement.type],
                        override val right: Value[SBigInt.type])
  extends TwoArgumentsOperation[SGroupElement.type, SBigInt.type, SGroupElement.type]
    with NotReadyValueGroupElement {
  override def companion = Exponentiate
  override def opType = Exponentiate.OpType

  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val leftV = left.evalTo[GroupElement](env)
    val rightV = right.evalTo[special.sigma.BigInt](env)
    addCost(Exponentiate.costKind)
    leftV.exp(rightV)
  }
}
object Exponentiate extends TwoArgumentOperationCompanion with FixedCostValueCompanion {
  val OpType = SFunc(Array(SGroupElement, SBigInt), SGroupElement)
  override def opCode: OpCode = ExponentiateCode
  /** Cost of: 1) calling EcPoint.multiply 2) wrapping in GroupElement */
  override val costKind = FixedCost(900)
  override def argInfos: Seq[ArgInfo] = ExponentiateInfo.argInfos
}

case class MultiplyGroup(override val left: Value[SGroupElement.type],
                         override val right: Value[SGroupElement.type])
  extends TwoArgumentsOperation[SGroupElement.type, SGroupElement.type, SGroupElement.type]
    with NotReadyValueGroupElement {
  override def companion = MultiplyGroup
  override def opType = MultiplyGroup.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val leftV = left.evalTo[GroupElement](env)
    val rightV = right.evalTo[GroupElement](env)
    addCost(MultiplyGroup.costKind)
    leftV.multiply(rightV)
  }
}
object MultiplyGroup extends TwoArgumentOperationCompanion with FixedCostValueCompanion {
  val OpType = SFunc(Array(SGroupElement, SGroupElement), SGroupElement)
  override def opCode: OpCode = MultiplyGroupCode
  /** Cost of: 1) calling EcPoint.add 2) wrapping in GroupElement */
  override val costKind = FixedCost(40)
  override def argInfos: Seq[ArgInfo] = MultiplyGroupInfo.argInfos
}
// Relation

sealed trait Relation[LIV <: SType, RIV <: SType] extends Triple[LIV, RIV, SBoolean.type]
  with NotReadyValueBoolean

trait SimpleRelation[T <: SType] extends Relation[T, T] {
  override def opType = SimpleRelation.GenericOpType
  lazy val opImpl = ArithOp.impls(left.tpe.typeCode)
}
object SimpleRelation {
  val GenericOpType = SFunc(SType.IndexedSeqOfT2, SBoolean)
}

trait RelationCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
}

/**
  * Less operation for SInt
  */
case class LT[T <: SType](override val left: Value[T], override val right: Value[T]) extends SimpleRelation[T] {
  override def companion = LT
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val lV = left.evalTo[Any](env)
    val rV = right.evalTo[Any](env)
    addCost(LT.costKind, left.tpe) { () =>
      opImpl.o.lt(lV, rV)
    }
  }
}
object LT extends RelationCompanion {
  override def opCode: OpCode = LtCode
  /** Cost of:
    * 1) resolving ArithOpCompanion by typeCode
    * 2) calling method of Numeric
    */
  override val costKind = new TypeBasedCost {
    override def costFunc(tpe: SType): Int = tpe match {
      case SBigInt => 20
      case _ => 20
    }
  }
  override def argInfos: Seq[ArgInfo] = LTInfo.argInfos
}
/**
  * Less or equals operation for SInt
  */
case class LE[T <: SType](override val left: Value[T], override val right: Value[T]) extends SimpleRelation[T] {
  override def companion = LE
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val lV = left.evalTo[Any](env)
    val rV = right.evalTo[Any](env)
    addCost(LE.costKind, left.tpe) { () =>
      opImpl.o.lteq(lV, rV)
    }
  }
}
object LE extends RelationCompanion {
  override def opCode: OpCode = LeCode
  /** Cost of:
    * 1) resolving ArithOpCompanion by typeCode
    * 2) calling method of Numeric
    */
  override val costKind = new TypeBasedCost {
    override def costFunc(tpe: SType): Int = tpe match {
      case SBigInt => 20 // cf. comparisonBigInt
      case _ => 20 // cf. comparisonCost
    }
  }
  override def argInfos: Seq[ArgInfo] = LEInfo.argInfos
}
/**
  * Greater operation for [[SNumericType]] values
  */
case class GT[T <: SType](override val left: Value[T], override val right: Value[T]) extends SimpleRelation[T] {
  override def companion = GT
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val lV = left.evalTo[Any](env)
    val rV = right.evalTo[Any](env)
    addCost(GT.costKind, left.tpe) { () =>
      opImpl.o.gt(lV, rV)
    }
  }
}
object GT extends RelationCompanion {
  override def opCode: OpCode = GtCode
  /** Cost of:
    * 1) resolving ArithOpCompanion by typeCode
    * 2) calling method of Numeric
    */
  override val costKind = new TypeBasedCost {
    override def costFunc(tpe: SType): Int = tpe match {
      case SBigInt => 20 // cf. comparisonBigInt
      case _ => 20 // cf. comparisonCost
    }
  }
  override def argInfos: Seq[ArgInfo] = GTInfo.argInfos
}
/**
  * Greater or equals operation for SInt
  */
case class GE[T <: SType](override val left: Value[T], override val right: Value[T]) extends SimpleRelation[T] {
  override def companion = GE
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val lV = left.evalTo[Any](env)
    val rV = right.evalTo[Any](env)
    addCost(GE.costKind, left.tpe) { () =>
      opImpl.o.gteq(lV, rV)
    }
  }
}
object GE extends RelationCompanion {
  override def opCode: OpCode = GeCode
  /** Cost of:
    * 1) resolving ArithOpCompanion by typeCode
    * 2) calling method of Numeric
    */
  override val costKind = new TypeBasedCost {
    override def costFunc(tpe: SType): Int = tpe match {
      case SBigInt => 20
      case _ => 20
    }
  }
  override def argInfos: Seq[ArgInfo] = GEInfo.argInfos
}

/**
  * Equals operation for SType
  * todo: make EQ to really accept only values of the same type, now EQ(TrueLeaf, IntConstant(5)) is valid
  */
case class EQ[S <: SType](
    override val left: Value[S],
    override val right: Value[S]) extends SimpleRelation[S] {
  override def companion = EQ
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val l = left.evalTo[S#WrappedType](env)
    Value.checkType(left, l) // necessary because cast to S#WrappedType is erased
    val r = right.evalTo[S#WrappedType](env)
    Value.checkType(right, r) // necessary because cast to S#WrappedType is erased
    DataValueComparer.equalDataValues(l, r)
  }
}
object EQ extends RelationCompanion {
  override def opCode: OpCode = EqCode
  override def costKind = DynamicCost
  override def argInfos: Seq[ArgInfo] = EQInfo.argInfos
}

/**
  * Non-Equals operation for SType
  */
case class NEQ[S <: SType](override val left: Value[S], override val right: Value[S])
  extends SimpleRelation[S] {
  override def companion = NEQ
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val l = left.evalTo[S#WrappedType](env)
    Value.checkType(left, l) // necessary because cast to S#WrappedType is erased
    val r = right.evalTo[S#WrappedType](env)
    Value.checkType(right, r) // necessary because cast to S#WrappedType is erased
    !DataValueComparer.equalDataValues(l, r)
  }
}
object NEQ extends RelationCompanion {
  override def opCode: OpCode = NeqCode
  override def costKind = DynamicCost
  override def argInfos: Seq[ArgInfo] = NEQInfo.argInfos
}

/**
  * Logical OR with lazy right argument which is evaluated only if left == false.
  * If left argument is true, the right is guaranteed to NOT be evaluated
  */
case class BinOr(override val left: BoolValue, override val right: BoolValue)
  extends Relation[SBoolean.type, SBoolean.type] {
  override def companion = BinOr
  override def opType = BinOr.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val l = left.evalTo[Boolean](env)
    addCost(BinOr.costKind)
    l || right.evalTo[Boolean](env)  // rely on short-cutting semantics of Scala's ||
  }
}
object BinOr extends RelationCompanion with FixedCostValueCompanion {
  val OpType = SFunc(Array(SBoolean, SBoolean), SBoolean)
  override def opCode: OpCode = BinOrCode
  /** Cost of: scala `||` operation
    * Old cost: ("BinOr", "(Boolean, Boolean) => Boolean", logicCost) */
  override val costKind = FixedCost(20)
  override def argInfos: Seq[ArgInfo] = BinOrInfo.argInfos
}

/**
  * Logical AND with lazy right argument which is evaluated only if left == true.
  * If left argument is false, the right is guaranteed to NOT be evaluated
  */
case class BinAnd(override val left: BoolValue, override val right: BoolValue)
  extends Relation[SBoolean.type, SBoolean.type] {
  override def companion = BinAnd
  override def opType = BinAnd.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val l = left.evalTo[Boolean](env)
    addCost(BinAnd.costKind)
    l && right.evalTo[Boolean](env)  // rely on short-cutting semantics of Scala's &&
  }
}
object BinAnd extends RelationCompanion with FixedCostValueCompanion {
  val OpType = SFunc(Array(SBoolean, SBoolean), SBoolean)
  override def opCode: OpCode = BinAndCode
  /** Cost of: scala `&&` operation
    * Old cost: ("BinAnd", "(Boolean, Boolean) => Boolean", logicCost) */
  override val costKind = FixedCost(20)
  override def argInfos: Seq[ArgInfo] = BinAndInfo.argInfos
}

case class BinXor(override val left: BoolValue, override val right: BoolValue)
  extends Relation[SBoolean.type, SBoolean.type] {
  override def companion = BinXor
  override def opType = BinXor.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val leftV = left.evalTo[Boolean](env)
    val rightV = right.evalTo[Boolean](env)
    addCost(BinXor.costKind)
    leftV ^ rightV
  }
}
object BinXor extends RelationCompanion with FixedCostValueCompanion {
  val OpType = SFunc(Array(SBoolean, SBoolean), SBoolean)
  override def opCode: OpCode = BinXorCode
  /** Cost of: scala `^` operation
    * Old cost: ("BinXor", "(Boolean, Boolean) => Boolean", logicCost) */
  override val costKind = FixedCost(20)
  override def argInfos: Seq[ArgInfo] = BinXorInfo.argInfos
}

/**
  * A tree node with three descendants
  */
sealed trait Quadruple[IV1 <: SType, IV2 <: SType, IV3 <: SType, OV <: SType] extends NotReadyValue[OV] {

  val first: Value[IV1]
  val second: Value[IV2]
  val third: Value[IV3]

  val opType = SFunc(Array(first.tpe, second.tpe, third.tpe), tpe)
}

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
trait QuadrupleCompanion extends ValueCompanion {
  def argInfos: Seq[ArgInfo]
}
object TreeLookup extends QuadrupleCompanion {
  override def opCode: OpCode = OpCodes.AvlTreeGetCode
  override def costKind: CostKind = Value.notSupportedError(this, "costKind")
  override def argInfos: Seq[ArgInfo] = TreeLookupInfo.argInfos
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
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val c = condition.evalTo[Boolean](env)
    addCost(If.costKind)
    if (c) {
      val res = trueBranch.evalTo[T#WrappedType](env)
      Value.checkType(trueBranch, res) // necessary because cast to T#WrappedType is erased
      res
    } else {
      val res = falseBranch.evalTo[T#WrappedType](env)
      Value.checkType(falseBranch, res) // necessary because cast to T#WrappedType is erased
      res
    }
  }
}
object If extends QuadrupleCompanion {
  override def opCode: OpCode = OpCodes.IfCode
  /** Cost of: conditional switching to the right branch (excluding the cost both
    * condition itself and the branches) */
  override val costKind = FixedCost(10)
  override def argInfos: Seq[ArgInfo] = IfInfo.argInfos
  val GenericOpType = SFunc(Array(SBoolean, SType.tT, SType.tT), SType.tT)
}

case class LogicalNot(input: Value[SBoolean.type]) extends NotReadyValueBoolean {
  override def companion = LogicalNot
  override def opType = LogicalNot.OpType
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    val inputV = input.evalTo[Boolean](env)
    addCost(LogicalNot.costKind)
    !inputV
  }
}
object LogicalNot extends FixedCostValueCompanion {
  val OpType = SFunc(Array(SBoolean), SBoolean)
  override def opCode: OpCode = OpCodes.LogicalNotCode
  /** Cost of: scala `!` operation */
  override val costKind = FixedCost(15)
}


