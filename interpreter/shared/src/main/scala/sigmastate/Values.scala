package sigmastate

import java.math.BigInteger
import java.util.{Arrays, Objects}
import sigma.kiama.rewriting.Rewriter.{count, everywherebu, strategy}
import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.validation.ValidationException
import sigma.data.{Nullable, RType}
import sigma.util.CollectionUtil._
import sigmastate.crypto.CryptoConstants.EcPointType
import sigmastate.interpreter.{CompanionDesc, ErgoTreeEvaluator, Interpreter, NamedDesc}
import sigmastate.serialization._
import sigmastate.serialization.OpCodes._
import sigmastate.TrivialProp.{FalseProp, TrueProp}
import sigmastate.Values.ErgoTree.substConstants
import sigmastate.crypto.DLogProtocol.ProveDlog
import sigmastate.crypto.{CryptoConstants, ProveDHTuple}
import sigmastate.lang.Terms._
import sigmastate.utxo._
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigma.util.Extensions.ByteOps
import sigmastate.interpreter.ErgoTreeEvaluator._
import debox.cfor
import scorex.util.encode.Base16
import sigma.ast.SCollection.{SByteArray, SIntArray}
import sigma.ast._
import sigmastate.exceptions.InterpreterException

import scala.language.implicitConversions
import sigmastate.lang.CheckingSigmaBuilder._
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.serialization.transformers.ProveDHTupleSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigma.{AvlTree, Coll, Colls, Header, PreHeader, _}
import sigmastate.lang.SourceContext
import sigma.util.safeNewArray
import sigmastate.serialization.ValueCodes.{ConstantCode, OpCode}

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable

object Values {
  /** Force initialization of reflection. */
  val reflection = InterpreterReflection

  type SValue = Value[SType]

  /** Base class for all ErgoTree expression nodes.
    * @see [[sigmastate.Values.ErgoTree]]
    */
  abstract class Value[+S <: SType] extends SigmaNode {
    /** The companion node descriptor with opCode, cost and other metadata. */
    def companion: ValueCompanion

    /** Unique id of the node class used in serialization of ErgoTree. */
    def opCode: OpCode = companion.opCode

    /** The type of the value represented by this node. If the value is an operation it is
      * the type of operation result. */
    def tpe: S

    /** Every value represents an operation and that operation can be associated with a function type,
      * describing functional meaning of the operation, kind of operation signature.
      * Thus, we can obtain global operation identifiers by combining Value.opName with Value.opType,
      * so that if (v1.opName == v2.opName) && (v1.opType == v2.opType) then v1 and v2 are functionally
      * point-wise equivalent.
      * This in particular means that if two _different_ ops have the same opType they _should_ have
      * different opNames.
      * Thus defined op ids are used in a v4.x Cost Model - a table of all existing primitives coupled with
      * performance parameters.
      * */
    def opType: SFunc

    /** Name of the operation. */
    def opName: String = this.getClass.getSimpleName

    /** Transforms this expression to SigmaProp expression or throws an exception. */
    def toSigmaProp: SigmaPropValue = this match {
      case b if b.tpe == SBoolean => BoolToSigmaProp(this.asBoolValue)
      case p if p.tpe == SSigmaProp => p.asSigmaProp
      case _ => sys.error(s"Expected SBoolean or SSigmaProp typed value, but was: $this")
    }

    /** Parser has some source information like line,column in the text. We need to keep it up until RuntimeCosting.
    * The way to do this is to add Nullable property to every Value. Since Parser is always using SigmaBuilder
    * to create nodes,
    * Adding additional (implicit source: SourceContext) parameter to every builder method would pollute its API
    * and also doesn't make sence during deserialization, where Builder is also used.
    * We can assume some indirect mechanism to pass current source context into every mkXXX method of Builder.
    * We can pass it using `scala.util.DynamicVariable` by wrapping each mkXXX call into `withValue { }` calls.
    * The same will happen in Typer.
    * We can take sourceContext from untyped nodes and use it while creating typed nodes.
    * And we can make sourceContext of every Value writeOnce value, i.e. it will be Nullable.Null by default,
    * but can be set afterwards, but only once.
    * This property will not participate in equality and other operations, so will be invisible for existing code.
    * But Builder can use it to set sourceContext if it is present.
    */
    private[sigmastate] var _sourceContext: Nullable[SourceContext] = Nullable.None
    def sourceContext: Nullable[SourceContext] = _sourceContext
    def sourceContext_=(srcCtx: Nullable[SourceContext]): Unit =
      if (_sourceContext.isEmpty) {
        _sourceContext = srcCtx
      } else {
        sys.error("_sourceContext can be set only once")
      }

    /** Defines an evaluation semantics of this tree node (aka Value or expression) in the given data environment.
      * Should be implemented by all the ErgoTree nodes (aka operations).
      * Thus, the ErgoTree interpreter implementation consists of combined implementations of this method.
      * NOTE, this method shouldn't be called directly, instead use `evalTo` method.
      *
      * @param  E   Evaluator which defines evaluation context, cost accumulator, settings etc.
      * @param  env immutable map, which binds variables (given by ids) to the values
      * @return the data value which is the result of evaluation
      */
    protected def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any =
      sys.error(s"Should be overriden in ${this.getClass}: $this")

    /** Evaluates this node to the value of the given expected type.
      * This method should called from all `eval` implementations.
      *
      * @tparam T expected type of the resulting value
      * @param  E   Evaluator which defines evaluation context, cost accumulator, settings etc.
      * @param  env immutable map, which binds variables (given by ids) to the values
      * @return the data value which is the result of evaluation
      */
    @inline
    final def evalTo[T](env: DataEnv)(implicit E: ErgoTreeEvaluator): T = {
      if (E.settings.isMeasureOperationTime) E.profiler.onBeforeNode(this)
      val v = eval(env)
      if (E.settings.isMeasureOperationTime) E.profiler.onAfterNode(this)
      v.asInstanceOf[T]
    }

    /** Add the cost given by the kind to the accumulator and associate it with this operation
      * node.
      */
    @inline
    final def addCost(costKind: FixedCost)(implicit E: ErgoTreeEvaluator): Unit = {
      E.addCost(costKind, this.companion.opDesc)
    }

    /** Add the cost given by the descriptor to the accumulator and associate it with this operation
      * node.
      */
    @inline
    final def addCost[R](costKind: TypeBasedCost, tpe: SType)(block: () => R)(implicit E: ErgoTreeEvaluator): R = {
      E.addTypeBasedCost(costKind, tpe, this.companion.opDesc)(block)
    }

    /** Add the cost of a repeated operation to the accumulator and associate it with this
      * operation. The number of items (loop iterations) is known in advance (like in
      * Coll.map operation)
      *
      * @param costKind cost descriptor of the operation
      * @param nItems   number of operations known in advance (before loop execution)
      */
    @inline
    final def addSeqCostNoOp(costKind: PerItemCost, nItems: Int)
                                (implicit E: ErgoTreeEvaluator): Unit = {
      E.addSeqCostNoOp(costKind, nItems, this.companion.opDesc)
    }

    /** Add the cost of a repeated operation to the accumulator and associate it with this
      * operation. The number of items (loop iterations) is known in advance (like in
      * Coll.map operation)
      *
      * @param costKind cost descriptor of the operation
      * @param nItems   number of operations known in advance (before loop execution)
      * @param block    operation executed under the given cost
      * @tparam R result type of the operation
      */
    @inline
    final def addSeqCost[R](costKind: PerItemCost, nItems: Int)
                           (block: () => R)(implicit E: ErgoTreeEvaluator): R = {
      E.addSeqCost(costKind, nItems, this.companion.opDesc)(block)
    }
  }

  object Value {
    type PropositionCode = Byte

    implicit def liftByte (n: Byte) : Value[SByte.type]  = ByteConstant(n)
    implicit def liftShort(n: Short): Value[SShort.type] = ShortConstant(n)
    implicit def liftInt  (n: Int)  : Value[SInt.type]   = IntConstant(n)
    implicit def liftLong (n: Long) : Value[SLong.type]  = LongConstant(n)

    implicit def liftByteArray(arr: Array[Byte]): Value[SByteArray] = ByteArrayConstant(arr)

    implicit def liftBigInt(arr: BigInt): Value[SBigInt.type] = BigIntConstant(arr)

    implicit def liftGroupElement(g: GroupElement): Value[SGroupElement.type] = GroupElementConstant(g)
    implicit def liftECPoint(g: EcPointType): Value[SGroupElement.type] = GroupElementConstant(g)

    implicit def liftSigmaProp(g: SigmaProp): Value[SSigmaProp.type] = SigmaPropConstant(g)
    implicit def liftSigmaBoolean(sb: SigmaBoolean): Value[SSigmaProp.type] = SigmaPropConstant(SigmaDsl.SigmaProp(sb))

    object Typed {
      def unapply(v: SValue): Option[(SValue, SType)] = Some((v, v.tpe))
    }
    def notSupportedError(v: Any, opName: String) =
      throw new IllegalArgumentException(s"Method $opName is not supported for node $v")

    /** Immutable empty array of values. Can be used to avoid allocation. */
    val EmptyArray = Array.empty[SValue]

    /** Immutable empty Seq of values. Can be used to avoid allocation. */
    val EmptySeq: IndexedSeq[SValue] = EmptyArray

    /** Traverses the given expression tree and counts the number of deserialization
      * operations. If it is non-zero, returns true. */
    def hasDeserialize(exp: SValue): Boolean = {
      val deserializeNode: PartialFunction[Any, Int] = {
        case _: DeserializeContext[_] => 1
        case _: DeserializeRegister[_] => 1
      }
      val c = count(deserializeNode)(exp)
      c > 0
    }

    def typeError(node: SValue, evalResult: Any) = {
      val tpe = node.tpe
      throw new InterpreterException(
        s"""Invalid type returned by evaluator:
          |  expression: $node
          |  expected type: $tpe
          |  resulting value: $evalResult
            """.stripMargin)
    }

    def typeError(tpe: SType, evalResult: Any) = {
      throw new InterpreterException(
        s"""Invalid type returned by evaluator:
          |  expected type: $tpe
          |  resulting value: $evalResult
            """.stripMargin)
    }

    def checkType(node: SValue, evalResult: Any) = {
      val tpe = node.tpe
      if (!SType.isValueOfType(evalResult, tpe))
        typeError(node, evalResult)
    }

    def checkType(tpe: SType, evalResult: Any) = {
      if (!SType.isValueOfType(evalResult, tpe))
        typeError(tpe, evalResult)
    }
  }

  /** Base class for all companion objects which are used as operation descriptors. */
  trait ValueCompanion extends SigmaNodeCompanion {
    import ValueCompanion._
    /** Unique id of the node class used in serialization of ErgoTree. */
    def opCode: OpCode

    /** Returns cost descriptor of this operation. */
    def costKind: CostKind

    override def toString: String = s"${this.getClass.getSimpleName}(${opCode.toUByte})"

    def typeName: String = this.getClass.getSimpleName.replace("$", "")

    def init() {
      if (this.opCode != 0 && _allOperations.contains(this.opCode))
        throw sys.error(s"Operation $this already defined")
      _allOperations += (this.opCode -> this)
    }

    init()

    val opDesc = CompanionDesc(this)
  }
  object ValueCompanion {
    private val _allOperations: mutable.HashMap[Byte, ValueCompanion] = mutable.HashMap.empty
    lazy val allOperations = _allOperations.toMap
  }

  /** Should be inherited by companion objects of operations with fixed cost kind. */
  trait FixedCostValueCompanion extends ValueCompanion {
    /** Returns cost descriptor of this operation. */
    override def costKind: FixedCost
  }

  /** Should be inherited by companion objects of operations with per-item cost kind. */
  trait PerItemCostValueCompanion extends ValueCompanion {
    /** Returns cost descriptor of this operation. */
    override def costKind: PerItemCost
  }

  /** Base class for ErgoTree nodes which represents a data value which has already been
    * evaluated and no further evaluation (aka reduction) is necessary by the interpreter.
    *
    * @see Constant, ConcreteCollection, Tuple
    */
  abstract class EvaluatedValue[+S <: SType] extends Value[S] {
    /** The evaluated data value of the corresponding underlying data type. */
    val value: S#WrappedType

    override def opType: SFunc = {
      val resType = tpe match {
        case ct : SCollection[_] =>
          SCollection(ct.typeParams.head.ident)
        case ft @ SFunc(_, _, _) =>
          ft.getGenericType
        case _ => tpe
      }
      SFunc(ArraySeq.empty, resType)
    }
  }

  /** Base class for all constant literals whose data value is already known and never
    * changes.
    * @see ConstantNode
    */
  abstract class Constant[+S <: SType] extends EvaluatedValue[S]

  /** ErgoTree node which represents data literals, i.e. data values embedded in an
    * expression.
    *
    * @param value data value of the underlying Scala type
    * @param tpe   type descriptor of the data value and also the type of the value
    *              represented by this node.
    * @see Constant
    */
  case class ConstantNode[S <: SType](value: S#WrappedType, tpe: S) extends Constant[S] {
    require(sigmastate.crypto.Platform.isCorrectType(value, tpe),
      s"Invalid type of constant value $value, expected type $tpe")
    override def companion: ValueCompanion = Constant
    override def opCode: OpCode = companion.opCode
    override def opName: String = s"Const"

    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      addCost(Constant.costKind)
      value
    }

    override def equals(obj: scala.Any): Boolean = (obj != null) && (this.eq(obj.asInstanceOf[AnyRef]) || (obj match {
      case c: Constant[_] => tpe == c.tpe && Objects.deepEquals(value, c.value)
      case _ => false
    }))

    override def hashCode(): Int = Arrays.deepHashCode(Array(value.asInstanceOf[AnyRef], tpe))

    override def toString: String = tpe.asInstanceOf[SType] match {
      case SGroupElement if value.isInstanceOf[GroupElement] =>
        s"ConstantNode(${showECPoint(value.asInstanceOf[GroupElement])},$tpe)"
      case SGroupElement  =>
        sys.error(s"Invalid value in Constant($value, $tpe)")
      case SInt => s"IntConstant($value)"
      case SLong => s"LongConstant($value)"
      case SBoolean if value == true => "TrueLeaf"
      case SBoolean if value == false => "FalseLeaf"
      case _ => s"ConstantNode($value,$tpe)"
    }
  }

  object Constant extends FixedCostValueCompanion {
    override def opCode: OpCode = ConstantCode
    /** Cost of: returning value from Constant node. */
    override val costKind = FixedCost(JitCost(5))

    /** Immutable empty array, can be used to save allocations in many places. */
    val EmptyArray = Array.empty[Constant[SType]]

    /** Immutable empty IndexedSeq, can be used to save allocations in many places. */
    val EmptySeq: IndexedSeq[Constant[SType]] = Array.empty[Constant[SType]]

    /** Helper factory method. */
    def apply[S <: SType](value: S#WrappedType, tpe: S): Constant[S] = ConstantNode(value, tpe)

    /** Recognizer of Constant tree nodes used in patterns. */
    def unapply[S <: SType](v: EvaluatedValue[S]): Option[(S#WrappedType, S)] = v match {
      case ConstantNode(value, tpe) => Some((value, tpe))
      case _ => None
    }

  }

  /** Placeholder for a constant in ErgoTree. Zero based index in ErgoTree.constants array. */
  case class ConstantPlaceholder[S <: SType](id: Int, override val tpe: S) extends Value[S] {
    def opType = SFunc(SInt, tpe)
    override def companion: ValueCompanion = ConstantPlaceholder
    override protected def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      val c = E.constants(id)
      addCost(ConstantPlaceholder.costKind)
      val res = c.value
      Value.checkType(c, res)
      res
    }
  }
  object ConstantPlaceholder extends ValueCompanion {
    override def opCode: OpCode = ConstantPlaceholderCode
    /** Cost of: accessing Constant in array by index. */
    override val costKind = FixedCost(JitCost(1))
  }

  trait NotReadyValue[S <: SType] extends Value[S] {
  }

  /** Base class for references to context variables. */
  trait ContextVariable[S <: SType] extends NotReadyValue[S] {
  }

  /** Reference a context variable by id. */
  trait TaggedVariable[T <: SType] extends ContextVariable[T] {
    val varId: Byte
  }

  case class TaggedVariableNode[T <: SType](varId: Byte, override val tpe: T)
    extends TaggedVariable[T] {
    override def companion = TaggedVariable
    def opType: SFunc = Value.notSupportedError(this, "opType")
  }

  object TaggedVariable extends ValueCompanion {
    override def opCode: OpCode = TaggedVariableCode
    override def costKind: CostKind = FixedCost(JitCost(1))
    def apply[T <: SType](varId: Byte, tpe: T): TaggedVariable[T] =
      TaggedVariableNode(varId, tpe)
  }

  /** High-level interface to internal representation of Unit constants in ErgoTree. */
  object UnitConstant {
    /** ErgoTree node that represent a literal of Unit type. It is global immutable value
      * which should be reused wherever necessary to avoid allocations.
      */
    val instance = apply()

    /** Constucts a fresh new instance of Unit literal node. */
    def apply() = Constant[SUnit.type]((), SUnit)

    /** Recognizer to pattern match on Unit constant literal nodes (aka Unit constants). */
    def unapply(node: SValue): Boolean = node match {
      case ConstantNode(_, SUnit) => true
      case _ => false
    }
  }

  type BoolValue = Value[SBoolean.type]
  type ByteValue = Value[SByte.type]
  type ShortValue = Value[SShort.type]
  type IntValue = Value[SInt.type]
  type LongValue = Value[SLong.type]
  type StringValue = Value[SString.type]
  type BigIntValue = Value[SBigInt.type]
  type BoxValue = Value[SBox.type]
  type GroupElementValue = Value[SGroupElement.type]
  type SigmaPropValue = Value[SSigmaProp.type]
  type AvlTreeValue = Value[SAvlTree.type]
  type SAnyValue = Value[SAny.type]

  type ByteConstant = Constant[SByte.type]
  type ShortConstant = Constant[SShort.type]
  type IntConstant = Constant[SInt.type]
  type LongConstant = Constant[SLong.type]
  type StringConstant = Constant[SString.type]
  type BigIntConstant = Constant[SBigInt.type]
  type BoxConstant = Constant[SBox.type]
  type GroupElementConstant = Constant[SGroupElement.type]
  type SigmaPropConstant = Constant[SSigmaProp.type]
  type AvlTreeConstant = Constant[SAvlTree.type]

  object ByteConstant {
    def apply(value: Byte): Constant[SByte.type] = Constant[SByte.type](value, SByte)
  }
  object ShortConstant {
    def apply(value: Short): Constant[SShort.type]  = Constant[SShort.type](value, SShort)
  }
  object IntConstant {
    def apply(value: Int): Constant[SInt.type]  = Constant[SInt.type](value, SInt)
    def unapply(v: SValue): Option[Int] = v match {
      case Constant(value: Int, SInt) => Some(value)
      case _ => None
    }

    def Zero = IntConstant(0)
  }
  object LongConstant {
    def apply(value: Long): Constant[SLong.type]  = Constant[SLong.type](value, SLong)
    def unapply(v: SValue): Option[Long] = v match {
      case Constant(value: Long, SLong) => Some(value)
      case _ => None
    }
  }
  object BigIntConstant {
    def apply(value: BigInt): Constant[SBigInt.type] = Constant[SBigInt.type](value, SBigInt)
    def apply(value: BigInteger): Constant[SBigInt.type] = Constant[SBigInt.type](SigmaDsl.BigInt(value), SBigInt)
    def apply(value: Long): Constant[SBigInt.type] = Constant[SBigInt.type](SigmaDsl.BigInt(BigInteger.valueOf(value)), SBigInt)
  }

  object StringConstant {
    def apply(value: String): Constant[SString.type]  = Constant[SString.type](value, SString)
    def unapply(v: SValue): Option[String] = v match {
      case Constant(value: String, SString) => Some(value)
      case _ => None
    }
  }

  object BoxConstant {
    def apply(value: Box): Constant[SBox.type] = Constant[SBox.type](value, SBox)
  }

  object GroupElementConstant {
    def apply(value: EcPointType): Constant[SGroupElement.type] = apply(SigmaDsl.GroupElement(value))
    def apply(value: GroupElement): Constant[SGroupElement.type] = Constant[SGroupElement.type](value, SGroupElement)
    def unapply(v: SValue): Option[GroupElement] = v match {
      case Constant(value: GroupElement, SGroupElement) => Some(value)
      case _ => None
    }
  }

  val FalseSigmaProp = SigmaPropConstant(SigmaDsl.SigmaProp(TrivialProp.FalseProp))
  val TrueSigmaProp = SigmaPropConstant(SigmaDsl.SigmaProp(TrivialProp.TrueProp))

  implicit def boolToSigmaProp(b: BoolValue): SigmaPropValue = BoolToSigmaProp(b)

  object SigmaPropConstant {
    def apply(value: SigmaProp): Constant[SSigmaProp.type] = Constant[SSigmaProp.type](value, SSigmaProp)
    def apply(value: SigmaBoolean): Constant[SSigmaProp.type] = Constant[SSigmaProp.type](SigmaDsl.SigmaProp(value), SSigmaProp)
    def unapply(v: SValue): Option[SigmaProp] = v match {
      case Constant(value: SigmaProp, SSigmaProp) => Some(value)
      case _ => None
    }
  }

  object AvlTreeConstant {
    def apply(value: AvlTree): Constant[SAvlTree.type] = Constant[SAvlTree.type](value, SAvlTree)
  }

  object PreHeaderConstant {
    def apply(value: PreHeader): Constant[SPreHeader.type]  = Constant[SPreHeader.type](value, SPreHeader)
    def unapply(v: SValue): Option[PreHeader] = v match {
      case Constant(value: PreHeader, SPreHeader) => Some(value)
      case _ => None
    }
  }
  
  object HeaderConstant {
    def apply(value: Header): Constant[SHeader.type]  = Constant[SHeader.type](value, SHeader)
    def unapply(v: SValue): Option[Header] = v match {
      case Constant(value: Header, SHeader) => Some(value)
      case _ => None
    }
  }

  trait NotReadyValueInt extends NotReadyValue[SInt.type] {
    override def tpe = SInt
  }

  trait NotReadyValueLong extends NotReadyValue[SLong.type] {
    override def tpe = SLong
  }

  trait NotReadyValueBigInt extends NotReadyValue[SBigInt.type] {
    override def tpe = SBigInt
  }

  type TaggedBoolean = TaggedVariable[SBoolean.type]
  type TaggedByte = TaggedVariable[SByte.type]
  type TaggedShort = TaggedVariable[SShort.type]
  type TaggedInt = TaggedVariable[SInt.type]
  type TaggedLong = TaggedVariable[SLong.type]
  type TaggedBigInt = TaggedVariable[SBigInt.type]
  type TaggedBox = TaggedVariable[SBox.type]
  type TaggedGroupElement = TaggedVariable[SGroupElement.type]
  type TaggedSigmaProp = TaggedVariable[SSigmaProp.type]
  type TaggedAvlTree = TaggedVariable[SAvlTree.type]
  type TaggedByteArray = TaggedVariable[SCollection[SByte.type]]

  def TaggedBox(id: Byte): Value[SBox.type] = mkTaggedVariable(id, SBox)
  def TaggedAvlTree(id: Byte): Value[SAvlTree.type] = mkTaggedVariable(id, SAvlTree)

  /** Base type for evaluated tree nodes of Coll type. */
  trait EvaluatedCollection[T <: SType, C <: SCollection[T]] extends EvaluatedValue[C] {
    /** Type descriptor of the collection elements. */
    def elementType: T
  }

  type CollectionConstant[T <: SType] = Constant[SCollection[T]]
  type CollectionValue[T <: SType] = Value[SCollection[T]]

  object CollectionConstant {
    def apply[T <: SType](value: Coll[T#WrappedType], elementType: T): Constant[SCollection[T]] =
      Constant[SCollection[T]](value, SCollection(elementType))
    def unapply[T <: SType](node: Value[SCollection[T]]): Option[(Coll[T#WrappedType], T)] = node match {
      case c: Constant[SCollection[a]] @unchecked if c.tpe.isCollection =>
        val v = c.value.asInstanceOf[Coll[T#WrappedType]]
        val t = c.tpe.elemType
        Some((v, t))
      case _ => None
    }
  }

  val ByteArrayTypeCode = (SCollectionType.CollectionTypeCode + SByte.typeCode).toByte

  object ByteArrayConstant {
    def apply(value: Coll[Byte]): CollectionConstant[SByte.type] = CollectionConstant[SByte.type](value, SByte)
    def apply(value: Array[Byte]): CollectionConstant[SByte.type] = CollectionConstant[SByte.type](value.toColl, SByte)
    def unapply(node: SValue): Option[Coll[Byte]] = node match {
      case coll: CollectionConstant[SByte.type] @unchecked => coll match {
        case CollectionConstant(arr, SByte) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object ShortArrayConstant {
    def apply(value: Coll[Short]): CollectionConstant[SShort.type] = CollectionConstant[SShort.type](value, SShort)
    def apply(value: Array[Short]): CollectionConstant[SShort.type] = CollectionConstant[SShort.type](value.toColl, SShort)
    def unapply(node: SValue): Option[Coll[Short]] = node match {
      case coll: CollectionConstant[SShort.type] @unchecked => coll match {
        case CollectionConstant(arr, SShort) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object IntArrayConstant {
    def apply(value: Coll[Int]): CollectionConstant[SInt.type] = CollectionConstant[SInt.type](value, SInt)
    def apply(value: Array[Int]): CollectionConstant[SInt.type] = CollectionConstant[SInt.type](value.toColl, SInt)
    def unapply(node: SValue): Option[Coll[Int]] = node match {
      case coll: CollectionConstant[SInt.type] @unchecked => coll match {
        case CollectionConstant(arr, SInt) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object LongArrayConstant {
    def apply(value: Coll[Long]): CollectionConstant[SLong.type] = CollectionConstant[SLong.type](value, SLong)
    def apply(value: Array[Long]): CollectionConstant[SLong.type] = CollectionConstant[SLong.type](value.toColl, SLong)
    def unapply(node: SValue): Option[Coll[Long]] = node match {
      case coll: CollectionConstant[SLong.type] @unchecked => coll match {
        case CollectionConstant(arr, SLong) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  object BigIntArrayConstant {
    def apply(value: Coll[BigInt]): CollectionConstant[SBigInt.type] = CollectionConstant[SBigInt.type](value, SBigInt)
    def apply(value: Array[BigInt]): CollectionConstant[SBigInt.type] = CollectionConstant[SBigInt.type](value.toColl, SBigInt)
    def unapply(node: SValue): Option[Coll[BigInt]] = node match {
      case coll: CollectionConstant[SBigInt.type] @unchecked => coll match {
        case CollectionConstant(arr, SBigInt) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  val BoolArrayTypeCode = (SCollectionType.CollectionTypeCode + SBoolean.typeCode).toByte

  object BoolArrayConstant {
    def apply(value: Coll[Boolean]): CollectionConstant[SBoolean.type] = CollectionConstant[SBoolean.type](value, SBoolean)
    def apply(value: Array[Boolean]): CollectionConstant[SBoolean.type] = apply(value.toColl)
    def unapply(node: SValue): Option[Coll[Boolean]] = node match {
      case coll: CollectionConstant[SBoolean.type] @unchecked => coll match {
        case CollectionConstant(arr, SBoolean) => Some(arr)
        case _ => None
      }
      case _ => None
    }
  }

  trait NotReadyValueByteArray extends NotReadyValue[SByteArray] {
    override def tpe = SByteArray
  }

  trait NotReadyValueAvlTree extends NotReadyValue[SAvlTree.type] {
    override def tpe = SAvlTree
  }

  /** ErgoTree node that represents the operation of obtaining the generator of elliptic curve group.
    * The generator g of the group is an element of the group such that, when written
    * multiplicative form, every element of the group is a power of g.
    */
  case object GroupGenerator extends EvaluatedValue[SGroupElement.type] with ValueCompanion {
    override def opCode: OpCode = OpCodes.GroupGeneratorCode
    override val costKind = FixedCost(JitCost(10))
    override def tpe = SGroupElement
    override val value = SigmaDsl.GroupElement(CryptoConstants.dlogGroup.generator)
    override def companion = this
    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      addCost(costKind)
      SigmaDsl.groupGenerator
    }
  }


  trait NotReadyValueGroupElement extends NotReadyValue[SGroupElement.type] {
    override def tpe = SGroupElement
  }

  type BooleanConstant = Constant[SBoolean.type]

  object BooleanConstant {
    def fromBoolean(v: Boolean): BooleanConstant = if (v) TrueLeaf else FalseLeaf
    def apply(value: Boolean): BooleanConstant = Constant[SBoolean.type](value, SBoolean)
    def unapply(v: SValue): Option[Boolean] = v match {
      case Constant(value: Boolean, SBoolean) => Some(value)
      case _ => None
    }
  }

  /** ErgoTree node which represents `true` literal. */
  object TrueLeaf extends ConstantNode[SBoolean.type](true, SBoolean) with ValueCompanion {
    override def companion = this
    override def opCode: OpCode = TrueCode
    override def costKind: FixedCost = Constant.costKind
    override def toString: String = "TrueLeaf"
  }

  /** ErgoTree node which represents `false` literal. */
  object FalseLeaf extends ConstantNode[SBoolean.type](false, SBoolean) with ValueCompanion {
    override def companion = this
    override def opCode: OpCode = FalseCode
    override def costKind: FixedCost = Constant.costKind
    override def toString: String = "FalseLeaf"
  }

  trait NotReadyValueBoolean extends NotReadyValue[SBoolean.type] {
    override def tpe = SBoolean
  }

  /** Algebraic data type of sigma proposition expressions.
    * Values of this type are used as values of SigmaProp type of SigmaScript and SigmaDsl
    */
  trait SigmaBoolean {
    /** Unique id of the node class used in serialization of SigmaBoolean. */
    val opCode: OpCode
    /** Size of the proposition tree (number of nodes). */
    def size: Int
  }

  object SigmaBoolean {
    /** Compute total size of the trees in the collection of children. */
    def totalSize(children: Seq[SigmaBoolean]): Int = {
      var res = 0
      val len = children.length
      cfor(0)(_ < len, _ + 1) { i =>
        res += children(i).size
      }
      res
    }

    /** HOTSPOT: don't beautify this code */
    object serializer extends SigmaSerializer[SigmaBoolean, SigmaBoolean] {
      val dhtSerializer = ProveDHTupleSerializer(ProveDHTuple.apply)
      val dlogSerializer = ProveDlogSerializer(ProveDlog.apply)

      // TODO v5.x: control maxTreeDepth same as in deserialize
      override def serialize(data: SigmaBoolean, w: SigmaByteWriter): Unit = {
        w.put(data.opCode)
        data match {
          case dlog: ProveDlog   => dlogSerializer.serialize(dlog, w)
          case dht: ProveDHTuple => dhtSerializer.serialize(dht, w)
          case _: TrivialProp => // besides opCode no additional bytes
          case and: CAND =>
            val nChildren = and.children.length
            w.putUShort(nChildren)
            cfor(0)(_ < nChildren, _ + 1) { i =>
              val c = and.children(i)
              serializer.serialize(c, w)
            }

          case or: COR =>
            val nChildren = or.children.length
            w.putUShort(nChildren)
            cfor(0)(_ < nChildren, _ + 1) { i =>
              val c = or.children(i)
              serializer.serialize(c, w)
            }

          case th: CTHRESHOLD =>
            w.putUShort(th.k)
            val nChildren = th.children.length
            w.putUShort(nChildren)
            cfor(0)(_ < nChildren, _ + 1) { i =>
              val c = th.children(i)
              serializer.serialize(c, w)
            }
        }
      }

      override def parse(r: SigmaByteReader): SigmaBoolean = {
        val depth = r.level
        r.level = depth + 1
        val opCode = r.getByte()
        val res = opCode match {
          case FalseProp.opCode => FalseProp
          case TrueProp.opCode  => TrueProp
          case ProveDlogCode => dlogSerializer.parse(r)
          case ProveDiffieHellmanTupleCode => dhtSerializer.parse(r)
          case AndCode =>
            val n = r.getUShort()
            val children = safeNewArray[SigmaBoolean](n)
            cfor(0)(_ < n, _ + 1) { i =>
              children(i) = serializer.parse(r)
            }
            CAND(children)
          case OrCode =>
            val n = r.getUShort()
            val children = safeNewArray[SigmaBoolean](n)
            cfor(0)(_ < n, _ + 1) { i =>
              children(i) = serializer.parse(r)
            }
            COR(children)
          case AtLeastCode =>
            val k = r.getUShort()
            val n = r.getUShort()
            val children = safeNewArray[SigmaBoolean](n)
            cfor(0)(_ < n, _ + 1) { i =>
              children(i) = serializer.parse(r)
            }
            CTHRESHOLD(k, children)
        }
        r.level = r.level - 1
        res
      }
    }
  }

  trait NotReadyValueBox extends NotReadyValue[SBox.type] {
    def tpe = SBox
  }

  /** ErgoTree node which converts a collection of expressions into a tuple of data values
    * of different types. Each data value of the resulting collection is obtained by
    * evaluating the corresponding expression in `items`. All items may have different
    * types.
    *
    * @param items source collection of expressions
    */
  case class Tuple(items: IndexedSeq[Value[SType]])
      extends EvaluatedValue[STuple]  // note, this superclass is required as Tuple can be in a register
         with EvaluatedCollection[SAny.type, STuple] {
    override def companion = Tuple
    override lazy val tpe = STuple(items.map(_.tpe))
    override def opType: SFunc = ???
    override def elementType: SAny.type = SAny

    override lazy val value = {
      val xs = items.cast[EvaluatedValue[SAny.type]].map(_.value)
      implicit val tAny: RType[Any] = sigma.AnyType
      Colls.fromArray(xs.toArray)
    }

    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      // in v5.0 version we support only tuples of 2 elements to be equivalent with v4.x
      if (items.length != 2)
        Interpreter.error(s"Invalid tuple $this")

      val item0 = items(0)
      val x = item0.evalTo[Any](env)
      Value.checkType(item0, x)

      val item1 = items(1)
      val y = item1.evalTo[Any](env)
      Value.checkType(item1, y)

      val res = (x, y) // special representation for pairs (to pass directly to Coll primitives)

      addCost(Tuple.costKind)
      res
    }
  }

  object Tuple extends FixedCostValueCompanion {
    override def opCode: OpCode = TupleCode
    /** Cost of: 1) allocating a new tuple (of limited max size)*/
    override val costKind = FixedCost(JitCost(15))
    def apply(items: Value[SType]*): Tuple = Tuple(items.toIndexedSeq)
  }

  /** ErgoTree node which converts a collection of expressions into a collection of data
    * values. Each data value of the resulting collection is obtained by evaluating the
    * corresponding expression in `items`. All items must have the same type.
    *
    * @param items       source collection of expressions
    * @param elementType type descriptor of elements in the resulting collection
    */
  case class ConcreteCollection[V <: SType](items: Seq[Value[V]], elementType: V)
    extends EvaluatedCollection[V, SCollection[V]] {
// TODO uncomment and make sure Ergo works with it, i.e. complex data types are never used for `items`.
//      There is nothing wrong in using List, Vector and other fancy types as a concrete representation
//      of `items`, but these types have sub-optimal performance (2-3x overhead comparing to WrappedArray)
//      which is immediately visible in profile.
//      NOTE, the assert below should be commented before production release.
//      Is it there for debuging only, basically to catch call stacks where the fancy types may
//      occasionally be used.
//    assert(
//      items.isInstanceOf[mutable.WrappedArray[_]] ||
//      items.isInstanceOf[ArrayBuffer[_]] ||
//      items.isInstanceOf[mutable.ArraySeq[_]],
//      s"Invalid types of items ${items.getClass}")

    private val isBooleanConstants = elementType == SBoolean && items.forall(_.isInstanceOf[Constant[_]])
    override def companion =
      if (isBooleanConstants) ConcreteCollectionBooleanConstant
      else ConcreteCollection

    val tpe = SCollection[V](elementType)
    implicit lazy val tElement: RType[V#WrappedType] = Evaluation.stypeToRType(elementType)

    // TODO refactor: this method is not used and can be removed
    lazy val value = {
      val xs = items.cast[EvaluatedValue[V]].map(_.value)
      Colls.fromArray(xs.toArray(tElement.classTag))
    }

    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      val len = items.length
      addCost(ConcreteCollection.costKind)
      val is = Array.ofDim[V#WrappedType](len)(tElement.classTag)
      cfor(0)(_ < len, _ + 1) { i =>
        val item = items(i)
        val itemV = item.evalTo[V#WrappedType](env)
        Value.checkType(item, itemV) // necessary because cast to V#WrappedType is erased
        is(i) = itemV
      }
      Colls.fromArray(is)
    }
  }
  object ConcreteCollection extends FixedCostValueCompanion {
    override def opCode: OpCode = ConcreteCollectionCode
    /** Cost of: allocating new collection
      * @see ConcreteCollection_PerItem */
    override val costKind = FixedCost(JitCost(20))

    def fromSeq[V <: SType](items: Seq[Value[V]])(implicit tV: V): ConcreteCollection[V] =
      ConcreteCollection(items, tV)

    def fromItems[V <: SType](items: Value[V]*)(implicit tV: V): ConcreteCollection[V] =
      ConcreteCollection(items, tV)
  }
  object ConcreteCollectionBooleanConstant extends ValueCompanion {
    override def opCode: OpCode = ConcreteCollectionBooleanConstantCode
    override def costKind = ConcreteCollection.costKind
  }

  trait LazyCollection[V <: SType] extends NotReadyValue[SCollection[V]]

  implicit class CollectionOps[T <: SType](val coll: Value[SCollection[T]]) extends AnyVal {
    def length: Int = matchCase(_.items.length, _.value.length, _.items.length)
    def items = matchCase(_.items, _ => sys.error(s"Cannot get 'items' property of node $coll"), _.items)
//    def isEvaluatedCollection =
//      coll.evaluated && matchCase(_.items.forall(_.evaluated), _ => true, _.items.forall(_.evaluated))
    def matchCase[R](
        whenConcrete: ConcreteCollection[T] => R,
        whenConstant: CollectionConstant[T] => R,
        whenTuple: Tuple => R
    ): R = coll match {
      case cc: ConcreteCollection[T]@unchecked => whenConcrete(cc)
      case const: CollectionConstant[T]@unchecked => whenConstant(const)
      case tuple: Tuple => whenTuple(tuple)
      case _ => sys.error(s"Unexpected node $coll")
    }
  }

  implicit class SigmaPropValueOps(val p: Value[SSigmaProp.type]) extends AnyVal {
    def isProven: Value[SBoolean.type] = SigmaPropIsProven(p)
    def propBytes: Value[SByteArray] = SigmaPropBytes(p)
    def treeWithSegregation: ErgoTree = ErgoTree.withSegregation(p)
    def treeWithSegregation(headerFlags: Byte): ErgoTree =
      ErgoTree.withSegregation(headerFlags, p)
  }

  implicit class SigmaBooleanOps(val sb: SigmaBoolean) extends AnyVal {
    def toSigmaProp: SigmaPropValue = SigmaPropConstant(sb)
    def isProven: Value[SBoolean.type] = SigmaPropIsProven(SigmaPropConstant(sb))
    def showToString: String = sb match {
      case ProveDlog(v) =>
        s"ProveDlog(${showECPoint(v)})"
      case ProveDHTuple(gv, hv, uv, vv) =>
        s"ProveDHTuple(${showECPoint(gv)}, ${showECPoint(hv)}, ${showECPoint(uv)}, ${showECPoint(vv)})"
      case _ => sb.toString
    }
  }

  sealed trait BlockItem extends NotReadyValue[SType] {
    def id: Int
    def rhs: SValue
    def isValDef: Boolean
  }
  object BlockItem {
    /** Immutable empty array, can be used to save allocations in many places. */
    val EmptyArray = Array.empty[BlockItem]

    /** Immutable empty IndexedSeq to save allocations in many places. */
    val EmptySeq: IndexedSeq[BlockItem] = EmptyArray
  }

  /** IR node for let-bound expressions `let x = rhs` which is ValDef, or `let f[T] = rhs` which is FunDef.
    * These nodes are used to represent ErgoTrees after common sub-expression elimination.
    * This representation is more compact in serialized form.
    * @param id unique identifier of the variable in the current scope. */
  case class ValDef(override val id: Int,
                    tpeArgs: Seq[STypeVar],
                    override val rhs: SValue) extends BlockItem {
    require(id >= 0, "id must be >= 0")
    override def companion = if (tpeArgs.isEmpty) ValDef else FunDef
    override def tpe: SType = rhs.tpe
    override def isValDef: Boolean = tpeArgs.isEmpty
    /** This is not used as operation, but rather to form a program structure */
    override def opType: SFunc = Value.notSupportedError(this, "opType")
  }
  object ValDef extends ValueCompanion {
    override def opCode: OpCode = ValDefCode
    override def costKind = Value.notSupportedError(this, "costKind")
    def apply(id: Int, rhs: SValue): ValDef = ValDef(id, Nil, rhs)
  }
  object FunDef extends ValueCompanion {
    override def opCode: OpCode = FunDefCode
    override def costKind = Value.notSupportedError(this, "costKind")
    def unapply(d: BlockItem): Option[(Int, Seq[STypeVar], SValue)] = d match {
      case ValDef(id, targs, rhs) if !d.isValDef => Some((id, targs, rhs))
      case _ => None
    }
  }

  /** Special node which represents a reference to ValDef it was introduced as result of
   * CSE. */
  case class ValUse[T <: SType](valId: Int, tpe: T) extends NotReadyValue[T] {
    override def companion = ValUse
    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")

    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      addCost(ValUse.costKind)
      val res = env.getOrElse(valId, error(s"cannot resolve $this"))
      Value.checkType(this, res)
      res
    }
  }
  object ValUse extends FixedCostValueCompanion {
    override def opCode: OpCode = ValUseCode
    /** Cost of: 1) Lookup in immutable HashMap by valId: Int 2) alloc of Some(v) */
    override val costKind = FixedCost(JitCost(5))
  }

  /** The order of ValDefs in the block is used to assign ids to ValUse(id) nodes
    * For all i: items(i).id == {number of ValDefs preceded in a graph} with respect to topological order.
    * Specific topological order doesn't really matter, what is important is to preserve semantic linkage
    * between ValUse(id) and ValDef with the corresponding id.
    * This convention allow to valid serializing ids because we always serializing and deserializing
    * in a fixed well defined order.
    */
  case class BlockValue(items: IndexedSeq[BlockItem], result: SValue) extends NotReadyValue[SType] {
    override def companion = BlockValue
    def tpe: SType = result.tpe

    /** This is not used as operation, but rather to form a program structure */
    def opType: SFunc = Value.notSupportedError(this, "opType")

    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      var curEnv = env
      val len = items.length
      addSeqCostNoOp(BlockValue.costKind, len)
      cfor(0)(_ < len, _ + 1) { i =>
        val vd = items(i).asInstanceOf[ValDef]
        val v = vd.rhs.evalTo[Any](curEnv)
        Value.checkType(vd, v)
        E.addFixedCost(FuncValue.AddToEnvironmentDesc_CostKind,
                       FuncValue.AddToEnvironmentDesc) {
          curEnv = curEnv + (vd.id -> v)
        }
      }
      val res = result.evalTo[Any](curEnv)
      Value.checkType(result, res)
      res
    }
  }
  object BlockValue extends ValueCompanion {
    override def opCode: OpCode = BlockValueCode
    override val costKind = PerItemCost(
      baseCost = JitCost(1), perChunkCost = JitCost(1), chunkSize = 10)
  }
  /**
    * @param args parameters list, where each parameter has an id and a type.
    * @param body expression, which refers function parameters with ValUse.
    */
  case class FuncValue(args: IndexedSeq[(Int,SType)], body: Value[SType]) extends NotReadyValue[SFunc] {
    import FuncValue._
    override def companion = FuncValue
    lazy val tpe: SFunc = {
      val nArgs = args.length
      val argTypes = new Array[SType](nArgs)
      cfor(0)(_ < nArgs, _ + 1) { i =>
        argTypes(i) = args(i)._2
      }
      SFunc(argTypes, body.tpe)
    }
    /** This is not used as operation, but rather to form a program structure */
    override def opType: SFunc = SFunc(ArraySeq.empty, tpe)

    protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
      addCost(FuncValue.costKind)
      if (args.length == 1) {
        val arg0 = args(0)
        (vArg: Any) => {
          Value.checkType(arg0._2, vArg)
          var env1: DataEnv = null
          E.addFixedCost(AddToEnvironmentDesc_CostKind, AddToEnvironmentDesc) {
            env1 = env + (arg0._1 -> vArg)
          }
          val res = body.evalTo[Any](env1)
          Value.checkType(body, res)
          res
        }
      } else {
        Interpreter.error(s"Function must have 1 argument, but was: $this")
      }
    }
  }
  object FuncValue extends FixedCostValueCompanion {
    val AddToEnvironmentDesc = NamedDesc("AddToEnvironment")
    /** Cost of: adding value to evaluator environment */
    val AddToEnvironmentDesc_CostKind = FixedCost(JitCost(5))
    override def opCode: OpCode = FuncValueCode
    /** Cost of: 1) switch on the number of args 2) allocating a new Scala closure
      * Old cost: ("Lambda", "() => (D1) => R", lambdaCost),*/
    override val costKind = FixedCost(JitCost(5))
    def apply(argId: Int, tArg: SType, body: SValue): FuncValue =
      FuncValue(IndexedSeq((argId,tArg)), body)
  }

  implicit class OptionValueOps[T <: SType](val p: Value[SOption[T]]) extends AnyVal {
    def get: Value[T] = OptionGet(p)
    def getOrElse(default: Value[T]): Value[T] = OptionGetOrElse(p, default)
    def isDefined: Value[SBoolean.type] = OptionIsDefined(p)
  }

  def GetVarBoolean(varId: Byte): GetVar[SBoolean.type] = GetVar(varId, SBoolean)
  def GetVarByte(varId: Byte): GetVar[SByte.type] = GetVar(varId, SByte)
  def GetVarShort(varId: Byte): GetVar[SShort.type] = GetVar(varId, SShort)
  def GetVarInt(varId: Byte): GetVar[SInt.type] = GetVar(varId, SInt)
  def GetVarLong(varId: Byte): GetVar[SLong.type] = GetVar(varId, SLong)
  def GetVarBigInt(varId: Byte): GetVar[SBigInt.type] = GetVar(varId, SBigInt)
  def GetVarBox(varId: Byte): GetVar[SBox.type] = GetVar(varId, SBox)
  def GetVarSigmaProp(varId: Byte): GetVar[SSigmaProp.type] = GetVar(varId, SSigmaProp)
  def GetVarByteArray(varId: Byte): GetVar[SCollection[SByte.type]] = GetVar(varId, SByteArray)
  def GetVarIntArray(varId: Byte): GetVar[SCollection[SInt.type]] = GetVar(varId, SIntArray)

  /** This is alternative representation of ErgoTree expression when it cannot be parsed
    * due to `error`. This is used by the nodes running old versions of code to recognize
    * soft-fork conditions and skip validation of box propositions which are unparsable. */
  case class UnparsedErgoTree(bytes: mutable.WrappedArray[Byte], error: ValidationException)

  /** The root of ErgoScript IR. Serialized instances of this class are self sufficient and can be passed around.
    * ErgoTreeSerializer defines top-level serialization format of the scripts.
    * The interpretation of the byte array depend on the first `header` byte, which uses VLQ encoding up to 30 bits.
    * Currently we define meaning for only first byte, which may be extended in future versions.
    *   7  6  5  4  3  2  1  0
    * -------------------------
    * |  |  |  |  |  |  |  |  |
    * -------------------------
    *  Bit 7 == 1 if the header contains more than 1 byte (default == 0)
    *  Bit 6 - reserved for GZIP compression (should be 0)
    *  Bit 5 == 1 - reserved for context dependent costing (should be = 0)
    *  Bit 4 == 1 if constant segregation is used for this ErgoTree (default = 0)
    *                (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/264)
    *  Bit 3 == 1 if size of the whole tree is serialized after the header byte (default = 0)
    *  Bits 2-0 - language version (current version == 0)
    *
    *  Currently we don't specify interpretation for the second and other bytes of the header.
    *  We reserve the possibility to extend header by using Bit 7 == 1 and chain additional bytes as in VLQ.
    *  Once the new bytes are required, a new version of the language should be created and implemented.
    *  That new language will give an interpretation for the new bytes.
    *
    *  Consistency between fields is ensured by private constructor and factory methods in `ErgoTree` object.
    *  For performance reasons, ErgoTreeSerializer can be configured to perform additional constant segregation.
    *  In such a case after deserialization there may be more constants segregated. This is done for example to
    *  support caching optimization described in #264 mentioned above.
    *
    *  The default behavior of ErgoTreeSerializer is to preserve original structure of ErgoTree and check
    *  consistency. In case of any inconsistency the serializer throws exception.
    *  
    *  @param header      the first byte of serialized byte array which determines interpretation of the rest of the array
    *
    *  @param constants   If isConstantSegregation == true contains the constants for which there may be
    *                     ConstantPlaceholders in the tree.
    *                     If isConstantSegregation == false this array should be empty and any placeholder in
    *                     the tree will lead to exception.
    *
    *  @param root        On the right side it has valid expression of `SigmaProp` type. Or alternatively,
    *                     on the left side, it has unparsed bytes along with the ValidationException,
    *                     which caused the deserializer to fail.
    *                     `Right(tree)` if isConstantSegregation == true contains ConstantPlaceholder
    *                     instead of some Constant nodes. Otherwise, it may not contain placeholders.
    *                     It is possible to have both constants and placeholders in the tree, but for every placeholder
    *                     there should be a constant in `constants` array.
    *  @param givenComplexity  structural complexity of the tree or 0 if is not specified at construction time.
    *                          Access to this private value is provided via `complexity` property.
    *                          In case of 0, the complexity is computed using ErgoTree deserializer, which can do this.
    *                          When specified it should be computed as the sum of complexity values taken
    *                          from ComplexityTable for all tree nodes. It approximates the time needed to process
    *                          the tree by sigma compiler to obtain cost formula. Overly complex trees can thus
    *                          be rejected even before compiler starts working.
    *  @param propositionBytes original bytes of this tree from which it has been deserialized.
    *                          If null then the bytes are not provided, and will be lazily generated when `bytes`
    *                          method is called.
    *                          These bytes are obtained in two ways:
    *                          1) in the ErgoTreeSerializer from Reader
    *                          2) in the alternative constructor using ErgoTreeSerializer.serializeErgoTree
    *  @param givenDeserialize optional flag, which contains information about presence of
    *                          deserialization operations in the tree. If it is None, the information is not
    *                          available. If Some(true) then there are deserialization operations, otherwise
    *                          the tree doesn't contain deserialization and is eligible
    *                          for optimized execution.
    *                          ErgoTreeSerializer parsing method computes the value of
    *                          this flag and provides it to the constructor.
    */
  case class ErgoTree private[sigmastate](
    header: Byte,
    constants: IndexedSeq[Constant[SType]],
    root: Either[UnparsedErgoTree, SigmaPropValue],
    private val givenComplexity: Int,
    private val propositionBytes: Array[Byte],
    private val givenDeserialize: Option[Boolean]
  ) {

    def this(header: Byte,
             constants: IndexedSeq[Constant[SType]],
             root: Either[UnparsedErgoTree, SigmaPropValue]) =
      this(
        header, constants, root, 0,
        propositionBytes = DefaultSerializer.serializeErgoTree(
          ErgoTree(header, constants, root, 0, null, None)
        ),
        givenDeserialize = None
      )

    require(isConstantSegregation || constants.isEmpty)
    require(version == 0 || hasSize, s"For newer version the size bit is required: $this")

    /** Then it throws the error from UnparsedErgoTree.
      * It does so on every usage of `proposition` because the lazy value remains uninitialized.
      */
    @deprecated("Use toProposition instead", "v2.1")
    lazy val proposition: SigmaPropValue = toProposition(isConstantSegregation)

    @inline final def version: Byte = ErgoTree.getVersion(header)
    @inline final def isRightParsed: Boolean = root.isRight
    @inline final def isConstantSegregation: Boolean = ErgoTree.isConstantSegregation(header)
    @inline final def hasSize: Boolean = ErgoTree.hasSize(header)

    private[sigmastate] var _bytes: Array[Byte] = propositionBytes

    /** Serialized bytes of this tree. */
    final def bytes: Array[Byte] = {
      if (_bytes == null) {
        _bytes = DefaultSerializer.serializeErgoTree(this)
      }
      _bytes
    }

    /** Hexadecimal encoded string of ErgoTree.bytes. */
    final def bytesHex: String = ErgoAlgos.encode(bytes)

    private var _complexity: Int = givenComplexity

    /** Structural complexity estimation of this tree.
      * @see ComplexityTable
      */
    lazy val complexity: Int = {
      if (_complexity == 0) {
        _complexity = DefaultSerializer.deserializeErgoTree(bytes).complexity
      }
      _complexity
    }

    private[sigmastate] var _hasDeserialize: Option[Boolean] = givenDeserialize

    /** Returns true if the tree contains at least one deserialization operation,
      * false otherwise.
      */
    lazy val hasDeserialize: Boolean = {
      if (_hasDeserialize.isEmpty) {
        _hasDeserialize = Some(root match {
          case Right(p) => Value.hasDeserialize(p)
          case _ => false
        })
      }
      _hasDeserialize.get
    }

    /** Serialized proposition expression of SigmaProp type with
      * ConstantPlaceholder nodes not replaced by Constant nodes.
      */
    lazy val template: Array[Byte] = {
      val r = SigmaSerializer.startReader(bytes)
      DefaultSerializer.deserializeHeaderWithTreeBytes(r)._4
    }

    /** Base16 encoding of `template` bytes. */
    def templateHex: String = Base16.encode(template)

    /** Get proposition expression from this contract.
      * When root.isRight then
      *   if replaceConstants == false this is the same as `root.right.get`.
      *   Otherwise, it is equivalent to `root.right.get` where all placeholders are replaced by Constants.
      * When root.isLeft then
      *   throws the error from UnparsedErgoTree.
      *   It does so on every usage of `proposition` because the lazy value remains uninitialized.
      */
    def toProposition(replaceConstants: Boolean): SigmaPropValue = root match {
      case Right(tree) =>
        val prop = if (replaceConstants)
          substConstants(tree, constants).asSigmaProp
        else
          tree
        prop
      case Left(UnparsedErgoTree(_, error)) =>
        throw error
    }

    /** The default equality of case class is overridden to exclude `complexity`. */
    override def canEqual(that: Any): Boolean = that.isInstanceOf[ErgoTree]

    override def hashCode(): Int = header * 31 + Objects.hash(constants, root)

    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) ||
      ((obj.asInstanceOf[AnyRef] != null) && (obj match {
        case other: ErgoTree =>
          other.header == header && other.constants == constants && other.root == root
        case _ => false
      }))
  }

  object ErgoTree {
    /** Current version of ErgoTree serialization format (aka bite-code language version)*/
    val VersionFlag: Byte = 0

    /** Default value of ErgoTree.header byte */
    val DefaultHeader: Byte = (0 | VersionFlag).toByte

    /** Header flag to indicate that constant segregation should be applied. */
    val ConstantSegregationFlag: Byte = 0x10

    /** Header flag to indicate that whole size of ErgoTree should be saved before tree content. */
    val SizeFlag: Byte = 0x08

    /** Header mask to extract version bits. */
    val VersionMask: Byte = 0x07

    /** Default header with constant segregation enabled. */
    val ConstantSegregationHeader: Byte = (DefaultHeader | ConstantSegregationFlag).toByte

    /** @return true if the constant segregation flag is set to 1 in the given header byte. */
    @inline final def isConstantSegregation(header: Byte): Boolean = (header & ConstantSegregationFlag) != 0

    /** @return true if the size flag is set to 1 in the given header byte. */
    @inline final def hasSize(header: Byte): Boolean = (header & SizeFlag) != 0

    /** @return a value of the version bits from the given header byte. */
    @inline final def getVersion(header: Byte): Byte = (header & VersionMask).toByte

    /** Update the version bits of the given header byte with the given version value. */
    @inline final def updateVersionBits(header: Byte, version: Byte): Byte = {
      require(version < 8, s"ErgoTree.version should be < 8: $version")
      (header | version).toByte
    }

    /** Creates valid header byte with the given version.
      * The SizeFlag is set if version > 0 */
    @inline def headerWithVersion(version: Byte): Byte = {
      // take default header and embedd the given version in it
      var h = updateVersionBits(DefaultHeader, version)
      if (version > 0) {
        // set SizeFlag if version is greater then 0 (see require() in ErgoTree constructor)
        h = (h | ErgoTree.SizeFlag).toByte
      }
      h
    }

    /** Substitute [[ConstantPlaceholder]] nodes in the given expression with the constants
      * taken from the given collection.
      * @param root expression to transform
      * @param constants collection of constants to replace placeholders
      * @return new expression without placeholders
      */
    def substConstants(root: SValue, constants: IndexedSeq[Constant[SType]]): SValue = {
      val store = new ConstantStore(constants)
      val substRule = strategy[Any] {
        case ph: ConstantPlaceholder[_] =>
          Some(store.get(ph.id))
        case _ => None
      }
      everywherebu(substRule)(root).fold(root)(_.asInstanceOf[SValue])
    }

    /** Create an ErgoTree with the given parameters. */
    def apply(header: Byte, constants: IndexedSeq[Constant[SType]], root: SigmaPropValue): ErgoTree = {
      new ErgoTree(header, constants, Right(root))
    }

    val EmptyConstants: IndexedSeq[Constant[SType]] = Array[Constant[SType]]()

    /** Create new ErgoTree for the given proposition using the given header flags and
      * without performing constant segregation.
      */
    def withoutSegregation(root: SigmaPropValue): ErgoTree =
      ErgoTree(ErgoTree.DefaultHeader, EmptyConstants, root)

    /** Create new ErgoTree for the given proposition using the given header flags and
      * without performing constant segregation.
      */
    def withoutSegregation(headerFlags: Byte, root: SigmaPropValue): ErgoTree =
      ErgoTree((ErgoTree.DefaultHeader | headerFlags).toByte, EmptyConstants, root)

    /** Create new ErgoTree for the given proposition using default header.
      * If the property is not a simple constant, then constant segregation is performed.
      */
    implicit def fromProposition(prop: SigmaPropValue): ErgoTree = {
      fromProposition(ErgoTree.DefaultHeader, prop)
    }

    /** Create new ErgoTree for the given proposition using the given header flags.
      * If the property is not a simple constant, then constant segregation is performed.
      */
    def fromProposition(headerFlags: Byte, prop: SigmaPropValue): ErgoTree = {
      prop match {
        case SigmaPropConstant(_) => withoutSegregation(headerFlags, prop)
        case _ => withSegregation(headerFlags, prop)
      }
    }

    /** Create new ErgoTree for the given sigma proposition using default header and
      * without performing constant segregation.
      */
    implicit def fromSigmaBoolean(pk: SigmaBoolean): ErgoTree = {
      withoutSegregation(pk.toSigmaProp)
    }

    /** Create new ErgoTree for the given sigma proposition using the given header flags
      * and without performing constant segregation.
      */
    def fromSigmaBoolean(headerFlags: Byte, pk: SigmaBoolean): ErgoTree = {
      withoutSegregation(headerFlags, pk.toSigmaProp)
    }

    /** Build ErgoTree via serialization of the value with ConstantSegregationHeader, constants segregated
      * from the tree and ConstantPlaceholders referring to the segregated constants.
      *
      * This method uses single traverse of the tree to:
      * 1) find and segregate all constants;
      * 2) replace constants with ConstantPlaceholders in the `tree`;
      * 3) write the `tree` to the Writer's buffer obtaining `treeBytes`;
      * 4) deserialize `tree` with ConstantPlaceholders.
      * @param headerFlags additional header flags to combine with
      *                    ConstantSegregationHeader flag.
      * @param prop expression to be transformed into ErgoTree
      **/
    def withSegregation(headerFlags: Byte, prop: SigmaPropValue): ErgoTree = {
      val constantStore = new ConstantStore()
      val byteWriter = SigmaSerializer.startWriter(constantStore)
      // serialize value and segregate constants into constantStore
      ValueSerializer.serialize(prop, byteWriter)
      val extractedConstants = constantStore.getAll
      val r = SigmaSerializer.startReader(byteWriter.toBytes)
      r.constantStore = new ConstantStore(extractedConstants)
      // deserialize value with placeholders
      val valueWithPlaceholders = ValueSerializer.deserialize(r).asSigmaProp
      val header = (ErgoTree.ConstantSegregationHeader | headerFlags).toByte
      new ErgoTree(header, extractedConstants, Right(valueWithPlaceholders))
    }

    /** Create new ErgoTree for the given sigma proposition using default header and
      * also performing constant segregation.
      */
    def withSegregation(prop: SigmaPropValue): ErgoTree =
      withSegregation(DefaultHeader, prop)

    /** Deserializes an ErgoTree instance from a hexadecimal string.
      *
      * @param hex a hexadecimal string representing the serialized ErgoTree
      */
    def fromHex(hex: String): ErgoTree = {
      val bytes = Base16.decode(hex).get
      fromBytes(bytes)
    }

    /** Deserializes an ErgoTree instance from an array of bytes.
      *
      * @param bytes an array of bytes representing the serialized ErgoTree
      */
    def fromBytes(bytes: Array[Byte]): ErgoTree = {
      ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bytes)
    }

  }

}
