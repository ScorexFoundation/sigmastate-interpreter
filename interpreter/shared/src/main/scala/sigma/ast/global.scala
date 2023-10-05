package sigma.ast

import sigma.ast.SCollection.{SByteArray, SIntArray}
import sigma.data.{CSigmaProp, TrivialProp}
import sigmastate.ErgoTree.HeaderType
import sigmastate._
import sigmastate.lang.StdSigmaBuilder.mkTaggedVariable
import sigmastate.utxo._

object global {
  /** Force initialization of reflection. */
  val reflection = InterpreterReflection

  type SValue = Value[SType]

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

  type BooleanConstant = Constant[SBoolean.type]

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

  val FalseSigmaProp = SigmaPropConstant(CSigmaProp(TrivialProp.FalseProp))

  val TrueSigmaProp  = SigmaPropConstant(CSigmaProp(TrivialProp.TrueProp))

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

  type CollectionConstant[T <: SType] = Constant[SCollection[T]]

  type CollectionValue[T <: SType] = Value[SCollection[T]]

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
    def isProven: Value[SBoolean.type] = {
      SigmaPropIsProven(p)
    }

    def propBytes: Value[SByteArray] = SigmaPropBytes(p)

    def treeWithSegregation(header: HeaderType): ErgoTree =
      ErgoTree.withSegregation(header, p)
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

  implicit def boolToSigmaProp(b: BoolValue): SigmaPropValue = BoolToSigmaProp(b)

}
