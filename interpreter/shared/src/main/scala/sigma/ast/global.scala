package sigma.ast

import sigma.ast.SCollection.{SByteArray, SIntArray}
import sigma.data.{CSigmaProp, TrivialProp}
import sigmastate.ErgoTree.HeaderType
import sigmastate._
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

  type CollectionConstant[T <: SType] = Constant[SCollection[T]]

  type CollectionValue[T <: SType] = Value[SCollection[T]]

  val FalseSigmaProp = SigmaPropConstant(CSigmaProp(TrivialProp.FalseProp))

  val TrueSigmaProp  = SigmaPropConstant(CSigmaProp(TrivialProp.TrueProp))

  implicit class CollectionOps[T <: SType](val coll: Value[SCollection[T]]) extends AnyVal {
    def length: Int = matchCase(_.items.length, _.value.length, _.items.length)

    def items = matchCase(_.items, _ => sys.error(s"Cannot get 'items' property of node $coll"), _.items)

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
