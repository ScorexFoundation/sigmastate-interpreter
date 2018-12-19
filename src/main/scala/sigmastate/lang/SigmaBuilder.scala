package sigmastate.lang

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.{ProveDHTuple, DLogProtocol}
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{FuncValue, FalseLeaf, Constant, SValue, TrueLeaf, BlockValue, ConstantNode, SomeValue, ConstantPlaceholder, BigIntValue, BoolValue, Value, SigmaPropValue, Tuple, GroupElementValue, TaggedVariableNode, SigmaBoolean, BlockItem, ValUse, TaggedVariable, ConcreteCollection, NoneValue}
import sigmastate._
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.Constraints.{TypeConstraint2, sameType2, onlyNumeric2}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.ConstraintFailed
import sigmastate.serialization.OpCodes
import sigmastate.utxo._
import scalan.Nullable

trait SigmaBuilder {

  def mkEQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def mkNEQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]

  def mkGT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def mkGE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def mkLT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def mkLE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]

  def mkArith[T <: SNumericType](left: Value[T], right: Value[T], opCode: Byte): Value[T]
  def mkPlus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkMinus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkMultiply[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkDivide[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkModulo[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkMin[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkMax[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]

  def mkOR(input: Value[SCollection[SBoolean.type]]): BoolValue
  def mkAND(input: Value[SCollection[SBoolean.type]]): BoolValue

  def mkAnyOf(input: Seq[Value[SBoolean.type]]): BoolValue
  def mkAllOf(input: Seq[Value[SBoolean.type]]): BoolValue

  def mkBinOr(left: BoolValue, right: BoolValue): BoolValue
  def mkBinAnd(left: BoolValue, right: BoolValue): BoolValue
  def mkAtLeast(bound: Value[SInt.type], input: Value[SCollection[SSigmaProp.type]]): SigmaPropValue
  def mkBinXor(left: BoolValue, right: BoolValue): BoolValue

  def mkExponentiate(left: Value[SGroupElement.type],
                     right: Value[SBigInt.type]): Value[SGroupElement.type]
  def mkMultiplyGroup(left: Value[SGroupElement.type],
                      right: Value[SGroupElement.type]): Value[SGroupElement.type]
  def mkXor(left: Value[SByteArray], right: Value[SByteArray]): Value[SByteArray]

  def mkTreeModifications(tree: Value[SAvlTree.type],
                          operations: Value[SByteArray],
                          proof: Value[SByteArray]): Value[SOption[SByteArray]]

  def mkTreeLookup(tree: Value[SAvlTree.type],
                   key: Value[SByteArray],
                   proof: Value[SByteArray]): Value[SOption[SByteArray]]

  def mkIsMember(tree: Value[SAvlTree.type],
                 key: Value[SByteArray],
                 proof: Value[SByteArray]): Value[SBoolean.type]

  def mkIf[T <: SType](condition: Value[SBoolean.type],
                       trueBranch: Value[T],
                       falseBranch: Value[T]): Value[T]

  def mkLongToByteArray(input: Value[SLong.type]): Value[SByteArray]
  def mkByteArrayToBigInt(input: Value[SByteArray]): Value[SBigInt.type]
  def mkUpcast[T <: SNumericType, R <: SNumericType](input: Value[T], tpe: R): Value[R]
  def mkDowncast[T <: SNumericType, R <: SNumericType](input: Value[T], tpe: R): Value[R]

  def mkCalcBlake2b256(input: Value[SByteArray]): Value[SByteArray]
  def mkCalcSha256(input: Value[SByteArray]): Value[SByteArray]
  def mkDecodePoint(input: Value[SByteArray]): GroupElementValue

  def mkAppend[IV <: SType](input: Value[SCollection[IV]],
                            col2: Value[SCollection[IV]]): Value[SCollection[IV]]

  def mkSlice[IV <: SType](input: Value[SCollection[IV]],
                           from: Value[SInt.type],
                           until: Value[SInt.type]): Value[SCollection[IV]]

  def mkMapCollection[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                                mapper: Value[SFunc]): Value[SCollection[OV]]

  def mkFilter[IV <: SType](input: Value[SCollection[IV]],
                            id: Byte,
                            condition: Value[SBoolean.type]): Value[SCollection[IV]]

  def mkExists[IV <: SType](input: Value[SCollection[IV]],
                            condition: Value[SFunc]): Value[SBoolean.type]

  def mkForAll[IV <: SType](input: Value[SCollection[IV]],
                            condition: Value[SFunc]): Value[SBoolean.type]

  def mkFuncValue(args: IndexedSeq[(Int,SType)], body: Value[SType]): Value[SFunc]

  def mkFold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                          zero: Value[OV],
                          foldOp: Value[SFunc]): Value[OV]

  def mkByIndex[IV <: SType](input: Value[SCollection[IV]],
                               index: Value[SInt.type],
                               default: Option[Value[IV]] = None): Value[IV]

  def mkSelectField(input: Value[STuple], fieldIndex: Byte): Value[SType]
  def mkSizeOf[IV <: SType](input: Value[SCollection[IV]]): Value[SInt.type]
  def mkExtractAmount(input: Value[SBox.type]): Value[SLong.type]
  def mkExtractScriptBytes(input: Value[SBox.type]): Value[SByteArray]
  def mkExtractBytes(input: Value[SBox.type]): Value[SByteArray]
  def mkExtractBytesWithNoRef(input: Value[SBox.type]): Value[SByteArray]
  def mkExtractId(input: Value[SBox.type]): Value[SByteArray]
  def mkExtractCreationInfo(input: Value[SBox.type]): Value[STuple]

  def mkExtractRegisterAs[IV <: SType](input: Value[SBox.type],
                                      registerId: RegisterId,
                                      tpe: SOption[IV]): Value[SType]

  def mkDeserializeContext[T <: SType](id: Byte, tpe: T): Value[T]
  def mkDeserializeRegister[T <: SType](reg: RegisterId,
                                        tpe: T,
                                        default: Option[Value[T]] = None): Value[T]

  def mkTuple(items: Seq[Value[SType]]): Value[SType]

  def mkProveDiffieHellmanTuple(gv: Value[SGroupElement.type],
                                hv: Value[SGroupElement.type],
                                uv: Value[SGroupElement.type],
                                vv: Value[SGroupElement.type]): SigmaBoolean
  def mkProveDlog(value: Value[SGroupElement.type]): SigmaBoolean

  /** Logically inverse to mkSigmaPropIsProven */
  def mkBoolToSigmaProp(value: BoolValue): SigmaPropValue
  /** Logically inverse to mkBoolToSigmaProp */
  def mkSigmaPropIsProven(value: Value[SSigmaProp.type]): BoolValue

  def mkSigmaPropBytes(value: Value[SSigmaProp.type]): Value[SByteArray]
  def mkSigmaAnd(items: Seq[SigmaPropValue]): SigmaPropValue
  def mkSigmaOr(items: Seq[SigmaPropValue]): SigmaPropValue

  def mkConcreteCollection[T <: SType](items: IndexedSeq[Value[T]],
                                       elementType: T): Value[SCollection[T]]

  def mkTaggedVariable[T <: SType](varId: Byte, tpe: T): TaggedVariable[T]

  def mkSomeValue[T <: SType](x: Value[T]): Value[SOption[T]]
  def mkNoneValue[T <: SType](elemType: T): Value[SOption[T]]

  def mkBlock(bindings: Seq[Val], result: Value[SType]): Value[SType]
  def mkBlockValue(items: IndexedSeq[BlockItem], result: Value[SType]): Value[SType]
  def mkValUse(valId: Int, tpe: SType): Value[SType]
  def mkZKProofBlock(body: Value[SSigmaProp.type]): Value[SBoolean.type]
  def mkVal(name: String, givenType: SType, body: Value[SType]): Val
  def mkSelect(obj: Value[SType], field: String, resType: Option[SType] = None): Value[SType]
  def mkIdent(name: String, tpe: SType): Value[SType]
  def mkApply(func: Value[SType], args: IndexedSeq[Value[SType]]): Value[SType]
  def mkApplyTypes(input: Value[SType], tpeArgs: Seq[SType]): Value[SType]
  def mkMethodCallLike(obj: Value[SType],
                   name: String,
                   args: IndexedSeq[Value[SType]],
                   tpe: SType = NoType): Value[SType]
  def mkMethodCall(obj: Value[SType],
                  method: SMethod,
                  args: IndexedSeq[Value[SType]]): Value[SType]
  def mkLambda(args: IndexedSeq[(String,SType)],
               givenResType: SType,
               body: Option[Value[SType]]): Value[SFunc]
  def mkGenLambda(tpeParams: Seq[STypeParam], args: IndexedSeq[(String,SType)],
               givenResType: SType,
               body: Option[Value[SType]]): Value[SFunc]

  def mkConstant[T <: SType](value: T#WrappedType, tpe: T): Constant[T]
  def mkConstantPlaceholder[T <: SType](id: Int, tpe: T): Value[SType]
  def mkCollectionConstant[T <: SType](values: Array[T#WrappedType],
                                       elementType: T): Constant[SCollection[T]]
  def mkStringConcat(left: Value[SString.type], right: Value[SString.type]): Value[SString.type]

  def mkGetVar[T <: SType](varId: Byte, tpe: T): Value[SOption[T]]
  def mkOptionGet[T <: SType](input: Value[SOption[T]]): Value[T]
  def mkOptionGetOrElse[T <: SType](input: Value[SOption[T]], default: Value[T]): Value[T]
  def mkOptionIsDefined[T <: SType](input: Value[SOption[T]]): Value[SBoolean.type]

  def mkModQ(input: Value[SBigInt.type]): Value[SBigInt.type]
  def mkPlusModQ(left: Value[SBigInt.type], right: Value[SBigInt.type]): Value[SBigInt.type]
  def mkMinusModQ(left: Value[SBigInt.type], right: Value[SBigInt.type]): Value[SBigInt.type]

  def mkLogicalNot(input: Value[SBoolean.type]): Value[SBoolean.type]

  def mkNegation[T <: SNumericType](input: Value[T]): Value[T]
  def mkBitInversion[T <: SNumericType](input: Value[T]): Value[T]
  def mkBitOr[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkBitAnd[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkBitXor[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkBitShiftRight[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkBitShiftLeft[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]

  def liftAny(v: Any): Nullable[SValue] = v match {
    case arr: Array[Boolean] => Nullable(mkCollectionConstant[SBoolean.type](arr, SBoolean))
    case arr: Array[Byte] => Nullable(mkCollectionConstant[SByte.type](arr, SByte))
    case arr: Array[Short] => Nullable(mkCollectionConstant[SShort.type](arr, SShort))
    case arr: Array[Int] => Nullable(mkCollectionConstant[SInt.type](arr, SInt))
    case arr: Array[Long] => Nullable(mkCollectionConstant[SLong.type](arr, SLong))
    case arr: Array[BigInteger] => Nullable(mkCollectionConstant[SBigInt.type](arr, SBigInt))
    case arr: Array[String] => Nullable(mkCollectionConstant[SString.type](arr, SString))
    case v: Byte => Nullable(mkConstant[SByte.type](v, SByte))
    case v: Short => Nullable(mkConstant[SShort.type](v, SShort))
    case v: Int => Nullable(mkConstant[SInt.type](v, SInt))
    case v: Long => Nullable(mkConstant[SLong.type](v, SLong))
    case v: BigInteger => Nullable(mkConstant[SBigInt.type](v, SBigInt))
    case v: CryptoConstants.EcPointType => Nullable(mkConstant[SGroupElement.type](v, SGroupElement))
    case b: Boolean => Nullable(if(b) TrueLeaf else FalseLeaf)
    case v: String => Nullable(mkConstant[SString.type](v, SString))
    case b: ErgoBox => Nullable(mkConstant[SBox.type](b, SBox))
    case avl: AvlTreeData => Nullable(mkConstant[SAvlTree.type](avl, SAvlTree))
    case sb: SigmaBoolean => Nullable(mkConstant[SSigmaProp.type](sb, SSigmaProp))
    case v: SValue => Nullable(v)
    case _ => Nullable.None
  }
}

class StdSigmaBuilder extends SigmaBuilder {

  protected def equalityOp[T <: SType, R](left: Value[T],
                                        right: Value[T],
                                        cons: (Value[T], Value[T]) => R): R = cons(left, right)

  protected def comparisonOp[T <: SType, R](left: Value[T],
                                          right: Value[T],
                                          cons: (Value[T], Value[T]) => R): R = cons(left, right)

  protected def arithOp[T <: SNumericType, R](left: Value[T],
                                            right: Value[T],
                                            cons: (Value[T], Value[T]) => R): R = cons(left, right)

  override def mkEQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    equalityOp(left, right, EQ.apply[T])

  override def mkNEQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    equalityOp(left, right, NEQ.apply[T])

  override def mkGT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, GT.apply[T])

  override def mkGE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, GE.apply[T])

  override def mkLT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, LT.apply[T])

  override def mkLE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, LE.apply[T])

  override def mkArith[T <: SNumericType](left: Value[T], right: Value[T], opCode: Byte): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => ArithOp[T](l, r, opCode) })

  override def mkPlus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkArith(left, right, OpCodes.PlusCode)

  override def mkMinus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkArith(left, right, OpCodes.MinusCode)

  override def mkMultiply[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkArith(left, right, OpCodes.MultiplyCode)

  override def mkDivide[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkArith(left, right, OpCodes.DivisionCode)

  override def mkModulo[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkArith(left, right, OpCodes.ModuloCode)

  override def mkMin[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkArith(left, right, OpCodes.MinCode)

  override def mkMax[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    mkArith(left, right, OpCodes.MaxCode)

  override def mkOR(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type] =
    OR(input)

  override def mkAND(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type] =
    AND(input)

  override def mkAnyOf(input: Seq[Value[SBoolean.type]]) = OR(input)
  override def mkAllOf(input: Seq[Value[SBoolean.type]]) = AND(input)

  override def mkBinOr(left: BoolValue, right: BoolValue) = BinOr(left, right)

  override def mkBinAnd(left: BoolValue, right: BoolValue) = BinAnd(left, right)

  override def mkAtLeast(bound: Value[SInt.type], input: Value[SCollection[SSigmaProp.type]]): SigmaPropValue =
    AtLeast(bound, input)

  override def mkBinXor(left: BoolValue, right: BoolValue): BoolValue = BinXor(left, right)

  override def mkExponentiate(left: Value[SGroupElement.type], right: Value[SBigInt.type]): Value[SGroupElement.type] =
    Exponentiate(left, right)

  override def mkMultiplyGroup(left: Value[SGroupElement.type], right: Value[SGroupElement.type]): Value[SGroupElement.type] =
    MultiplyGroup(left, right)

  override def mkXor(left: Value[SByteArray], right: Value[SByteArray]): Value[SByteArray] =
    Xor(left, right)

  override def mkTreeLookup(tree: Value[SAvlTree.type],
                            key: Value[SByteArray],
                            proof: Value[SByteArray]): Value[SOption[SByteArray]] =
    TreeLookup(tree, key, proof)

  override def mkTreeModifications(tree: Value[SAvlTree.type],
                                   operations: Value[SByteArray],
                                   proof: Value[SByteArray]): Value[SOption[SByteArray]] =
    TreeModifications(tree, operations, proof)

  override def mkIsMember(tree: Value[SAvlTree.type],
                          key: Value[SByteArray],
                          proof: Value[SByteArray]): Value[SBoolean.type] =
    OptionIsDefined(TreeLookup(tree, key, proof))

  override def mkIf[T <: SType](condition: Value[SBoolean.type],
                                trueBranch: Value[T],
                                falseBranch: Value[T]): Value[T] =
    If(condition, trueBranch, falseBranch)

  override def mkLongToByteArray(input: Value[SLong.type]): Value[SByteArray] =
    LongToByteArray(input)

  override def mkByteArrayToBigInt(input: Value[SByteArray]): Value[SBigInt.type] =
    ByteArrayToBigInt(input)


  override def mkUpcast[T <: SNumericType, R <: SNumericType](input: Value[T],
                                                              tpe: R): Value[R] =
    Upcast(input, tpe)


  override def mkDowncast[T <: SNumericType, R <: SNumericType](input: Value[T], tpe: R): Value[R] =
    Downcast(input, tpe)

  override def mkCalcBlake2b256(input: Value[SByteArray]): Value[SByteArray] =
    CalcBlake2b256(input)

  override def mkCalcSha256(input: Value[SByteArray]): Value[SByteArray] =
    CalcSha256(input)

  override def mkDecodePoint(input: Value[SByteArray]): GroupElementValue =
    DecodePoint(input)

  override def mkMapCollection[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                                         mapper: Value[SFunc]): Value[SCollection[OV]] =
    MapCollection(input, mapper)

  override def mkAppend[IV <: SType](input: Value[SCollection[IV]],
                                     col2: Value[SCollection[IV]]): Value[SCollection[IV]] =
    Append(input, col2)

  override def mkSlice[IV <: SType](input: Value[SCollection[IV]],
                                    from: Value[SInt.type],
                                    until: Value[SInt.type]): Value[SCollection[IV]] =
    Slice(input, from, until)

  override def mkFilter[IV <: SType](input: Value[SCollection[IV]],
                                     id: Byte,
                                     condition: Value[SBoolean.type]): Value[SCollection[IV]] =
    Filter(input, id, condition)

  override def mkExists[IV <: SType](input: Value[SCollection[IV]],
                                     condition: Value[SFunc]): Value[SBoolean.type] =
    Exists(input, condition)

  override def mkForAll[IV <: SType](input: Value[SCollection[IV]],
                                     condition: Value[SFunc]): Value[SBoolean.type] =
    ForAll(input, condition)

  def mkFuncValue(args: IndexedSeq[(Int,SType)], body: Value[SType]): Value[SFunc] =
    FuncValue(args, body)

  override def mkFold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                   zero: Value[OV],
                                   foldOp: Value[SFunc]): Value[OV] =
    Fold(input, zero, foldOp)

  override def mkByIndex[IV <: SType](input: Value[SCollection[IV]],
                                      index: Value[SInt.type],
                                      default: Option[Value[IV]] = None): Value[IV] =
    ByIndex(input, index, default)

  override def mkSelectField(input: Value[STuple], fieldIndex: Byte): Value[SType] =
    SelectField(input, fieldIndex)

  override def mkSizeOf[V <: SType](input: Value[SCollection[V]]): Value[SInt.type] =
    SizeOf(input)

  override def mkExtractAmount(input: Value[SBox.type]): Value[SLong.type] =
    ExtractAmount(input)

  override def mkExtractScriptBytes(input: Value[SBox.type]): Value[SByteArray] =
    ExtractScriptBytes(input)

  override def mkExtractBytes(input: Value[SBox.type]): Value[SByteArray] =
    ExtractBytes(input)

  override def mkExtractBytesWithNoRef(input: Value[SBox.type]): Value[SByteArray] =
    ExtractBytesWithNoRef(input)

  override def mkExtractId(input: Value[SBox.type]): Value[SByteArray] =
    ExtractId(input)

  override def mkExtractCreationInfo(input: Value[SBox.type]): Value[STuple] =
    ExtractCreationInfo(input)

  override def mkExtractRegisterAs[IV <: SType](input: Value[SBox.type],
                                                registerId: RegisterId,
                                                tpe: SOption[IV]): Value[SType] =
    ExtractRegisterAs(input, registerId, tpe)

  override def mkDeserializeContext[T <: SType](id: Byte, tpe: T): Value[T] =
    DeserializeContext(id, tpe)

  override def mkDeserializeRegister[T <: SType](reg: RegisterId,
                                                 tpe: T,
                                                 default: Option[Value[T]] = None): Value[T] =
    DeserializeRegister(reg, tpe, default)

  override def mkTuple(items: Seq[Value[SType]]): Value[SType] =
    Tuple(items.toIndexedSeq)

  override def mkProveDiffieHellmanTuple(gv: Value[SGroupElement.type],
                                         hv: Value[SGroupElement.type],
                                         uv: Value[SGroupElement.type],
                                         vv: Value[SGroupElement.type]): SigmaBoolean =
    ProveDHTuple(gv, hv, uv, vv)

  override def mkProveDlog(value: Value[SGroupElement.type]): SigmaBoolean =
    ProveDlog(value)

  override def mkBoolToSigmaProp(value: BoolValue) = BoolToSigmaProp(value)

  override def mkSigmaPropIsProven(value: Value[SSigmaProp.type]) = SigmaPropIsProven(value)

  override def mkSigmaPropBytes(value: Value[SSigmaProp.type]) = SigmaPropBytes(value)

  override def mkSigmaAnd(items: Seq[SigmaPropValue]): SigmaPropValue = SigmaAnd(items)

  override def mkSigmaOr(items: Seq[SigmaPropValue]): SigmaPropValue = SigmaOr(items)

  override def mkConcreteCollection[T <: SType](items: IndexedSeq[Value[T]],
                                                elementType: T): Value[SCollection[T]] =
    ConcreteCollection(items, elementType)

  override def mkTaggedVariable[T <: SType](varId: Byte, tpe: T): TaggedVariable[T] =
    TaggedVariableNode(varId, tpe)

  override def mkSomeValue[T <: SType](x: Value[T]): Value[SOption[T]] = SomeValue(x)
  override def mkNoneValue[T <: SType](elemType: T): Value[SOption[T]] = NoneValue(elemType)

  override def mkBlock(bindings: Seq[Val], result: Value[SType]): Value[SType] =
    Block(bindings, result)

  override def mkBlockValue(items: IndexedSeq[BlockItem], result: Value[SType]): Value[SType] =
    BlockValue(items, result)

  override def mkValUse(valId: Int, tpe: SType): Value[SType] =
    ValUse(valId, tpe)

  override def mkZKProofBlock(body: Value[SSigmaProp.type]): BoolValue =
    ZKProofBlock(body)

  override def mkVal(name: String, givenType: SType, body: Value[SType]): Val =
    ValNode(name, givenType, body)

  override def mkSelect(obj: Value[SType],
                        field: String,
                        resType: Option[SType] = None): Value[SType] =
    Select(obj, field, resType)

  override def mkIdent(name: String, tpe: SType): Value[SType] = Ident(name, tpe)

  override def mkApply(func: Value[SType], args: IndexedSeq[Value[SType]]): Value[SType] =
    Apply(func, args)

  override def mkApplyTypes(input: Value[SType], tpeArgs: Seq[SType]): Value[SType] =
    ApplyTypes(input, tpeArgs)

  override def mkMethodCallLike(obj: Value[SType],
                            name: String,
                            args: IndexedSeq[Value[SType]],
                            tpe: SType): Value[SType] =
    MethodCallLike(obj, name, args, tpe)

  override def mkMethodCall(obj: Value[SType],
                            method: SMethod,
                            args: IndexedSeq[Value[SType]]): Value[SType] =
    MethodCall(obj, method, args)

  override def mkLambda(args: IndexedSeq[(String, SType)],
                        givenResType: SType,
                        body: Option[Value[SType]]): Value[SFunc] =
    Lambda(Nil, args, givenResType, body)

  def mkGenLambda(tpeParams: Seq[STypeParam],
                  args: IndexedSeq[(String, SType)],
                  givenResType: SType,
                  body: Option[Value[SType]]) =
    Lambda(tpeParams, args, givenResType, body)

  override def mkConstant[T <: SType](value: T#WrappedType, tpe: T): Constant[T] =
    ConstantNode[T](value, tpe)


  override def mkConstantPlaceholder[T <: SType](id: Int, tpe: T): Value[SType] =
    ConstantPlaceholder[T](id, tpe)

  override def mkCollectionConstant[T <: SType](values: Array[T#WrappedType],
                                                elementType: T): Constant[SCollection[T]] =
    ConstantNode[SCollection[T]](values, SCollection(elementType))

  override def mkStringConcat(left: Value[SString.type], right: Value[SString.type]): Value[SString.type] =
    StringConcat(left, right)

  override def mkGetVar[T <: SType](varId: Byte, tpe: T): Value[SOption[T]] =
    GetVar(varId, tpe)

  override def mkOptionGet[T <: SType](input: Value[SOption[T]]): Value[T] =
    OptionGet(input)

  override def mkOptionGetOrElse[T <: SType](input: Value[SOption[T]], default: Value[T]): Value[T] =
    OptionGetOrElse(input, default)

  override def mkOptionIsDefined[T <: SType](input: Value[SOption[T]]): Value[SBoolean.type] =
    OptionIsDefined(input)

  override def mkModQ(input: Value[SBigInt.type]): Value[SBigInt.type] =
    ModQ(input)

  override def mkPlusModQ(left: Value[SBigInt.type], right: Value[SBigInt.type]): Value[SBigInt.type] =
    ModQArithOp(left, right, OpCodes.PlusModQCode)

  override def mkMinusModQ(left: Value[SBigInt.type], right: Value[SBigInt.type]): Value[SBigInt.type] =
    ModQArithOp(left, right, OpCodes.MinusModQCode)

  override def mkLogicalNot(input: Value[SBoolean.type]): Value[SBoolean.type] =
    LogicalNot(input)

  override def mkNegation[T <: SNumericType](input: Value[T]): Value[T] =
    Negation(input)

  override def mkBitInversion[T <: SNumericType](input: Value[T]): Value[T] =
    BitInversion(input)

  override def mkBitOr[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    BitOp(left, right, OpCodes.BitOrCode)

  override def mkBitAnd[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    BitOp(left, right, OpCodes.BitAndCode)

  override def mkBitXor[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    BitOp(left, right, OpCodes.BitXorCode)

  override def mkBitShiftRight[T <: SNumericType](bits: Value[T], shift: Value[T]): Value[T] =
    BitOp(bits, shift, OpCodes.BitShiftRightCode)

  override def mkBitShiftLeft[T <: SNumericType](bits: Value[T], shift: Value[T]): Value[T] =
    BitOp(bits, shift, OpCodes.BitShiftLeftCode)

}

trait TypeConstraintCheck {

  def check2[T <: SType](left: Value[T],
                                 right: Value[T],
                                 constraints: Seq[TypeConstraint2]): Unit =
    constraints.foreach { c =>
      if (!c(left.tpe, right.tpe))
        throw new ConstraintFailed(s"Failed constraint $c for binary operation parameters ($left(tpe: ${left.tpe}), $right(tpe: ${right.tpe}))")
    }
}

trait TransformingSigmaBuilder extends StdSigmaBuilder with TypeConstraintCheck {

  private def applyUpcast[T <: SType](left: Value[T], right: Value[T]):(Value[T], Value[T]) =
    (left.tpe, right.tpe) match {
      case (t1: SNumericType, t2: SNumericType) if t1 != t2 =>
        val tmax = t1 max t2
        val l = left.upcastTo(tmax)
        val r = right.upcastTo(tmax)
        (l.asValue[T], r.asValue[T])
      case _ =>
        (left, right)
    }

  override protected def equalityOp[T <: SType, R](left: Value[T],
                                        right: Value[T],
                                        cons: (Value[T], Value[T]) => R): R = {
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    cons(l, r)
  }

  override protected def comparisonOp[T <: SType, R](left: Value[T],
                                          right: Value[T],
                                          cons: (Value[T], Value[T]) => R): R = {
    check2(left, right, Seq(onlyNumeric2))
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    cons(l, r)
  }

  override protected def arithOp[T <: SNumericType, R](left: Value[T],
                                            right: Value[T],
                                            cons: (Value[T], Value[T]) => R): R = {
    val (l, r) = applyUpcast(left, right)
    cons(l, r)
  }
}

trait CheckingSigmaBuilder extends StdSigmaBuilder with TypeConstraintCheck {

  override protected def equalityOp[T <: SType, R](left: Value[T],
                                        right: Value[T],
                                        cons: (Value[T], Value[T]) => R): R = {
    check2(left, right, Seq(sameType2))
    cons(left, right)
  }

  override protected def comparisonOp[T <: SType, R](left: Value[T],
                                          right: Value[T],
                                          cons: (Value[T], Value[T]) => R): R = {
    check2(left, right, Seq(onlyNumeric2, sameType2))
    cons(left, right)
  }

  override protected def arithOp[T <: SNumericType, R](left: Value[T],
                                            right: Value[T],
                                            cons: (Value[T], Value[T]) => R): R = {
    check2(left, right, Seq(sameType2))
    cons(left, right)
  }
}

case object StdSigmaBuilder extends StdSigmaBuilder

case object CheckingSigmaBuilder extends StdSigmaBuilder with CheckingSigmaBuilder

case object DefaultSigmaBuilder extends StdSigmaBuilder with CheckingSigmaBuilder
case object TransformingSigmaBuilder extends StdSigmaBuilder with TransformingSigmaBuilder
case object DeserializationSigmaBuilder extends StdSigmaBuilder with TransformingSigmaBuilder

object Constraints {
  type Constraint2 = (SType.TypeCode, SType.TypeCode) => Boolean
  type TypeConstraint2 = (SType, SType) => Boolean
  type ConstraintN = Seq[SType.TypeCode] => Boolean

  def onlyNumeric2: TypeConstraint2 = {
    case (_: SNumericType, _: SNumericType) => true
    case _ => false
  }

  def sameType2: TypeConstraint2 = {
    case (v1, v2) => v1.tpe == v2.tpe
  }

  def sameTypeN: ConstraintN = { tcs => tcs.tail.forall(_ == tcs.head) }
}
