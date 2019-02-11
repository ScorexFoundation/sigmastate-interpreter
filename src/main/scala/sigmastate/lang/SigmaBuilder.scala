package sigmastate.lang

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoBox.RegisterId
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{StringConstant, FuncValue, FalseLeaf, Constant, SValue, TrueLeaf, BlockValue, ConstantNode, SomeValue, ConstantPlaceholder, BigIntValue, BoolValue, Value, SigmaPropValue, Tuple, GroupElementValue, TaggedVariableNode, SigmaBoolean, BlockItem, UnitConstant, ValUse, TaggedVariable, ConcreteCollection, NoneValue}
import sigmastate._
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.Constraints.{TypeConstraint2, sameType2, onlyNumeric2}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.lang.Constraints.{TypeConstraint2, sameType2, onlyNumeric2}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.ConstraintFailed
import sigmastate.serialization.OpCodes
import sigmastate.utxo._
import scalan.Nullable
import sigmastate.basics.ProveDHTuple
import sigmastate.eval.CostingSigmaDslBuilder
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.sigma.{GroupElement, SigmaProp}

import scala.util.DynamicVariable

trait SigmaBuilder {

  val currentSrcCtx = new DynamicVariable[Nullable[SourceContext]](Nullable.None)

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
  def mkStringConcat(left: Constant[SString.type], right: Constant[SString.type]): Value[SString.type]

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
  def mkBitShiftRight[T <: SNumericType](bits: Value[T], shift: Value[T]): Value[T]
  def mkBitShiftLeft[T <: SNumericType](bits: Value[T], shift: Value[T]): Value[T]
  def mkBitShiftRightZeroed[T <: SNumericType](bits: Value[T], shift: Value[T]): Value[T]

  def mkUnitConstant: Value[SUnit.type]

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
    case n: special.sigma.BigInt => Nullable(mkConstant[SBigInt.type](CostingSigmaDslBuilder.toBigInteger(n), SBigInt))

    case v: EcPointType => Nullable(mkConstant[SGroupElement.type](v, SGroupElement))
    case ge: GroupElement => Nullable(mkConstant[SGroupElement.type](CostingSigmaDslBuilder.toECPoint(ge).asInstanceOf[EcPointType], SGroupElement))

    case b: Boolean => Nullable(if(b) TrueLeaf else FalseLeaf)
    case v: String => Nullable(mkConstant[SString.type](v, SString))
    case b: ErgoBox => Nullable(mkConstant[SBox.type](b, SBox))
    case avl: AvlTreeData => Nullable(mkConstant[SAvlTree.type](avl, SAvlTree))

    case sb: SigmaBoolean => Nullable(mkConstant[SSigmaProp.type](sb, SSigmaProp))
    case p: SigmaProp => Nullable(mkConstant[SSigmaProp.type](CostingSigmaDslBuilder.toSigmaBoolean(p), SSigmaProp))

    case v: SValue => Nullable(v)
    case _ => Nullable.None
  }

  def unliftAny(v: SValue): Nullable[Any] = v match {
    case Constant(v, t) => Nullable(v)
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
    equalityOp(left, right, EQ.apply[T]).withSrcCtx(currentSrcCtx.value)

  override def mkNEQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    equalityOp(left, right, NEQ.apply[T]).withSrcCtx(currentSrcCtx.value)

  override def mkGT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, GT.apply[T]).withSrcCtx(currentSrcCtx.value)

  override def mkGE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, GE.apply[T]).withSrcCtx(currentSrcCtx.value)

  override def mkLT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, LT.apply[T]).withSrcCtx(currentSrcCtx.value)

  override def mkLE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, LE.apply[T]).withSrcCtx(currentSrcCtx.value)

  override def mkArith[T <: SNumericType](left: Value[T], right: Value[T], opCode: Byte): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => ArithOp[T](l, r, opCode) })
      .withSrcCtx(currentSrcCtx.value)

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
    OR(input).withSrcCtx(currentSrcCtx.value)

  override def mkAND(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type] =
    AND(input).withSrcCtx(currentSrcCtx.value)

  override def mkAnyOf(input: Seq[Value[SBoolean.type]]) =
    OR(input).withSrcCtx(currentSrcCtx.value)
  override def mkAllOf(input: Seq[Value[SBoolean.type]]) =
    AND(input).withSrcCtx(currentSrcCtx.value)

  override def mkBinOr(left: BoolValue, right: BoolValue) =
    BinOr(left, right).withSrcCtx(currentSrcCtx.value)

  override def mkBinAnd(left: BoolValue, right: BoolValue) =
    BinAnd(left, right).withSrcCtx(currentSrcCtx.value)

  override def mkAtLeast(bound: Value[SInt.type], input: Value[SCollection[SSigmaProp.type]]): SigmaPropValue =
    AtLeast(bound, input).withSrcCtx(currentSrcCtx.value)

  override def mkBinXor(left: BoolValue, right: BoolValue): BoolValue =
    BinXor(left, right).withSrcCtx(currentSrcCtx.value)

  override def mkExponentiate(left: Value[SGroupElement.type], right: Value[SBigInt.type]): Value[SGroupElement.type] =
    Exponentiate(left, right).withSrcCtx(currentSrcCtx.value)

  override def mkMultiplyGroup(left: Value[SGroupElement.type], right: Value[SGroupElement.type]): Value[SGroupElement.type] =
    MultiplyGroup(left, right).withSrcCtx(currentSrcCtx.value)

  override def mkXor(left: Value[SByteArray], right: Value[SByteArray]): Value[SByteArray] =
    Xor(left, right).withSrcCtx(currentSrcCtx.value)

  override def mkTreeLookup(tree: Value[SAvlTree.type],
                            key: Value[SByteArray],
                            proof: Value[SByteArray]): Value[SOption[SByteArray]] =
    TreeLookup(tree, key, proof).withSrcCtx(currentSrcCtx.value)

  override def mkTreeModifications(tree: Value[SAvlTree.type],
                                   operations: Value[SByteArray],
                                   proof: Value[SByteArray]): Value[SOption[SByteArray]] =
    TreeModifications(tree, operations, proof).withSrcCtx(currentSrcCtx.value)

  override def mkIsMember(tree: Value[SAvlTree.type],
                          key: Value[SByteArray],
                          proof: Value[SByteArray]): Value[SBoolean.type] =
    OptionIsDefined(TreeLookup(tree, key, proof)).withSrcCtx(currentSrcCtx.value)

  override def mkIf[T <: SType](condition: Value[SBoolean.type],
                                trueBranch: Value[T],
                                falseBranch: Value[T]): Value[T] =
    If(condition, trueBranch, falseBranch).withSrcCtx(currentSrcCtx.value)

  override def mkLongToByteArray(input: Value[SLong.type]): Value[SByteArray] =
    LongToByteArray(input).withSrcCtx(currentSrcCtx.value)

  override def mkByteArrayToBigInt(input: Value[SByteArray]): Value[SBigInt.type] =
    ByteArrayToBigInt(input).withSrcCtx(currentSrcCtx.value)


  override def mkUpcast[T <: SNumericType, R <: SNumericType](input: Value[T],
                                                              tpe: R): Value[R] =
    Upcast(input, tpe).withSrcCtx(currentSrcCtx.value)


  override def mkDowncast[T <: SNumericType, R <: SNumericType](input: Value[T], tpe: R): Value[R] =
    Downcast(input, tpe).withSrcCtx(currentSrcCtx.value)

  override def mkCalcBlake2b256(input: Value[SByteArray]): Value[SByteArray] =
    CalcBlake2b256(input).withSrcCtx(currentSrcCtx.value)

  override def mkCalcSha256(input: Value[SByteArray]): Value[SByteArray] =
    CalcSha256(input).withSrcCtx(currentSrcCtx.value)

  override def mkDecodePoint(input: Value[SByteArray]): GroupElementValue =
    DecodePoint(input).withSrcCtx(currentSrcCtx.value)

  override def mkMapCollection[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                                         mapper: Value[SFunc]): Value[SCollection[OV]] =
    MapCollection(input, mapper).withSrcCtx(currentSrcCtx.value)

  override def mkAppend[IV <: SType](input: Value[SCollection[IV]],
                                     col2: Value[SCollection[IV]]): Value[SCollection[IV]] =
    Append(input, col2).withSrcCtx(currentSrcCtx.value)

  override def mkSlice[IV <: SType](input: Value[SCollection[IV]],
                                    from: Value[SInt.type],
                                    until: Value[SInt.type]): Value[SCollection[IV]] =
    Slice(input, from, until).withSrcCtx(currentSrcCtx.value)

  override def mkFilter[IV <: SType](input: Value[SCollection[IV]],
                                     id: Byte,
                                     condition: Value[SBoolean.type]): Value[SCollection[IV]] =
    Filter(input, id, condition).withSrcCtx(currentSrcCtx.value)

  override def mkExists[IV <: SType](input: Value[SCollection[IV]],
                                     condition: Value[SFunc]): Value[SBoolean.type] =
    Exists(input, condition).withSrcCtx(currentSrcCtx.value)

  override def mkForAll[IV <: SType](input: Value[SCollection[IV]],
                                     condition: Value[SFunc]): Value[SBoolean.type] =
    ForAll(input, condition).withSrcCtx(currentSrcCtx.value)

  def mkFuncValue(args: IndexedSeq[(Int,SType)], body: Value[SType]): Value[SFunc] =
    FuncValue(args, body).withSrcCtx(currentSrcCtx.value)

  override def mkFold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                   zero: Value[OV],
                                   foldOp: Value[SFunc]): Value[OV] =
    Fold(input, zero, foldOp).withSrcCtx(currentSrcCtx.value)

  override def mkByIndex[IV <: SType](input: Value[SCollection[IV]],
                                      index: Value[SInt.type],
                                      default: Option[Value[IV]] = None): Value[IV] =
    ByIndex(input, index, default).withSrcCtx(currentSrcCtx.value)

  override def mkSelectField(input: Value[STuple], fieldIndex: Byte): Value[SType] =
    SelectField(input, fieldIndex).withSrcCtx(currentSrcCtx.value)

  override def mkSizeOf[V <: SType](input: Value[SCollection[V]]): Value[SInt.type] =
    SizeOf(input).withSrcCtx(currentSrcCtx.value)

  override def mkExtractAmount(input: Value[SBox.type]): Value[SLong.type] =
    ExtractAmount(input).withSrcCtx(currentSrcCtx.value)

  override def mkExtractScriptBytes(input: Value[SBox.type]): Value[SByteArray] =
    ExtractScriptBytes(input).withSrcCtx(currentSrcCtx.value)

  override def mkExtractBytes(input: Value[SBox.type]): Value[SByteArray] =
    ExtractBytes(input).withSrcCtx(currentSrcCtx.value)

  override def mkExtractBytesWithNoRef(input: Value[SBox.type]): Value[SByteArray] =
    ExtractBytesWithNoRef(input).withSrcCtx(currentSrcCtx.value)

  override def mkExtractId(input: Value[SBox.type]): Value[SByteArray] =
    ExtractId(input).withSrcCtx(currentSrcCtx.value)

  override def mkExtractCreationInfo(input: Value[SBox.type]): Value[STuple] =
    ExtractCreationInfo(input).withSrcCtx(currentSrcCtx.value)

  override def mkExtractRegisterAs[IV <: SType](input: Value[SBox.type],
                                                registerId: RegisterId,
                                                tpe: SOption[IV]): Value[SType] =
    ExtractRegisterAs(input, registerId, tpe).withSrcCtx(currentSrcCtx.value)

  override def mkDeserializeContext[T <: SType](id: Byte, tpe: T): Value[T] =
    DeserializeContext(id, tpe).withSrcCtx(currentSrcCtx.value)

  override def mkDeserializeRegister[T <: SType](reg: RegisterId,
                                                 tpe: T,
                                                 default: Option[Value[T]] = None): Value[T] =
    DeserializeRegister(reg, tpe, default).withSrcCtx(currentSrcCtx.value)

  override def mkTuple(items: Seq[Value[SType]]): Value[SType] =
    Tuple(items.toIndexedSeq).withSrcCtx(currentSrcCtx.value)

  override def mkProveDiffieHellmanTuple(gv: Value[SGroupElement.type],
                                         hv: Value[SGroupElement.type],
                                         uv: Value[SGroupElement.type],
                                         vv: Value[SGroupElement.type]): SigmaBoolean =
    ProveDHTuple(gv, hv, uv, vv).withSrcCtx(currentSrcCtx.value).asInstanceOf[ProveDHTuple]

  override def mkProveDlog(value: Value[SGroupElement.type]): SigmaBoolean =
    ProveDlog(value).withSrcCtx(currentSrcCtx.value).asInstanceOf[ProveDlog]

  override def mkBoolToSigmaProp(value: BoolValue): SigmaPropValue =
    BoolToSigmaProp(value).withSrcCtx(currentSrcCtx.value)

  override def mkSigmaPropIsProven(value: Value[SSigmaProp.type]): BoolValue =
    SigmaPropIsProven(value).withSrcCtx(currentSrcCtx.value)

  override def mkSigmaPropBytes(value: Value[SSigmaProp.type]): Value[SByteArray] =
    SigmaPropBytes(value).withSrcCtx(currentSrcCtx.value)

  override def mkSigmaAnd(items: Seq[SigmaPropValue]): SigmaPropValue =
    SigmaAnd(items).withSrcCtx(currentSrcCtx.value)

  override def mkSigmaOr(items: Seq[SigmaPropValue]): SigmaPropValue =
    SigmaOr(items).withSrcCtx(currentSrcCtx.value)

  override def mkConcreteCollection[T <: SType](items: IndexedSeq[Value[T]],
                                                elementType: T): Value[SCollection[T]] =
    ConcreteCollection(items, elementType).withSrcCtx(currentSrcCtx.value)

  override def mkTaggedVariable[T <: SType](varId: Byte, tpe: T): TaggedVariable[T] =
    TaggedVariableNode(varId, tpe).withSrcCtx(currentSrcCtx.value).asInstanceOf[TaggedVariable[T]]

  override def mkSomeValue[T <: SType](x: Value[T]): Value[SOption[T]] =
    SomeValue(x).withSrcCtx(currentSrcCtx.value)
  override def mkNoneValue[T <: SType](elemType: T): Value[SOption[T]] =
    NoneValue(elemType).withSrcCtx(currentSrcCtx.value)

  override def mkBlock(bindings: Seq[Val], result: Value[SType]): Value[SType] =
    Block(bindings, result).withSrcCtx(currentSrcCtx.value)

  override def mkBlockValue(items: IndexedSeq[BlockItem], result: Value[SType]): Value[SType] =
    BlockValue(items, result).withSrcCtx(currentSrcCtx.value)

  override def mkValUse(valId: Int, tpe: SType): Value[SType] =
    ValUse(valId, tpe).withSrcCtx(currentSrcCtx.value)

  override def mkZKProofBlock(body: Value[SSigmaProp.type]): BoolValue =
    ZKProofBlock(body).withSrcCtx(currentSrcCtx.value)

  override def mkVal(name: String,
                     givenType: SType,
                     body: Value[SType]): Val = {
    ValNode(name, givenType, body).withSrcCtx(currentSrcCtx.value).asInstanceOf[Val]
  }

  override def mkSelect(obj: Value[SType],
                        field: String,
                        resType: Option[SType] = None): Value[SType] =
    Select(obj, field, resType).withSrcCtx(currentSrcCtx.value)

  override def mkIdent(name: String, tpe: SType): Value[SType] =
    Ident(name, tpe).withSrcCtx(currentSrcCtx.value)

  override def mkApply(func: Value[SType], args: IndexedSeq[Value[SType]]): Value[SType] =
    Apply(func, args).withSrcCtx(currentSrcCtx.value)

  override def mkApplyTypes(input: Value[SType], tpeArgs: Seq[SType]): Value[SType] =
    ApplyTypes(input, tpeArgs).withSrcCtx(currentSrcCtx.value)

  override def mkMethodCallLike(obj: Value[SType],
                            name: String,
                            args: IndexedSeq[Value[SType]],
                            tpe: SType): Value[SType] =
    MethodCallLike(obj, name, args, tpe).withSrcCtx(currentSrcCtx.value)

  override def mkMethodCall(obj: Value[SType],
                            method: SMethod,
                            args: IndexedSeq[Value[SType]]): Value[SType] =
    MethodCall(obj, method, args).withSrcCtx(currentSrcCtx.value)

  override def mkLambda(args: IndexedSeq[(String, SType)],
                        givenResType: SType,
                        body: Option[Value[SType]]): Value[SFunc] =
    Lambda(Nil, args, givenResType, body).withSrcCtx(currentSrcCtx.value)

  def mkGenLambda(tpeParams: Seq[STypeParam],
                  args: IndexedSeq[(String, SType)],
                  givenResType: SType,
                  body: Option[Value[SType]]) =
    Lambda(tpeParams, args, givenResType, body).withSrcCtx(currentSrcCtx.value)

  override def mkConstant[T <: SType](value: T#WrappedType, tpe: T): Constant[T] =
    ConstantNode[T](value, tpe).withSrcCtx(currentSrcCtx.value).asInstanceOf[Constant[T]]


  override def mkConstantPlaceholder[T <: SType](id: Int, tpe: T): Value[SType] =
    ConstantPlaceholder[T](id, tpe).withSrcCtx(currentSrcCtx.value)

  override def mkCollectionConstant[T <: SType](values: Array[T#WrappedType],
                                                elementType: T): Constant[SCollection[T]] =
    ConstantNode[SCollection[T]](values, SCollection(elementType))
      .withSrcCtx(currentSrcCtx.value).asInstanceOf[ConstantNode[SCollection[T]]]

  override def mkStringConcat(left: Constant[SString.type], right: Constant[SString.type]): Value[SString.type] =
    StringConstant(left.value + right.value).withSrcCtx(currentSrcCtx.value)

  override def mkGetVar[T <: SType](varId: Byte, tpe: T): Value[SOption[T]] =
    GetVar(varId, tpe).withSrcCtx(currentSrcCtx.value)

  override def mkOptionGet[T <: SType](input: Value[SOption[T]]): Value[T] =
    OptionGet(input).withSrcCtx(currentSrcCtx.value)

  override def mkOptionGetOrElse[T <: SType](input: Value[SOption[T]], default: Value[T]): Value[T] =
    OptionGetOrElse(input, default).withSrcCtx(currentSrcCtx.value)

  override def mkOptionIsDefined[T <: SType](input: Value[SOption[T]]): Value[SBoolean.type] =
    OptionIsDefined(input).withSrcCtx(currentSrcCtx.value)

  override def mkModQ(input: Value[SBigInt.type]): Value[SBigInt.type] =
    ModQ(input).withSrcCtx(currentSrcCtx.value)

  override def mkPlusModQ(left: Value[SBigInt.type], right: Value[SBigInt.type]): Value[SBigInt.type] =
    ModQArithOp(left, right, OpCodes.PlusModQCode).withSrcCtx(currentSrcCtx.value)

  override def mkMinusModQ(left: Value[SBigInt.type], right: Value[SBigInt.type]): Value[SBigInt.type] =
    ModQArithOp(left, right, OpCodes.MinusModQCode).withSrcCtx(currentSrcCtx.value)

  override def mkLogicalNot(input: Value[SBoolean.type]): Value[SBoolean.type] =
    LogicalNot(input).withSrcCtx(currentSrcCtx.value)

  override def mkNegation[T <: SNumericType](input: Value[T]): Value[T] =
    Negation(input).withSrcCtx(currentSrcCtx.value)

  override def mkBitInversion[T <: SNumericType](input: Value[T]): Value[T] =
    BitInversion(input).withSrcCtx(currentSrcCtx.value)

  override def mkBitOr[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    BitOp(left, right, OpCodes.BitOrCode).withSrcCtx(currentSrcCtx.value)

  override def mkBitAnd[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    BitOp(left, right, OpCodes.BitAndCode).withSrcCtx(currentSrcCtx.value)

  override def mkBitXor[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    BitOp(left, right, OpCodes.BitXorCode).withSrcCtx(currentSrcCtx.value)

  override def mkBitShiftRight[T <: SNumericType](bits: Value[T], shift: Value[T]): Value[T] =
    BitOp(bits, shift, OpCodes.BitShiftRightCode).withSrcCtx(currentSrcCtx.value)

  override def mkBitShiftLeft[T <: SNumericType](bits: Value[T], shift: Value[T]): Value[T] =
    BitOp(bits, shift, OpCodes.BitShiftLeftCode).withSrcCtx(currentSrcCtx.value)

  override def mkBitShiftRightZeroed[T <: SNumericType](bits: Value[T], shift: Value[T]): Value[T] =
    BitOp(bits, shift, OpCodes.BitShiftRightZeroedCode).withSrcCtx(currentSrcCtx.value)

  override def mkUnitConstant: Value[SUnit.type] = UnitConstant().withSrcCtx(currentSrcCtx.value)
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
