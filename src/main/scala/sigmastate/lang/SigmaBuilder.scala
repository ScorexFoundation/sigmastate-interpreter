package sigmastate.lang

import org.ergoplatform.ErgoBox.RegisterId
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{ConcreteCollection, Constant, ConstantNode, NoneValue, SValue, SigmaBoolean, SomeValue, TaggedVariable, TaggedVariableNode, Tuple, Value}
import sigmastate._
import sigmastate.lang.Constraints.{TypeConstraint2, onlyNumeric2, sameType2}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{ArithException, ConstraintFailed}
import sigmastate.serialization.OpCodes
import sigmastate.utxo._

trait SigmaBuilder {

  def mkEQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def mkNEQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]

  def mkGT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def mkGE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def mkLT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def mkLE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]

  def mkPlus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkMinus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkMultiply[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkDivide[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def mkModulo[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]

  def mkOR(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type]
  def mkAND(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type]

  def mkExponentiate(left: Value[SGroupElement.type],
                     right: Value[SBigInt.type]): Value[SGroupElement.type]
  def mkMultiplyGroup(left: Value[SGroupElement.type],
                      right: Value[SGroupElement.type]): Value[SGroupElement.type]
  def mkXor(left: Value[SByteArray], right: Value[SByteArray]): Value[SByteArray]

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

  def mkMapCollection[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                                id: Byte,
                                                mapper: SValue): Value[SCollection[OV]]
  def mkAppend[IV <: SType](input: Value[SCollection[IV]],
                            col2: Value[SCollection[IV]]): Value[SCollection[IV]]

  def mkSlice[IV <: SType](input: Value[SCollection[IV]],
                           from: Value[SInt.type],
                           until: Value[SInt.type]): Value[SCollection[IV]]

  def mkWhere[IV <: SType](input: Value[SCollection[IV]],
                           id: Byte,
                           condition: Value[SBoolean.type]): Value[SCollection[IV]]

  def mkExists[IV <: SType](input: Value[SCollection[IV]],
                            id: Byte,
                            condition: Value[SBoolean.type]): Value[SBoolean.type]

  def mkForAll[IV <: SType](input: Value[SCollection[IV]],
                               id: Byte,
                               condition: Value[SBoolean.type]): Value[SBoolean.type]

  def mkFold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                          id: Byte,
                          zero: Value[OV],
                          accId: Byte,
                          foldOp: SValue): Value[OV]

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

  def mkExtractRegisterAs[IV <: SType](input: Value[SBox.type],
                                      registerId: RegisterId,
                                      tpe: IV,
                                      default: Option[Value[IV]]): Value[IV]

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

  def mkConcreteCollection[T <: SType](items: IndexedSeq[Value[T]],
                                       elementType: T): Value[SCollection[T]]

  def mkTaggedVariable[T <: SType](varId: Byte, tpe: T): TaggedVariable[T]

  def mkSomeValue[T <: SType](x: Value[T]): Value[SOption[T]]
  def mkNoneValue[T <: SType](elemType: T): Value[SOption[T]]

  def mkBlock(bindings: Seq[Let], result: Value[SType]): Value[SType]
  def mkLet(name: String, givenType: SType, body: Value[SType]): Let
  def mkSelect(obj: Value[SType], field: String, resType: Option[SType] = None): Value[SType]
  def mkIdent(name: String, tpe: SType): Value[SType]
  def mkApply(func: Value[SType], args: IndexedSeq[Value[SType]]): Value[SType]
  def mkApplyTypes(input: Value[SType], tpeArgs: Seq[SType]): Value[SType]
  def mkMethodCall(obj: Value[SType],
                   name: String,
                   args: IndexedSeq[Value[SType]],
                   tpe: SType = NoType): Value[SType]
  def mkLambda(args: IndexedSeq[(String,SType)],
               givenResType: SType,
               body: Option[Value[SType]]): Value[SFunc]

  def mkConstant[T <: SType](value: T#WrappedType, tpe: T): Constant[T]
  def mkCollectionConstant[T <: SType](values: Array[T#WrappedType],
                                       elementType: T): Constant[SCollection[T]]
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

  override def mkPlus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => ArithOp[T](l, r, OpCodes.PlusCode) })

  override def mkMinus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => ArithOp[T](l, r, OpCodes.MinusCode) })

  override def mkMultiply[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => ArithOp[T](l, r, OpCodes.MultiplyCode) })

  override def mkDivide[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => ArithOp[T](l, r, OpCodes.DivisionCode) })

  override def mkModulo[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => ArithOp[T](l, r, OpCodes.ModuloCode) })

  override def mkOR(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type] =
    OR(input)

  override def mkAND(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type] =
    AND(input)

  override def mkExponentiate(left: Value[SGroupElement.type], right: Value[SBigInt.type]): Value[SGroupElement.type] =
    Exponentiate(left, right)

  override def mkMultiplyGroup(left: Value[SGroupElement.type], right: Value[SGroupElement.type]): Value[SGroupElement.type] =
    MultiplyGroup(left, right)

  override def mkXor(left: Value[SByteArray], right: Value[SByteArray]): Value[SByteArray] =
    Xor(left, right)

  override def mkIsMember(tree: Value[SAvlTree.type],
                          key: Value[SByteArray],
                          proof: Value[SByteArray]): Value[SBoolean.type] =
    IsMember(tree, key, proof)

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

  override def mkMapCollection[IV <: SType, OV <: SType](input: Value[SCollection[IV]], id: Byte, mapper: SValue): Value[SCollection[OV]] =
    MapCollection(input, id, mapper)

  override def mkAppend[IV <: SType](input: Value[SCollection[IV]],
                                     col2: Value[SCollection[IV]]): Value[SCollection[IV]] =
    Append(input, col2)

  override def mkSlice[IV <: SType](input: Value[SCollection[IV]],
                                    from: Value[SInt.type],
                                    until: Value[SInt.type]): Value[SCollection[IV]] =
    Slice(input, from, until)

  override def mkWhere[IV <: SType](input: Value[SCollection[IV]],
                                    id: Byte,
                                    condition: Value[SBoolean.type]): Value[SCollection[IV]] =
    Where(input, id, condition)

  override def mkExists[IV <: SType](input: Value[SCollection[IV]],
                                     id: Byte,
                                     condition: Value[SBoolean.type]): Value[SBoolean.type] =
    Exists(input, id, condition)

  override def mkForAll[IV <: SType](input: Value[SCollection[IV]],
                                     id: Byte,
                                     condition: Value[SBoolean.type]): Value[SBoolean.type] =
    ForAll(input, id, condition)

  override def mkFold[IV <: SType, OV <: SType](input: Value[SCollection[IV]],
                                   id: Byte,
                                   zero: Value[OV],
                                   accId: Byte,
                                   foldOp: SValue): Value[OV] =
    Fold(input, id, zero, accId, foldOp)

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

  override def mkExtractRegisterAs[IV <: SType](input: Value[SBox.type],
                                                registerId: RegisterId,
                                                tpe: IV,
                                                default: Option[Value[IV]] = None): Value[IV] =
    ExtractRegisterAs(input, registerId, tpe, default)

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
    ProveDiffieHellmanTuple(gv, hv, uv, vv)

  override def mkProveDlog(value: Value[SGroupElement.type]): SigmaBoolean =
    ProveDlog(value)

  override def mkConcreteCollection[T <: SType](items: IndexedSeq[Value[T]],
                                                elementType: T): Value[SCollection[T]] =
    ConcreteCollection(items, elementType)

  override def mkTaggedVariable[T <: SType](varId: Byte, tpe: T): TaggedVariable[T] =
    TaggedVariableNode(varId, tpe)

  override def mkSomeValue[T <: SType](x: Value[T]): Value[SOption[T]] = SomeValue(x)
  override def mkNoneValue[T <: SType](elemType: T): Value[SOption[T]] = NoneValue(elemType)

  override def mkBlock(bindings: Seq[Let], result: Value[SType]): Value[SType] =
    Block(bindings, result)

  override def mkLet(name: String, givenType: SType, body: Value[SType]): Let =
    LetNode(name, givenType, body)

  override def mkSelect(obj: Value[SType],
                        field: String,
                        resType: Option[SType] = None): Value[SType] =
    Select(obj, field, resType)

  override def mkIdent(name: String, tpe: SType): Value[SType] = Ident(name, tpe)

  override def mkApply(func: Value[SType], args: IndexedSeq[Value[SType]]): Value[SType] =
    Apply(func, args)

  override def mkApplyTypes(input: Value[SType], tpeArgs: Seq[SType]): Value[SType] =
    ApplyTypes(input, tpeArgs)

  override def mkMethodCall(obj: Value[SType],
                            name: String,
                            args: IndexedSeq[Value[SType]],
                            tpe: SType): Value[SType] =
    MethodCall(obj, name, args, tpe)

  override def mkLambda(args: IndexedSeq[(String, SType)],
                        givenResType: SType,
                        body: Option[Value[SType]]): Value[SFunc] =
    Lambda(args, givenResType, body)

  override def mkConstant[T <: SType](value: T#WrappedType, tpe: T): Constant[T] =
    ConstantNode[T](value, tpe)

  override def mkCollectionConstant[T <: SType](values: Array[T#WrappedType],
                                                elementType: T): Constant[SCollection[T]] =
    ConstantNode[SCollection[T]](values, SCollection(elementType))
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

case object CheckingSigmaBuilder extends StdSigmaBuilder with CheckingSigmaBuilder {

//  override def mkDowncast[T <: SNumericType, R <: SNumericType](input: Value[T], tpe: R): Value[R] = (input, tpe) match {
//      // todo implement
//    case (Constant(value: Short, SShort), SByte)
//      if value < Byte.MinValue || value > Byte.MaxValue =>
//      throw new ArithException(s"Byte overflow in Downcast($value)")
//    case (Constant(value: Int, SInt), SByte)
//      if value < Byte.MinValue || value > Byte.MaxValue =>
//      throw new ArithException(s"Byte overflow in Downcast($value)")
//    case _ => super.mkDowncast(input, tpe)
//  }
}

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
