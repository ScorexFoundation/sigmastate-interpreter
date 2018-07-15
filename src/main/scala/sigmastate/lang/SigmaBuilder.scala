package sigmastate.lang

import sigmastate.SCollection.SByteArray
import sigmastate.Values.{Constant, SValue, Value}
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

  def mkIntToByte(input: Value[SInt.type]): Value[SByte.type]
  def mkLongToByteArray(input: Value[SLong.type]): Value[SByteArray]

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

  def mkFold[IV <: SType](input: Value[SCollection[IV]],
                          id: Byte,
                          zero: Value[IV],
                          accId: Byte,
                          foldOp: SValue): Value[IV]

  def mkByIndex[IV <: SType](input: Value[SCollection[IV]],
                               index: Value[SInt.type],
                               default: Option[Value[IV]] = None): Value[IV]
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

  override def mkIntToByte(input: Value[SInt.type]): Value[SByte.type] =
    IntToByte(input)

  override def mkLongToByteArray(input: Value[SLong.type]): Value[SByteArray] =
    LongToByteArray(input)

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

  override def mkFold[IV <: SType](input: Value[SCollection[IV]],
                                   id: Byte,
                                   zero: Value[IV],
                                   accId: Byte,
                                   foldOp: SValue): Value[IV] =
    Fold(input, id, zero, accId, foldOp)

  override def mkByIndex[IV <: SType](input: Value[SCollection[IV]],
                                      index: Value[SInt.type],
                                      default: Option[Value[IV]] = None): Value[IV] =
    ByIndex(input, index, default)
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
  override def mkIntToByte(input: Value[SInt.type]): Value[SByte.type] = input match {
    case Constant(value: Int, SInt) if value > Byte.MaxValue =>
      throw new ArithException(s"Byte overflow in IntToByte($value)")
    case _ => super.mkIntToByte(input)
  }
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
