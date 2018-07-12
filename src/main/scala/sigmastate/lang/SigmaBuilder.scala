package sigmastate.lang

import sigmastate.Values.Value
import sigmastate._
import sigmastate.lang.Constraints.{TypeConstraint2, onlyNumeric2, sameType2}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{BuilderException, ConstraintFailed}
import sigmastate.serialization.OpCodes

trait SigmaBuilder {

  def EQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def NEQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]

  def GT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def GE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def LT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]
  def LE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type]

  def Plus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def Minus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def Multiply[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def Divide[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]
  def Modulo[T <: SNumericType](left: Value[T], right: Value[T]): Value[T]

  def OR(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type]
  def AND(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type]

  def Exponentiate(left: Value[SGroupElement.type],
                   right: Value[SBigInt.type]): Value[SGroupElement.type]

  def error(msg: String) = throw new BuilderException(msg, None)
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

  override def EQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    equalityOp(left, right, sigmastate.EQ.apply[T])

  override def NEQ[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    equalityOp(left, right, sigmastate.NEQ.apply[T])

  override def GT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, sigmastate.GT.apply[T])

  override def GE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, sigmastate.GE.apply[T])

  override def LT[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, sigmastate.LT.apply[T])

  override def LE[T <: SType](left: Value[T], right: Value[T]): Value[SBoolean.type] =
    comparisonOp(left, right, sigmastate.LE.apply[T])

  override def Plus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => sigmastate.ArithOp[T](l, r, OpCodes.PlusCode) })

  override def Minus[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => sigmastate.ArithOp[T](l, r, OpCodes.MinusCode) })

  override def Multiply[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => sigmastate.ArithOp[T](l, r, OpCodes.MultiplyCode) })

  override def Divide[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => sigmastate.ArithOp[T](l, r, OpCodes.DivisionCode) })

  override def Modulo[T <: SNumericType](left: Value[T], right: Value[T]): Value[T] =
    arithOp(left, right, { (l: Value[T], r: Value[T]) => sigmastate.ArithOp[T](l, r, OpCodes.ModuloCode) })

  override def OR(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type] =
    sigmastate.OR(input)

  override def AND(input: Value[SCollection[SBoolean.type]]): Value[SBoolean.type] =
    sigmastate.AND(input)

  override def Exponentiate(left: Value[SGroupElement.type], right: Value[SBigInt.type]): Value[SGroupElement.type] =
    sigmastate.Exponentiate(left, right)
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
