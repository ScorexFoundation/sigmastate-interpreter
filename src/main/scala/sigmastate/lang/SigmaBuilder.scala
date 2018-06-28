package sigmastate.lang

import sigmastate.Values.Value
import sigmastate._
import sigmastate.lang.Constraints.{TypeConstraint2, onlyNumeric2, sameType2}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{BuilderException, ConstraintFailed}
import sigmastate.serialization.OpCodes

trait SigmaBuilder {

  def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S]
  def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S]

  def GT[S <: SType](left: Value[S], right: Value[S]): GT[S]
  def GE[S <: SType](left: Value[S], right: Value[S]): GE[S]
  def LT[S <: SType](left: Value[S], right: Value[S]): LT[S]
  def LE[S <: SType](left: Value[S], right: Value[S]): LE[S]

  def Plus[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S]
  def Minus[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S]
  def Multiply[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S]
  def Divide[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S]
  def Modulo[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S]

  def error(msg: String) = throw new BuilderException(msg, None)
}

class StdSigmaBuilder extends SigmaBuilder {

  protected def equalityOp[S <: SType, R](left: Value[S],
                                        right: Value[S],
                                        cons: (Value[S], Value[S]) => R): R = cons(left, right)

  protected def comparisonOp[S <: SType, R](left: Value[S],
                                          right: Value[S],
                                          cons: (Value[S], Value[S]) => R): R = cons(left, right)

  protected def arithOp[S <: SNumericType, R](left: Value[S],
                                            right: Value[S],
                                            cons: (Value[S], Value[S]) => R): R = cons(left, right)

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] =
    equalityOp(left, right, sigmastate.EQ.apply)

  override def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S] =
    equalityOp(left, right, sigmastate.NEQ.apply)

  override def GT[S <: SType](left: Value[S], right: Value[S]): GT[S] =
    comparisonOp(left, right, sigmastate.GT.apply)

  override def GE[S <: SType](left: Value[S], right: Value[S]): GE[S] =
    comparisonOp(left, right, sigmastate.GE.apply)

  override def LT[S <: SType](left: Value[S], right: Value[S]): LT[S] =
    comparisonOp(left, right, sigmastate.LT.apply)

  override def LE[S <: SType](left: Value[S], right: Value[S]): LE[S] =
    comparisonOp(left, right, sigmastate.LE.apply)

  override def Plus[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S] =
    arithOp(left, right, { (l: Value[S], r: Value[S]) => sigmastate.ArithOp[S](l, r, OpCodes.PlusCode) })

  override def Minus[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S] =
    arithOp(left, right, { (l: Value[S], r: Value[S]) => sigmastate.ArithOp[S](l, r, OpCodes.MinusCode) })

  override def Multiply[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S] =
    arithOp(left, right, { (l: Value[S], r: Value[S]) => sigmastate.ArithOp[S](l, r, OpCodes.MultiplyCode) })

  override def Divide[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S] =
    arithOp(left, right, { (l: Value[S], r: Value[S]) => sigmastate.ArithOp[S](l, r, OpCodes.DivisionCode) })

  override def Modulo[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S] =
    arithOp(left, right, { (l: Value[S], r: Value[S]) => sigmastate.ArithOp[S](l, r, OpCodes.ModuloCode) })
}

trait TypeConstraintCheck {

  def check2[S <: SType](left: Value[S],
                                 right: Value[S],
                                 constraints: Seq[TypeConstraint2]): Unit =
    constraints.foreach { c =>
      if (!c(left.tpe, right.tpe))
        throw new ConstraintFailed(s"Failed constraint $c for binary operation parameters ($left(tpe: ${left.tpe}), $right(tpe: ${right.tpe}))")
    }
}

trait TransformingSigmaBuilder extends StdSigmaBuilder with TypeConstraintCheck {

  private def applyUpcast[S <: SType](left: Value[S], right: Value[S]):(Value[S], Value[S]) =
    (left.tpe, right.tpe) match {
      case (t1: SNumericType, t2: SNumericType) if t1 != t2 =>
        val tmax = t1 max t2
        val l = left.upcastTo(tmax)
        val r = right.upcastTo(tmax)
        (l.asValue[S], r.asValue[S])
      case _ =>
        (left, right)
    }

  override protected def equalityOp[S <: SType, R](left: Value[S],
                                        right: Value[S],
                                        cons: (Value[S], Value[S]) => R): R = {
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    cons(l, r)
  }

  override protected def comparisonOp[S <: SType, R](left: Value[S],
                                          right: Value[S],
                                          cons: (Value[S], Value[S]) => R): R = {
    check2(left, right, Seq(onlyNumeric2))
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    cons(l, r)
  }

  override protected def arithOp[S <: SNumericType, R](left: Value[S],
                                            right: Value[S],
                                            cons: (Value[S], Value[S]) => R): R = {
    val (l, r) = applyUpcast(left, right)
    cons(l, r)
  }
}

trait CheckingSigmaBuilder extends StdSigmaBuilder with TypeConstraintCheck {

  override protected def equalityOp[S <: SType, R](left: Value[S],
                                        right: Value[S],
                                        cons: (Value[S], Value[S]) => R): R = {
    check2(left, right, Seq(sameType2))
    cons(left, right)
  }

  override protected def comparisonOp[S <: SType, R](left: Value[S],
                                          right: Value[S],
                                          cons: (Value[S], Value[S]) => R): R = {
    check2(left, right, Seq(onlyNumeric2, sameType2))
    cons(left, right)
  }

  override protected def arithOp[S <: SNumericType, R](left: Value[S],
                                            right: Value[S],
                                            cons: (Value[S], Value[S]) => R): R = {
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
