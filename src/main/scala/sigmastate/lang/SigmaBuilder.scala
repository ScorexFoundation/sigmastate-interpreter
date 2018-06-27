package sigmastate.lang

import sigmastate.Values.Value
import sigmastate._
import sigmastate.lang.Constraints.{TypeConstraint2, onlyNumeric2, sameType2}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.BuilderException
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode

trait SigmaBuilder {

  def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S]
  def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S]

  def GT[S <: SType](left: Value[S], right: Value[S]): GT[S]
  def GE[S <: SType](left: Value[S], right: Value[S]): GE[S]
  def LT[S <: SType](left: Value[S], right: Value[S]): LT[S]
  def LE[S <: SType](left: Value[S], right: Value[S]): LE[S]

  def Plus[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S]

  def error(msg: String) = throw new BuilderException(msg, None)
}

class StdSigmaBuilder extends SigmaBuilder {

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] = sigmastate.EQ(left, right)
  override def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S] = sigmastate.NEQ(left, right)
  override def GT[S <: SType](left: Value[S], right: Value[S]): GT[S] = sigmastate.GT(left, right)
  override def GE[S <: SType](left: Value[S], right: Value[S]): GE[S] = sigmastate.GE(left, right)
  override def LT[S <: SType](left: Value[S], right: Value[S]): LT[S] = sigmastate.LT(left, right)
  override def LE[S <: SType](left: Value[S], right: Value[S]): LE[S] = sigmastate.LE(left, right)

  override def Plus[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S] =
    sigmastate.ArithOp(left, right, OpCodes.PlusCode)
}

trait TypeConstraintCheck{

  def check2[S <: SType](left: Value[S],
                                 right: Value[S],
                                 constraints: Seq[TypeConstraint2]): Unit =
    constraints.foreach { c =>
      if (!c(left.tpe, right.tpe))
        throw new BuilderException(s"Failed constraint $c for binary operation parameters ($left(tpe: ${left.tpe}), $right(tpe: ${right.tpe}))")
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

  // fixme DRY

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] = {
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    super.EQ(l, r)
  }

  override def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S] = {
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    super.NEQ(l, r)
  }

  override def GT[S <: SType](left: Value[S], right: Value[S]): GT[S] = {
    check2(left, right, Seq(onlyNumeric2))
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    super.GT(l, r)
  }

  override def GE[S <: SType](left: Value[S], right: Value[S]): GE[S] = {
    check2(left, right, Seq(onlyNumeric2))
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    super.GE(l, r)
  }

  override def LT[S <: SType](left: Value[S], right: Value[S]): LT[S] = {
    check2(left, right, Seq(onlyNumeric2))
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    super.LT(l, r)
  }

  override def LE[S <: SType](left: Value[S], right: Value[S]): LE[S] = {
    check2(left, right, Seq(onlyNumeric2))
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(sameType2))
    super.LE(l, r)
  }

  override def Plus[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S] = {
    val (l, r) = applyUpcast(left, right)
    super.Plus(l, r)
  }
}

trait CheckingSigmaBuilder extends StdSigmaBuilder with TypeConstraintCheck {

  // fixme DRY

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] = {
    check2(left, right, Seq(sameType2))
    super.EQ(left, right)
  }

  override def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S] = {
    check2(left, right, Seq(sameType2))
    super.NEQ(left, right)
  }

  override def GT[S <: SType](left: Value[S], right: Value[S]): GT[S] = {
    check2(left, right, Seq(onlyNumeric2, sameType2))
    super.GT(left, right)
  }

  override def GE[S <: SType](left: Value[S], right: Value[S]): GE[S] = {
    check2(left, right, Seq(onlyNumeric2, sameType2))
    super.GE(left, right)
  }

  override def LT[S <: SType](left: Value[S], right: Value[S]): LT[S] = {
    check2(left, right, Seq(onlyNumeric2, sameType2))
    super.LT(left, right)
  }

  override def LE[S <: SType](left: Value[S], right: Value[S]): LE[S] = {
    check2(left, right, Seq(onlyNumeric2, sameType2))
    super.LE(left, right)
  }

  override def Plus[S <: SNumericType](left: Value[S], right: Value[S]): ArithOp[S] = {
    check2(left, right, Seq(sameType2))
    super.Plus(left, right)
  }
}

case object StdSigmaBuilder extends StdSigmaBuilder
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
