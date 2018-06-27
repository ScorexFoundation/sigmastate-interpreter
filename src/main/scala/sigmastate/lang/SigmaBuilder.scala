package sigmastate.lang

import sigmastate.Values.Value
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.BuilderException
import sigmastate.serialization.Constraints

trait SigmaBuilder {

  def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S]
  def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S]
  def GT[S <: SType](left: Value[S], right: Value[S]): GT[S]

  def error(msg: String) = throw new BuilderException(msg, None)
}

class StdSigmaBuilder extends SigmaBuilder {

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] = sigmastate.EQ(left, right)
  override def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S] = sigmastate.NEQ(left, right)
  override def GT[S <: SType](left: Value[S], right: Value[S]): GT[S] = sigmastate.GT(left, right)
}

trait TypeConstraintCheck{

  def check2[S <: SType](left: Value[S],
                                 right: Value[S],
                                 constraints: Seq[Constraints.TypeConstraint2]): Unit =
    constraints.foreach { c =>
      if (!c(left.tpe, right.tpe))
        throw new BuilderException(s"Failed constraint $c for binary operation parameters ($left, $right)")
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

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] = {
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(Constraints.sameType2))
    super.EQ(l, r)
  }

  override def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S] = {
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(Constraints.sameType2))
    super.NEQ(l, r)
  }

  override def GT[S <: SType](left: Value[S], right: Value[S]): GT[S] = {
    check2(left, right, Seq(Constraints.onlyNumeric2))
    val (l, r) = applyUpcast(left, right)
    check2(l, r, Seq(Constraints.sameType2))
    super.GT(l, r)
  }
}

trait CheckingSigmaBuilder extends StdSigmaBuilder with TypeConstraintCheck {

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] = {
    check2(left, right, Seq(Constraints.sameType2))
    super.EQ(left, right)
  }

  override def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S] = {
    check2(left, right, Seq(Constraints.sameType2))
    super.NEQ(left, right)
  }

  override def GT[S <: SType](left: Value[S], right: Value[S]): GT[S] = {
    check2(left, right, Seq(Constraints.onlyNumeric2, Constraints.sameType2))
    super.GT(left, right)
  }
}

case object DefaultSigmaBuilder extends StdSigmaBuilder with CheckingSigmaBuilder

case object TransformingSigmaBuilder extends StdSigmaBuilder with TransformingSigmaBuilder

case object DeserializationSigmaBuilder extends StdSigmaBuilder with TransformingSigmaBuilder
