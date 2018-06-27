package sigmastate.lang

import sigmastate.Values.Value
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.BuilderException
import sigmastate.serialization.Constraints
import sigmastate.{EQ, NEQ, SNumericType, SType}

trait SigmaBuilder {

  def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S]
  def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S]

  def error(msg: String) = throw new BuilderException(msg, None)
}

class StdSigmaBuilder extends SigmaBuilder {

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] = sigmastate.EQ(left, right)
  override def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S] = sigmastate.NEQ(left, right)
}

trait TransformingSigmaBuilder extends StdSigmaBuilder {

  private def enforceTypeConstraints[S <: SType, R](left: Value[S],
                                    right: Value[S],
                                    cons: (Value[S], Value[S]) => R): R =
    (left.tpe, right.tpe) match {
    case (t1: SNumericType, t2: SNumericType) if t1 != t2 =>
      val tmax = t1 max t2
      val l = left.upcastTo(tmax)
      val r = right.upcastTo(tmax)
      cons(l.asValue[S], r.asValue[S])
    case (t1, t2) =>
      if (t1 == t2)
        cons(left, right)
      else
        error(s"Invalid binary operation parameters ($left, $right): type mismatch $t1 != $t2")
    }

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] =
    enforceTypeConstraints(left, right, super.EQ[S])

  override def NEQ[S <: SType](left: Value[S], right: Value[S]): NEQ[S] =
    enforceTypeConstraints(left, right, super.NEQ[S])
}

trait CheckingSigmaBuilder extends StdSigmaBuilder {

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] = {
    val constraints = Seq(Constraints.sameType2)
    constraints.foreach { c =>
      if (!c(left.tpe, right.tpe))
        error(s"Failed constraint $c for binary operation EQ ($left, $right)")
    }
    super.EQ(left, right)
  }
}

case object DefaultSigmaBuilder extends StdSigmaBuilder with CheckingSigmaBuilder

case object TransformingSigmaBuilder extends StdSigmaBuilder with TransformingSigmaBuilder

case object DeserializationSigmaBuilder extends StdSigmaBuilder with TransformingSigmaBuilder
