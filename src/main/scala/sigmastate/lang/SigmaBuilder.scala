package sigmastate.lang

import sigmastate.Values.Value
import sigmastate.serialization.Constraints
import sigmastate.{EQ, SNumericType, SType}
import sigmastate.lang.Terms._

trait SigmaBuilder {

  def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S]

  def error(msg: String) = throw new BuilderException(msg, None)
}

class StdSigmaBuilder extends SigmaBuilder {

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] =
    sigmastate.EQ(left, right)
}

trait TransformingSigmaBuilder extends StdSigmaBuilder {

  override def EQ[S <: SType](left: Value[S], right: Value[S]): EQ[S] =  (left.tpe, right.tpe) match {
    case (t1: SNumericType, t2: SNumericType) if t1 != t2 =>
      val tmax = t1 max t2
      val l = left.upcastTo(tmax)
      val r = right.upcastTo(tmax)
      super.EQ(l.asValue[S], r.asValue[S])
    case _ =>
      super.EQ(left, right)
  }
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

case object DeserializationSigmaBuilder extends StdSigmaBuilder with TransformingSigmaBuilder
