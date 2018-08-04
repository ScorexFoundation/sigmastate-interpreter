package sigmastate.eval

import sigmastate.Values.Value
import sigmastate.lang.Costing

import scala.collection.mutable
import scala.util.Try

trait Evaluation extends Costing {
  import Context._
  import Sigma._
  import Col._
  import Box._

  val ContextM = ContextMethods
  val SigmaM = SigmaMethods
  val ColM = ColMethods
  val BoxM = BoxMethods

  def isValidCostPrimitive(d: Def[_]): Unit = d match {
    case _: Const[_] =>
    case _: IntPlusMonoid =>
    case _: Lambda[_,_] =>
    case _: ThunkDef[_] =>
    case ApplyBinOp(_: NumericPlus[_],_,_) =>
    case ContextM.OUTPUTS(_) | ContextM.INPUTS(_) | ContextM.getVar(_,_,_) =>
    case SigmaM.propBytes(_) =>
    case ColM.length(_) | ColM.map(_,_) | ColM.sum(_,_) =>
    case BoxM.propositionBytes(_) =>
  }

  def verifyCostFunc(costF: Rep[Context => Int]): Try[Unit] = {
    val Def(Lambda(lam,_,_,_)) = costF
    Try { lam.scheduleAll.foreach(te => isValidCostPrimitive(te.rhs)) }
  }

  def findIsValid[T](f: Rep[Context => T]): Option[Sym] = {
    val Def(Lambda(lam,_,_,_)) = f
    val ok = lam.scheduleAll.collectFirst {
      case TableEntry(s, SigmaM.isValid(_)) => s
    }
    ok
  }

  def verifyIsValid[T](f: Rep[Context => T]): Try[Unit] = {
    val isValidOpt = findIsValid(f)
    Try {
      isValidOpt match {
        case Some(s) =>
          if (f.getLambda.y != s) !!!(s"Sigma.isValid found in none-root position", s)
        case None =>
      }
    }
  }

  import sigmastate._
  import special.sigma.{Context => SigmaContext}

  type ContextFunc[T <: SType] = SigmaContext => Value[T]

  def compile[T <: SType](f: Rep[Context => T#WrappedType]): ContextFunc[T] = {
    val g = new PGraph(f)
    val dataEnv = mutable.Map[Sym, Any]()
    def out(s: Sym, v: Any) = dataEnv += (s -> v)

    def evaluate(te: TableEntry[_]): Unit = te.rhs match {
      case Const(x) => out(te.sym, x)
      case Lambda(l, _, x, y) =>
        val f = (ctx: Any) => {
          dataEnv += (x -> ctx)
          l.schedule.foreach(evaluate(_))
          dataEnv(y)
        }
        out(te.sym, f)
      case _ => !!!(s"Don't know how to evaluate($te)")
    }
    g.schedule.foreach(te => evaluate(te))
    val fun = dataEnv(f).asInstanceOf[SigmaContext => Any]
    val res = (ctx: SigmaContext) => {
      fun(ctx) match {
        case v: Value[_] => v
        case x => builder.liftAny(x).get
      }
    }
    res.asInstanceOf[ContextFunc[T]]
  }

}
