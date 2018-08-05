package sigmastate.eval

import java.lang.reflect.Method

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

  def compile[T <: SType](dataEnv: mutable.Map[Sym, AnyRef], f: Rep[Context => T#WrappedType]): ContextFunc[T] = {

    def getArgTypes(args: Seq[AnyRef]) = {
      val types = args.map {
        case s: Sym => dataEnv(s).getClass
        case _: Seq[_] => classOf[Seq[_]]
      }
      types
    }

    def getArgValues(args: Seq[AnyRef]): Seq[AnyRef] = {
      val vs = args.map {
        case s: Sym => dataEnv(s)
        case vec: Vector[AnyRef]@unchecked => Vector(getArgValues(vec):_*)
      }
      vs
    }

    def getObjMethod(objClass: Class[_], objMethod: Method, argTypes: Seq[Class[_]]): Method = {
      val methods = objClass.getMethods
      val lookupName = objMethod.getName
      val resMethods = methods.filter(m => m.getName == lookupName)
      def error = !!!(s"Cannot resolve of pre-staged method $objMethod in class $objClass")
      resMethods.length match {
        case 0 =>
          error
        case 1 =>
          resMethods(0)
        case _ =>
          val res = resMethods.find { m =>
            val mArgTypes = m.getParameterTypes
            val N = mArgTypes.length
            (N == argTypes.length) && {
              (0 until N).forall { i =>
                mArgTypes(i).isAssignableFrom(argTypes(i))
              }
            }
          }
          res.getOrElse(error)
      }
    }

    def evaluate(te: TableEntry[_]): Unit = {
      def out(v: AnyRef) = dataEnv += (te.sym -> v)
      te.rhs match {
        case Const(x) => out(x.asInstanceOf[AnyRef])
        case _: DslBuilder | _: ColBuilder | _: IntPlusMonoid =>
          dataEnv.getOrElse(te.sym, !!!(s"Cannot resolve companion instance for $te"))
        case MethodCall(obj, m, args, _) =>
          val argTypes = getArgTypes(args)
          val argValues = getArgValues(args)
          val objValue = dataEnv(obj)
          val objMethod = getObjMethod(objValue.getClass, m, argTypes)
          out(objMethod.invoke(objValue, argValues:_*))
        case ApplyBinOp(op: BinOp[a,r], xSym, ySym) =>
          val x = dataEnv(xSym)
          val y = dataEnv(ySym)
          out(op.applySeq(x, y).asInstanceOf[AnyRef])
        case Lambda(l, _, x, y) =>
          val f = (ctx: AnyRef) => {
            dataEnv += (x -> ctx)
            l.schedule.foreach(evaluate(_))
            dataEnv(y)
          }
          out(f)
        case _ => !!!(s"Don't know how to evaluate($te)")
      }
    }

    val g = new PGraph(f)
    g.schedule.foreach(evaluate(_))
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
