package sigmastate.eval

import java.lang.reflect.Method

import scapi.sigma.DLogProtocol
import sigmastate.Values.{ConcreteCollection, Value, SigmaBoolean, Constant}
import sigmastate.lang.Costing

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Try

trait Evaluation extends Costing {
  import Context._
  import Sigma._
  import Col._
  import Box._
  import SigmaDslBuilder._

  val ContextM = ContextMethods
  val SigmaM = SigmaMethods
  val ColM = ColMethods
  val BoxM = BoxMethods
  val SDBM = SigmaDslBuilderMethods

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
  import Values.{FalseLeaf, TrueLeaf}
  import special.sigma.{Context => SigmaContext}

  type ContextFunc[T <: SType] = SigmaContext => Value[T]

  val sigmaDslBuilderValue: special.sigma.SigmaDslBuilder
  val costedBuilderValue: special.collection.ConcreteCostedBuilder
  val monoidBuilderValue: special.collection.MonoidBuilder

  def compile[T <: SType](dataEnv: mutable.Map[Sym, AnyRef], f: Rep[Context => T#WrappedType]): ContextFunc[T] = {

    object In { def unapply(s: Sym): Option[Any] = Some(dataEnv(s)) }

    def getArgTypes(args: Seq[AnyRef]) = {
      val types = args.map {
        case s: Sym => dataEnv(s).getClass
        case _: Seq[_] => classOf[Seq[_]]
        case e: Elem[_] => classOf[ClassTag[_]]
      }
      types
    }

    def getArgValues(args: Seq[AnyRef]): Seq[AnyRef] = {
      val vs = args.map {
        case s: Sym => dataEnv(s)
        case vec: Seq[AnyRef]@unchecked => getArgValues(vec)
        case e: Elem[_] => e.classTag
      }
      vs
    }

    def getObjMethod(objClass: Class[_], objMethod: Method, args: Seq[AnyRef]): Method = {
      val argTypes = getArgTypes(args)
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

    def getObjMethodAndArgs(objClass: Class[_], mc: MethodCall): (Method, Seq[AnyRef]) = mc match {
      case ColM.map(col, f) =>
        val args = Seq(f, f.elem.eRange)
        val m = getObjMethod(objClass, mc.method, args)
        val argValues = getArgValues(args)
        (m, argValues)
      case _ =>
        val m = getObjMethod(objClass, mc.method, mc.args)
        val argValues = getArgValues(mc.args)
        (m, argValues)
    }

    def evaluate(te: TableEntry[_]): Unit = {
      def out(v: Any) = dataEnv += (te.sym -> v.asInstanceOf[AnyRef])
      try {
        te.rhs match {
          case Const(x) => out(x.asInstanceOf[AnyRef])
          case _: DslBuilder | _: ColBuilder | _: IntPlusMonoid =>
            dataEnv.getOrElse(te.sym, !!!(s"Cannot resolve companion instance for $te"))
          case SigmaM.propBytes(prop) =>
            val sigmaBool = dataEnv(prop).asInstanceOf[SigmaBoolean]
            out(sigmaDslBuilderValue.Cols.fromArray(sigmaBool.bytes))
          case SigmaM.isValid(In(prop: AnyRef)) =>
            out(prop)
            
          case SigmaM.and_bool_&&(In(l: Value[SBoolean.type]@unchecked), In(b: Boolean)) =>
            if (b)
              out(l)
            else
              out(DLogProtocol.TrivialSigma(false))
          case SigmaM.or_bool_||(In(l: Value[SBoolean.type]@unchecked), In(b: Boolean)) =>
            if (b)
              out(DLogProtocol.TrivialSigma(true))
            else
              out(l)

          case SDBM.anyZK(_, In(items: special.collection.Col[Value[SBoolean.type]]@unchecked)) =>
            out(new OR(ConcreteCollection(items.arr.toIndexedSeq, SBoolean)).function(null, null))
          case SDBM.allZK(_, In(items: special.collection.Col[Value[SBoolean.type]]@unchecked)) =>
            out(new AND(ConcreteCollection(items.arr.toIndexedSeq, SBoolean)).function(null, null))

          case mc @ MethodCall(obj, m, args, _) =>
            val objValue = dataEnv(obj)
            val (objMethod, argValues) = getObjMethodAndArgs(objValue.getClass, mc)
            val res = objMethod.invoke(objValue, argValues:_*) match {
              case Constant(v, _) => v
              case v => v
            }
            out(res)
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
      catch {
        case e: Throwable =>
          !!!(s"Error in evaluate($te)", e)
      }
      println(s"${te.sym} -> ${dataEnv(te.sym)}")
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
