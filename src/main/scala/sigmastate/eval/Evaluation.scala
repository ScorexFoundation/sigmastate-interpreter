package sigmastate.eval

import java.lang.reflect.Method
import java.math.BigInteger

import org.ergoplatform._
import sigmastate._
import sigmastate.Values.{BlockValue, BoolValue, BooleanConstant, CollectionConstant, ConcreteCollection, Constant, EvaluatedValue, FuncValue, GroupElementConstant, SValue, SigmaBoolean, SigmaPropConstant, ValDef, ValUse, Value}
import sigmastate.lang.Terms.{OperationId, ValueOps}
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.{CostTable, CostTableStat, ExtractAmount, SizeOf}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}
import scala.util.Try
import SType._
import org.bouncycastle.math.ec.ECPoint
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.CryptoFunctions
import special.sigma.InvalidType
import scalan.Nullable
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.{DLogProtocol, ProveDHTuple}

trait Evaluation extends RuntimeCosting { IR =>
  import Context._
  import SigmaProp._
  import Col._
  import ReplCol._
  import CReplCol._
  import Box._
  import AvlTree._
  import ColBuilder._
  import SigmaDslBuilder._
  import CostedBuilder._
  import CCostedBuilder._
  import Monoid._
  import MonoidBuilder._
  import MonoidBuilderInst._
  import TrivialSigma._
  import ProveDlogEvidence._
  import ProveDHTEvidence._
  import WBigInteger._
  import WArray._
  import WOption._
  import WECPoint._
  import Liftables._

  val okPrintEvaluatedEntries: Boolean = false

  private val ContextM = ContextMethods
  private val SigmaM = SigmaPropMethods
  private val ColM = ColMethods
  private val BoxM = BoxMethods
  private val AvlM = AvlTreeMethods
  private val CBM = ColBuilderMethods
  private val SDBM = SigmaDslBuilderMethods
  private val AM = WArrayMethods
  private val OM = WOptionMethods
  private val BIM = WBigIntegerMethods

  def isValidCostPrimitive(d: Def[_]): Unit = d match {
    case _: Const[_] =>
    case _: Tup[_,_] | _: First[_,_] | _: Second[_,_] =>
    case _: FieldApply[_] =>
    case _: IntPlusMonoid =>
    case _: Lambda[_,_] =>
    case _: ThunkDef[_] =>
    case ApplyUnOp(_: NumericToLong[_] | _: NumericToInt[_], _) =>
    case ApplyBinOp(_: NumericPlus[_] | _: NumericTimes[_] | _: OrderingMax[_] | _: IntegralDivide[_] ,_,_) =>
    case ContextM.SELF(_) | ContextM.OUTPUTS(_) | ContextM.INPUTS(_) | ContextM.LastBlockUtxoRootHash(_) |
         ContextM.getVar(_,_,_) |
         ContextM.cost(_) | ContextM.dataSize(_) =>
    case SigmaM.propBytes(_) =>
    case ColM.length(_) | ColM.map(_,_) | ColM.sum(_,_) | ColM.zip(_,_) | ColM.slice(_,_,_) | ColM.apply(_,_) | ColM.append(_,_) =>
    case CBM.replicate(_,_,_) | CBM.fromItems(_,_,_) =>
    case BoxM.propositionBytes(_) | BoxM.bytesWithoutRef(_) | BoxM.cost(_) | BoxM.dataSize(_) | BoxM.getReg(_,_,_) =>
    case AvlM.dataSize(_) =>
    case OM.get(_) | OM.getOrElse(_,_) | OM.fold(_,_,_) | OM.isDefined(_) =>
    case _: CostOf | _: SizeOf[_] =>
    case _: Upcast[_,_] =>
    case _: Apply[_,_] =>
    case _ => !!!(s"Invalid primitive in Cost function: $d")
  }

  def verifyCostFunc(costF: Rep[Context => Int]): Try[Unit] = {
    val Def(Lambda(lam,_,_,_)) = costF
    Try { lam.scheduleAll.foreach(te => isValidCostPrimitive(te.rhs)) }
  }

  def findIsProven[T](f: Rep[Context => T]): Option[Sym] = {
    val Def(Lambda(lam,_,_,_)) = f
    val ok = lam.scheduleAll.find(te => te.rhs match {
      case SigmaM.isValid(_) => true
      case _ => false
    }).map(_.sym)
    ok
  }

  def verifyIsProven[T](f: Rep[Context => T]): Try[Unit] = {
    val isProvenOpt = findIsProven(f)
    Try {
      isProvenOpt match {
        case Some(s) =>
          if (f.getLambda.y != s) !!!(s"Sigma.isProven found in none-root position", s)
        case None =>
      }
    }
  }
  object IsTupleFN {
    def unapply(fn: String): Nullable[Byte] = {
      if (fn.startsWith("_")) Nullable[Byte](fn.substring(1).toByte)
      else Nullable.None.asInstanceOf[Nullable[Byte]]
    }
  }
  import sigmastate._
  import Values.{TrueLeaf, FalseLeaf}
  import special.sigma.{Context => SigmaContext}

  type ContextFunc[T <: SType] = SigmaContext => Value[T]

  val sigmaDslBuilderValue: special.sigma.SigmaDslBuilder
  val costedBuilderValue: special.collection.CCostedBuilder
  val monoidBuilderValue: special.collection.MonoidBuilder

  def getDataEnv: DataEnv = {
    val env = Map[Sym, AnyRef](
      sigmaDslBuilder -> sigmaDslBuilderValue,
      sigmaDslBuilder.Cols -> sigmaDslBuilderValue.Cols,
      costedBuilder -> costedBuilderValue,
      costedBuilder.monoidBuilder -> monoidBuilderValue,
      costedBuilder.monoidBuilder.intPlusMonoid -> monoidBuilderValue.intPlusMonoid,
      costedBuilder.monoidBuilder.longPlusMonoid -> monoidBuilderValue.longPlusMonoid
    )
    env
  }

  case class EvaluatedEntry(env: DataEnv, sym: Sym, value: AnyRef)

  def printEnvEntry(sym: Sym, value: AnyRef) = {
    def trim[A](arr: Array[A]) = arr.take(arr.length min 100)
    def show(x: Any) = x match {
      case arr: Array[_] => s"Array(${trim(arr).mkString(",")})"
      case col: special.collection.Col[_] => s"Coll(${trim(col.arr).mkString(",")})"
      case p: ECPoint => CryptoFunctions.showECPoint(p)
      case ProveDlog(GroupElementConstant(g)) => s"ProveDlog(${CryptoFunctions.showECPoint(g)})"
      case ProveDHTuple(
              GroupElementConstant(g), GroupElementConstant(h), GroupElementConstant(u), GroupElementConstant(v)) =>
        s"ProveDHT(${CryptoFunctions.showECPoint(g)},${CryptoFunctions.showECPoint(h)},${CryptoFunctions.showECPoint(u)},${CryptoFunctions.showECPoint(v)})"
      case _ => x.toString
    }
    sym match {
      case x if x.isVar => s"Var($sym -> ${show(value)})"
      case Def(Lambda(_, _, x, y)) => s"Lam($x => $y)"
      case _ => s"$sym -> ${show(value)}"
    }
  }

  def onEvaluatedGraphNode(env: DataEnv, sym: Sym, value: AnyRef): Unit = {
    if (okPrintEvaluatedEntries)
      println(printEnvEntry(sym, value))
  }

  def compile[T <: SType](dataEnv: Map[Sym, AnyRef], f: Rep[Context => T#WrappedType]): ContextFunc[T] = {

    def evaluate(ctxSym: Rep[Context], te: TableEntry[_]): EnvRep[_] = EnvRep { dataEnv =>
      object In { def unapply(s: Sym): Option[Any] = Some(dataEnv(s)) }
      def out(v: Any): (DataEnv, Sym) = { val vBoxed = v.asInstanceOf[AnyRef]; (dataEnv + (te.sym -> vBoxed), te.sym) }
      try {
        var startTime = if (okMeasureOperationTime) System.nanoTime() else 0L
        val res: (DataEnv, Sym) = te.rhs match {
          case d @ ContextM.getVar(ctx @ In(ctxObj: CostingDataContext), _, elem) =>
            val mc = d.asInstanceOf[MethodCall]
            val declaredTpe = elemToSType(elem)
            val valueInCtx = ctx.elem.invokeUnlifted(mc, dataEnv)
            val data = valueInCtx match {
              case Some(Constant(v, `declaredTpe`)) =>
                Some(ErgoLikeContext.toTestData(v, declaredTpe, ctxObj.isCost)(IR))
              case None => None
              case _ => throw new InvalidType(s"Expected Constant($declaredTpe) but found $valueInCtx")
            }
            out(data)
          case d @ BoxM.getReg(box, _, elem) =>
            val ctxObj = dataEnv(ctxSym).asInstanceOf[CostingDataContext]
            val mc = d.asInstanceOf[MethodCall]
            val declaredTpe = elemToSType(elem)
            val valueInReg = box.elem.invokeUnlifted(mc, dataEnv)
            val data = valueInReg match {
              case Some(Constant(v, `declaredTpe`)) =>
                Some(ErgoLikeContext.toTestData(v, declaredTpe, ctxObj.isCost)(IR))
              case None => None
              case _ => throw new InvalidType(
                s"Expected Some(Constant($declaredTpe)) but found $valueInReg value of register: $d")
            }
            out(data)
          case Const(x) => out(x.asInstanceOf[AnyRef])

          case Tup(In(a), In(b)) => out((a,b))
          case First(In(p: Tuple2[_,_])) => out(p._1)
          case Second(In(p: Tuple2[_,_])) => out(p._2)
          case FieldApply(In(data: special.collection.Col[a]), IsTupleFN(i)) =>
            out(data(i-1))
          case wc: LiftedConst[_,_] => out(wc.constValue)
          case _: DslBuilder | _: ColBuilder | _: CostedBuilder | _: IntPlusMonoid | _: LongPlusMonoid =>
            out(dataEnv.getOrElse(te.sym, !!!(s"Cannot resolve companion instance for $te")))
          case SigmaM.propBytes(prop) =>
            val sigmaBool = dataEnv(prop).asInstanceOf[SigmaBoolean]
            out(sigmaDslBuilderValue.Cols.fromArray(sigmaBool.bytes))
          case SigmaM.isValid(In(prop: AnyRef)) =>
            out(prop)

          case SigmaM.and_sigma_&&(In(l: SigmaBoolean), In(r: SigmaBoolean)) =>
            out(CAND.normalized(Seq(l, r)))

          case SigmaM.or_sigma_||(In(l: SigmaBoolean), In(r: SigmaBoolean)) =>
            out(COR.normalized(Seq(l, r)))

          case SigmaM.and_bool_&&(In(l: SigmaBoolean), In(b: Boolean)) =>
            if (b) {
              out(l)
            } else
              out(TrivialProp.FalseProp)

          case SigmaM.or_bool_||(In(l: SigmaBoolean), In(b: Boolean)) =>
            if (b)
              out(TrivialProp.TrueProp)
            else {
              out(l)
            }
//          case SigmaM.lazyAnd(In(l: SigmaBoolean), In(y)) =>
//            val th = y.asInstanceOf[() => SigmaBoolean]
//            out(AND(l, th()).function(null, null))
//          case SigmaM.lazyOr(In(l: SigmaBoolean), In(y)) =>
//            val th = y.asInstanceOf[() => SigmaBoolean]
//            out(OR(l, th()).function(null, null))

          case SDBM.anyZK(_, In(items: special.collection.Col[SigmaBoolean]@unchecked)) =>
            out(COR.normalized(items.arr.toSeq))
          case SDBM.allZK(_, In(items: special.collection.Col[SigmaBoolean]@unchecked)) =>
            out(CAND.normalized(items.arr.toSeq))
          case SDBM.atLeast(dsl, In(bound: Int), In(children: special.collection.Col[SigmaBoolean]@unchecked)) =>
            out(AtLeast.reduce(bound, children.arr.toSeq))
          case SDBM.sigmaProp(_, In(b: Boolean)) =>
            val res = sigmastate.TrivialProp(b)
            out(res)
          case SDBM.substConstants(_,
            In(input: special.collection.Col[Byte]@unchecked),
            In(positions: special.collection.Col[Int]@unchecked),
            In(newVals: special.collection.Col[Any]@unchecked), _) =>
            val typedNewVals = newVals.arr.map(_.asInstanceOf[Value[SType]])
            val byteArray = SubstConstants.eval(input.arr, positions.arr, typedNewVals)
            out(sigmaDslBuilderValue.Cols.fromArray(byteArray))

          case AM.length(In(arr: Array[_])) => out(arr.length)
          case CBM.replicate(In(b: special.collection.ColBuilder), In(n: Int), xSym @ In(x)) =>
            out(b.replicate(n, x)(xSym.elem.classTag))

          // NOTE: This is a fallback rule which should be places AFTER all other MethodCall patterns
          case mc @ MethodCall(obj, m, args, _) =>
            val dataRes = obj.elem.invokeUnlifted(mc, dataEnv)
            val res = dataRes match {
              case Constant(v, _) => v
              case v => v
            }
            out(res)
          case ApplyUnOp(op: UnOp[l,r], In(x)) =>
            out(op.applySeq(x).asInstanceOf[AnyRef])
          case ApplyBinOp(op: BinOp[l,r], In(x), In(y)) =>
            out(op.applySeq(x, y).asInstanceOf[AnyRef])
          case ApplyBinOpLazy(op, In(x: Boolean), In(y)) if op == Or =>
            if (x) out(true)
            else {
              val th = y.asInstanceOf[() => Any]
              out(th())
            }
          case ApplyBinOpLazy(op, In(x: Boolean), In(y)) if op == And =>
            if (x) {
              val th = y.asInstanceOf[() => Any]
              out(th())
            } else
              out(false)
          case IfThenElseLazy(In(cond: Boolean), In(t), In(e)) =>
            if (cond) {
              val th = t.asInstanceOf[() => Any]
              out(th())
            } else {
              val th = e.asInstanceOf[() => Any]
              out(th())
            }

          case Lambda(l, _, x, y) =>
            val f = (ctx: AnyRef) => {
              val resEnv = l.schedule.foldLeft(dataEnv + (x -> ctx)) { (env, te) =>
                val (e, _) = evaluate(ctxSym, te).run(env)
                e
              }
              resEnv(y)
            }
            out(f)
          case Apply(In(_f), In(x: AnyRef), _) =>
            val f = _f.asInstanceOf[AnyRef => Any]
            out(f(x))
          case First(In(p: Tuple2[_,_])) => out(p._1)
          case Second(In(p: Tuple2[_,_])) => out(p._2)
          case ThunkDef(y, schedule) =>
            val th = () => {
              val resEnv = schedule.foldLeft(dataEnv) { (env, te) =>
                val (e, _) = evaluate(ctxSym, te).run(env)
                e
              }
              resEnv(y)
            }
            out(th)

          case TrivialSigmaCtor(In(isValid: Boolean)) =>
            val res = sigmastate.TrivialProp(isValid)
            out(res)
          case ProveDlogEvidenceCtor(In(g: EcPointType)) =>
            val res = DLogProtocol.ProveDlog(GroupElementConstant(g))
            out(res)
          case ProveDHTEvidenceCtor(In(g: EcPointType), In(h: EcPointType), In(u: EcPointType), In(v: EcPointType)) =>
            val res = ProveDHTuple(GroupElementConstant(g), GroupElementConstant(h), GroupElementConstant(u), GroupElementConstant(v))
            out(res)

          case CReplColCtor(In(value), In(len: Int)) =>
            val res = sigmaDslBuilderValue.Cols.replicate(len, value)
            out(res)
          case CostOf(opName, tpe) =>
            val operId = OperationId(opName, tpe)
            val cost = CostTable.DefaultCosts(operId)
            out(cost)
          case SizeOf(sym @ In(data)) =>
            val tpe = elemToSType(sym.elem)
            val size = tpe match {
              case SAvlTree => data.asInstanceOf[special.sigma.AvlTree].dataSize
              case _ => tpe.dataSize(data.asWrappedType)
            }
            out(size)
          case TypeSize(tpe) =>
            val size = tpe.dataSize(SType.DummyValue)
            out(size)
          case Downcast(In(from), eTo) =>
            val tpe = elemToSType(eTo).asNumType
            out(tpe.downcast(from.asInstanceOf[AnyVal]))
          case Upcast(In(from), eTo) =>
            val tpe = elemToSType(eTo).asNumType
            out(tpe.upcast(from.asInstanceOf[AnyVal]))

          case SimpleStruct(_, fields) =>
            val items = fields.map { case (_, In(fieldValue)) => fieldValue }.toArray
            out(sigmaDslBuilderValue.Cols.fromArray(items))

          case _ =>
            !!!(s"Don't know how to evaluate($te)")
        }
        if (okMeasureOperationTime) {
          val endTime = System.nanoTime()
          val estimatedTime = endTime - startTime
          te.sym.getMetadata(OperationIdKey) match {
            case Some(opId: OperationId) =>
              if (opId.opType.tRange.isCollection) {
                val col = res._1(res._2).asInstanceOf[SCol[Any]]
                val colTime = if (col.length > 1) estimatedTime / col.length else estimatedTime
                CostTableStat.addOpTime(opId, colTime, col.length)
              }
              else
                CostTableStat.addOpTime(opId, estimatedTime, len = 1)
            case _ =>
          }
        }
        onEvaluatedGraphNode(res._1, res._2, res._1(res._2))
        res
      }
      catch {
        case e: Throwable =>
          !!!(s"Error in evaluate($te)", e)
      }
    }

    val res = (ctx: SContext) => {
      val g = new PGraph(f)
      val ctxSym = f.getLambda.x
      val resEnv = g.schedule.foldLeft(dataEnv + (ctxSym -> ctx)) { (env, te) =>
        val (e, _) = evaluate(ctxSym, te).run(env)
        e
      }
      val fun = resEnv(f).asInstanceOf[SigmaContext => Any]
      fun(ctx) match {
        case sb: SigmaBoolean => builder.liftAny(sb).get
        case v: Value[_] => v
        case col: special.collection.Col[_] =>
          val et = elemToSType(f.elem.eRange).asCollection[SType]
          CollectionConstant(col.arr.asInstanceOf[Array[SType#WrappedType]], et.elemType)
        case x => builder.liftAny(x).get
      }
    }
    res.asInstanceOf[ContextFunc[T]]
  }
}


