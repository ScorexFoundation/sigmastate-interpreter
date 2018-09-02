package sigmastate.eval

import java.lang.reflect.Method
import org.ergoplatform.{Height, Outputs, Self, Inputs}
import scapi.sigma.DLogProtocol
import sigmastate.SType
import sigmastate.Values.{FuncValue, Constant, SValue, BlockValue, BoolValue, Value, BooleanConstant, SigmaBoolean, ValDef, ValUse, ConcreteCollection}
import sigmastate.lang.Terms.{OperationId, ValueOps}
import sigmastate.lang.Costing
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.{Exists1, CostTable, ExtractAmount, SizeOf}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Try
import SType._

trait Evaluation extends Costing {
  import Context._
  import Sigma._
  import Col._
  import Box._
  import ColBuilder._
  import SigmaDslBuilder._
  import ConcreteCostedBuilder._
  import MonoidBuilderInst._
  import TrivialSigma._

  private val ContextM = ContextMethods
  private val SigmaM = SigmaMethods
  private val ColM = ColMethods
  private val BoxM = BoxMethods
  private val CBM = ColBuilderMethods
  private val SDBM = SigmaDslBuilderMethods

  def isValidCostPrimitive(d: Def[_]): Unit = d match {
    case _: Const[_] =>
    case _: IntPlusMonoid =>
    case _: Lambda[_,_] =>
    case _: ThunkDef[_] =>
    case ApplyBinOp(_: NumericPlus[_]| _: NumericTimes[_],_,_) =>
    case ContextM.OUTPUTS(_) | ContextM.INPUTS(_) | ContextM.getVar(_,_,_) =>
    case SigmaM.propBytes(_) =>
    case ColM.length(_) | ColM.map(_,_) | ColM.sum(_,_) =>
    case BoxM.propositionBytes(_) =>
    case _: CostOf =>
    case _ => !!!(s"Invalid primitive in Cost function: $d")
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
  import Values.{TrueLeaf, FalseLeaf}
  import special.sigma.{Context => SigmaContext}

  type ContextFunc[T <: SType] = SigmaContext => Value[T]

  val sigmaDslBuilderValue: special.sigma.SigmaDslBuilder
  val costedBuilderValue: special.collection.ConcreteCostedBuilder
  val monoidBuilderValue: special.collection.MonoidBuilder

  def getDataEnv: mutable.Map[Sym, AnyRef] = {
    val env = mutable.Map[Sym, AnyRef](
      sigmaDslBuilder -> sigmaDslBuilderValue,
      sigmaDslBuilder.Cols -> sigmaDslBuilderValue.Cols,
      costedBuilder -> costedBuilderValue,
      costedBuilder.monoidBuilder -> monoidBuilderValue,
      costedBuilder.monoidBuilder.intPlusMonoid -> monoidBuilderValue.intPlusMonoid
    )
    env
  }

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
          case wc: WrapperConst[_] => out(wc.wrappedValue)
          case _: DslBuilder | _: ColBuilder | _: IntPlusMonoid =>
            dataEnv.getOrElse(te.sym, !!!(s"Cannot resolve companion instance for $te"))
          case SigmaM.propBytes(prop) =>
            val sigmaBool = dataEnv(prop).asInstanceOf[SigmaBoolean]
            out(sigmaDslBuilderValue.Cols.fromArray(sigmaBool.bytes))
          case SigmaM.isValid(In(prop: AnyRef)) =>
            out(prop)
            
          case SigmaM.and_bool_&&(In(l), In(b: Boolean)) =>
            if (b)
              out(l)
            else
              out(sigmastate.TrivialSigma(FalseLeaf))
          case SigmaM.or_bool_||(In(l), In(b: Boolean)) =>
            if (b)
              out(sigmastate.TrivialSigma(TrueLeaf))
            else
              out(l)

          case SigmaM.lazyAnd(In(l: SigmaBoolean), In(y)) =>
            val th = y.asInstanceOf[() => SigmaBoolean]
            out(AND(l, th()).function(null, null))
          case SigmaM.lazyOr(In(l: SigmaBoolean), In(y)) =>
            val th = y.asInstanceOf[() => SigmaBoolean]
            out(OR(l, th()).function(null, null))

          case SDBM.anyZK(_, In(items: special.collection.Col[Value[SBoolean.type]]@unchecked)) =>
            out(OR(items.arr).function(null, null))
          case SDBM.allZK(_, In(items: special.collection.Col[Value[SBoolean.type]]@unchecked)) =>
            out(AND(items.arr).function(null, null))

          case mc @ MethodCall(obj, m, args, _) =>
            val objValue = dataEnv(obj)
            val (objMethod, argValues) = getObjMethodAndArgs(objValue.getClass, mc)
            val res = objMethod.invoke(objValue, argValues:_*) match {
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
          case Lambda(l, _, x, y) =>
            val f = (ctx: AnyRef) => {
              dataEnv += (x -> ctx)
              l.schedule.foreach(evaluate(_))
              dataEnv(y)
            }
            out(f)
          case Apply(In(_f), In(x: AnyRef), _) =>
            val f = _f.asInstanceOf[AnyRef => Any]
            out(f(x))
          case ThunkDef(y, schedule) =>
            val th = () => {
              schedule.foreach(evaluate(_))
              dataEnv(y)
            }
            out(th)
          case TrivialSigmaCtor(In(isValid: Boolean)) =>
            out(sigmastate.TrivialSigma(BooleanConstant(isValid)))
          case CostOf(opName, opCode, tpe) =>
            val operId = OperationId(opName, tpe)
            val cost = CostTable.DefaultCosts(operId)
            out(cost)
          case SizeOf(sym @ In(data)) =>
            val tpe = elemToSType(sym.elem)
            val size = tpe.dataSize(data.asWrappedType)
            out(size)
          case TypeSize(tpe) =>
            val size = tpe.dataSize(0.asWrappedType)
            out(size)
          case _ => !!!(s"Don't know how to evaluate($te)")
        }
      }
      catch {
        case e: Throwable =>
          !!!(s"Error in evaluate($te)", e)
      }
//      println(s"${te.sym} -> ${dataEnv(te.sym)}")
    }

    val g = new PGraph(f)
    g.schedule.foreach(evaluate(_))
    val fun = dataEnv(f).asInstanceOf[SigmaContext => Any]
    val res = (ctx: SigmaContext) => {
      fun(ctx) match {
        case v: Value[_] => v
        case col: special.collection.Col[_] => builder.liftAny(col.arr).get
        case x => builder.liftAny(x).get
      }
    }
    res.asInstanceOf[ContextFunc[T]]
  }

  /** Describes assignment of valIds for symbols which become ValDefs.
    * Each ValDef in current scope have entry in this map */
  type DefEnv = Map[Sym, (Int, SType)]

  object IsArithOp {
    def unapply(op: EndoBinOp[_]): Option[Byte] = op match {
      case _: NumericPlus[_]    => Some(PlusCode)
      case _: NumericMinus[_]   => Some(MinusCode)
      case _: NumericTimes[_]   => Some(MultiplyCode)
      case _: IntegralDivide[_] => Some(DivisionCode)
      case _: IntegralMod[_]    => Some(ModuloCode)
      case _ => None
    }
  }

  object IsRelationOp {
    def unapply(op: BinOp[_,_]): Option[(SValue, SValue) => Value[SBoolean.type]] = op match {
      case _: Equals[_]       => Some(builder.mkEQ[SType])
      case _: NotEquals[_]    => Some(builder.mkNEQ[SType])
      case _: OrderingGT[_]   => Some(builder.mkGT[SType])
      case _: OrderingLT[_]   => Some(builder.mkLT[SType])
      case _: OrderingGTEQ[_] => Some(builder.mkGE[SType])
      case _: OrderingLTEQ[_] => Some(builder.mkLE[SType])
      case _ => None
    }
  }

  object IsLogicalBinOp {
    def unapply(op: BinOp[_,_]): Option[(BoolValue, BoolValue) => Value[SBoolean.type]] = op match {
      case And => Some(builder.mkBinAnd)
      case Or  => Some(builder.mkBinOr)
      case _ => None
    }
  }

  object IsContextProperty {
    def unapply(d: Def[_]): Option[SValue] = d match {
      case ContextM.HEIGHT(_) => Some(Height)
      case ContextM.INPUTS(_) => Some(Inputs)
      case ContextM.OUTPUTS(_) => Some(Outputs)
      case ContextM.SELF(_) => Some(Self)
      case _ => None
    }
  }

  object IsInternalDef {
    def unapply(d: Def[_]): Option[Def[_]] = d match {
      case _: SigmaDslBuilder | _: ColBuilder => Some(d)
      case _ => None
    }
  }

  def buildValue(mainG: PGraph, env: DefEnv, s: Sym, defId: Int): SValue = {
    import builder._
    def recurse[T <: SType](s: Sym) = buildValue(mainG, env, s, defId).asValue[T]
    object In { def unapply(s: Sym): Option[SValue] = Some(buildValue(mainG, env, s, defId)) }
    s match {
      case _ if env.contains(s) =>
        val (id, tpe) = env(s)
        ValUse(id, tpe) // recursion base
      case Def(Lambda(lam, _, x, y)) =>
        val varId = defId + 1       // arguments are treated as ValDefs and occupy id space
        val env1 = env + (x -> (varId, elemToSType(x.elem)))
        val block = processAstGraph(mainG, env1, lam, varId + 1)
        val rhs = FuncValue(varId, elemToSType(x.elem), block)
        rhs
      case Def(Apply(fSym, xSym, _)) =>
        val Seq(f, x) = Seq(fSym, xSym).map(recurse)
        builder.mkApply(f, IndexedSeq(x))
      case Def(th @ ThunkDef(root, _)) =>
        val block = processAstGraph(mainG, env, th, defId)
        block
      case Def(Const(x)) =>
        val tpe = elemToSType(s.elem)
        mkConstant[tpe.type](x.asInstanceOf[tpe.WrappedType], tpe)
      case Def(IsContextProperty(v)) => v
      case ContextM.getVar(_, Def(Const(id: Byte)), eVar) =>
        val tpe = elemToSType(eVar)
        mkTaggedVariable(id, tpe)
      case Def(ApplyBinOp(IsArithOp(opCode), xSym, ySym)) =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        mkArith(x.asNumValue, y.asNumValue, opCode)
      case Def(ApplyBinOp(IsRelationOp(mkNode), xSym, ySym)) =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        mkNode(x, y)
      case Def(ApplyBinOpLazy(IsLogicalBinOp(mkNode), xSym, ySym)) =>
        val Seq(x, y) = Seq(xSym, ySym).map(recurse)
        mkNode(x, y)
      case ColM.length(col) =>
        utxo.SizeOf(recurse(col).asCollection[SType])
      case ColM.exists(colSym, pSym) =>
        val Seq(col, p) = Seq(colSym, pSym).map(recurse)
        mkExists1(col.asCollection[SType], p.asFunc)
      case ColM.forall(colSym, pSym) =>
        val Seq(col, p) = Seq(colSym, pSym).map(recurse)
        mkForAll1(col.asCollection[SType], p.asFunc)

      case BoxM.value(box) =>
        mkExtractAmount(recurse[SBox.type](box))
      case BoxM.propositionBytes(In(box)) =>
        mkExtractScriptBytes(box.asBox)

      case Def(AnyZk(_, colSyms)) =>
        val col = colSyms.map(recurse(_).asSigmaProp)
        SigmaOr(col)
      case Def(AllZk(_, colSyms)) =>
        val col = colSyms.map(recurse(_).asSigmaProp)
        SigmaAnd(col)

      case Def(AnyOf(_, colSyms)) =>
        val col = colSyms.map(recurse(_).asBoolValue)
        mkAnyOf(col)
      case Def(AllOf(_, colSyms)) =>
        val col = colSyms.map(recurse(_).asBoolValue)
        mkAllOf(col)

      case SigmaM.and_bool_&&(In(prop), In(cond)) =>
        SigmaAnd(Seq(prop.asSigmaProp, mkTrivialSigma(cond.asBoolValue)))
      case SigmaM.or_bool_||(In(prop), In(cond)) =>
        SigmaOr(Seq(prop.asSigmaProp, mkTrivialSigma(cond.asBoolValue)))
      case SigmaM.and_sigma_&&(In(p1), In(p2)) =>
        SigmaAnd(Seq(p1.asSigmaProp, p2.asSigmaProp))
      case SigmaM.or_sigma_||(In(p1), In(p2)) =>
        SigmaOr(Seq(p1.asSigmaProp, p2.asSigmaProp))
//      case SigmaM.lazyAnd(In(l), In(r)) =>
//        mkBinAnd(l.asSigmaProp, r.asSigmaProp)
//      case SigmaM.lazyOr(In(l), In(r)) =>
//        mkBinOr(l.asSigmaProp, r.asSigmaProp)
      case SigmaM.isValid(In(prop)) =>
        mkSigmaPropIsValid(prop.asSigmaProp)
      case SigmaM.propBytes(In(prop)) =>
        mkSigmaPropBytes(prop.asSigmaProp)
      case Def(TrivialSigmaCtor(In(cond))) =>
        mkTrivialSigma(cond.asBoolValue)
      case Def(d) =>
        !!!(s"Don't know how to buildValue($mainG, $s -> $d, $env, $defId)")
    }
  }

  def processAstGraph(mainG: PGraph, env: DefEnv, subG: AstGraph, defId: Int): SValue = {
    val valdefs = new ArrayBuffer[ValDef]
    var curId = defId
    var curEnv = env
    for (TableEntry(s, d) <- subG.schedule) {
      if (mainG.hasManyUsagesGlobal(s) && IsContextProperty.unapply(d).isEmpty && IsInternalDef.unapply(d).isEmpty) {
        val rhs = buildValue(mainG, curEnv, s, curId)
        curId += 1
        val vd = ValDef(curId, Seq(), rhs)
        curEnv = curEnv + (s -> (curId, elemToSType(s.elem)))  // assign valId to s, so it can be use in ValUse
        valdefs += vd
      }
    }
    val Seq(root) = subG.roots
    val rhs = buildValue(mainG, curEnv, root, curId)
    val res = if (valdefs.nonEmpty) BlockValue(valdefs.toIndexedSeq, rhs) else rhs
    res
  }

  def buildTree[T <: SType](f: Rep[Context => T#WrappedType]): Value[T] = {
    val Def(Lambda(lam,_,_,_)) = f
    val mainG = new PGraph(lam.y)
    val block = processAstGraph(mainG, Map(), mainG, 0)
    block.asValue[T]
  }
}
