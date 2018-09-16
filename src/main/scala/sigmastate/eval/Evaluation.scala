package sigmastate.eval

import java.lang.reflect.Method
import java.math.BigInteger

import org.ergoplatform.{Height, Outputs, Self, Inputs}
import scapi.sigma.DLogProtocol
import sigmastate.SType
import sigmastate.Values.{FuncValue, Constant, SValue, BlockValue, SigmaPropConstant, BoolValue, Value, BooleanConstant, SigmaBoolean, ValDef, GroupElementConstant, ValUse, ConcreteCollection}
import sigmastate.lang.Terms.{OperationId, ValueOps}
import sigmastate.lang.Costing
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.{Exists1, CostTable, ExtractAmount, SizeOf}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}
import scala.util.Try
import SType._
import org.bouncycastle.math.ec.ECPoint
import sigmastate.interpreter.CryptoConstants.EcPointType

trait Evaluation extends Costing {
  import Context._
  import SigmaProp._
  import Col._
  import Box._
  import ColBuilder._
  import SigmaDslBuilder._
  import ConcreteCostedBuilder._
  import MonoidBuilderInst._
  import TrivialSigma._
  import ProveDlogEvidence._
  import WBigInteger._
  import WArray._
  import WOption._
  import WECPoint._
  import Liftables._
  
  private val ContextM = ContextMethods
  private val SigmaM = SigmaPropMethods
  private val ColM = ColMethods
  private val BoxM = BoxMethods
  private val CBM = ColBuilderMethods
  private val SDBM = SigmaDslBuilderMethods
  private val AM = WArrayMethods
  private val OM = WOptionMethods
  private val BIM = WBigIntegerMethods

  def isValidCostPrimitive(d: Def[_]): Unit = d match {
    case _: Const[_] =>
    case _: IntPlusMonoid =>
    case _: Lambda[_,_] =>
    case _: ThunkDef[_] =>
    case ApplyBinOp(_: NumericPlus[_]| _: NumericTimes[_],_,_) =>
    case ContextM.SELF(_) | ContextM.OUTPUTS(_) | ContextM.INPUTS(_) | ContextM.LastBlockUtxoRootHash(_) |
         ContextM.getVar(_,_,_) | ContextM.deserialize(_,_,_) |
         ContextM.cost(_) | ContextM.dataSize(_) =>
    case SigmaM.propBytes(_) =>
    case ColM.length(_) | ColM.map(_,_) | ColM.sum(_,_) | ColM.zip(_,_) =>
    case CBM.replicate(_,_,_) =>
    case BoxM.propositionBytes(_) | BoxM.cost(_) | BoxM.dataSize(_) | BoxM.getReg(_,_,_) =>
    case OM.get(_) =>
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

  def compile[T <: SType](dataEnv: Map[Sym, AnyRef], f: Rep[Context => T#WrappedType]): ContextFunc[T] = {

    def evaluate(te: TableEntry[_]): EnvRep[_] = EnvRep { dataEnv =>
      object In { def unapply(s: Sym): Option[Any] = Some(dataEnv(s)) }
      def out(v: Any): (DataEnv, Sym) = { (dataEnv + (te.sym -> v.asInstanceOf[AnyRef]), te.sym) }
      try {
        val res: (DataEnv, Sym) = te.rhs match {
          case Const(x) => out(x.asInstanceOf[AnyRef])
          case wc: LiftedConst[_,_] => out(wc.constValue)
          case _: DslBuilder | _: ColBuilder | _: IntPlusMonoid | _: LongPlusMonoid =>
            out(dataEnv.getOrElse(te.sym, !!!(s"Cannot resolve companion instance for $te")))
          case SigmaM.propBytes(prop) =>
            val sigmaBool = dataEnv(prop).asInstanceOf[SigmaBoolean]
            out(sigmaDslBuilderValue.Cols.fromArray(sigmaBool.bytes))
          case SigmaM.isValid(In(prop: AnyRef)) =>
            out(prop)

          case SigmaM.and_sigma_&&(In(l), In(r)) =>
            out(AND(l.asInstanceOf[BoolValue], r.asInstanceOf[BoolValue]))

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

          case AM.length(In(arr: Array[_])) => out(arr.length)
          case CBM.replicate(In(b: special.collection.ColBuilder), In(n: Int), xSym @ In(x)) =>
            out(b.replicate(n, x)(xSym.elem.classTag))
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
          case Lambda(l, _, x, y) =>
            val f = (ctx: AnyRef) => {
              val resEnv = l.schedule.foldLeft(dataEnv + (x -> ctx)) { (env, te) =>
                val (e, _) = evaluate(te).run(env)
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
              schedule.foreach(evaluate(_))
              dataEnv(y)
            }
            out(th)
          case TrivialSigmaCtor(In(isValid: Boolean)) =>
            out(sigmastate.TrivialSigma(BooleanConstant(isValid)))
          case ProveDlogEvidenceCtor(In(g: ECPoint)) =>
            val res = DLogProtocol.ProveDlog(GroupElementConstant(g.asInstanceOf[EcPointType]))
            out(res)
          case CostOf(opName, tpe) =>
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
        println(s"${te.sym} -> ${res._1(te.sym)}")
        res
      }
      catch {
        case e: Throwable =>
          !!!(s"Error in evaluate($te)", e)
      }
    }

    val g = new PGraph(f)
    val resEnv = g.schedule.foldLeft(dataEnv) { (env, te) =>
      val (e, _) = evaluate(te).run(env)
      e
    }
    val fun = resEnv(f).asInstanceOf[SigmaContext => Any]
    val res = (ctx: SContext) => {
      fun(ctx) match {
        case sb: SigmaBoolean => builder.liftAny(sb).get
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
        val rhs = mkFuncValue(Vector((varId, elemToSType(x.elem))), block)
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
      case CBM.fromArray(_, arr @ Def(wc: LiftedConst[a,_])) =>
        val colTpe = elemToSType(s.elem)
        mkConstant[colTpe.type](wc.constValue.asInstanceOf[colTpe.WrappedType], colTpe)
      case Def(wc: LiftedConst[a,_]) =>
        val tpe = elemToSType(s.elem)
        mkConstant[tpe.type](wc.constValue.asInstanceOf[tpe.WrappedType], tpe)
      case Def(IsContextProperty(v)) => v
      case ContextM.getVar(_, Def(Const(id: Byte)), eVar) =>
        val tpe = elemToSType(eVar)
        mkTaggedVariable(id, tpe)
      case BIM.subtract(In(x), In(y)) =>
        mkArith(x.asNumValue, y.asNumValue, MinusCode)
      case BIM.add(In(x), In(y)) =>
        mkArith(x.asNumValue, y.asNumValue, PlusCode)
      case BIM.multiply(In(x), In(y)) =>
        mkArith(x.asNumValue, y.asNumValue, MultiplyCode)
      case BIM.divide(In(x), In(y)) =>
        mkArith(x.asNumValue, y.asNumValue, DivisionCode)
      case BIM.mod(In(x), In(y)) =>
        mkArith(x.asNumValue, y.asNumValue, ModuloCode)
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

      case ColM.map(colSym, fSym) =>
        val Seq(col, f) = Seq(colSym, fSym).map(recurse)
        mkMapCollection1(col.asCollection[SType], f.asFunc)

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
      case Def(ProveDlogEvidenceCtor(In(g))) =>
        SigmaPropConstant(mkProveDlog(g.asGroupElement))
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
