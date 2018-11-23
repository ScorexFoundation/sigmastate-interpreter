package sigmastate.eval

import scala.collection.mutable.ArrayBuffer
import sigmastate._
import sigmastate.Values.{BlockValue, BoolValue, BooleanConstant, ConcreteCollection, Constant, ConstantNode, FuncValue, GroupElementConstant, SValue, SigmaBoolean, SigmaPropConstant, ValDef, ValUse, Value}
import sigmastate.serialization.OpCodes._
import org.ergoplatform.{Height, Inputs, Outputs, Self}
import java.lang.reflect.Method
import java.math.BigInteger

import org.ergoplatform.{Height, Inputs, Outputs, Self}
import scapi.sigma.DLogProtocol
import sigmastate._
import sigmastate.lang.Terms.{OperationId, ValueOps}
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.{ConstantStore, ValueSerializer}
import sigmastate.utxo.{CostTable, ExtractAmount, SizeOf}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, classTag}
import scala.util.Try
import SType._
import org.bouncycastle.math.ec.ECPoint
import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.lang.SigmaBuilder

trait TreeBuilding extends RuntimeCosting { IR: Evaluation =>
  import Liftables._
  import Context._
  import SigmaProp._
  import Col._
  import Box._
  import ColBuilder._
  import SigmaDslBuilder._
  import CCostedBuilder._
  import MonoidBuilderInst._
  import TrivialSigma._
  import ProveDlogEvidence._
  import ProveDHTEvidence._
  import WBigInteger._
  import WArray._
  import WOption._
  import WECPoint._
  
  private val ContextM = ContextMethods
  private val SigmaM = SigmaPropMethods
  private val ColM = ColMethods
  private val BoxM = BoxMethods
  private val CBM = ColBuilderMethods
  private val SDBM = SigmaDslBuilderMethods
  private val AM = WArrayMethods
  private val OM = WOptionMethods
  private val BIM = WBigIntegerMethods

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

  def buildValue(mainG: PGraph,
                 env: DefEnv,
                 s: Sym,
                 defId: Int,
                 constantsProcessing: Option[ConstantStore]): SValue = {
    import builder._
    def recurse[T <: SType](s: Sym) = buildValue(mainG, env, s, defId, constantsProcessing).asValue[T]
    object In { def unapply(s: Sym): Option[SValue] = Some(buildValue(mainG, env, s, defId, constantsProcessing)) }
    s match {
      case _ if env.contains(s) =>
        val (id, tpe) = env(s)
        ValUse(id, tpe) // recursion base
      case Def(Lambda(lam, _, x, y)) =>
        val varId = defId + 1       // arguments are treated as ValDefs and occupy id space
      val env1 = env + (x -> (varId, elemToSType(x.elem)))
        val block = processAstGraph(mainG, env1, lam, varId + 1, constantsProcessing)
        val rhs = mkFuncValue(Vector((varId, elemToSType(x.elem))), block)
        rhs
      case Def(Apply(fSym, xSym, _)) =>
        val Seq(f, x) = Seq(fSym, xSym).map(recurse)
        builder.mkApply(f, IndexedSeq(x))
      case Def(th @ ThunkDef(root, _)) =>
        val block = processAstGraph(mainG, env, th, defId, constantsProcessing)
        block
      case Def(Const(x)) =>
        val tpe = elemToSType(s.elem)
        constantsProcessing match {
          case Some(s) =>
            val constant = mkConstant[tpe.type](x.asInstanceOf[tpe.WrappedType], tpe)
              .asInstanceOf[ConstantNode[SType]]
            s.put(constant)(builder)
          case None =>
            mkConstant[tpe.type](x.asInstanceOf[tpe.WrappedType], tpe)
        }
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

      case ColM.apply(colSym, In(index)) =>
        val col = recurse(colSym)
        mkByIndex(col, index.asIntValue, None)

      case ColM.length(col) =>
        utxo.SizeOf(recurse(col).asCollection[SType])

      case ColM.exists(colSym, pSym) =>
        val Seq(col, p) = Seq(colSym, pSym).map(recurse)
        mkExists(col.asCollection[SType], 21, p.asBoolValue)

      case ColM.forall(colSym, pSym) =>
        val Seq(col, p) = Seq(colSym, pSym).map(recurse)
        mkForAll1(col.asCollection[SType], p.asFunc)

      case ColM.map(colSym, fSym) =>
        val Seq(col, f) = Seq(colSym, fSym).map(recurse)
        mkMapCollection(col.asCollection[SType], f.asFunc)

      case ColM.getOrElse(colSym, In(index), defValSym) =>
        val col = recurse(colSym)
        val defVal = recurse(defValSym)
        mkByIndex(col, index.asIntValue, Some(defVal))

      case ColM.append(col1Sym, col2Sym) =>
        val Seq(col1, col2) = Seq(col1Sym, col2Sym).map(recurse)
        mkAppend(col1, col2)

      case BoxM.value(box) =>
        mkExtractAmount(recurse[SBox.type](box))
      case BoxM.propositionBytes(In(box)) =>
        mkExtractScriptBytes(box.asBox)

      case Def(AnyZk(_, colSyms, _)) =>
        val col = colSyms.map(recurse(_).asSigmaProp)
        SigmaOr(col)
      case Def(AllZk(_, colSyms, _)) =>
        val col = colSyms.map(recurse(_).asSigmaProp)
        SigmaAnd(col)

      case Def(AnyOf(_, colSyms, _)) =>
        val col = colSyms.map(recurse(_).asBoolValue)
        mkAnyOf(col)
      case Def(AllOf(_, colSyms, _)) =>
        val col = colSyms.map(recurse(_).asBoolValue)
        mkAllOf(col)

      case SigmaM.and_bool_&&(In(prop), In(cond)) =>
        SigmaAnd(Seq(prop.asSigmaProp, mkBoolToSigmaProp(cond.asBoolValue)))
      case SigmaM.or_bool_||(In(prop), In(cond)) =>
        SigmaOr(Seq(prop.asSigmaProp, mkBoolToSigmaProp(cond.asBoolValue)))
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
        mkBoolToSigmaProp(cond.asBoolValue)
      case Def(ProveDlogEvidenceCtor(In(g))) =>
        SigmaPropConstant(mkProveDlog(g.asGroupElement))
      case Def(ProveDHTEvidenceCtor(In(g), In(h), In(u), In(v))) =>
        SigmaPropConstant(mkProveDiffieHellmanTuple(g.asGroupElement, h.asGroupElement, u.asGroupElement, v.asGroupElement))
      case Def(d) =>
        !!!(s"Don't know how to buildValue($mainG, $s -> $d, $env, $defId)")
    }
  }

  private def processAstGraph(mainG: PGraph,
                              env: DefEnv,
                              subG: AstGraph,
                              defId: Int,
                              constantsProcessing: Option[ConstantStore]): SValue = {
    val valdefs = new ArrayBuffer[ValDef]
    var curId = defId
    var curEnv = env
    for (te <- subG.schedule) {
      val s = te.sym; val d = te.rhs
      if (mainG.hasManyUsagesGlobal(s) && IsContextProperty.unapply(d).isEmpty && IsInternalDef.unapply(d).isEmpty) {
        val rhs = buildValue(mainG, curEnv, s, curId, constantsProcessing)
        curId += 1
        val vd = ValDef(curId, Seq(), rhs)
        curEnv = curEnv + (s -> (curId, elemToSType(s.elem)))  // assign valId to s, so it can be use in ValUse
        valdefs += vd
      }
    }
    val Seq(root) = subG.roots
    val rhs = buildValue(mainG, curEnv, root, curId, constantsProcessing)
    val res = if (valdefs.nonEmpty) BlockValue(valdefs.toIndexedSeq, rhs) else rhs
    res
  }

  def buildTree[T <: SType](f: Rep[Context => Any],
                            constantsProcessing: Option[ConstantStore] = None): Value[T] = {
    val Def(Lambda(lam,_,_,_)) = f
    val mainG = new PGraph(lam.y)
    val block = processAstGraph(mainG, Map(), mainG, 0, constantsProcessing)
    block.asValue[T]
  }
}
