package sigmastate.eval

import java.math.BigInteger

import scala.language.implicitConversions
import scala.language.existentials
import org.bouncycastle.math.ec.ECPoint
import scalan.{Lazy, Nullable, SigmaLibrary}
import scalan.util.CollectionUtil.TraversableOps
import org.ergoplatform._
import sigmastate._
import sigmastate.Values._
import sigmastate.interpreter.{CryptoConstants, CryptoFunctions}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.CosterException
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo._
import ErgoLikeContext._
import scalan.compilation.GraphVizConfig
import SType._
import scorex.crypto.hash.{Blake2b256, Sha256}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.Terms
import scalan.staged.Slicing
import sigmastate.basics.{DLogProtocol, ProveDHTuple}

trait RuntimeCosting extends SigmaLibrary with DataCosting with Slicing { IR: Evaluation =>
  import Context._;
  import WArray._;
  import WECPoint._;
  import WBigInteger._;
  import WOption._
  import Col._;
  import ColBuilder._;
  import SigmaProp._;
  import TrivialSigma._
  import Box._
  import ColOverArrayBuilder._;
  import CostedBuilder._
  import CCostedBuilder._
  import Costed._;
  import CostedContext._
  import CCostedContext._
  import CostedPrim._;
  import CCostedPrim._;
  import CostedPair._;
  import CCostedPair._;
  import CostedFunc._;
  import CCostedFunc._;
  import CostedCol._;
  import CCostedCol._;
  import CostedBox._;
  import CCostedBox._;
  import CostedBuilder._;
  import CostedOption._;
  import CostedNone._
  import CostedSome._
  import ProveDlogEvidence._
  import ProveDHTEvidence._
  import SigmaDslBuilder._
  import TrivialSigma._
  import MonoidBuilder._
  import MonoidBuilderInst._
  import AvlTree._
  import CostedAvlTree._
  import CCostedAvlTree._
  import Monoid._
  import IntPlusMonoid._
  import WSpecialPredef._
  import TestSigmaDslBuilder._
  import CostModel._

  override val performViewsLifting = false
  val okMeasureOperationTime: Boolean = false

  this.keepOriginalFunc = false  // original lambda contains invocations of evalNode and we don't want that
//  this.useAlphaEquality = false
//  unfoldWithOriginalFunc = unfoldWithOrig

  def createSliceAnalyzer = new SliceAnalyzer

  val ColMarking = new TraversableMarkingFor[Col]
  val WOptionMarking = new TraversableMarkingFor[WOption]

  override def createEmptyMarking[T](eT: Elem[T]): SliceMarking[T] = eT match {
    case _: BoxElem[_] | _: WBigIntegerElem[_] | _: IntPlusMonoidElem | _: ColOverArrayBuilderElem =>
      EmptyBaseMarking(eT)
    case ae: ColElem[a,_] =>
      val eA = ae.eItem
      ColMarking(KeyPath.None, EmptyMarking(eA)).asMark[T]
    case ae: WOptionElem[a,_] =>
      val eA = ae.eItem
      WOptionMarking(KeyPath.None, EmptyMarking(eA)).asMark[T]
    case _ =>
      super.createEmptyMarking(eT)
  }

  override def createAllMarking[T](e: Elem[T]): SliceMarking[T] = e match {
    case _: BoxElem[_] | _: WBigIntegerElem[_] | _: IntPlusMonoidElem | _: ColOverArrayBuilderElem =>
      AllBaseMarking(e)
    case colE: ColElem[a,_] =>
      implicit val eA = colE.eItem
      ColMarking[a](KeyPath.All, AllMarking(eA)).asMark[T]
    case optE: WOptionElem[a,_] =>
      implicit val eA = optE.eItem
      WOptionMarking[a](KeyPath.All, AllMarking(eA)).asMark[T]
    case _ =>
      super.createAllMarking(e)
  }

  case class CostOf(opName: String, opType: SFunc) extends BaseDef[Int]

  def costOf(opName: String, opType: SFunc): Rep[Int] = CostOf(opName, opType)
  def costOfProveDlog = costOf("ProveDlogEval", SFunc(SUnit, SSigmaProp))
  def costOfDHTuple = costOf("ProveDHTuple", SFunc(SUnit, SSigmaProp)) * 2  // cost ???

  case class ConstantPlaceholder[T](index: Int)(implicit eT: LElem[T]) extends Def[T] {
    def selfType: Elem[T] = eT.value
  }

  def constantPlaceholder[T](index: Int)(implicit eT: LElem[T]): Rep[T] = ConstantPlaceholder[T](index)

  def perKbCostOf(node: SValue, dataSize: Rep[Long]) = {
    val opName = s"${node.getClass.getSimpleName}_per_kb"
    (dataSize.div(1024L).toInt + 1) * costOf(opName, node.opType)
  }

  def perItemCostOf(node: SValue, arrLength: Rep[Int]) = {
    val opName = s"${node.getClass.getSimpleName}_per_item"
    costOf(opName, node.opType) * arrLength
  }

  def constCost(tpe: SType): Rep[Int] = tpe match {
    case f: SFunc =>
      costOf(s"Lambda", Constant[SType](SType.DummyValue, tpe).opType)
    case _ =>
      costOf(s"Const", Constant[SType](SType.DummyValue, tpe).opType)
  }

  def constCost[T: Elem]: Rep[Int] = {
    val tpe = elemToSType(element[T])
    constCost(tpe)
  }

  def costOf(v: SValue): Rep[Int] = {
    costOf(v.opName, v.opType)
  }

  trait CostedStruct extends Costed[Struct] { }
  case class CostedStructCtor(costedFields: Rep[Struct], structCost: Rep[Int]) extends CostedStruct {
    override def transform(t: Transformer) = CostedStructCtor(t(costedFields), t(structCost))

    implicit val eVal: Elem[Struct] = {
      val fields = costedFields.elem.fields.map { case (fn, cE) => (fn, cE.asInstanceOf[CostedElem[_, _]].eVal) }
      structElement(fields)
    }
    val selfType: Elem[Costed[Struct]] = costedElement(eVal)

    def builder: Rep[CostedBuilder] = costedBuilder

    def value: Rep[Struct] = costedFields.mapFields { case cf: RCosted[a]@unchecked => cf.value }

    def cost: Rep[Int] = {
      val costs = costedFields.fields.map { case (_, cf: RCosted[a]@unchecked) => cf.cost }
      val costsCol = colBuilder.fromItems(costs:_*)
      costsCol.sum(intPlusMonoid)
    }

    def dataSize: Rep[Long] = {
      val sizes = costedFields.fields.map { case (_, cf: RCosted[a]@unchecked) => cf.dataSize }
      val sizesCol = colBuilder.fromItems(sizes:_*)
      sizesCol.sum(longPlusMonoid)
    }
  }

  def RCostedStruct(costedFields: Rep[Struct], structCost: Rep[Int]): Rep[Costed[Struct]] = CostedStructCtor(costedFields, structCost)

  object ConstantSizeType {
    def unapply(e: Elem[_]): Nullable[SType] = {
      val tpe = elemToSType(e)
      if (tpe.isConstantSize) Nullable(tpe)
      else Nullable.None
    }
  }

  override def sizeOf[T](value: Rep[T]): Rep[Long] = value.elem match {
    case _: BoxElem[_] =>
      asRep[Box](value).dataSize
    case ce: ColElem[a,_] =>
      val xs = asRep[Col[a]](value)
      implicit val eA = xs.elem.eItem
      val tpe = elemToSType(eA)
      if (tpe.isConstantSize)
        typeSize(tpe) * xs.length.toLong
      else
        xs.map(fun(sizeOf(_))).sum(longPlusMonoid)
    case ConstantSizeType(tpe) =>
      typeSize(tpe)
    case _ =>
      super.sizeOf(value)
  }

  /** Graph node to represent computation of size for types with isConstantSize == true. */
  case class TypeSize(tpe: SType) extends BaseDef[Long] {
    assert(tpe.isConstantSize, s"Expected isConstantSize type but was TypeSize($tpe)")
  }

  def typeSize(tpe: SType): Rep[Long] = {
    assert(tpe.isConstantSize)
    val size = tpe.dataSize(SType.DummyValue)
    toRep(size)
  }

  def typeSize[T: Elem]: Rep[Long] = {
    val tpe = elemToSType(element[T])
    typeSize(tpe)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case CostOf(name, opType) => s"CostOf($name:$opType)"
    case WECPointConst(p) => CryptoFunctions.showECPoint(p)
    case ac: WArrayConst[_,_] =>
      val trimmed = ac.constValue.take(ac.constValue.length min 10)
      s"WArray(len=${ac.constValue.length}; ${trimmed.mkString(",")},...)"
    case _ => super.formatDef(d)
  }

  type RCol[T] = Rep[Col[T]]
  type RCostedCol[T] = Rep[CostedCol[T]]
  type RCostedFunc[A,B] = Rep[Costed[A] => Costed[B]]

  implicit class RCostedFuncOps[A,B](f: RCostedFunc[A,B]) {
    implicit val eA = f.elem.eDom.eVal
    /**NOTE: when removeIsValid == true the resulting type B may change from Boolean to SigmaProp
      * This should be kept in mind at call site */
    def sliceCalc(okRemoveIsProven: Boolean): Rep[A => Any] = {
      val _f = { x: Rep[A] => f(RCCostedPrim(x, 0, 0L)).value }
      val res = if (okRemoveIsProven) fun(removeIsProven(_f)) else fun(_f)
      res
    }

    def sliceCalc: Rep[A => B] = fun { x: Rep[A] => f(RCCostedPrim(x, 0, 0L)).value }
    def sliceCost: Rep[((A, (Int,Long))) => Int] = fun { in: Rep[(A, (Int, Long))] =>
      val Pair(x, Pair(c, s)) = in
      f(RCCostedPrim(x, c, s)).cost
    }
    def sliceSize: Rep[Long => Long] = fun { x: Rep[Long] =>
      val arg = RCCostedPrim(variable[A], 0, x)
      f(arg).dataSize
    }

  }

  type CostedColFunc[A,B] = Costed[A] => CostedCol[B]
  type RCostedColFunc[A,B] = Rep[CostedColFunc[A, B]]

  implicit class RCostedColFuncOps[A,B](f: RCostedColFunc[A,B]) {
    implicit val eA = f.elem.eDom.eVal
    def sliceValues: Rep[A => Col[B]] = fun { x: Rep[A] => f(RCCostedPrim(x, 0, 0L)).values }
    def sliceCosts: Rep[((A, (Int,Long))) => (Col[Int], Int)] = fun { in: Rep[(A, (Int, Long))] =>
      val Pair(x, Pair(c, s)) = in
      val colC = f(RCCostedPrim(x, c, s))
      Pair(colC.costs, colC.valuesCost)
    }
    def sliceSizes: Rep[((A, Long)) => Col[Long]] = fun { in: Rep[(A, Long)] =>
      val Pair(x, s) = in
      f(RCCostedPrim(x, 0, s)).sizes
    }
  }

  implicit def extendCostedFuncElem[E,A,B](e: Elem[CostedFunc[E,A,B]]): CostedFuncElem[E,A,B,_] = e.asInstanceOf[CostedFuncElem[E,A,B,_]]

  implicit def extendCostedElem[A](elem: Elem[Costed[A]]): CostedElem[A, Costed[A]] =
    elem.asInstanceOf[CostedElem[A, Costed[A]]]

  implicit def extendCostedColElem[A](elem: Elem[CostedCol[A]]): CostedColElem[A, CostedCol[A]] =
    elem.asInstanceOf[CostedColElem[A, CostedCol[A]]]

  def splitCostedFunc2[A,B](f: RCostedFunc[A,B]): (Rep[A=>B], Rep[((A, (Int, Long))) => Int]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc
    val costF = f.sliceCost
    (calcF, costF)
  }
  def splitCostedFunc2[A, B](f: RCostedFunc[A,B], okRemoveIsValid: Boolean): (Rep[A=>Any], Rep[((A, (Int, Long))) => Int]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc(okRemoveIsValid)
    val costF = f.sliceCost
    (calcF, costF)
  }
  def splitCostedFunc[A,B](f: RCostedFunc[A,B]): (Rep[A=>B], Rep[((A, (Int, Long))) => Int], Rep[Long => Long]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc
    val costF = f.sliceCost
    val sizeF = f.sliceSize
    (calcF, costF, sizeF)
  }

  def splitCostedColFunc[A,B](f: RCostedColFunc[A,B]): (Rep[A=>Col[B]], Rep[((A, (Int, Long))) => (Col[Int], Int)], Rep[((A, Long)) => Col[Long]]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceValues
    val costF = f.sliceCosts
    val sizeF = f.sliceSizes
    (calcF, costF, sizeF)
  }

  type RWOption[T] = Rep[WOption[T]]

  object CostedFoldExtractors {
    val CM = CostedMethods
    val COM = CostedOptionMethods
    val WOM = WOptionMethods
    type Result = (RWOption[A], Th[B], RFunc[A, Costed[B]]) forSome {type A; type B}

    object IsGetCost {
      def unapply(d: Def[_]): Nullable[Result] = d match {
        case CM.cost(COM.get(WOM.fold(opt, th, f))) =>
          val res = (opt, th, f).asInstanceOf[Result]
          Nullable(res)
        case _ => Nullable.None
      }
    }
    object IsGetDataSize {
      def unapply(d: Def[_]): Nullable[Result] = d match {
        case CM.dataSize(COM.get(WOM.fold(opt, th, f))) =>
          val res = (opt, th, f).asInstanceOf[Result]
          Nullable(res)
        case _ => Nullable.None
      }
    }
    object IsGet {
      def unapply(d: Def[_]): Nullable[Result] = d match {
        case COM.get(WOM.fold(opt, th, f)) =>
          val res = (opt, th, f).asInstanceOf[Result]
          Nullable(res)
        case _ => Nullable.None
      }
    }
  }

  object IsConstSizeCostedCol {
    def unapply(d: Def[_]): Nullable[Rep[Costed[Col[A]]] forSome {type A}] = d.selfType match {
      case ce: CostedElem[_,_] if !ce.isInstanceOf[CostedColElem[_, _]] =>
        ce.eVal match {
          case colE: ColElem[a,_] if colE.eItem.isConstantSize =>
            val res = d.self.asInstanceOf[Rep[Costed[Col[A]]] forSome {type A}]
            Nullable(res)
          case _ => Nullable.None
        }
      case _ => Nullable.None
    }
  }

  implicit class ElemOpsForCosting(e: Elem[_]) {
    def isConstantSize: Boolean = elemToSType(e).isConstantSize
  }

  type CostedThunk[T] = Th[Costed[T]]

  override def rewriteDef[T](d: Def[T]): Rep[_] = {
    val CBM = ColBuilderMethods
    val SigmaM = SigmaPropMethods
    val CCM = CostedColMethods
    val CostedM = CostedMethods
    val CostedOptionM = CostedOptionMethods
    val CostedBoxM = CostedBoxMethods
    val WOptionM = WOptionMethods
    val CM = ColMethods
    val CostedBuilderM = CostedBuilderMethods
    val SPCM = WSpecialPredefCompanionMethods

    d match {
      case ApplyBinOpLazy(op, SigmaM.isValid(l), Def(ThunkDef(root, sch))) if root.elem == BooleanElement =>
        // don't need new Thunk because sigma logical ops always strict
        val r = asRep[SigmaProp](RTrivialSigma(asRep[Boolean](root)))
        val res = if (op == And)
          l && r
        else
          l || r
        res.isValid

      case ApplyBinOpLazy(op, l, Def(ThunkDef(root @ SigmaM.isValid(prop), sch))) if l.elem == BooleanElement =>
        val l1 = asRep[SigmaProp](RTrivialSigma(asRep[Boolean](l)))
        // don't need new Thunk because sigma logical ops always strict
        val res = if (op == And)
          l1 && prop
        else
          l1 || prop
        res.isValid

      case TrivialSigmaCtor(SigmaM.isValid(p)) => p

      case CCM.mapCosted(xs: RCostedCol[a], _f: RCostedFunc[_, b]) =>
        val f = asRep[Costed[a] => Costed[b]](_f)
        val (calcF, costF, sizeF) = splitCostedFunc[a, b](f)
        val vals = xs.values.map(calcF)
        val mRes = AllMarking(element[Int])
        val mCostF = sliceAnalyzer.analyzeFunc(costF, mRes)
        implicit val eA = xs.elem.eItem
        implicit val eB = f.elem.eRange.eVal

        val costs = mCostF.mDom match {
          case PairMarking(markA,_) if markA.isEmpty =>
            val slicedCostF = fun { in: Rep[(Int, Long)] => costF(Pair(variable[a], in)) }
            xs.costs.zip(xs.sizes).map(slicedCostF)
          case _ =>
            xs.values.zip(xs.costs.zip(xs.sizes)).map(costF)
        }
        val tpeB = elemToSType(eB)
        val sizes = if (tpeB.isConstantSize) {
          colBuilder.replicate(xs.sizes.length, typeSize(tpeB))
        } else
          xs.sizes.map(sizeF)
        RCCostedCol(vals, costs, sizes, xs.valuesCost)

      case CCM.foldCosted(xs: RCostedCol[a], zero: RCosted[b], _f) =>
        val f = asRep[Costed[(b,a)] => Costed[b]](_f)
        val (calcF, costF, sizeF) = splitCostedFunc[(b,a), b](f)
        val resV = xs.values.fold(zero.value, calcF)
        val mRes = AllMarking(element[Int])
        val mCostF = sliceAnalyzer.analyzeFunc(costF, mRes)

        mCostF.mDom match {
          case PairMarking(markA,_) if markA.isEmpty =>
            implicit val eA = xs.elem.eItem
            implicit val eB = zero.elem.eVal
            val slicedCostF = fun { in: Rep[(Int, Long)] => costF(Pair(variable[(b,a)], in)) }
            val cost = xs.costs.zip(xs.sizes).map(slicedCostF).sum(intPlusMonoid)
            if (elemToSType(zero.elem.eVal).isConstantSize)
              RCCostedPrim(resV, cost, zero.dataSize)
            else {
              // TODO costing: make more accurate cost estimation
              RCCostedPrim(resV, cost, zero.dataSize)
            }
          case _ =>
            error(s"Cost of the folded function depends on data: $d")
        }

//      case CCM.filterCosted(xs: RCostedCol[a], _f: RCostedFunc[_,_]) =>
//        val f = asRep[Costed[a] => Costed[Boolean]](_f)
//        val (calcF, costF, _) = splitCostedFunc[a, Boolean](f)
//        val vals = xs.values.filter(calcF)
//        val costs = xs.costs.zip(xs.sizes).map(costF)  // TODO how to filter our sizes and costs
//        val sizes = colBuilder.replicate(xs.sizes.length, 1L)
//        RCostedCol(vals, costs, sizes, xs.valuesCost)

      case CostedBoxM.creationInfo(boxC) =>
        val info = boxC.value.creationInfo
        val cost = boxC.cost + sigmaDslBuilder.CostModel.SelectField
        val l = RCCostedPrim(info._1, cost, 4L)
        val r = mkCostedCol(info._2, 34, cost)
        RCCostedPair(l, r)

      case CostedOptionM.get(optC @ CostedBoxM.getReg(_, Def(Const(2)), regE)) /*if regId == ErgoBox.R2.asIndex*/ =>
        require(regE.isInstanceOf[ColElem[_,_]],
          s"Predefined register R${ErgoBox.R2.asIndex} should have Coll[(Coll[Byte], Long)] type but was $regE")
        val values = asRep[Col[(Col[Byte], Long)]](optC.value.get)
        val costs = colBuilder.replicate(values.length, 0)
        val sizes = colBuilder.replicate(values.length, Blake2b256.DigestSize.toLong + SLong.dataSize(0.asWrappedType))
        RCCostedCol(values, costs, sizes, optC.cost + sigmaDslBuilder.CostModel.SelectField)

      case CostedM.value(Def(CCostedFuncCtor(_, func: RCostedFunc[a,b], _,_))) =>
        func.sliceCalc

//      case CostedFoldExtractors.IsGetCost(opt: RWOption[a], th: CostedThunk[b]@unchecked, f) =>
//        implicit val eA = opt.elem.eItem
//        opt.fold(Thunk { forceThunkByMirror(th).cost }, fun { x: Rep[a] => asRep[a => Costed[b]](f)(x).cost })
//
//      case CostedFoldExtractors.IsGetDataSize(opt: RWOption[a], th: CostedThunk[b]@unchecked, f) =>
//        implicit val eA = opt.elem.eItem
//        opt.fold(Thunk { forceThunkByMirror(th).dataSize }, fun { x: Rep[a] => asRep[a => Costed[b]](f)(x).dataSize })

      // Rule: opt.fold(default, f).value ==> opt.fold(default.value, x => f(x).value)
      case CostedM.value(WOptionM.fold(opt, _th @ Def(ThunkDef(_, _)), _f)) =>
        implicit val eA: Elem[Any] = opt.elem.eItem.asElem[Any]
        val th = asRep[Thunk[Costed[Any]]](_th)
        val f = asRep[Any => Costed[Any]](_f)
        opt.fold(Thunk(forceThunkByMirror(th).value), fun { x: Rep[Any] => f(x).value })

      // Rule: opt.fold(default, f).cost ==> opt.fold(default.cost, x => f(x).cost)
      case CostedM.cost(WOptionM.fold(opt, _th @ Def(ThunkDef(_, _)), _f)) =>
        implicit val eA: Elem[Any] = opt.elem.eItem.asElem[Any]
        val th = asRep[Thunk[Costed[Any]]](_th)
        val f = asRep[Any => Costed[Any]](_f)
        opt.fold(Thunk(forceThunkByMirror(th).cost), fun { x: Rep[Any] => f(x).cost })

      // Rule: opt.fold(default, f).dataSize ==> opt.fold(default.dataSize, x => f(x).dataSize)
      case CostedM.dataSize(WOptionM.fold(opt, _th @ Def(ThunkDef(_, _)), _f)) =>
        implicit val eA: Elem[Any] = opt.elem.eItem.asElem[Any]
        val th = asRep[Thunk[Costed[Any]]](_th)
        val f = asRep[Any => Costed[Any]](_f)
        opt.fold(Thunk(forceThunkByMirror(th).dataSize), fun { x: Rep[Any] => f(x).dataSize })

      case CostedFoldExtractors.IsGet(opt: RWOption[a], _, _f) =>
        implicit val eA = opt.elem.eItem
        val f = asRep[a => CostedOption[Any]](_f)
        f(opt.get).get

      case CostedOptionM.getOrElse(WOptionM.fold(opt: RWOption[a], _, _f), _default) =>
        implicit val eA = opt.elem.eItem
        val f = asRep[a => CostedOption[a]](_f)
        val default = asRep[Costed[a]](_default)
        f(opt.getOrElse(Thunk(default.value))).getOrElse(default)

      case CostedOptionM.isDefined(WOptionM.fold(opt: RWOption[a], _, _f)) =>
        implicit val eA = opt.elem.eItem
        RCCostedPrim(opt.isDefined, costedBuilder.SelectFieldCost, 1L)

      case CCostedPrimCtor(v, c, s) =>
        v.elem.asInstanceOf[Elem[_]] match {
          case be: BoxElem[_] => RCCostedBox(asRep[Box](v), c)
          case be: AvlTreeElem[_] => RCCostedAvlTree(asRep[AvlTree](v), c)
          case pe: PairElem[a,b] =>
            val p = asRep[(a,b)](v)
            // TODO costing: this is approximation (we essentially double the cost and size)
            RCCostedPair(RCCostedPrim(p._1, c, s), RCCostedPrim(p._2, c, s))
          case ce: ColElem[a,_] if ce.eItem.isConstantSize =>
            val col = asRep[Col[a]](v)
            mkCostedCol(col, col.length, c)
          case _ => super.rewriteDef(d)
        }

      case CostedBuilderM.costedValue(b, x, SPCM.some(cost)) =>
        dataCost(x, Some(asRep[Int](cost)))

      case IsConstSizeCostedCol(col) =>
        mkCostedCol(col.value, col.value.length, col.cost)

      case _ if isCostingProcess =>
        // apply special rules for costing function
        d match {
          case CM.length(Def(IfThenElseLazy(_, Def(ThunkDef(t: RCol[a]@unchecked,_)), Def(ThunkDef(_e,_)))))  =>
            val e = asRep[Col[a]](_e)
            t.length max e.length
          case _ => super.rewriteDef(d)
        }
      case _ => super.rewriteDef(d)
    }
  }

//  override def transformDef[A](d: Def[A], t: Transformer): Rep[A] = d match {
//    // not the same as super because mkMethodCall can produce a more precise return type
//    case MethodCall(receiver, method, args, neverInvoke) =>
//      val args1 = args.map(transformProductParam(_, t).asInstanceOf[AnyRef])
//      val receiver1 = t(receiver)
//      // in the case neverInvoke is false, the method is invoked in rewriteDef
//      mkMethodCall(receiver1, method, args1, neverInvoke).asInstanceOf[Rep[A]]
//    case _ => super.transformDef(d, t)
//  }

  lazy val BigIntegerElement: Elem[WBigInteger] = wBigIntegerElement

  override def toRep[A](x: A)(implicit eA: Elem[A]):Rep[A] = eA match {
    case BigIntegerElement => Const(x)
    case _ => super.toRep(x)
  }

  /** Should be specified in the final cake */
  val builder: sigmastate.lang.SigmaBuilder
  import builder._

  var _colBuilder: Rep[ColBuilder] = _
  var _costedBuilder: Rep[CostedBuilder] = _
  var _intPlusMonoid: Rep[Monoid[Int]] = _
  var _longPlusMonoid: Rep[Monoid[Long]] = _
  var _sigmaDslBuilder: Rep[SigmaDslBuilder] = _

  init() // initialize global context state

  def colBuilder: Rep[ColBuilder] = _colBuilder
  def costedBuilder: Rep[CostedBuilder] = _costedBuilder
  def intPlusMonoid: Rep[Monoid[Int]] = _intPlusMonoid
  def longPlusMonoid: Rep[Monoid[Long]] = _longPlusMonoid
  def sigmaDslBuilder: Rep[SigmaDslBuilder] = _sigmaDslBuilder

  protected def init(): Unit = {
    _colBuilder = RColOverArrayBuilder()
    _costedBuilder = RCCostedBuilder()
    _intPlusMonoid = costedBuilder.monoidBuilder.intPlusMonoid
    _longPlusMonoid = costedBuilder.monoidBuilder.longPlusMonoid
    _sigmaDslBuilder = RTestSigmaDslBuilder()
  }

// This is experimental alternative which is 10x faster in MeasureIRContext benchmark
// However it is not fully correct. It can be used if current implementation is not fast enough.
//  def colBuilder: Rep[ColBuilder] = {
//    if (_colBuilder == null) _colBuilder = RColOverArrayBuilder()
//    _colBuilder
//  }
//  def costedBuilder: Rep[CostedBuilder] = {
//    if (_costedBuilder == null) _costedBuilder = RCCostedBuilder()
//    _costedBuilder
//  }
//  def intPlusMonoid: Rep[Monoid[Int]] = {
//    if (_intPlusMonoid == null) _intPlusMonoid = costedBuilder.monoidBuilder.intPlusMonoid
//    _intPlusMonoid
//  }
//  def longPlusMonoid: Rep[Monoid[Long]] = {
//    if (_longPlusMonoid == null) _longPlusMonoid = costedBuilder.monoidBuilder.longPlusMonoid
//    _longPlusMonoid
//  }
//  def sigmaDslBuilder: Rep[SigmaDslBuilder] = {
//    if (_sigmaDslBuilder == null) _sigmaDslBuilder = RTestSigmaDslBuilder()
//    _sigmaDslBuilder
//  }
//
//  protected override def onReset(): Unit = {
//    super.onReset()
//    _colBuilder = null
//    _costedBuilder = null
//    _intPlusMonoid = null
//    _longPlusMonoid = null
//    _sigmaDslBuilder = null
//  }

  protected override def onReset(): Unit = {
    super.onReset()
    init()
  }

  import Cost._

  def removeIsProven[T,R](f: Rep[T] => Rep[Any]): Rep[T] => Rep[Any] = { x: Rep[T] =>
    val y = f(x);
    val res = y match {
      case SigmaPropMethods.isValid(p) => p
      case v => v
    }
    res
  }

  private[sigmastate] var funUnderCosting: Sym = null
  def isCostingProcess: Boolean = funUnderCosting != null

  def costingOf[T,R](f: Rep[T => Costed[R]]): Rep[T] => Rep[Int] = { x: Rep[T] =>
    funUnderCosting = f
    val c = f(x).cost;
    funUnderCosting = null
    c
  }

  def split2[T,R](f: Rep[T => Costed[R]]): Rep[(T => Any, T => Int)] = {
    implicit val eT = f.elem.eDom
    val calc = fun(removeIsProven { x: Rep[T] =>
      val y = f(x);
      y.value
    })
    val cost = fun(costingOf(f))
    Pair(calc, cost)
  }

  def split3[T,R](f: Rep[T => Costed[R]]): Rep[(T => Any, (T => Int, T => Long))] = {
    implicit val eT = f.elem.eDom
    val calc = fun(removeIsProven { x: Rep[T] =>
      val y = f(x);
      y.value
    })
    val cost = fun { x: Rep[T] => f(x).cost }
    val size = fun { x: Rep[T] => f(x).dataSize }
    Tuple(calc, cost, size)
  }

  def stypeToElem[T <: SType](t: T): Elem[T#WrappedType] = (t match {
    case SBoolean => BooleanElement
    case SByte => ByteElement
    case SShort => ShortElement
    case SInt => IntElement
    case SLong => LongElement
    case SString => StringElement
    case SAny => AnyElement
    case SBigInt => wBigIntegerElement
    case SBox => boxElement
    case SGroupElement => wECPointElement
    case SAvlTree => avlTreeElement
    case SSigmaProp => sigmaPropElement
    case STuple(items) => tupleStructElement(items.map(stypeToElem(_)):_*)
    case c: SCollectionType[a] => colElement(stypeToElem(c.elemType))
    case _ => error(s"Don't know how to convert SType $t to Elem")
  }).asElem[T#WrappedType]

  def elemToSType[T](e: Elem[T]): SType = e match {
    case BooleanElement => SBoolean
    case ByteElement => SByte
    case ShortElement => SShort
    case IntElement => SInt
    case LongElement => SLong
    case StringElement => SString
    case AnyElement => SAny
    case _: WBigIntegerElem[_] => SBigInt
    case _: WECPointElem[_] => SGroupElement
    case _: AvlTreeElem[_] => SAvlTree
    case oe: WOptionElem[_, _] => sigmastate.SOption(elemToSType(oe.eItem))
    case _: BoxElem[_] => SBox
    case _: SigmaPropElem[_] => SSigmaProp
    case se: StructElem[_] =>
      assert(se.fieldNames.zipWithIndex.forall { case (n,i) => n == s"_${i+1}" })
      STuple(se.fieldElems.map(elemToSType(_)).toIndexedSeq)
    case ce: ColElem[_, _] => SCollection(elemToSType(ce.eItem))
    case fe: FuncElem[_, _] => SFunc(elemToSType(fe.eDom), elemToSType(fe.eRange))
    case pe: PairElem[_, _] => STuple(elemToSType(pe.eFst), elemToSType(pe.eSnd))
    case _ => error(s"Don't know how to convert Elem $e to SType")
  }

  /** For a given data type returns the corresponding specific descendant of CostedElem[T] */
  def elemToCostedElem[T](implicit e: Elem[T]): Elem[Costed[T]] = (e match {
    case e: AvlTreeElem[_] => costedAvlTreeElement
    case e: BoxElem[_] => costedBoxElement
    case oe: WOptionElem[a,_] => costedOptionElement(oe.eItem)
    case ce: ColElem[a,_] => costedColElement(ce.eItem)
    case fe: FuncElem[_, _] => costedFuncElement(UnitElement, fe.eDom, fe.eRange)
    case pe: PairElem[_, _] => costedPairElement(pe.eFst, pe.eSnd)
    case _ => costedElement(e)
  }).asInstanceOf[Elem[Costed[T]]]

  import Liftables._
  def liftableFromElem[WT](eWT: Elem[WT]): Liftable[_,WT] = (eWT match {
    case BooleanElement => BooleanIsLiftable
    case ByteElement => ByteIsLiftable
    case ShortElement => ShortIsLiftable
    case IntElement => IntIsLiftable
    case LongElement => LongIsLiftable
    case StringElement => StringIsLiftable
    case UnitElement => UnitIsLiftable
    case e: WBigIntegerElem[_] => LiftableBigInteger
    case e: WECPointElem[_] => LiftableECPoint
    case ae: WArrayElem[t,_] =>
      implicit val lt = liftableFromElem[t](ae.eItem)
      liftableArray(lt)
  }).asInstanceOf[Liftable[_,WT]]

  import NumericOps._
  private lazy val elemToNumericMap = Map[Elem[_], Numeric[_]](
    (ByteElement, numeric[Byte]),
    (ShortElement, numeric[Short]),
    (IntElement, numeric[Int]),
    (LongElement, numeric[Long]),
    (BigIntegerElement, numeric[BigInteger])
  )
  private lazy val elemToIntegralMap = Map[Elem[_], Integral[_]](
    (ByteElement, integral[Byte]),
    (ShortElement, integral[Short]),
    (IntElement, integral[Int]),
    (LongElement, integral[Long]),
    (BigIntegerElement, integral[BigInteger])
  )
  private lazy val elemToOrderingMap = Map[Elem[_], Ordering[_]](
    (ByteElement, implicitly[Ordering[Byte]]),
    (ShortElement, implicitly[Ordering[Short]]),
    (IntElement, implicitly[Ordering[Int]]),
    (LongElement, implicitly[Ordering[Long]]),
    (BigIntegerElement, implicitly[Ordering[BigInteger]])
  )

  def elemToNumeric [T](e: Elem[T]): Numeric[T]  = elemToNumericMap(e).asInstanceOf[Numeric[T]]
  def elemToIntegral[T](e: Elem[T]): Integral[T] = elemToIntegralMap(e).asInstanceOf[Integral[T]]
  def elemToOrdering[T](e: Elem[T]): Ordering[T] = elemToOrderingMap(e).asInstanceOf[Ordering[T]]

  def opcodeToEndoBinOp[T](opCode: Byte, eT: Elem[T]): EndoBinOp[T] = opCode match {
    case OpCodes.PlusCode => NumericPlus(elemToNumeric(eT))(eT)
    case OpCodes.MinusCode => NumericMinus(elemToNumeric(eT))(eT)
    case OpCodes.MultiplyCode => NumericTimes(elemToNumeric(eT))(eT)
    case OpCodes.DivisionCode => IntegralDivide(elemToIntegral(eT))(eT)
    case OpCodes.ModuloCode => IntegralMod(elemToIntegral(eT))(eT)
    case OpCodes.MinCode => OrderingMin(elemToOrdering(eT))(eT)
    case OpCodes.MaxCode => OrderingMax(elemToOrdering(eT))(eT)
    case _ => error(s"Cannot find EndoBinOp for opcode $opCode")
  }

  def opcodeToBinOp[A](opCode: Byte, eA: Elem[A]): BinOp[A,_] = opCode match {
    case OpCodes.EqCode  => Equals[A]()(eA)
    case OpCodes.NeqCode => NotEquals[A]()(eA)
    case OpCodes.GtCode  => OrderingGT[A](elemToOrdering(eA))
    case OpCodes.LtCode  => OrderingLT[A](elemToOrdering(eA))
    case OpCodes.GeCode  => OrderingGTEQ[A](elemToOrdering(eA))
    case OpCodes.LeCode  => OrderingLTEQ[A](elemToOrdering(eA))
    case _ => error(s"Cannot find BinOp for opcode $opCode")
  }

  type RCosted[A] = Rep[Costed[A]]

  /** This method works by:
    * 1) staging block in the new scope of new thunk
    * 2) extracting value, cost, or dataSize respectively
    * 3) building the schedule by deps, effectively doing DCE along the way
    * This has an effect of splitting the costed graph into three separate graphs*/
  def evalCostedBlock[T](block: => Rep[Costed[T]]): Rep[Costed[T]] = {
    val v = Thunk.forced {
      val costed = block
      costed.value
    }
    val c = Thunk.forced {
      val costed = block
      costed.cost
    }
    val s = Thunk.forced {
      val costed = block
      costed.dataSize
    }
    RCCostedPrim(v, c, s)
  }

  def adaptSigmaBoolean(v: BoolValue) = v match {
    case sb: SigmaBoolean => sb.isProven
    case _ => v
  }

    /** Helper to create costed collection of bytes */
  def mkCostedCol[T](col: Rep[Col[T]], len: Rep[Int], cost: Rep[Int]): Rep[CostedCol[T]] = {
    val costs = colBuilder.replicate(len, 0)
    val sizes = colBuilder.replicate(len, typeSize(col.elem.eItem))
    RCCostedCol(col, costs, sizes, cost)
  }

  def mkCosted[T](v: Rep[T], cost: Rep[Int], size: Rep[Long]): Rep[Costed[T]] = {
    val res = v.elem match {
      case colE: ColElem[a,_] =>
        val xs = asRep[Col[a]](v)
        mkCostedCol(xs, xs.length, cost)
      case _ =>
        RCCostedPrim(v, cost, size)
    }
    asRep[Costed[T]](res)
  }

  type CostingEnv = Map[Any, RCosted[_]]

  import sigmastate._

  val OperationIdKey = MetaKey[AnyRef]("OperationId")(AnyRefElement)

  protected def isOperationNode(v: SValue): Boolean = v match {
    case _: Block | _: BlockValue | _: TaggedVariableNode[_] | _: ValNode | _: ValDef | _: ValUse[_] | _: FuncValue => false
    case _ => true
  }

  protected def onTreeNodeCosted[T <: SType](
        ctx: Rep[CostedContext], env: CostingEnv,
        node: Value[T], costed: RCosted[T#WrappedType]): Unit = {
    if (okMeasureOperationTime && isOperationNode(node)) {
      asRep[Any](costed) match {
        case Def(CCostedPrimCtor(v, c, s)) =>
          v.setMetadata(OperationIdKey)(node.opId)
        case Def(CCostedColCtor(vs,_,_,_)) =>
          vs.setMetadata(OperationIdKey)(node.opId)
        case _ =>
      }
    }
  }

  protected def evalNode[T <: SType](ctx: Rep[CostedContext], env: CostingEnv, node: Value[T]): RCosted[T#WrappedType] = {
    import WOption._

    def eval[T <: SType](node: Value[T]): RCosted[T#WrappedType] = evalNode(ctx, env, node)
    def withDefaultSize[T](v: Rep[T], cost: Rep[Int]): RCosted[T] = RCCostedPrim(v, cost, sizeOf(v))
    object In { def unapply(v: SValue): Nullable[RCosted[Any]] = Nullable(asRep[Costed[Any]](evalNode(ctx, env, v))) }
    class InCol[T] { def unapply(v: SValue): Nullable[Rep[CostedCol[T]]] = Nullable(asRep[CostedCol[T]](evalNode(ctx, env, v))) }
    val InColByte = new InCol[Byte]; val InColAny = new InCol[Any]; val InColInt = new InCol[Int]
    object InSeq { def unapply(items: Seq[SValue]): Nullable[Seq[RCosted[Any]]] = {
      val res = items.map { x: SValue =>
        val xC = eval(x)
        asRep[Costed[Any]](xC)
      }
      Nullable(res)
    }}
    object InSeqUnzipped { def unapply(items: Seq[SValue]): Nullable[(Seq[Rep[Any]], Seq[Rep[Int]], Seq[Rep[Long]])] = {
      val res = items.mapUnzip { x: SValue =>
        val xC = eval(x)
        (asRep[Any](xC.value), xC.cost, xC.dataSize)
      }
      Nullable(res)
    }}
    val res: Rep[Any] = node match {
      case TaggedVariableNode(id, _) =>
        env.getOrElse(id, !!!(s"TaggedVariable $id not found in environment $env"))

      case c @ Constant(v, tpe) => v match {
        case p: DLogProtocol.ProveDlog =>
          val ge = asRep[Costed[WECPoint]](eval(p.value))
          val resV: Rep[SigmaProp] = RProveDlogEvidence(ge.value)
          RCCostedPrim(resV, ge.cost + costOfProveDlog, CryptoConstants.groupSize.toLong)
        case p @ ProveDHTuple(gv, hv, uv, vv) =>
          val gvC = asRep[Costed[WECPoint]](eval(gv))
          val hvC = asRep[Costed[WECPoint]](eval(hv))
          val uvC = asRep[Costed[WECPoint]](eval(uv))
          val vvC = asRep[Costed[WECPoint]](eval(vv))
          val resV: Rep[SigmaProp] = RProveDHTEvidence(gvC.value, hvC.value, uvC.value, vvC.value)
          val cost = gvC.cost + hvC.cost + uvC.cost + vvC.cost + costOfDHTuple
          RCCostedPrim(resV, cost, CryptoConstants.groupSize.toLong * 4)
        case bi: BigInteger =>
          assert(tpe == SBigInt)
          val resV = liftConst(bi)
          RCCostedPrim(resV, costOf(c), SBigInt.MaxSizeInBytes)
        case ge: ECPoint =>
          assert(tpe == SGroupElement)
          val resV = liftConst(ge)
//          val size = SGroupElement.dataSize(ge.asWrappedType)
          withDefaultSize(resV, costOf(c))
        case arr: Array[a] =>
          val tpeA = tpe.asCollection[SType].elemType
          stypeToElem(tpeA) match {
            case eWA: Elem[wa] =>
              implicit val l = liftableFromElem[wa](eWA).asInstanceOf[Liftable[a,wa]]
              val arrSym = liftConst[Array[a], WArray[wa]](arr)
              val resVals  = colBuilder.fromArray(arrSym)
              val resCosts = colBuilder.replicate(arrSym.length, 0)
              val resSizes =
                if (tpeA.isConstantSize)
                  colBuilder.replicate(resVals.length, typeSize(tpeA))
                else {
                  val sizesConst: Array[Long] = arr.map { x: a => tpeA.dataSize(x.asWrappedType) }
                  val sizesArr = liftConst(sizesConst)
                  colBuilder.fromArray(sizesArr)
                }
              RCCostedCol(resVals, resCosts, resSizes, costOf(c))
          }
        case box: ErgoBox =>
          val boxV = liftConst(box.toTestBox(false)(IR))
          RCCostedBox(boxV, costOf(c))
        case treeData: AvlTreeData =>
          val tree: special.sigma.AvlTree = CostingAvlTree(IR, treeData)
          val treeV = liftConst(tree)
          RCCostedAvlTree(treeV, costOf(c))
        case _ =>
          val resV = toRep(v)(stypeToElem(tpe))
          withDefaultSize(resV, costOf(c))
      }

      case Height  => ctx.HEIGHT
      case Inputs  => ctx.INPUTS
      case Outputs => ctx.OUTPUTS
      case Self    => ctx.SELF
      case LastBlockUtxoRootHash => ctx.LastBlockUtxoRootHash
      case MinerPubkey => ctx.MinerPubKey

      case op @ GetVar(id, optTpe) =>
        val res = ctx.getVar(id)(stypeToElem(optTpe.elemType))
        res

//      case op @ TaggedVariableNode(id, tpe) =>
//        val resV = ctx.getVar(id)(stypeToElem(tpe))
//        withDefaultSize(resV, costOf(op))

      case Terms.Block(binds, res) =>
        var curEnv = env
        for (Val(n, _, b) <- binds) {
          if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
          val bC = evalNode(ctx, curEnv, b)
          curEnv = curEnv + (n -> bC)
        }
        val res1 = evalNode(ctx, curEnv, res)
        res1

      case BlockValue(binds, res) =>
        var curEnv = env
        for (ValDef(n, _, b) <- binds) {
          if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
          val bC = evalNode(ctx, curEnv, b)
          curEnv = curEnv + (n -> bC)
        }
        val res1 = evalNode(ctx, curEnv, res)
        res1

      case ValUse(valId, _) =>
        env.getOrElse(valId, !!!(s"ValUse $valId not found in environment $env"))

      case sigmastate.Exponentiate(In(_l), In(_r)) =>
        val l = asRep[Costed[WECPoint]](_l)
        val r = asRep[Costed[WBigInteger]](_r)
        val value = sigmaDslBuilder.exponentiate(l.value, r.value)
        val cost = l.cost + r.cost + costOf(node)
        RCCostedPrim(value, cost, CryptoConstants.groupSize.toLong)

      case sigmastate.MultiplyGroup(In(_l), In(_r)) =>
        val l = asRep[Costed[WECPoint]](_l)
        val r = asRep[Costed[WECPoint]](_r)
        val value = l.value.add(r.value)
        val cost = l.cost + r.cost + costOf(node)
        RCCostedPrim(value, cost, CryptoConstants.groupSize.toLong)

      case Values.GroupGenerator =>
        val value = sigmaDslBuilder.groupGenerator
        RCCostedPrim(value, costOf(node), CryptoConstants.groupSize.toLong)

      case sigmastate.ByteArrayToBigInt(In(_arr)) =>
        val arrC = asRep[Costed[Col[Byte]]](_arr)
        val arr = arrC.value
        val value = sigmaDslBuilder.byteArrayToBigInt(arr)
        val size = arrC.dataSize
        val cost = arrC.cost + costOf(node) + costOf("new_BigInteger_per_item", node.opType) * size.toInt
        RCCostedPrim(value, cost, size)

      case sigmastate.LongToByteArray(In(_x)) =>
        val xC = asRep[Costed[Long]](_x)
        val x = xC.value
        val col = sigmaDslBuilder.longToByteArray(x) // below we assume col.length == typeSize[Long]
        val cost = xC.cost + costOf(node)
        val len = typeSize[Long].toInt
        mkCostedCol(col, len, cost)

      case TreeLookup(In(_tree), InColByte(key), InColByte(proof)) =>
        val tree = asRep[CostedAvlTree](_tree)
        val value = sigmaDslBuilder.treeLookup(tree.value, key.value, proof.value)
        val size = tree.dataSize + key.dataSize + proof.dataSize
        val cost = tree.cost + key.cost + proof.cost + perKbCostOf(node, size)
        value.fold[CostedOption[Col[Byte]]](
          Thunk(RCostedNone(cost)),
          fun { x: Rep[Col[Byte]] => RCostedSome(mkCostedCol(x, size.toInt, cost)) })

      case TreeModifications(In(_tree), InColByte(operations), InColByte(proof)) =>
        val tree = asRep[CostedAvlTree](_tree)
        val value = sigmaDslBuilder.treeModifications(tree.value, operations.value, proof.value)
        val size = tree.dataSize + operations.dataSize + proof.dataSize
        val cost = tree.cost + operations.cost + proof.cost + perKbCostOf(node, size)
        value.fold[CostedOption[Col[Byte]]](
          Thunk(RCostedNone(cost)),
          fun { x: Rep[Col[Byte]] => RCostedSome(mkCostedCol(x, size.toInt, cost)) })

      // opt.get =>
      case utxo.OptionGet(In(_opt)) =>
        val opt = asRep[CostedOption[Any]](_opt)
        opt.get

      // opt.isDefined =>
      case utxo.OptionIsDefined(In(_opt)) =>
        val opt = asRep[CostedOption[Any]](_opt)
        opt.isDefined

      // opt.getOrElse =>
      case utxo.OptionGetOrElse(In(_opt), In(_default)) =>
        val opt = asRep[CostedOption[Any]](_opt)
        opt.getOrElse(_default)

      case SelectField(In(_tup), fieldIndex) =>
        _tup.elem.eVal.asInstanceOf[Elem[_]] match {
          case se: StructElem[_] =>
            val tup = asRep[Costed[Struct]](_tup)
            val fn = STuple.componentNames(fieldIndex - 1)
            withDefaultSize(tup.value.getUntyped(fn), costedBuilder.SelectFieldCost)
          case pe: PairElem[a,b] =>
            assert(fieldIndex == 1 || fieldIndex == 2, s"Invalid field index $fieldIndex of the pair ${_tup}: $pe")
            val pair = asRep[CostedPair[a,b]](_tup)
            val res = if (fieldIndex == 1) pair.l else pair.r
            res
        }

      case Values.Tuple(InSeq(items)) =>
        val fields = items.zipWithIndex.map { case (x, i) => (s"_${i+1}", x)}
        RCostedStruct(struct(fields), costedBuilder.ConstructTupleCost)

      case node: BooleanTransformer[_] =>
        val eIn = stypeToElem(node.input.tpe.elemType)
        val xs = asRep[CostedCol[Any]](eval(node.input))
        val eAny = xs.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        implicit val eArg: Elem[Costed[Any]] = eAny match {
          case _: BoxElem[_] => element[CostedBox].asElem[Costed[Any]]
          case _ => costedElement(eAny)
        }
        val condC = asRep[CostedFunc[Unit, Any, SType#WrappedType]](evalNode(ctx, env, node.condition)).func
        val (calcF, costF) = splitCostedFunc2(condC, okRemoveIsValid = true)
        val values = xs.values.map(calcF)
        val cost = xs.values.zip(xs.costs.zip(xs.sizes)).map(costF).sum(intPlusMonoid)
        val value = calcF.elem.eRange match {
          case e if e == BooleanElement =>
            node match {
              case _: ForAll[_] => xs.values.forall(asRep[Any => Boolean](calcF))
              case _: Exists[_] => xs.values.exists(asRep[Any => Boolean](calcF))
            }
          case _: SigmaPropElem[_] =>
            if (node.isInstanceOf[ForAll[_]])
              sigmaDslBuilder.allZK(asRep[Col[SigmaProp]](values))
            else
              sigmaDslBuilder.anyZK(asRep[Col[SigmaProp]](values))
        }
        withDefaultSize(value, cost)

      case MapCollection(input, sfunc) =>
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = asRep[CostedCol[Any]](evalNode(ctx, env, input))
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any], _]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val mapperC = asRep[CostedFunc[Unit, Any, SType#WrappedType]](evalNode(ctx, env, sfunc)).func
        val res = inputC.mapCosted(mapperC)
        res

      case Fold(input, zero, sfunc) =>
        val eItem = stypeToElem(input.tpe.elemType)
        val eState = stypeToElem(zero.tpe)
        (eState, eItem) match { case (eState: Elem[s], eItem: Elem[a]) =>
          val inputC = asRep[CostedCol[a]](eval(input))
          implicit val eA = inputC.elem.asInstanceOf[CostedElem[Col[a],_]].eVal.eA
          assert(eItem == eA, s"Types should be equal: but $eItem != $eA")

          val zeroC = asRep[Costed[s]](eval(zero))
          implicit val eS = zeroC.elem.eVal
          assert(eState == eS, s"Types should be equal: but $eState != $eS")

          val foldOpC = fun { in: Rep[CostedPair[s, a]] =>
            val acc = in.l; val item = in.r
            val out = sfunc match {
              case Terms.Lambda(_, Seq((accN, _), (n, _)), _, Some(op)) =>
                evalNode(ctx, env + (accN -> acc, n -> item), op)
              case FuncValue(Seq((tpl, _)), op) =>
                evalNode(ctx, env + (tpl -> in), op)
            }
            asRep[Costed[s]](out)
          }
          val res = inputC.foldCosted(zeroC, asRep[Costed[(s,a)] => Costed[s]](foldOpC))
          res
        }

      case op @ Slice(In(input), In(from), In(until)) =>
        val inputC = asRep[CostedCol[Any]](input)
        val fromC = asRep[Costed[Int]](from)
        val untilC = asRep[Costed[Int]](until)
        val f = fromC.value
        val u = untilC.value
        val vals = inputC.values.slice(f, u)
        val costs = inputC.costs.slice(f, u)
        val sizes = inputC.sizes.slice(f, u)
        RCCostedCol(vals, costs, sizes, inputC.valuesCost + costOf(op))

      case Append(In(_col1), In(_col2)) =>
        val col1 = asRep[CostedCol[Any]](_col1)
        val col2 = asRep[CostedCol[Any]](_col2)
        val values = col1.values.append(col2.values)
        val costs = col1.costs.append(col2.costs)
        val sizes = col1.sizes.append(col2.sizes)
        RCCostedCol(values, costs, sizes, costOf(node))

      case Terms.Apply(Select(col, "where", _), Seq(Terms.Lambda(_, Seq((n, t)), _, Some(body)))) =>
        val input = col.asValue[SCollection[SType]]
        val cond = body.asValue[SBoolean.type]
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = asRep[CostedCol[Any]](evalNode(ctx, env, input))
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val condC = fun { x: Rep[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), cond)
        }
        val res = inputC.filterCosted(condC)
        res

//      case Terms.Apply(Select(col,"fold", _), Seq(zero, Terms.Lambda(Seq((zeroArg, tZero), (opArg, tOp)), _, Some(body)))) =>
//        val taggedZero = mkTaggedVariable(21, tZero)
//        val taggedOp = mkTaggedVariable(22, tOp)
//        val env1 = env ++ Seq(zeroArg -> taggedZero, opArg -> taggedOp)
//        some(mkFold(col.asValue[SCollection[SType]], taggedZero.varId, zero, taggedOp.varId, body))(env1)


      case Terms.Apply(f, Seq(x)) if f.tpe.isFunc =>
        val fC = asRep[CostedFunc[Unit, Any, Any]](evalNode(ctx, env, f))
        val xC = asRep[Costed[Any]](evalNode(ctx, env, x))
        if (f.tpe.asFunc.tRange.isCollection) {
          val (calcF, costF, sizeF) = splitCostedColFunc(asRep[CostedColFunc[Any,Any]](fC.func))
          val value = xC.value
          val values: Rep[Col[Any]] = Apply(calcF, value, false)
          val costRes: Rep[(Col[Int], Int)] = Apply(costF, Pair(value, Pair(xC.cost, xC.dataSize)), false)
          val sizes: Rep[Col[Long]]= Apply(sizeF, Pair(value, xC.dataSize), false)
          RCCostedCol(values, costRes._1, sizes, costRes._2)
        }
        else {
          val (calcF, costF, sizeF) = splitCostedFunc(fC.func)
          val value = xC.value
          val y: Rep[Any] = Apply(calcF, value, false)
          val c: Rep[Int] = Apply(costF, Pair(value, Pair(xC.cost, xC.dataSize)), false)
          val s: Rep[Long]= Apply(sizeF, xC.dataSize, false)
          RCCostedPrim(y, c, s)
        }

      case opt: OptionValue[_] =>
        error(s"Option constructors are not supported: $opt")

      case CalcBlake2b256(In(input)) =>
        val bytesC = asRep[Costed[Col[Byte]]](input)
        val res = sigmaDslBuilder.blake2b256(bytesC.value)
        val cost = bytesC.cost + perKbCostOf(node, bytesC.dataSize)
        mkCostedCol(res, Blake2b256.DigestSize, cost)
      case CalcSha256(In(input)) =>
        val bytesC = asRep[Costed[Col[Byte]]](input)
        val res = sigmaDslBuilder.sha256(bytesC.value)
        val cost = bytesC.cost + perKbCostOf(node, bytesC.dataSize)
        mkCostedCol(res, Sha256.DigestSize, cost)

      case utxo.SizeOf(In(xs)) =>
        xs.elem.eVal match {
          case ce: ColElem[a,_] =>
            val xsC = asRep[Costed[Col[a]]](xs)
            val v = xsC.value.length
            withDefaultSize(v, xsC.cost + costOf(node))
          case se: StructElem[_] =>
            val xsC = asRep[Costed[Struct]](xs)
            withDefaultSize(se.fields.length, xsC.cost + costOf(node))
        }

      case ByIndex(xs, i, default) =>
        val xsC = asRep[CostedCol[Any]](eval(xs))
        val iC = asRep[Costed[Int]](eval(i))
        val iV = iC.value
        val size = xsC.sizes(iV)
        default match {
          case Some(defaultValue) =>
            val defaultC = asRep[Costed[Any]](eval(defaultValue))
            val default = defaultC.value
            val value = xsC.value.getOrElse(iV, default)
            val cost = xsC.cost + iC.cost + defaultC.cost + costOf(node)
            RCCostedPrim(value, cost, size)
          case None =>
            RCCostedPrim(xsC.value(iV), xsC.cost + iC.cost + costOf(node), size)
        }

      case SigmaPropIsProven(p) =>
        val pC = asRep[Costed[SigmaProp]](eval(p))
        val v = pC.value.isValid
        val c = pC.cost + costOf(node)
        val s = pC.dataSize // NOTE: we pass SigmaProp's size, this is handled in buildCostedGraph
        RCCostedPrim(v, c, s)
      case SigmaPropBytes(p) =>
        val pC = asRep[Costed[SigmaProp]](eval(p))
        val v = pC.value.propBytes
        withDefaultSize(v, pC.cost + costOf(node))

      case utxo.ExtractId(In(box)) =>  // TODO costing: use special CostedColFixed for fixed-size collections
        val boxC = asRep[Costed[Box]](box)
        val id = boxC.value.id
        mkCostedCol(id, Blake2b256.DigestSize, boxC.cost + costOf(node))
      case utxo.ExtractBytesWithNoRef(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        mkCostedCol(boxC.value.bytesWithoutRef, ErgoBox.MaxBoxSize, boxC.cost + costOf(node))
      case utxo.ExtractAmount(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        withDefaultSize(boxC.value.value, boxC.cost + costOf(node))
      case utxo.ExtractScriptBytes(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val bytes = boxC.value.propositionBytes
        mkCostedCol(bytes, ErgoBox.MaxBoxSize, boxC.cost + costOf(node))
      case utxo.ExtractBytes(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val bytes = boxC.value.bytes
        mkCostedCol(bytes, ErgoBox.MaxBoxSize, boxC.cost + costOf(node))
      case utxo.ExtractCreationInfo(In(box)) =>
        val boxC = asRep[CostedBox](box)
        boxC.creationInfo
      case utxo.ExtractRegisterAs(In(box), regId, optTpe) =>
        val boxC = asRep[CostedBox](box)
        implicit val elem = stypeToElem(optTpe.elemType).asElem[Any]
        val valueOpt = boxC.getReg(regId.number.toInt)(elem)
        valueOpt

      case BoolToSigmaProp(bool) =>
        val boolC = eval(bool)
        val value = sigmaDslBuilder.sigmaProp(boolC.value)
        RCCostedPrim(value, boolC.cost + costOf(node), 1L)

      case AtLeast(bound, input) =>
        val inputC = asRep[CostedCol[Any]](evalNode(ctx, env, input))
        if (inputC.values.length.isConst) {
          val inputCount = inputC.values.length.asValue
          if (inputCount > AtLeast.MaxChildrenCount)
            error(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: $inputCount")
        }
        val boundC = eval(bound)
        val res = sigmaDslBuilder.atLeast(boundC.value, asRep[Col[SigmaProp]](inputC.values))
        val cost = boundC.cost + inputC.cost + costOf(node)
        RCCostedPrim(res, cost, CryptoConstants.groupSize.toLong)

      case op: ArithOp[t] if op.tpe == SBigInt =>
        import OpCodes._
        val xC = asRep[Costed[WBigInteger]](eval(op.left))
        val yC = asRep[Costed[WBigInteger]](eval(op.right))
        val opName = op.opName
        var v: Rep[WBigInteger] = null;
        val s: Rep[Long] = SBigInt.MaxSizeInBytes
        op.opCode match {
          case PlusCode =>
            v = xC.value.add(yC.value)
          case MinusCode =>
            v = xC.value.subtract(yC.value)
          case MultiplyCode =>
            v = xC.value.multiply(yC.value)
          case DivisionCode =>
            v = xC.value.divide(yC.value)
          case ModuloCode =>
            v = xC.value.mod(yC.value)
          case MinCode =>
            v = xC.value.min(yC.value)
          case MaxCode =>
            v = xC.value.max(yC.value)
          case code => error(s"Cannot perform Costing.evalNode($op): unknown opCode ${code}")
        }
        val c = xC.cost + yC.cost + costOf(op)
        RCCostedPrim(v, c, s)

      case op: ArithOp[t] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToEndoBinOp(op.opCode, et)
        val x = evalNode(ctx, env, op.left)
        val y = evalNode(ctx, env, op.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          withDefaultSize(ApplyBinOp(binop, x.value, y.value), x.cost + y.cost + costOf(op))
        }

//      case ModQ(input) =>
//        val inputC = asRep[Costed[WBigInteger]](eval(input))
//        val v = inputC.value.modQ
//        RCCostedPrim(v, inputC.cost + costOf(node), SBigInt.MaxSizeInBytes)

      case OR(input) => input match {
        case ConcreteCollection(items, tpe) =>
          val itemsC = items.map(item => eval(adaptSigmaBoolean(item)))
          val res = sigmaDslBuilder.anyOf(colBuilder.fromItems(itemsC.map(_.value): _*))
          val costs = colBuilder.fromItems(itemsC.map(_.cost): _*)
          val cost = costs.sum(intPlusMonoid) + perItemCostOf(node, costs.length)
          withDefaultSize(res, cost)
        case _ =>
          val inputC = asRep[CostedCol[Boolean]](eval(input))
          val res = sigmaDslBuilder.anyOf(inputC.value)
          val cost = inputC.cost + perItemCostOf(node, inputC.sizes.length)
          withDefaultSize(res, cost)
      }

      case AND(input) => input match {
        case ConcreteCollection(items, tpe) =>
          val itemsC = items.map(item => eval(adaptSigmaBoolean(item)))
          val res = sigmaDslBuilder.allOf(colBuilder.fromItems(itemsC.map(_.value): _*))
          val costs = colBuilder.fromItems(itemsC.map(_.cost): _*)
          val cost = costs.sum(intPlusMonoid) + perItemCostOf(node, costs.length)
          withDefaultSize(res, cost)
        case _ =>
          val inputC = asRep[CostedCol[Boolean]](eval(input))
          val res = sigmaDslBuilder.allOf(inputC.value)
          val cost = inputC.cost + perItemCostOf(node, inputC.sizes.length)
          withDefaultSize(res, cost)
      }

      case BinOr(l, r) =>
        val lC = evalNode(ctx, env, l)
        val rValTh = Thunk(evalNode(ctx, env, r).value)
        val rCost = evalNode(ctx, env, r).cost   // cost graph is built without Thunk (upper bound approximation)
        withDefaultSize(Or.applyLazy(lC.value, rValTh), lC.cost + rCost + costOf(node))

      case BinAnd(l, r) =>
        val lC = evalNode(ctx, env, l)
        val rValTh = Thunk(evalNode(ctx, env, r).value)
        val rCost = evalNode(ctx, env, r).cost
        withDefaultSize(And.applyLazy(lC.value, rValTh), lC.cost + rCost + costOf(node))

      case SigmaAnd(items) =>
        val itemsC = items.map(eval)
        val res = sigmaDslBuilder.allZK(colBuilder.fromItems(itemsC.map(_.value.asRep[SigmaProp]): _*))
        val costs = colBuilder.fromItems(itemsC.map(_.cost): _*)
        val cost = costs.sum(intPlusMonoid) + perItemCostOf(node, costs.length)
        withDefaultSize(res, cost)

      case SigmaOr(items) =>
        val itemsC = items.map(eval)
        val res = sigmaDslBuilder.anyZK(colBuilder.fromItems(itemsC.map(_.value.asRep[SigmaProp]): _*))
        val costs = colBuilder.fromItems(itemsC.map(_.cost): _*)
        val cost = costs.sum(intPlusMonoid) + perItemCostOf(node, costs.length)
        withDefaultSize(res, cost)

      case op: Relation[t,_] if op.tpe == SBigInt =>
        import OpCodes._
        op.opCode match {
          case GtCode =>
            val x = asRep[Costed[WBigInteger]](evalNode(ctx, env, op.left))
            val y = asRep[Costed[WBigInteger]](evalNode(ctx, env, op.right))
            val resSize = x.dataSize.min(y.dataSize)
            val cost = x.cost + y.cost + costOf(op) + costOf(">_per_item", op.opType) * resSize.toInt
            RCCostedPrim(x.value.compareTo(y.value) > 0, cost, resSize)
          //          case MinusCode => NumericMinus(elemToNumeric(eT))(eT)
          //          case MultiplyCode => NumericTimes(elemToNumeric(eT))(eT)
          //          case DivisionCode => IntegralDivide(elemToIntegral(eT))(eT)
          //          case ModuloCode => IntegralMod(elemToIntegral(eT))(eT)
          case _ => error(s"Cannot perform Costing.evalNode($op)")
        }
      case rel: Relation[t, _] =>
        val tpe = rel.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToBinOp(rel.opCode, et)
        val x = eval(rel.left)
        val y = eval(rel.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          val value = binop.apply(x.value, asRep[t#WrappedType](y.value))
          val cost =
            if (tpe.isConstantSize) x.cost + y.cost
            else x.cost + y.cost + perKbCostOf(node, x.dataSize + y.dataSize)
          val res = withDefaultSize(value, cost)
          res
        }

      case If(c, t, e) =>
        val cC = evalNode(ctx, env, c)
        def tC = evalNode(ctx, env, t)
        def eC = evalNode(ctx, env, e)
        val resV = IF (cC.value) THEN tC.value ELSE eC.value
        val resCost = cC.cost + (tC.cost max eC.cost) + costOf("If", SFunc(Vector(SBoolean, If.tT, If.tT), If.tT))
        mkCosted(resV, resCost, tC.dataSize max eC.dataSize)

      case l @ Terms.Lambda(_, Seq((n, argTpe)), tpe, Some(body)) =>
        val eAny = stypeToElem(argTpe).asElem[Any]
        val xElem = elemToCostedElem(eAny)
        val f = fun { x: Rep[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), body)
        }(Lazy(xElem))
        RCCostedFunc(RCCostedPrim((), 0, 0L), f, costOf(node), l.tpe.dataSize(SType.DummyValue))

      case l @ FuncValue(Seq((n, argTpe)), body) =>
        val eAny = stypeToElem(argTpe).asElem[Any]
        val xElem = elemToCostedElem(eAny)
        val f = fun { x: Rep[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), body)
        }(Lazy(xElem))
        RCCostedFunc(RCCostedPrim((), 0, 0L), f, costOf(node), l.tpe.dataSize(0.asWrappedType))

      case col @ ConcreteCollection(InSeqUnzipped(vs, cs, ss), elemType) =>
        implicit val eAny = stypeToElem(elemType).asElem[Any]
        val values = colBuilder.fromItems(vs: _*)
        val costs = colBuilder.fromItems(cs: _*)
        val sizes = colBuilder.fromItems(ss: _*)
        RCCostedCol(values, costs, sizes, costOf(col))

      case sigmastate.Upcast(In(inputC), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        val res = upcast(inputC.value)(elem)
        withDefaultSize(res, inputC.cost + costOf(node))

      case sigmastate.Downcast(In(inputC), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        val res = downcast(inputC.value)(elem)
        withDefaultSize(res, inputC.cost + costOf(node))

      case LongToByteArray(In(input)) =>
        val inputC = asRep[Costed[Long]](input)
        val res = sigmaDslBuilder.longToByteArray(inputC.value)
        val cost = inputC.cost + costOf(node)
        withDefaultSize(res, cost)

      case Xor(InColByte(l), InColByte(r)) =>
        val values = colBuilder.xor(l.value, r.value)
        val sizes = r.sizes
        val len = sizes.length
        val costs = colBuilder.replicate(len, 0)
        val cost = perKbCostOf(node, len.toLong)
        RCCostedCol(values, costs, sizes, cost)

// TODO should be
//      case ErgoAddressToSigmaProp(input) =>
//        val inputC = evalNode(ctx, env, input)
//        withDefaultSize(inputC.value, inputC.cost + costOf(node))

      case sigmastate.Values.ConstantPlaceholder(index, tpe) =>
        val elem = toLazyElem(stypeToElem(tpe))
        val res = constantPlaceholder(index)(elem)
        withDefaultSize(res, costOf(node))

      case SubstConstants(InColByte(bytes), InColInt(positions), InColAny(newValues)) =>
        val values = sigmaDslBuilder.substConstants(bytes.values, positions.values, newValues.values)(AnyElement)
        val len = bytes.sizes.length.toLong + newValues.sizes.sum(longPlusMonoid)
        val cost = bytes.cost + positions.cost + newValues.cost + perKbCostOf(node, len)
        mkCostedCol(values, len.toInt, cost)

      case DecodePoint(InColByte(bytes)) =>
        val res = sigmaDslBuilder.decodePoint(bytes.values)
        withDefaultSize(res, costOf(node))

      case _ =>
        error(s"Don't know how to evalNode($node)")
    }
    val resC = asRep[Costed[T#WrappedType]](res)
    onTreeNodeCosted(ctx, env, node, resC)
    resC
  }


  def buildCostedGraph[T <: SType](envVals: Map[Any, SValue], tree: Value[T]): Rep[Context => Costed[T#WrappedType]] = {
    fun { ctx: Rep[Context] =>
      val ctxC = RCCostedContext(ctx)
      val env = envVals.mapValues(v => evalNode(ctxC, Map(), v))
      val res = evalNode(ctxC, env, tree)
      res
//      val res1 = res match {
//        case RCostedPrim(SigmaPropMethods.isProven(p), Def(ApplyBinOp(op, l, r)), s) if op.isInstanceOf[NumericPlus[_]] =>
//          RCostedPrim(p, l.asRep[Int], s)
//        case _ => res
//      }
//      res1.asRep[Costed[T#WrappedType]]
    }
  }

  def cost(env: ScriptEnv, typed: SValue): Rep[Context => Costed[SType#WrappedType]] = {
    val cg = buildCostedGraph[SType](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    cg
  }

  def error(msg: String) = throw new CosterException(msg, None)
}
