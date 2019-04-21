package sigmastate.eval

import java.math.BigInteger

import scala.language.implicitConversions
import scala.language.existentials
import org.bouncycastle.math.ec.ECPoint
import scalan.{Nullable, MutableLazy, Lazy, RType, SigmaLibrary}
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
import sigmastate.eval._
import sigma.util.Extensions._
import ErgoLikeContext._
import scalan.compilation.GraphVizConfig
import SType._
import scalan.RType._
import scorex.crypto.hash.{Sha256, Blake2b256}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.{Terms, SourceContext}
import scalan.staged.Slicing
import sigma.types.PrimViewType
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.{ProveDHTuple, DLogProtocol}
import sigmastate.eval.Evaluation.rtypeToSType
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.CollType
import special.Types._
import special.sigma.{GroupElementRType, TestGroupElement, AvlTreeRType, BigIntegerRType, BoxRType, ECPointRType, BigIntRType, SigmaPropRType}
import special.sigma.Extensions._

trait RuntimeCosting extends CostingRules with DataCosting with Slicing { IR: Evaluation =>
  import Context._;
  import Header._;
  import PreHeader._;
  import WArray._;
  import WBigInteger._
  import WECPoint._
  import GroupElement._;
  import BigInt._;
  import WOption._
  import Coll._;
  import CollBuilder._;
  import SigmaProp._;
  import Box._
  import CollOverArrayBuilder._;
  import CostedBuilder._
  import SizeBuilder._
  import CCostedBuilder._
  import CSizeBuilder._
  import Size._;
  import SizeBox._;
  import SizeColl._;
  import SizeOption._;
  import SizePair._;
  import SizeContext._
  import CSizeContext._
  import CSizePrim._
  import CSizePair._
  import CSizeColl._
  import CSizeOption._
  import Costed._;
  import CostedPrim._;
  import CCostedPrim._;
  import CostedPair._;
  import CCostedPair._;
  import CostedFunc._;
  import CCostedFunc._;
  import CostedColl._;
  import CCostedColl._;
  import CostedBuilder._;
  import CostedOption._;
  import CCostedOption._
  import SigmaDslBuilder._
  import MonoidBuilder._
  import MonoidBuilderInst._
  import AvlTree._
  import Monoid._
  import IntPlusMonoid._
  import LongPlusMonoid._
  import WSpecialPredef._
  import TestSigmaDslBuilder._
  import CostModel._
  import Liftables._

  override val performViewsLifting = false
  val okMeasureOperationTime: Boolean = false

  this.isInlineThunksOnForce = true  // this required for splitting of cost graph
  this.keepOriginalFunc = false  // original lambda of Lambda node contains invocations of evalNode and we don't want that
  this.useAlphaEquality = false
//  unfoldWithOriginalFunc = unfoldWithOrig

  /** Whether to create CostOf nodes or substutute costs from CostTable as constants in the graph.
    * true - substitute; false - create CostOf nodes */
  var substFromCostTable: Boolean = true

  /** Whether to save calcF and costF graphs in the file given by ScriptNameProp environment variable */
  var saveGraphsInFile: Boolean = true

//  /** Pass configuration which is used by default in IRContext. */
//  val calcPass = new DefaultPass("calcPass", Pass.defaultPassConfig.copy(constantPropagation = true))
//
//  /** Pass configuration which is used during splitting cost function out of cost graph.
//    * @see `RuntimeCosting.split2` */
//  val costPass = new DefaultPass("costPass", Pass.defaultPassConfig.copy(constantPropagation = true))

/**  To enable specific configuration uncomment one of the lines above and use it in the beginPass below. */
//  beginPass(costPass)

  def createSliceAnalyzer = new SliceAnalyzer

  val CollMarking = new TraversableMarkingFor[Coll]
  val WOptionMarking = new TraversableMarkingFor[WOption]

  override def createEmptyMarking[T](eT: Elem[T]): SliceMarking[T] = eT match {
    case _: BoxElem[_] | _: BigIntElem[_] | _: IntPlusMonoidElem | _: LongPlusMonoidElem |
         _: CollOverArrayBuilderElem | _: SigmaPropElem[_] =>
      EmptyBaseMarking(eT)
    case ae: CollElem[a,_] =>
      val eA = ae.eItem
      CollMarking(KeyPath.None, EmptyMarking(eA)).asMark[T]
    case ae: WOptionElem[a,_] =>
      val eA = ae.eItem
      WOptionMarking(KeyPath.None, EmptyMarking(eA)).asMark[T]
    case _ =>
      super.createEmptyMarking(eT)
  }

  override def createAllMarking[T](e: Elem[T]): SliceMarking[T] = e match {
    case _: BoxElem[_] | _: BigIntElem[_] | _: IntPlusMonoidElem | _: LongPlusMonoidElem | _: CollOverArrayBuilderElem =>
      AllBaseMarking(e)
    case colE: CollElem[a,_] =>
      implicit val eA = colE.eItem
      CollMarking[a](KeyPath.All, AllMarking(eA)).asMark[T]
    case optE: WOptionElem[a,_] =>
      implicit val eA = optE.eItem
      WOptionMarking[a](KeyPath.All, AllMarking(eA)).asMark[T]
    case _ =>
      super.createAllMarking(e)
  }

  case class CostOf(opName: String, opType: SFunc) extends BaseDef[Int] {
    override def transform(t: Transformer): Def[IntPlusMonoidData] = this
    def eval: Int = {
      val operId = OperationId(opName, opType)
      val cost = CostTable.DefaultCosts(operId)
      cost
    }
  }

  /** Graph node which represents cost of operation, which depends on size of the data.
    * @param operId   id of the operation in CostTable
    * @param size     size of the data which is used to compute operation cost
    */
  case class PerKbCostOf(operId: OperationId, size: Rep[Long]) extends BaseDef[Int] {
    override def transform(t: Transformer): Def[IntPlusMonoidData] = PerKbCostOf(operId, t(size))
    /** Cost rule which is used to compute operation cost, depending on dataSize.
      * Per kilobite cost of the oparation is obtained from CostTable and multiplied on
      * the data size in Kb. */
    def eval(dataSize: Long): Int = {
      val cost = CostTable.DefaultCosts(operId)
      ((dataSize / 1024L).toInt + 1) * cost
    }
  }

  def costOf(costOp: CostOf, doEval: Boolean): Rep[Int] = {
    val res = if (doEval) toRep(costOp.eval)
    else (costOp: Rep[Int])
    res
  }

  def costOf(opName: String, opType: SFunc, doEval: Boolean): Rep[Int] = {
    val costOp = CostOf(opName, opType)
    costOf(costOp, doEval)
  }

  def costOf(opName: String, opType: SFunc): Rep[Int] = {
    costOf(opName, opType, substFromCostTable)
  }

  def costOf(method: SMethod): Rep[Int] = {
    val methodTemplate = method.objType.getMethodById(method.methodId)
    val opId = methodTemplate.opId
    costOf(opId.name, opId.opType.copy(tpeParams = Nil), substFromCostTable)
  }

  def perKbCostOf(method: SMethod, dataSize: Rep[Long]): Rep[Int] = {
    val methodTemplate = method.objType.getMethodById(method.methodId)
    val opId = methodTemplate.opId
    perKbCostOf(opId.name, opId.opType.copy(tpeParams = Nil), dataSize)
  }

  val _costOfProveDlogEval = CostOf("ProveDlogEval", SFunc(SUnit, SSigmaProp))
  val _costOfProveDHTuple = CostOf("ProveDHTuple", SFunc(SUnit, SSigmaProp))

  def costOfProveDlog: Rep[Int] = costOf(_costOfProveDlogEval, substFromCostTable)
  def costOfDHTuple: Rep[Int] = costOf(_costOfProveDHTuple, substFromCostTable)  // see CostTable for how it relate to ProveDlogEval

  def costOfSigmaTree(sigmaTree: SigmaBoolean): Int = sigmaTree match {
    case _: ProveDlog => _costOfProveDlogEval.eval
    case _: ProveDHTuple => _costOfProveDHTuple.eval
    case CAND(children) => children.map(costOfSigmaTree(_)).sum
    case COR(children)  => children.map(costOfSigmaTree(_)).sum
    case CTHRESHOLD(_, children)  => children.map(costOfSigmaTree(_)).sum
    case _ => CostTable.MinimalCost
  }

  case class ConstantPlaceholder[T](index: Int)(implicit eT: LElem[T]) extends Def[T] {
    def selfType: Elem[T] = eT.value
  }

  def constantPlaceholder[T](index: Int)(implicit eT: LElem[T]): Rep[T] = ConstantPlaceholder[T](index)

  def perKbCostOf(opName: String, opType: SFunc, dataSize: Rep[Long]): Rep[Int] = {
    val opNamePerKb = s"${opName}_per_kb"
    PerKbCostOf(OperationId(opNamePerKb, opType), dataSize)
  }

  def perKbCostOf(node: SValue, dataSize: Rep[Long]): Rep[Int] = {
    perKbCostOf(node.getClass.getSimpleName, node.opType, dataSize)
  }

  def perItemCostOf(node: SValue, arrLength: Rep[Int]) = {
    val opName = s"${node.getClass.getSimpleName}_per_item"
    costOf(opName, node.opType) * arrLength
  }

  def constCost(tpe: SType): Rep[Int] = tpe match {
    case _: SFunc =>
      costOf(s"Lambda", Constant[SType](SFunc.identity.asWrappedType, tpe).opType)
    case _ =>
      costOf(s"Const", Constant[SType](SType.DummyValue, tpe).opType)
  }

  def constCost[T: Elem]: Rep[Int] = {
    val tpe = elemToSType(element[T])
    constCost(tpe)
  }

  def costOf(v: SValue): Rep[Int] = v match {
    case l: Terms.Lambda =>
      constCost(l.tpe)
    case l: FuncValue =>
      constCost(l.tpe)
    case _ =>
      costOf(v.opName, v.opType)
  }

  trait SizeStruct extends Size[Struct] {
    def sizeFields: Rep[Struct]
  }
  case class SizeStructCtor(sizeFields: Rep[Struct]) extends SizeStruct {
    override def transform(t: Transformer) = SizeStructCtor(t(sizeFields))

    implicit val eVal: Elem[Struct] = {
      val fields = sizeFields.elem.fields.map { case (fn, cE) => (fn, cE.asInstanceOf[SizeElem[_, _]].eVal) }
      structElement(fields)
    }
    val selfType: Elem[Size[Struct]] = sizeElement(eVal)

    def dataSize: Rep[Long] = {
      val sizes = sizeFields.fields.map { case (_, cf: RSize[a]@unchecked) => cf.dataSize }
      val sizesColl = colBuilder.fromItems(sizes:_*)
      sizesColl.sum(longPlusMonoid)
    }
  }
  def RSizeStruct(sizeFields: Rep[Struct]): Rep[Size[Struct]] = SizeStructCtor(sizeFields)

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
      opCost(costs, structCost)
    }

    override def size: Rep[Size[Struct]] = {
      val sizeFields = costedFields.mapFields { case cf: RCosted[a]@unchecked => cf.size }
      SizeStructCtor(sizeFields)
    }
  }

  def RCostedStruct(costedFields: Rep[Struct], structCost: Rep[Int]): Rep[Costed[Struct]] = CostedStructCtor(costedFields, structCost)

  // SizeThunk =============================================
  trait SizeThunk[A] extends Size[Thunk[A]] { }

  case class SizeThunkCtor[A](sizeBlock: Rep[Thunk[Size[A]]]) extends SizeThunk[A] {
    override def transform(t: Transformer) = SizeThunkCtor(t(sizeBlock))
    implicit val eVal: Elem[Thunk[A]] = thunkElement(sizeBlock.elem.eItem.eVal)
    val selfType: Elem[Size[Thunk[A]]] = sizeElement(eVal)

    override def dataSize: Rep[Long] = sizeBlock.force().dataSize
  }

  def RSizeThunk[A](sizeBlock: Rep[Thunk[Size[A]]]): Rep[Size[Thunk[A]]] = SizeThunkCtor(sizeBlock)
  // ---------------------------------------------------------


  // CostedThunk =============================================
  trait CostedThunk[A] extends Costed[Thunk[A]] { }

  case class CostedThunkCtor[A](costedBlock: Rep[Thunk[Costed[A]]], thunkCost: Rep[Int]) extends CostedThunk[A] {
    override def transform(t: Transformer) = CostedThunkCtor(t(costedBlock), t(thunkCost))
    implicit val eVal: Elem[Thunk[A]] = thunkElement(costedBlock.elem.eItem.eVal)
    val selfType: Elem[Costed[Thunk[A]]] = costedElement(eVal)

    def builder: Rep[CostedBuilder] = costedBuilder
    def value: Rep[Thunk[A]] = Thunk { costedBlock.force().value }
    def cost: Rep[Int] = costedBlock.force().cost
    override def size: RSize[Thunk[A]] = SizeThunkCtor(Thunk { costedBlock.force().size })
  }

  def RCostedThunk[A](costedBlock: Rep[Thunk[Costed[A]]], thunkCost: Rep[Int]): Rep[Costed[Thunk[A]]] = CostedThunkCtor(costedBlock, thunkCost)
  // ---------------------------------------------------------

  object ConstantSizeType {
    def unapply(e: Elem[_]): Nullable[SType] = {
      val tpe = elemToSType(e)
      if (tpe.isConstantSize) Nullable(tpe)
      else Nullable.None
    }
  }

  override def sizeOf[T](value: Rep[T]): Rep[Long] = value.elem match {
    case ConstantSizeType(tpe) =>
      typeSize(tpe)
    case _ =>
      !!!(s"Cannot get sizeOf($value: ${value.elem})", value)
  }

  /** Graph node to represent computation of size for types with isConstantSize == true. */
  case class TypeSize(tpe: SType) extends BaseDef[Long] {
    assert(tpe.isConstantSize, s"Expected isConstantSize type but was TypeSize($tpe)")
  }

  def typeSize(tpe: SType): Rep[Long] = {
    assert(tpe.isConstantSize, {
      s"Expected constant size type but was $tpe"
    })
    val size = tpe.dataSize(SType.DummyValue)
    toRep(size)
  }

  def typeSize[T: Elem]: Rep[Long] = {
    val tpe = elemToSType(element[T])
    typeSize(tpe)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case CostOf(name, opType) => s"CostOf($name:$opType)"
    case GroupElementConst(p) => p.showToString
    case SigmaPropConst(sp) => sp.toString
    case ac: WArrayConst[_,_] =>
      val trimmed = ac.constValue.take(ac.constValue.length min 10)
      s"WArray(len=${ac.constValue.length}; ${trimmed.mkString(",")},...)"
    case _ => super.formatDef(d)
  }

  type RColl[T] = Rep[Coll[T]]
  type ROption[T] = Rep[WOption[T]]
  type RCostedColl[T] = Rep[CostedColl[T]]
  type RCostedOption[T] = Rep[CostedOption[T]]
  type RFuncCosted[A,B] = Rep[Costed[A] => Costed[B]]

  implicit class RFuncCostedOps[A,B](f: RFuncCosted[A,B]) {
    implicit val eA = f.elem.eDom.eVal
    /**NOTE: when removeIsValid == true the resulting type B may change from Boolean to SigmaProp
      * This should be kept in mind at call site */
    def sliceCalc(okRemoveIsProven: Boolean): Rep[A => Any] = {
      val _f = { x: Rep[A] => f(RCCostedPrim(x, 0, zeroSize(x.elem))).value }
      val res = if (okRemoveIsProven) fun(removeIsProven(_f)) else fun(_f)
      res
    }

    def sliceCalc: Rep[A => B] = fun { x: Rep[A] => f(RCCostedPrim(x, 0, zeroSize(x.elem))).value }

    def sliceCost: Rep[((Int,Size[A])) => Int] = fun { in: Rep[(Int, Size[A])] =>
      val Pair(c, s) = in
      f(RCCostedPrim(placeholder[A], c, s)).cost
    }

    def sliceCostEx: Rep[((A, (Int,Size[A]))) => Int] = fun { in: Rep[(A, (Int, Size[A]))] =>
      val Pair(ctx, Pair(c, s)) = in
      f(RCCostedPrim(ctx, c, s)).cost
    }

    def sliceSize: Rep[Size[A] => Size[B]] = fun { in: Rep[Size[A]] =>
      val s = in
      val arg = RCCostedPrim(placeholder[A], 0, s)
      f(arg).size
    }
  }

  type CostedCollFunc[A,B] = Costed[A] => CostedColl[B]
  type CostedOptionFunc[A,B] = Costed[A] => CostedOption[B]
  type RCostedCollFunc[A,B] = Rep[CostedCollFunc[A, B]]
  type RCostedOptionFunc[A,B] = Rep[CostedOptionFunc[A, B]]

  implicit class RCostedCollFuncOps[A,B](f: RCostedCollFunc[A,B]) {
    implicit val eA = f.elem.eDom.eVal
    def sliceValues: Rep[A => Coll[B]] = fun { x: Rep[A] => f(RCCostedPrim(x, 0, zeroSize(x.elem))).values }
    def sliceCosts: Rep[((Int,Size[A])) => (Coll[Int], Int)] = fun { in: Rep[(Int, Size[A])] =>
      val Pair(c, s) = in
      val colC = f(RCCostedPrim(placeholder[A], c, s))
      Pair(colC.costs, colC.valuesCost)
    }
    def sliceSizes: Rep[Size[A] => Coll[Size[B]]] = fun { in: Rep[Size[A]] =>
      val s = in
      f(RCCostedPrim(placeholder[A], 0, s)).sizes
    }
  }

//  implicit class RCostedOptionFuncOps[A,B](f: RCostedOptionFunc[A,B]) {
//    implicit val eA = f.elem.eDom.eVal
//    def sliceValues: Rep[A => WOption[B]] = fun { x: Rep[A] => f(RCCostedPrim(x, 0, zeroSizeData(x.elem))).values }
//    def sliceCosts: Rep[((A, (Int,Long))) => (Coll[Int], Int)] = fun { in: Rep[(A, (Int, Long))] =>
//      val Pair(x, Pair(c, s)) = in
//      val colC = f(RCCostedPrim(x, c, s))
//      Pair(colC.costs, colC.valuesCost)
//    }
//    def sliceSizes: Rep[((A, Long)) => Coll[Long]] = fun { in: Rep[(A, Long)] =>
//      val Pair(x, s) = in
//      f(RCCostedPrim(x, 0, s)).sizes
//    }
//  }

  implicit def extendCostedFuncElem[E,A,B](e: Elem[CostedFunc[E,A,B]]): CostedFuncElem[E,A,B,_] = e.asInstanceOf[CostedFuncElem[E,A,B,_]]

  implicit def extendSizeElem[A](elem: Elem[Size[A]]): SizeElem[A, Size[A]] =
    elem.asInstanceOf[SizeElem[A, Size[A]]]

  implicit def extendCostedElem[A](elem: Elem[Costed[A]]): CostedElem[A, Costed[A]] =
    elem.asInstanceOf[CostedElem[A, Costed[A]]]

  implicit def extendCostedCollElem[A](elem: Elem[CostedColl[A]]): CostedCollElem[A, CostedColl[A]] =
    elem.asInstanceOf[CostedCollElem[A, CostedColl[A]]]

  def splitCostedFunc2[A,B](f: RFuncCosted[A,B]): (Rep[A=>B], Rep[((Int, Size[A])) => Int]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc
    val costF = f.sliceCost
    (calcF, costF)
  }
  def splitCostedFunc2[A, B](f: RFuncCosted[A,B], okRemoveIsValid: Boolean): (Rep[A=>Any], Rep[((Int, Size[A])) => Int]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc(okRemoveIsValid)
    val costF = f.sliceCost
    (calcF, costF)
  }
  def splitCostedFunc[A,B](f: RFuncCosted[A,B]): (Rep[A=>B], Rep[((Int, Size[A])) => Int], Rep[Size[A] => Size[B]]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc
    val costF = f.sliceCost
    val sizeF = f.sliceSize
    (calcF, costF, sizeF)
  }

  def splitCostedCollFunc[A,B](f: RCostedCollFunc[A,B]): (Rep[A=>Coll[B]], Rep[((Int, Size[A])) => (Coll[Int], Int)], Rep[Size[A] => Coll[Size[B]]]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceValues
    val costF = f.sliceCosts
    val sizeF = f.sliceSizes
    (calcF, costF, sizeF)
  }

//  def splitCostedOptionFunc[A,B](f: RCostedOptionFunc[A,B]): (Rep[A=>WOption[B]], Rep[((A, (Int, Long))) => (WOption[Int], Int)], Rep[((A, Long)) => WOption[Long]]) = {
//    implicit val eA = f.elem.eDom.eVal
//    val calcF = f.sliceValues
//    val costF = f.sliceCosts
//    val sizeF = f.sliceSizes
//    (calcF, costF, sizeF)
//  }

  object CostedFoldExtractors {
    val CM = CostedMethods
    val COM = CostedOptionMethods
    val WOM = WOptionMethods
    type Result = (ROption[A], Th[B], RFunc[A, Costed[B]]) forSome {type A; type B}

//    object IsGetCost {
//      def unapply(d: Def[_]): Nullable[Result] = d match {
//        case CM.cost(COM.get(WOM.fold(opt, th, f))) =>
//          val res = (opt, th, f).asInstanceOf[Result]
//          Nullable(res)
//        case _ => Nullable.None
//      }
//    }
//    object IsGetDataSize {
//      def unapply(d: Def[_]): Nullable[Result] = d match {
//        case CM.dataSize(COM.get(WOM.fold(opt, th, f))) =>
//          val res = (opt, th, f).asInstanceOf[Result]
//          Nullable(res)
//        case _ => Nullable.None
//      }
//    }
//    object IsGet {
//      def unapply(d: Def[_]): Nullable[Result] = d match {
//        case COM.get(WOM.fold(opt, th, f)) =>
//          val res = (opt, th, f).asInstanceOf[Result]
//          Nullable(res)
//        case _ => Nullable.None
//      }
//    }
  }

  object IsConstSizeCostedColl {
    def unapply(d: Def[_]): Nullable[Rep[Costed[Coll[A]]] forSome {type A}] = d.selfType match {
      case ce: CostedElem[_,_] if !ce.isInstanceOf[CostedCollElem[_, _]] =>
        ce.eVal match {
          case colE: CollElem[a,_] if colE.eItem.isConstantSize =>
            val res = d.self.asInstanceOf[Rep[Costed[Coll[A]]] forSome {type A}]
            Nullable(res)
          case _ => Nullable.None
        }
      case _ => Nullable.None
    }
  }

  object IsCostedPair {
    def unapply(d: Def[_]): Nullable[Rep[Costed[(A, B)]] forSome {type A; type B}] = d.selfType match {
      case ce: CostedElem[_,_] if !ce.isInstanceOf[CostedPairElem[_, _, _]] =>
        ce.eVal match {
          case _: PairElem[a,b]  =>
            val res = d.self.asInstanceOf[Rep[Costed[(A, B)]] forSome {type A; type B}]
            Nullable(res)
          case _ => Nullable.None
        }
      case _ => Nullable.None
    }
  }

  implicit class ElemOpsForCosting(e: Elem[_]) {
    def isConstantSize: Boolean = e.sourceType.isConstantSize
  }

  type CostedTh[T] = Th[Costed[T]]

  class ElemAccessor[T](prop: Rep[T] => Elem[_]) {
    def unapply(s: Sym): Option[Elem[_]] = { val sR = asRep[T](s); Some(prop(sR)) }
  }
  object ElemAccessor {
    def apply[T](prop: Rep[T] => Elem[_]): ElemAccessor[T] = new ElemAccessor(prop)
  }

  val EValOfSizeColl = ElemAccessor[Coll[Size[Any]]](_.elem.eItem.eVal)

  override def rewriteDef[T](d: Def[T]): Rep[_] = {
    val CBM = CollBuilderMethods
    val SigmaM = SigmaPropMethods
    val CCM = CostedCollMethods
    val CostedM = CostedMethods
    val CostedOptionM = CostedOptionMethods
    val WOptionM = WOptionMethods
    val WArrayM = WArrayMethods
    val CM = CollMethods
    val CostedBuilderM = CostedBuilderMethods
    val SPCM = WSpecialPredefCompanionMethods
    val SDBM = SigmaDslBuilderMethods
    val SM = SizeMethods
    val SBM = SizeBoxMethods

    d match {
      // Rule: cast(eTo, x) if x.elem <:< eTo  ==>  x
      case Cast(eTo: Elem[to], x) if eTo.runtimeClass.isAssignableFrom(x.elem.runtimeClass) =>
        x

      case SM.dataSize(Def(CSizeCollCtor(CBM.replicate(_, n, s: RSize[a]@unchecked)))) => s.dataSize * n.toLong

      case SM.dataSize(Def(CSizeCollCtor(sizes @ EValOfSizeColl(eVal)))) if eVal.isConstantSize =>
        sizes.length.toLong * typeSize(eVal)

      case CM.map(CM.zip(CBM.replicate(_, n, x: Rep[a]), ys: RColl[b]@unchecked), _f) =>
        val f = asRep[((a,b)) => Any](_f)
        implicit val eb = ys.elem.eItem
        ys.map(fun { y: Rep[b] => f(Pair(x, y))})

      case WArrayM.length(Def(arrC: WArrayConst[_,_])) => arrC.constValue.length
      // Rule: l.isValid op Thunk {... root} => (l op TrivialSigma(root)).isValid
      case ApplyBinOpLazy(op, SigmaM.isValid(l), Def(ThunkDef(root, sch))) if root.elem == BooleanElement =>
        // don't need new Thunk because sigma logical ops always strict
        val r = asRep[SigmaProp](sigmaDslBuilder.sigmaProp(asRep[Boolean](root)))
        val res = if (op == And)
          l && r
        else
          l || r
        res.isValid

      // Rule: l op Thunk {... prop.isValid} => (TrivialSigma(l) op prop).isValid
      case ApplyBinOpLazy(op, l, Def(ThunkDef(root @ SigmaM.isValid(prop), sch))) if l.elem == BooleanElement =>
        val l1 = asRep[SigmaProp](sigmaDslBuilder.sigmaProp(asRep[Boolean](l)))
        // don't need new Thunk because sigma logical ops always strict
        val res = if (op == And)
          l1 && prop
        else
          l1 || prop
        res.isValid

      case SDBM.sigmaProp(_, SigmaM.isValid(p)) => p

      case CCM.foldCosted(xs: RCostedColl[a], zero: RCosted[b], _f) =>
        val f = asRep[Costed[(b,a)] => Costed[b]](_f)
        val (calcF/*: Rep[((b,a)) => b]*/,
             costF/*: Rep[((Int, Size[(b,a)])) => Int]*/,
             sizeF/*: Rep[Size[(b,a)] => Size[b]]*/) = splitCostedFunc[(b,a), b](f)

        val resV = xs.values.foldLeft(zero.value, calcF)

        implicit val eA: Elem[a] = xs.elem.eItem
        implicit val eB: Elem[b] = zero.elem.eVal
        val Pair(resS, resC) = xs.costs.zip(xs.sizes).foldLeft(
            Pair(zero.size, 0),
            fun { in: Rep[((Size[b], Int), (Int, Size[a]))] =>
              val Pair(Pair(accSizeB, accCost), Pair(xCost, xSize)) = in
              val sBA = RCSizePair(accSizeB, xSize)
              val res = Pair(sizeF(sBA), costF(Pair(xCost, sBA)))
              res
            }
        )
        RCCostedPrim(resV, resC, resS)
        
//      case CCM.filterCosted(xs: RCostedColl[a], _f: RCostedFunc[_,_]) =>
//        val f = asRep[Costed[a] => Costed[Boolean]](_f)
//        val (calcF, costF, _) = splitCostedFunc[a, Boolean](f)
//        val vals = xs.values.filter(calcF)
//        val costs = xs.costs.zip(xs.sizes).map(costF)  // TODO how to filter our sizes and costs
//        val sizes = colBuilder.replicate(xs.sizes.length, 1L)
//        RCostedColl(vals, costs, sizes, xs.valuesCost)


      case CostedM.cost(Def(CCostedCollCtor(_, costs, _, accCost))) => opCost(Seq(accCost), costs.sum(intPlusMonoid))
      case CostedM.cost(Def(CCostedOptionCtor(_, costOpt, _, accCost))) => opCost(Seq(accCost), costOpt.getOrElse(Thunk(0)))
      case CostedM.cost(Def(CCostedPairCtor(l, r, accCost))) => opCost(Seq(accCost), l.cost + r.cost)

      case CostedM.value(Def(CCostedFuncCtor(_, func: RFuncCosted[a,b], _,_))) =>
        func.sliceCalc


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

      // Rule: opt.fold(default, f).size ==> opt.fold(default.size, x => f(x).size)
      case CostedM.size(WOptionM.fold(opt, _th @ Def(ThunkDef(_, _)), _f)) =>
        implicit val eA: Elem[Any] = opt.elem.eItem.asElem[Any]
        val th = asRep[Thunk[Costed[Any]]](_th)
        val f = asRep[Any => Costed[Any]](_f)
        opt.fold(Thunk(forceThunkByMirror(th).size), fun { x: Rep[Any] => f(x).size })

      case CCostedPrimCtor(v, c, s) =>
        val res = v.elem.asInstanceOf[Elem[_]] match {
          case pe: PairElem[a,b] /*if s.elem.isInstanceOf[CSizePairElem[_,_]]*/ =>
            val p = asRep[(a,b)](v)
            costedPrimToPair(p, c, asRep[Size[(a,b)]](s))
          case ce: CollElem[a,_] /*if s.elem.isInstanceOf[CSizeCollElem[_]]*/ =>
            val col = asRep[Coll[a]](v)
            costedPrimToColl(col, c, asRep[Size[Coll[a]]](s))
          case oe: WOptionElem[a,_] /*if s.elem.isInstanceOf[CSizeOptionElem[_]]*/ =>
            val opt = asRep[WOption[a]](v)
            costedPrimToOption(opt, c, asRep[Size[WOption[a]]](s))
          case _ => super.rewriteDef(d)
        }
        res

//      case CostedBuilderM.costedValue(b, x, SPCM.some(cost)) =>
//        dataCost(x, Some(asRep[Int](cost)))

      case IsConstSizeCostedColl(col) if !d.isInstanceOf[MethodCall] => // see also rewriteNonInvokableMethodCall
        mkCostedColl(col.value, col.value.length, col.cost)

      case _ if isCostingProcess =>
        // apply special rules for costing function
        d match {
          case CM.length(Def(IfThenElseLazy(_, Def(ThunkDef(t: RColl[a]@unchecked,_)), Def(ThunkDef(_e,_)))))  =>
            val e = asRep[Coll[a]](_e)
            t.length max e.length
          case _ => super.rewriteDef(d)
        }
      case _ => super.rewriteDef(d)
    }
  }

  def costedPrimToColl[A](coll: Rep[Coll[A]], c: Rep[Int], s: RSize[Coll[A]]): RCostedColl[A] = s.elem.asInstanceOf[Any] match {
    case se: SizeElem[_,_] if se.eVal.isInstanceOf[CollElem[_,_]] =>
      val sizes = asSizeColl(s).sizes
      val costs = colBuilder.replicate(sizes.length, 0)
      mkCostedColl(coll, costs, sizes, c)
    case _ =>
      !!!(s"Expected Size[Coll[A]] node but was $s -> ${s.rhs}")
  }

  def costedPrimToOption[A](opt: Rep[WOption[A]], c: Rep[Int], s: RSize[WOption[A]]) = s.elem.asInstanceOf[Any] match {
    case se: SizeElem[_,_] if se.eVal.isInstanceOf[WOptionElem[_,_]] =>
      val sizeOpt = asSizeOption(s).sizeOpt
      mkCostedOption(opt, SOME(0), sizeOpt, c)
    case _ =>
      !!!(s"Expected RCSizeOption node but was $s -> ${s.rhs}")
  }

  def costedPrimToPair[A,B](p: Rep[(A,B)], c: Rep[Int], s: RSize[(A,B)]) = s.elem.asInstanceOf[Any] match {
    case se: SizeElem[_,_] if se.eVal.isInstanceOf[PairElem[_,_]] =>
      val sPair = asSizePair(s)
      RCCostedPair(RCCostedPrim(p._1, 0, sPair.l), RCCostedPrim(p._2, 0, sPair.r), c)
    case _ =>
      !!!(s"Expected RCSizePair node but was $s -> ${s.rhs}")
  }

  override def rewriteNonInvokableMethodCall(mc: MethodCall): Rep[_] = mc match {
    case IsConstSizeCostedColl(col: RCosted[Coll[Any]]@unchecked) =>
      costedPrimToColl(col.value, col.cost, asSizeColl(col.size))
    case IsCostedPair(p) =>
      val v = asRep[(Any,Any)](p.value)
      val c = p.cost
      val s = asRep[Size[(Any,Any)]](p.size)
      costedPrimToPair(v, c, asSizePair(s))
    case _ =>
      super.rewriteNonInvokableMethodCall(mc)
  }

  override def transformDef[A](d: Def[A], t: Transformer): Rep[A] = d match {
    case c: CostOf => c.self
    case _ => super.transformDef(d, t)
  }

  /** Should be specified in the final cake */
  val builder: sigmastate.lang.SigmaBuilder
  import builder._

  /** Lazy values, which are immutable, but can be reset, so that the next time they are accessed
    * the expression is re-evaluated. Each value should be reset in onReset() method. */
  private val _sigmaDslBuilder: LazyRep[SigmaDslBuilder] = MutableLazy(RTestSigmaDslBuilder())
  def sigmaDslBuilder: Rep[SigmaDslBuilder] = _sigmaDslBuilder.value

  private val _colBuilder: LazyRep[CollBuilder] = MutableLazy(sigmaDslBuilder.Colls)
  def colBuilder: Rep[CollBuilder] = _colBuilder.value

  private val _sizeBuilder: LazyRep[SizeBuilder] = MutableLazy(RCSizeBuilder())
  def sizeBuilder: Rep[SizeBuilder] = _sizeBuilder.value

  private val _costedBuilder: LazyRep[CostedBuilder] = MutableLazy(RCCostedBuilder())
  def costedBuilder: Rep[CostedBuilder] = _costedBuilder.value

  private val _monoidBuilder: LazyRep[MonoidBuilder] = MutableLazy(costedBuilder.monoidBuilder)
  def monoidBuilder: Rep[MonoidBuilder] = _monoidBuilder.value

  private val _intPlusMonoid: LazyRep[Monoid[Int]] = MutableLazy(monoidBuilder.intPlusMonoid)
  def intPlusMonoid: Rep[Monoid[Int]] = _intPlusMonoid.value

  private val _longPlusMonoid: LazyRep[Monoid[Long]] = MutableLazy(monoidBuilder.longPlusMonoid)
  def longPlusMonoid: Rep[Monoid[Long]] = _longPlusMonoid.value

  private val _costedGlobal: LazyRep[Costed[SigmaDslBuilder]] =
    MutableLazy(RCCostedPrim(sigmaDslBuilder, 0, costedBuilder.mkSizePrim(1L, sigmaDslBuilderElement)))
  def costedGlobal: RCosted[SigmaDslBuilder] = _costedGlobal.value

  protected override def onReset(): Unit = {
    super.onReset()
    // WARNING: every lazy value should be listed here, otherwise bevavior after resetContext is undefined and may throw.
    Seq(_sigmaDslBuilder, _colBuilder, _sizeBuilder, _costedBuilder,
        _monoidBuilder, _intPlusMonoid, _longPlusMonoid, _costedGlobal)
        .foreach(_.reset())
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

//  def costingOf[T,R](f: Rep[T => Costed[R]]): Rep[T] => Rep[Int] = { x: Rep[T] =>
//    funUnderCosting = f
//    val c = f(x).cost;
//    funUnderCosting = null
//    c
//  }

//  def sizingOf[T,R](f: Rep[T => Costed[R]]): Rep[T] => Rep[Long] = { x: Rep[T] =>
//    funUnderCosting = f
//    val c = f(x).dataSize;
//    funUnderCosting = null
//    c
//  }

//  def split2[T,R](f: Rep[((T, Size[T])) => Costed[R]]): Rep[(T => Any, T => Int)] = {
//    implicit val eT = f.elem.eDom
//    val calc = fun(removeIsProven { x: Rep[T] =>
//      val y = f(x);
//      y.value
//    })
//    val cost = fun(costingOf(f))
//    Pair(calc, cost)
//  }

//  def split3[T,R](f: Rep[T => Costed[R]]): Rep[(T => Any, (T => Int, T => Long))] = {
//    implicit val eT = f.elem.eDom
//    val calc = fun(removeIsProven { x: Rep[T] =>
//      val y = f(x);
//      y.value
//    })
//    val cost = fun(costingOf(f))
//    val size = fun(sizingOf(f))
//    Tuple(calc, cost, size)
//  }

  def stypeToElem[T <: SType](t: T): Elem[T#WrappedType] = (t match {
    case SBoolean => BooleanElement
    case SByte => ByteElement
    case SShort => ShortElement
    case SInt => IntElement
    case SLong => LongElement
    case SString => StringElement
    case SAny => AnyElement
    case SBigInt => bigIntElement
    case SBox => boxElement
    case SContext => contextElement
    case SGlobal => sigmaDslBuilderElement
    case SHeader => headerElement
    case SPreHeader => preHeaderElement
    case SGroupElement => groupElementElement
    case SAvlTree => avlTreeElement
    case SSigmaProp => sigmaPropElement
    case STuple(Seq(a, b)) => pairElement(stypeToElem(a), stypeToElem(b))
    case STuple(items) => tupleStructElement(items.map(stypeToElem(_)):_*)
    case c: SCollectionType[a] => collElement(stypeToElem(c.elemType))
    case o: SOption[a] => wOptionElement(stypeToElem(o.elemType))
    case SFunc(Seq(tpeArg), tpeRange, Nil) => funcElement(stypeToElem(tpeArg), stypeToElem(tpeRange))
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
    case _: BigIntElem[_] => SBigInt
    case _: GroupElementElem[_] => SGroupElement
    case _: AvlTreeElem[_] => SAvlTree
    case oe: WOptionElem[_, _] => sigmastate.SOption(elemToSType(oe.eItem))
    case _: BoxElem[_] => SBox
    case _: ContextElem[_] => SContext
    case _: SigmaDslBuilderElem[_] => SGlobal
    case _: HeaderElem[_] => SHeader
    case _: PreHeaderElem[_] => SPreHeader
    case _: SigmaPropElem[_] => SSigmaProp
    case se: StructElem[_] =>
      assert(se.fieldNames.zipWithIndex.forall { case (n,i) => n == s"_${i+1}" })
      STuple(se.fieldElems.map(elemToSType(_)).toIndexedSeq)
    case ce: CollElem[_, _] => SCollection(elemToSType(ce.eItem))
    case fe: FuncElem[_, _] => SFunc(elemToSType(fe.eDom), elemToSType(fe.eRange))
    case pe: PairElem[_, _] => STuple(elemToSType(pe.eFst), elemToSType(pe.eSnd))
    case _ => error(s"Don't know how to convert Elem $e to SType")
  }

  def rtypeToElem(t: RType[_]): Elem[_] = t match {
    case BooleanType => BooleanElement
    case ByteType => ByteElement
    case ShortType => ShortElement
    case IntType => IntElement
    case LongType => LongElement
    case StringType => StringElement
    case AnyType => AnyElement

    case BigIntegerRType => wBigIntegerElement
    case BigIntRType => bigIntElement

    case ECPointRType => wECPointElement
    case GroupElementRType => groupElementElement

    case AvlTreeRType => avlTreeElement
    case ot: OptionType[_] => wOptionElement(rtypeToElem(ot.tA))
    case BoxRType => boxElement
    case SigmaPropRType => sigmaPropElement
    case tup: TupleType => tupleStructElement(tup.items.map(t => rtypeToElem(t)):_*)
    case at:  ArrayType[_] => wArrayElement(rtypeToElem(at.tA))
    case ct:  CollType[_] => collElement(rtypeToElem(ct.tItem))
    case ft:  FuncType[_,_] => funcElement(rtypeToElem(ft.tDom), rtypeToElem(ft.tRange))
    case pt:  PairType[_,_] => pairElement(rtypeToElem(pt.tFst), rtypeToElem(pt.tSnd))
    case pvt: PrimViewType[_,_] => rtypeToElem(pvt.tVal)
    case _ => sys.error(s"Don't know how to convert RType $t to Elem")
  }


  /** For a given data type returns the corresponding specific descendant of CostedElem[T] */
  def elemToCostedElem[T](implicit e: Elem[T]): Elem[Costed[T]] = (e match {
    case oe: WOptionElem[a,_] => costedOptionElement(oe.eItem)
    case ce: CollElem[a,_] => costedCollElement(ce.eItem)
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
    case e: BigIntElem[_] => LiftableBigInt
    case e: GroupElementElem[_] => LiftableGroupElement
    case ae: WArrayElem[t,_] =>
      implicit val lt = liftableFromElem[t](ae.eItem)
      liftableArray(lt)
    case ce: CollElem[t,_] =>
      implicit val lt = liftableFromElem[t](ce.eItem)
      liftableColl(lt)
    case pe: PairElem[a,b] =>
      implicit val la = liftableFromElem[a](pe.eFst)
      implicit val lb = liftableFromElem[b](pe.eSnd)
      PairIsLiftable(la, lb)
    case pe: FuncElem[a,b] =>
      implicit val la = liftableFromElem[a](pe.eDom)
      implicit val lb = liftableFromElem[b](pe.eRange)
      FuncIsLiftable(la, lb)
  }).asInstanceOf[Liftable[_,WT]]

  import NumericOps._
  private lazy val elemToNumericMap = Map[Elem[_], Numeric[_]](
    (ByteElement, numeric[Byte]),
    (ShortElement, numeric[Short]),
    (IntElement, numeric[Int]),
    (LongElement, numeric[Long]),
    (bigIntElement, numeric[SBigInt])
  )
  private lazy val elemToIntegralMap = Map[Elem[_], Integral[_]](
    (ByteElement, integral[Byte]),
    (ShortElement, integral[Short]),
    (IntElement, integral[Int]),
    (LongElement, integral[Long]),
    (bigIntElement, integral[SBigInt])
  )
  private lazy val elemToOrderingMap = Map[Elem[_], Ordering[_]](
    (ByteElement, implicitly[Ordering[Byte]]),
    (ShortElement, implicitly[Ordering[Short]]),
    (IntElement, implicitly[Ordering[Int]]),
    (LongElement, implicitly[Ordering[Long]]),
    (bigIntElement, implicitly[Ordering[SBigInt]])
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
      costed.size
    }
    RCCostedPrim(v, c, s)
  }

  def adaptSigmaBoolean(v: BoolValue) = v match {
    case sb: SigmaBoolean => sb.isProven
    case _ => v
  }

    /** Helper to create costed collection of some constant size type T */
  def mkCostedColl[T](col: Rep[Coll[T]], len: Rep[Int], cost: Rep[Int]): Rep[CostedColl[T]] = {
    // TODO optimization: the method should be specialized on T so that mkSizePrim is not used
    val eT = col.elem.eItem
    val costs = colBuilder.replicate(len, 0)
    val sizes = colBuilder.replicate(len, costedBuilder.mkSizePrim(typeSize(eT), eT): RSize[T])
    RCCostedColl(col, costs, sizes, cost)
  }
//  def mkCostedColl[T](col: Rep[Coll[T]], cost: Rep[Int]): Rep[CostedColl[T]] = {
//    mkCostedColl(col, col.length, cost)
//  }

//  def mkCosted[T](v: Rep[T], cost: Rep[Int], size: Rep[Long]): Rep[Costed[T]] = {
//    val res = v.elem match {
//      case colE: CollElem[a,_] =>
//        val xs = asRep[Coll[a]](v)
//        costedPrimToColl(xs, cost, size)
//      case _ =>
//        RCCostedPrim(v, cost, size)
//    }
//    asRep[Costed[T]](res)
//  }

//  def mkCostedOption[T](opt: Rep[WOption[T]], sizeOpt: Rep[WOption[Long]], cost: Rep[Int]): Rep[CostedOption[T]] = {
//    val costOpt = RWSpecialPredef.some(0)
//    RCCostedOption(opt, costOpt, sizeOpt, cost)
//  }

  @inline final def asCosted[T](x: Rep[_]): Rep[Costed[T]] = x.asInstanceOf[Rep[Costed[T]]]

  type CostingEnv = Map[Any, RCosted[_]]

  import sigmastate._

  val OperationIdKey = MetaKey[AnyRef]("OperationId")(AnyRefElement)

  protected def isOperationNode(v: SValue): Boolean = v match {
    case _: Block | _: BlockValue | _: TaggedVariableNode[_] | _: ValNode | _: ValDef | _: ValUse[_] | _: FuncValue => false
    case _ => true
  }

  protected def onTreeNodeCosted[T <: SType](
        ctx: RCosted[Context], env: CostingEnv,
        node: Value[T], costed: RCosted[T#WrappedType]): Unit = {
    if (okMeasureOperationTime && isOperationNode(node)) {
      asRep[Any](costed) match {
        case Def(CCostedPrimCtor(v, c, s)) =>
          v.setMetadata(OperationIdKey)(node.opId)
        case Def(CCostedCollCtor(vs,_,_,_)) =>
          vs.setMetadata(OperationIdKey)(node.opId)
        case _ =>
      }
    }
  }

  @inline def SigmaDsl = sigmaDslBuilderValue
  @inline def Colls = sigmaDslBuilderValue.Colls

  protected implicit def groupElementToECPoint(g: special.sigma.GroupElement): EcPointType = SigmaDsl.toECPoint(g).asInstanceOf[EcPointType]

  def constantTypeSize[T](implicit eT: Elem[T]): RSize[T] = RCSizePrim(typeSize(eT), eT)

  def withConstantSize[T](v: Rep[T], cost: Rep[Int]): RCosted[T] = RCCostedPrim(v, cost, constantTypeSize(v.elem))

  def sizeOfData[ST,T](x: ST)(implicit lT: Liftable[ST,T]): RSize[T] = asRep[Size[T]](lT.sourceType match {
    case BooleanType => liftConst(Sized.sizeOf(x.asInstanceOf[Boolean]))
    case ByteType => liftConst(Sized.sizeOf(x.asInstanceOf[Byte]))
    case ShortType => liftConst(Sized.sizeOf(x.asInstanceOf[Short]))
    case IntType => liftConst(Sized.sizeOf(x.asInstanceOf[Int]))
    case LongType => liftConst(Sized.sizeOf(x.asInstanceOf[Long]))
    case BigIntRType => liftConst(Sized.sizeOf(x.asInstanceOf[SBigInt]))
    case GroupElementRType => liftConst(Sized.sizeOf(x.asInstanceOf[SGroupElement]))
    case AvlTreeRType => liftConst(Sized.sizeOf(x.asInstanceOf[SAvlTree]))
    case SigmaPropRType => liftConst(Sized.sizeOf(x.asInstanceOf[SSigmaProp]))
    case BoxRType => liftConst(Sized.sizeOf(x.asInstanceOf[SBox]))
    case special.sigma.HeaderRType => liftConst(Sized.sizeOf(x.asInstanceOf[SHeader]))
    case special.sigma.PreHeaderRType => liftConst(Sized.sizeOf(x.asInstanceOf[SPreHeader]))
    case ct: CollType[a] =>
      implicit val tA = ct.tItem
      implicit val sizedA = Sized.typeToSized(tA)
      liftConst(Sized.sizeOf(x.asInstanceOf[special.collection.Coll[a]]))
    case ct: OptionType[a] =>
      implicit val tA = ct.tA
      implicit val sizedA = Sized.typeToSized(tA)
      liftConst(Sized.sizeOf(x.asInstanceOf[Option[a]]))
    case ct: PairType[a, b] =>
      implicit val tA = ct.tFst
      implicit val tB = ct.tSnd
      implicit val sizedA = Sized.typeToSized(tA)
      implicit val sizedB = Sized.typeToSized(tB)
      liftConst(Sized.sizeOf(x.asInstanceOf[(a,b)]))
  })

  /** Build a new costed value with the given cost in a dependency list.
    * This is required to correctly handle tuple field accesses like `v._1`
    * and not to lose the cost of `v` in the cost of resulting value. */
  def attachCost[T](source: RCosted[T], accCost: Rep[Int], cost: Rep[Int]): RCosted[T] = asRep[Costed[T]] {
    def newCost(v: Sym, c: Rep[Int]) = opCost(v, Seq(accCost, c), cost) // put cost in dependency list

    source.elem.eVal match {
      case e: CollElem[a, _] =>
        val xsC = asCostedColl[a](asCosted[Coll[a]](source))
        val v = xsC.values
        val c = xsC.cost
        RCCostedColl(v, xsC.costs, xsC.sizes, newCost(v, c))
      case e: PairElem[a,b] =>
        val pC = asCostedPair[a,b](asCosted[(a,b)](source))
        RCCostedPair(pC.l, pC.r, newCost(pC, pC.cost))
      case e =>
        val c = source.cost  // this is a current cost of the value
        val v = source.value
        RCCostedPrim(v, newCost(v, c), source.size)
    }
  }

  protected def evalNode[T <: SType](ctx: RCosted[Context], env: CostingEnv, node: Value[T]): RCosted[T#WrappedType] = {
    import WOption._
    def eval[T <: SType](node: Value[T]): RCosted[T#WrappedType] = evalNode(ctx, env, node)
    object In { def unapply(v: SValue): Nullable[RCosted[Any]] = Nullable(asRep[Costed[Any]](evalNode(ctx, env, v))) }
    class InColl[T: Elem] { def unapply(v: SValue): Nullable[Rep[CostedColl[T]]] = Nullable(tryCast[CostedColl[T]](evalNode(ctx, env, v))) }
    val InCollByte = new InColl[Byte]; val InCollAny = new InColl[Any]()(AnyElement); val InCollInt = new InColl[Int]

    val InCollCollByte = new InColl[Coll[Byte]]
    val InPairCollByte = new InColl[(Coll[Byte], Coll[Byte])]

    object InSeq { def unapply(items: Seq[SValue]): Nullable[Seq[RCosted[Any]]] = {
      val res = items.map { x: SValue =>
        val xC = eval(x)
        asRep[Costed[Any]](xC)
      }
      Nullable(res)
    }}
    object InSeqUnzipped { def unapply(items: Seq[SValue]): Nullable[(Seq[Rep[Any]], Seq[Rep[Int]], Seq[RSize[Any]])] = {
      val res = items.mapUnzip { x: SValue =>
        val xC = eval(x)
        (asRep[Any](xC.value), xC.cost, asRep[Size[Any]](xC.size))
      }
      Nullable(res)
    }}
    val res: Rep[Any] = node match {
      case TaggedVariableNode(id, _) =>
        env.getOrElse(id, !!!(s"TaggedVariable $id not found in environment $env"))

      case c @ Constant(v, tpe) => v match {
        case p: SSigmaProp =>
          assert(tpe == SSigmaProp)
          val resV = liftConst(p)
          RCCostedPrim(resV, costOfSigmaTree(p), SizeOfSigmaProp(p))
        case bi: SBigInt =>
          assert(tpe == SBigInt)
          val resV = liftConst(bi)
          withConstantSize(resV, costOf(c))
        case p: SGroupElement =>
          assert(tpe == SGroupElement)
          val resV = liftConst(p)
//          val size = SGroupElement.dataSize(ge.asWrappedType)
          withConstantSize(resV, costOf(c))
        case coll: SColl[a] =>
          val tpeA = tpe.asCollection[SType].elemType
          stypeToElem(tpeA) match {
            case eWA: Elem[wa] =>
              implicit val l = liftableFromElem[wa](eWA).asInstanceOf[Liftable[a,wa]]
              val resVals = liftConst[SColl[a], Coll[wa]](coll)
              val resCosts = liftConst(Colls.replicate(coll.length, 0))
              val resSizes =
                if (tpeA.isConstantSize)
                  colBuilder.replicate(coll.length, constantTypeSize(eWA))
                else {
                  implicit val ewa = eWA
                  tryCast[SizeColl[wa]](sizeOfData[SColl[a], Coll[wa]](coll)).sizes
                }
              RCCostedColl(resVals, resCosts, resSizes, costOf(c))
          }
        case box: SBox =>
          val boxV = liftConst(box)
          RCCostedPrim(boxV, costOf(c), sizeOfData(box))
        case tree: special.sigma.AvlTree =>
          val treeV = liftConst(tree)
          RCCostedPrim(treeV, costOf(c), SizeAvlTree)
        case s: String =>
          val resV = toRep(s)(stypeToElem(tpe).asElem[String])
          RCCostedPrim(resV, costOf(c), SizeString)
        case _ =>
          val resV = toRep(v)(stypeToElem(tpe))
          withConstantSize(resV, costOf(c))
      }

      case org.ergoplatform.Context => ctx
      case Global => costedGlobal
      case Height  => ContextCoster(ctx, SContext.heightMethod, Nil)
      case Inputs  => ContextCoster(ctx, SContext.inputsMethod, Nil)
      case Outputs => ContextCoster(ctx, SContext.outputsMethod, Nil)
      case Self    => ContextCoster(ctx, SContext.selfMethod, Nil)
      case LastBlockUtxoRootHash => ContextCoster(ctx, SContext.lastBlockUtxoRootHashMethod, Nil)
      case MinerPubkey => ContextCoster(ctx, SContext.minerPubKeyMethod, Nil)

      case op @ GetVar(id, optTpe) =>
        stypeToElem(optTpe.elemType) match { case e: Elem[t] =>
          val v = ctx.value.getVar[t](id)(e)
          val s = tryCast[SizeContext](ctx.size).getVar(id)(e)
          RCCostedPrim(v, opCost(v, Nil, sigmaDslBuilder.CostModel.GetVar), s)
        }

      case Terms.Block(binds, res) =>
        var curEnv = env
        for (v @ Val(n, _, b) <- binds) {
          if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}", v.sourceContext.toOption)
          val bC = evalNode(ctx, curEnv, b)
          curEnv = curEnv + (n -> bC)
        }
        val res1 = evalNode(ctx, curEnv, res)
        res1

      case BlockValue(binds, res) =>
        var curEnv = env
        for (vd @ ValDef(n, _, b) <- binds) {
          if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}", vd.sourceContext.toOption)
          val bC = evalNode(ctx, curEnv, b)
          curEnv = curEnv + (n -> bC)
        }
        val res1 = evalNode(ctx, curEnv, res)
        res1

      case ValUse(valId, _) =>
        env.getOrElse(valId, !!!(s"ValUse $valId not found in environment $env"))

      case CreateProveDlog(In(_v)) =>
        val vC = asRep[Costed[GroupElement]](_v)
        val resV: Rep[SigmaProp] = sigmaDslBuilder.proveDlog(vC.value)
        val cost = opCost(Seq(vC.cost), costOfDHTuple)
        RCCostedPrim(resV, cost, mkSizeSigmaProp(vC.size.dataSize))

      case CreateProveDHTuple(In(_gv), In(_hv), In(_uv), In(_vv)) =>
        val gvC = asRep[Costed[GroupElement]](_gv)
        val hvC = asRep[Costed[GroupElement]](_hv)
        val uvC = asRep[Costed[GroupElement]](_uv)
        val vvC = asRep[Costed[GroupElement]](_vv)
        val resV: Rep[SigmaProp] = sigmaDslBuilder.proveDHTuple(gvC.value, hvC.value, uvC.value, vvC.value)
        val cost = opCost(Seq(gvC.cost, hvC.cost, uvC.cost, vvC.cost), costOfDHTuple)
        RCCostedPrim(resV, cost, mkSizeSigmaProp(gvC.size.dataSize * 4L))

      case sigmastate.Exponentiate(In(_l), In(_r)) =>
        val l = asRep[Costed[GroupElement]](_l)
        val r = asRep[Costed[BigInt]](_r)
        val value = l.value.exp(r.value)
        val cost = opCost(Seq(l.cost, r.cost), costOf(node))
        RCCostedPrim(value, cost, SizeGroupElement)

      case sigmastate.MultiplyGroup(In(_l), In(_r)) =>
        val l = asRep[Costed[GroupElement]](_l)
        val r = asRep[Costed[GroupElement]](_r)
        val value = l.value.multiply(r.value)
        val cost = opCost(Seq(l.cost, r.cost), costOf(node))
        RCCostedPrim(value, cost, SizeGroupElement)

      case Values.GroupGenerator =>
        SigmaDslBuilderCoster(costedGlobal, SGlobal.groupGeneratorMethod, Nil)

      case sigmastate.ByteArrayToBigInt(In(_arr)) =>
        val arrC = asRep[Costed[Coll[Byte]]](_arr)
        val arr = arrC.value
        val value = sigmaDslBuilder.byteArrayToBigInt(arr)
        val size = arrC.size.dataSize
        val cost = opCost(Seq(arrC.cost), costOf(node) + costOf("new_BigInteger_per_item", node.opType) * size.toInt)
        RCCostedPrim(value, cost, SizeBigInt)

      case sigmastate.LongToByteArray(In(_x)) =>
        val xC = asRep[Costed[Long]](_x)
        val col = sigmaDslBuilder.longToByteArray(xC.value) // below we assume col.length == typeSize[Long]
        val cost = opCost(Seq(xC.cost), costOf(node))
        val len = SizeLong.dataSize.toInt
        mkCostedColl(col, len, cost)

      // opt.get =>
      case utxo.OptionGet(In(_opt)) =>
        OptionCoster(_opt, SOption.GetMethod, Nil)

      // opt.isDefined =>
      case utxo.OptionIsDefined(In(_opt)) =>
        OptionCoster(_opt, SOption.IsDefinedMethod, Nil)

      // opt.getOrElse =>
      case utxo.OptionGetOrElse(In(_opt), In(_default)) =>
        OptionCoster(_opt, SOption.GetOrElseMethod, Seq(_default))

      case SelectField(In(_tup), fieldIndex) =>
        _tup.elem.eVal.asInstanceOf[Elem[_]] match {
          case se: StructElem[_] =>
            val tup = asRep[Costed[Struct]](_tup)
            val fn = STuple.componentNameByIndex(fieldIndex - 1)
            val v = tup.value.getUntyped(fn)
            val c = opCost(Seq(tup.cost), costedBuilder.SelectFieldCost)
            val s: RSize[Any] = ??? // TODO implement similar to Pair case
            RCCostedPrim(v, c, s)
          case pe: PairElem[a,b] =>
            assert(fieldIndex == 1 || fieldIndex == 2, s"Invalid field index $fieldIndex of the pair ${_tup}: $pe")
            implicit val ea = pe.eFst
            implicit val eb = pe.eSnd
            val pair = tryCast[CostedPair[a,b]](_tup)
            val res = if (fieldIndex == 1)
              attachCost(pair.l, pair.accCost, selectFieldCost)
            else
              attachCost(pair.r, pair.accCost, selectFieldCost)
            res
        }

      case Values.Tuple(InSeq(Seq(x, y))) =>
        RCCostedPair(x, y, opCost(Seq(x.cost, y.cost), CostTable.newPairValueCost))

      case Values.Tuple(InSeq(items)) =>
        val fields = items.zipWithIndex.map { case (x, i) => (s"_${i+1}", x)}
        val cost = opCost(items.map(_.cost), costedBuilder.ConstructTupleCost)
        RCostedStruct(struct(fields), cost)

      case node: BooleanTransformer[_] =>
        val eIn = stypeToElem(node.input.tpe.elemType)
        val xs = asRep[CostedColl[Any]](eval(node.input))
        val eAny = xs.elem.asInstanceOf[CostedElem[Coll[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        implicit val eArg: Elem[Costed[Any]] = costedElement(eAny)
        val conditionC = asRep[CostedFunc[Unit, Any, SType#WrappedType]](evalNode(ctx, env, node.condition))
        val condC = conditionC.func
        val (calcF, costF) = splitCostedFunc2(condC, okRemoveIsValid = true)
        val sizeF = condC.sliceSize
        val values = xs.values.map(calcF)
//        val mRes = AllMarking(element[Int])
//        val mCostF = sliceAnalyzer.analyzeFunc(costF, mRes)
//        val cost = mCostF.mDom match {
//          case PairMarking(markA,_) if markA.isEmpty => // no dependency on values
//            val slicedCostF = fun { in: Rep[(Int, Long)] => costF(Pair(variable[Any](Lazy(eAny)), in)) }
        val cost = xs.costs.zip(xs.sizes).map(costF).sum(intPlusMonoid)
//          case _ =>
//            xs.values.zip(xs.costs.zip(xs.sizes)).map(costF).sum(intPlusMonoid)
//        }
        val res = calcF.elem.eRange match {
          case e if e == BooleanElement =>
            node match {
              case _: ForAll[_] =>
                val value = xs.values.forall(asRep[Any => Boolean](calcF))
                withConstantSize(value, cost)
              case _: Exists[_] =>
                val value = xs.values.exists(asRep[Any => Boolean](calcF))
                withConstantSize(value, cost)
            }
          case _: SigmaPropElem[_] =>
            node match {
              case _: ForAll[_] =>
                val children = asRep[Coll[SigmaProp]](values)
                val size = mkSizeSigmaProp(CryptoConstants.EncodedGroupElementLength.toLong)
                RCCostedPrim(sigmaDslBuilder.allZK(children), cost, size)
              case _: Exists[_] =>
                val children = asRep[Coll[SigmaProp]](values)
                val size = mkSizeSigmaProp(CryptoConstants.EncodedGroupElementLength.toLong)
                RCCostedPrim(sigmaDslBuilder.anyZK(children), cost, size)
            }
        }
        res

      case MapCollection(input, sfunc) =>
        val inputC = evalNode(ctx, env, input)
        val mapper = evalNode(ctx, env, sfunc)
        val res = CollCoster(inputC, SCollection.MapMethod, Seq(mapper))
        res

      case Fold(input, zero, sfunc) =>
        val eItem = stypeToElem(input.tpe.elemType)
        val eState = stypeToElem(zero.tpe)
        (eState, eItem) match { case (eState: Elem[s], eItem: Elem[a]) =>
          val inputC = asRep[CostedColl[a]](eval(input))
          implicit val eA = inputC.elem.asInstanceOf[CostedElem[Coll[a],_]].eVal.eA
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
        val inputC = asRep[CostedColl[Any]](input)
        val fromC = asRep[Costed[Int]](from)
        val untilC = asRep[Costed[Int]](until)
        val f = fromC.value
        val u = untilC.value
        val vals = inputC.values.slice(f, u)
        val costs = inputC.costs
        val sizes = inputC.sizes
        RCCostedColl(vals, costs, sizes, opCost(Seq(inputC.valuesCost), costOf(op)))

      case Append(In(_col1), In(_col2)) =>
        val col1 = asRep[CostedColl[Any]](_col1)
        val col2 = asRep[CostedColl[Any]](_col2)
        val values = col1.values.append(col2.values)
        val costs = col1.costs.append(col2.costs)
        val sizes = col1.sizes.append(col2.sizes)
        RCCostedColl(values, costs, sizes, opCost(Seq(col1.cost, col2.cost), costOf(node)))

      case Filter(input, p) =>
        val inputC = evalNode(ctx, env, input)
        val pC = evalNode(ctx, env, p)
        val res = CollCoster(inputC, SCollection.FilterMethod, Seq(pC))
        res


//      case Terms.Apply(Select(col,"fold", _), Seq(zero, Terms.Lambda(Seq((zeroArg, tZero), (opArg, tOp)), _, Some(body)))) =>
//        val taggedZero = mkTaggedVariable(21, tZero)
//        val taggedOp = mkTaggedVariable(22, tOp)
//        val env1 = env ++ Seq(zeroArg -> taggedZero, opArg -> taggedOp)
//        some(mkFold(col.asValue[SCollection[SType]], taggedZero.varId, zero, taggedOp.varId, body))(env1)


      case Terms.Apply(f, Seq(x)) if f.tpe.isFunc =>
        val fC = asRep[CostedFunc[Unit, Any, Any]](evalNode(ctx, env, f))
        val xC = asRep[Costed[Any]](evalNode(ctx, env, x))
        f.tpe.asFunc.tRange match {
          case _: SCollectionType[_] =>
            val (calcF, costF, sizeF) = splitCostedCollFunc(asRep[CostedCollFunc[Any,Any]](fC.func))
            val value = xC.value
            val values: RColl[Any] = Apply(calcF, value, false)
            val costRes: Rep[(Coll[Int], Int)] = Apply(costF, Pair(xC.cost, xC.size), false)
            val sizes: RColl[Size[Any]] = Apply(sizeF, xC.size, false)
            RCCostedColl(values, costRes._1, sizes, costRes._2)
//          case optTpe: SOption[_] =>
//            val (calcF, costF, sizeF) = splitCostedOptionFunc(asRep[CostedOptionFunc[Any,Any]](fC.func))
//            val value = xC.value
//            val values: Rep[WOption[Any]] = Apply(calcF, value, false)
//            val costRes: Rep[(WOption[Int], Int)] = Apply(costF, Pair(value, Pair(xC.cost, xC.dataSize)), false)
//            val sizes: Rep[WOption[Long]]= Apply(sizeF, Pair(value, xC.dataSize), false)
//            RCCostedOption(values, costRes._1, sizes, costRes._2)
          case _ =>
            val calcF = fC.sliceCalc
            val costF = fC.sliceCost
            val sizeF = fC.sliceSize
            val value = xC.value
            val y: Rep[Any] = Apply(calcF, value, false)
            val c: Rep[Int] = opCost(Seq(fC.cost, xC.cost), Apply(costF, Pair(xC.cost, xC.size), false))
            val s: Rep[Size[Any]]= Apply(sizeF, xC.size, false)
            RCCostedPrim(y, c, s)
        }

      case opt: OptionValue[_] =>
        error(s"Option constructors are not supported: $opt", opt.sourceContext.toOption)

      case CalcBlake2b256(In(input)) =>
        val bytesC = asRep[Costed[Coll[Byte]]](input)
        val res = sigmaDslBuilder.blake2b256(bytesC.value)
        val cost = opCost(Seq(bytesC.cost), perKbCostOf(node, bytesC.size.dataSize))
        mkCostedColl(res, Blake2b256.DigestSize, cost)
      case CalcSha256(In(input)) =>
        val bytesC = asRep[Costed[Coll[Byte]]](input)
        val res = sigmaDslBuilder.sha256(bytesC.value)
        val cost = opCost(Seq(bytesC.cost), perKbCostOf(node, bytesC.size.dataSize))
        mkCostedColl(res, Sha256.DigestSize, cost)

      case utxo.SizeOf(In(xs)) =>
        xs.elem.eVal.asInstanceOf[Any] match {
          case ce: CollElem[a,_] =>
            val xsC = asRep[Costed[Coll[a]]](xs)
            val v = xsC.value.length
            RCCostedPrim(v, opCost(Seq(xsC.cost), costOf(node)), SizeInt)
          case se: StructElem[_] =>
            val xsC = asRep[Costed[Struct]](xs)
            RCCostedPrim(se.fields.length, opCost(Seq(xsC.cost), costOf(node)), SizeInt)
          case pe: PairElem[a,b] =>
            val xsC = asRep[Costed[(a,b)]](xs)
            RCCostedPrim(2, opCost(Seq(xsC.cost), costOf(node)), SizeInt)
        }

      case ByIndex(xs, i, default) =>
        val xsC = asRep[CostedColl[Any]](eval(xs))
        val iC = asRep[Costed[Int]](eval(i))
        val iV = iC.value
        val size = xsC.sizes(iV)  // TO
        default match {
          case Some(defaultValue) =>
            val defaultC = asRep[Costed[Any]](eval(defaultValue))
            val default = defaultC.value
            val value = xsC.value.getOrElse(iV, default)
            val cost = opCost(Seq(xsC.cost, iC.cost, defaultC.cost), costOf(node))
            RCCostedPrim(value, cost, size)
          case None =>
            RCCostedPrim(xsC.value(iV), opCost(Seq(xsC.cost, iC.cost), costOf(node)), size)
        }

      case SigmaPropIsProven(p) =>
        val pC = asRep[Costed[SigmaProp]](eval(p))
        val v = pC.value.isValid
        val c = opCost(Seq(pC.cost), costOf(node))
//        val s = pC.size // NOTE: we pass SigmaProp's size, this is handled in buildCostedGraph
        RCCostedPrim(v, c, SizeBoolean)
      case SigmaPropBytes(p) =>
        val pC = asRep[Costed[SigmaProp]](eval(p))
        val v = pC.value.propBytes
        mkCostedColl(v, pC.size.dataSize.toInt, opCost(Seq(pC.cost), costOf(node)))

      case utxo.ExtractId(In(box)) =>  // TODO costing: use special CostedCollFixed for fixed-size collections
        val boxC = asRep[Costed[Box]](box)
        val id = boxC.value.id
        mkCostedColl(id, Blake2b256.DigestSize, opCost(Seq(boxC.cost), costOf(node)))
      case utxo.ExtractBytesWithNoRef(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val sBox = tryCast[SizeBox](boxC.size)
        mkCostedColl(boxC.value.bytesWithoutRef, sBox.bytesWithoutRef.dataSize.toInt, opCost(Seq(boxC.cost), costOf(node)))
      case utxo.ExtractAmount(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        withConstantSize(boxC.value.value, opCost(Seq(boxC.cost), costOf(node)))
      case utxo.ExtractScriptBytes(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val sBox = tryCast[SizeBox](boxC.size)
        val bytes = boxC.value.propositionBytes
        mkCostedColl(bytes, sBox.propositionBytes.dataSize.toInt, opCost(Seq(boxC.cost), costOf(node)))
      case utxo.ExtractBytes(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val sBox = tryCast[SizeBox](boxC.size)
        val bytes = boxC.value.bytes
        mkCostedColl(bytes, sBox.bytes.dataSize.toInt, opCost(Seq(boxC.cost), costOf(node)))
      case utxo.ExtractCreationInfo(In(box)) =>
        BoxCoster(box, SBox.creationInfoMethod, Nil)
      case utxo.ExtractRegisterAs(In(box), regId, optTpe) =>
        implicit val elem = stypeToElem(optTpe.elemType).asElem[Any]
        val i: RCosted[Int] = RCCostedPrim(regId.number.toInt, 0, SizeInt)
        BoxCoster(box, SBox.getRegMethod, Seq(i), Seq(liftElem(elem)))

      case BoolToSigmaProp(bool) =>
        val boolC = eval(bool)
        val value = sigmaDslBuilder.sigmaProp(boolC.value)
        RCCostedPrim(value, opCost(Seq(boolC.cost), costOf(node)), mkSizeSigmaProp(1L))

      case AtLeast(bound, input) =>
        val inputC = asRep[CostedColl[SigmaProp]](evalNode(ctx, env, input))
        if (inputC.values.length.isConst) {
          val inputCount = inputC.values.length.asValue
          if (inputCount > AtLeast.MaxChildrenCount)
            error(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: $inputCount", node.sourceContext.toOption)
        }
        val boundC = eval(bound)
        val res = sigmaDslBuilder.atLeast(boundC.value, inputC.values)
        val cost = opCost(Seq(boundC.cost, inputC.cost), costOf(node))
        val sInput = tryCast[SizeColl[SigmaProp]](inputC.size)
        RCCostedPrim(res, cost, mkSizeSigmaProp(sInput.dataSize))

      case op: ArithOp[t] if op.tpe == SBigInt =>
        import OpCodes._
        val xC = asRep[Costed[BigInt]](eval(op.left))
        val yC = asRep[Costed[BigInt]](eval(op.right))
        val opName = op.opName
        var v: Rep[BigInt] = null;
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
          case code => error(s"Cannot perform Costing.evalNode($op): unknown opCode ${code}", op.sourceContext.toOption)
        }
        val c = opCost(Seq(xC.cost, yC.cost), costOf(op))
        RCCostedPrim(v, c, SizeBigInt)

      case op: ArithOp[t] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToEndoBinOp(op.opCode, et)
        val x = evalNode(ctx, env, op.left)
        val y = evalNode(ctx, env, op.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          withConstantSize(ApplyBinOp(binop, x.value, y.value), opCost(Seq(x.cost, y.cost), costOf(op)))
        }

      case LogicalNot(input) =>
        val inputC = evalNode(ctx, env, input)
        withConstantSize(ApplyUnOp(Not, inputC.value), opCost(Seq(inputC.cost), costOf(node)))

      case ModQ(input) =>
        val inputC = asRep[Costed[BigInt]](eval(input))
        val v = inputC.value.modQ
        RCCostedPrim(v, opCost(Seq(inputC.cost), costOf(node)), SizeBigInt)

      case OR(input) => input match {
        case ConcreteCollection(items, tpe) =>
          val itemsC = items.map(item => eval(adaptSigmaBoolean(item)))
          val res = sigmaDslBuilder.anyOf(colBuilder.fromItems(itemsC.map(_.value): _*))
          val costs = itemsC.map(_.cost)
          val nOps = costs.length - 1
          val cost = opCost(costs, perItemCostOf(node, nOps))
          withConstantSize(res, cost)
        case _ =>
          val inputC = asRep[CostedColl[Boolean]](eval(input))
          val res = sigmaDslBuilder.anyOf(inputC.value)
          val nOps = inputC.sizes.length - 1
          val cost = opCost(Seq(inputC.cost), perItemCostOf(node, nOps))
          withConstantSize(res, cost)
      }

      case AND(input) => input match {
        case ConcreteCollection(items, tpe) =>
          val itemsC = items.map(item => eval(adaptSigmaBoolean(item)))
          val res = sigmaDslBuilder.allOf(colBuilder.fromItems(itemsC.map(_.value): _*))
          val costs = itemsC.map(_.cost)
          val nOps = costs.length - 1
          val cost = opCost(costs, perItemCostOf(node, nOps))
          withConstantSize(res, cost)
        case _ =>
          val inputC = tryCast[CostedColl[Boolean]](eval(input))
          val res = sigmaDslBuilder.allOf(inputC.value)
          val nOps = inputC.sizes.length - 1
          val cost = opCost(Seq(inputC.cost), perItemCostOf(node, nOps))
          withConstantSize(res, cost)
      }

      case XorOf(input) => input match {
        case ConcreteCollection(items, tpe) =>
          val itemsC = items.map(item => eval(item))
          val res = sigmaDslBuilder.xorOf(colBuilder.fromItems(itemsC.map(_.value): _*))
          val costs = itemsC.map(_.cost)
          val nOps = costs.length - 1
          val cost = opCost(costs, perItemCostOf(node, nOps))
          withConstantSize(res, cost)
        case _ =>
          val inputC = tryCast[CostedColl[Boolean]](eval(input))
          val res = sigmaDslBuilder.xorOf(inputC.value)
          val nOps = inputC.sizes.length - 1
          val cost = opCost(Seq(inputC.cost), perItemCostOf(node, nOps))
          withConstantSize(res, cost)
      }

      case BinOr(l, r) =>
        val lC = evalNode(ctx, env, l)
        val rC = RCostedThunk(Thunk(evalNode(ctx, env, r)), 0)
        val v = Or.applyLazy(lC.value, rC.value)
        val c = opCost(Seq(lC.cost, rC.cost), costOf(node))
        withConstantSize(v, c)


      case BinAnd(l, r) =>
        val lC = evalNode(ctx, env, l)
        val rC = RCostedThunk(Thunk(evalNode(ctx, env, r)), 0)
        val v = And.applyLazy(lC.value, rC.value)
        val c = opCost(Seq(lC.cost, rC.cost), costOf(node))
        withConstantSize(v, c)

//      case BinXor(l, r) =>
//        val lC = evalNode(ctx, env, l)
//        val rC = RCostedThunk(Thunk(evalNode(ctx, env, r)), 0)
//        val v = sigmaDslBuilder.binXor(lC.value, rC.value)
//        val c = lC.cost + rC.cost + costOf(node)
//        withDefaultSize(v, c)

      case neg: Negation[t] =>
        val tpe = neg.input.tpe
        val et = stypeToElem(tpe)
        val op = NumericNegate(elemToNumeric(et))(et)
        val inputC = evalNode(ctx, env, neg.input)
        inputC match { case x: RCosted[a] =>
            withConstantSize(ApplyUnOp(op, x.value), opCost(Seq(x.cost), costOf(neg)))
        }

      case SigmaAnd(items) =>
        val itemsC = items.map(eval)
        val res = sigmaDslBuilder.allZK(colBuilder.fromItems(itemsC.map(s => asRep[SigmaProp](s.value)): _*))
        val costs = itemsC.map(_.cost)
        val cost = opCost(costs, perItemCostOf(node, costs.length))
        val size = colBuilder.fromItems(itemsC.map(_.size.dataSize): _*).sum(longPlusMonoid)
        RCCostedPrim(res, cost, mkSizeSigmaProp(size))

      case SigmaOr(items) =>
        val itemsC = items.map(eval)
        val res = sigmaDslBuilder.anyZK(colBuilder.fromItems(itemsC.map(s => asRep[SigmaProp](s.value)): _*))
        val costs = itemsC.map(_.cost)
        val cost = opCost(costs, perItemCostOf(node, costs.length))
        val size = colBuilder.fromItems(itemsC.map(_.size.dataSize): _*).sum(longPlusMonoid)
        RCCostedPrim(res, cost, mkSizeSigmaProp(size))

//      case If(c, t, e) =>
//        val cC = evalNode(ctx, env, c)
//        val tC = RCostedThunk(Thunk(evalNode(ctx, env, t)), 0)
//        val eC = RCostedThunk(Thunk(evalNode(ctx, env, e)), 0)
//        val resV = IF (cC.value) THEN tC.value ELSE eC.value
//        val resCost = cC.cost + (tC.cost max eC.cost) + costOf("If", SFunc(Vector(SBoolean, If.tT, If.tT), If.tT))
//        mkCosted(resV, resCost, tC.dataSize max eC.dataSize)

      case If(c, t, e) =>
        val cC = evalNode(ctx, env, c)
        def tC = evalNode(ctx, env, t)
        def eC = evalNode(ctx, env, e)
        val resV = IF (cC.value) THEN tC.value ELSE eC.value
        val resCost = opCost(Seq(cC.cost, tC.cost, eC.cost), costOf("If", SFunc(Vector(SBoolean, If.tT, If.tT), If.tT)))
        RCCostedPrim(resV, resCost, tC.size) // TODO implement tC.size max eC.size

      case rel: Relation[t, _] =>
        val tpe = rel.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToBinOp(rel.opCode, et)
        val x = eval(rel.left)
        val y = eval(rel.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          val value = binop.apply(x.value, asRep[t#WrappedType](y.value))
          val cost =
            if (tpe.isConstantSize) {
              val opcost = if (tpe == SBigInt) {
                costOf(rel.opName, SBigInt.RelationOpType)
              } else
                costOf(rel)
              opCost(Seq(x.cost, y.cost), opcost)
            }
            else opCost(Seq(x.cost, y.cost), perKbCostOf(node, x.size.dataSize + y.size.dataSize))
          val res = withConstantSize(value, cost)
          res
        }

      case l @ Terms.Lambda(_, Seq((n, argTpe)), tpe, Some(body)) =>
        val eArg = stypeToElem(argTpe).asElem[Any]
        val eCostedArg = elemToCostedElem(eArg)
        val f = fun { x: Rep[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), body)
        }(Lazy(eCostedArg))
        val eRes = f.elem.eRange.eVal
        mkCostedFunc(f, opCost(Nil, costOf(node)), l.tpe.dataSize(SType.DummyValue), eArg, eRes)

      case l @ FuncValue(Seq((n, argTpe)), body) =>
        val eArg = stypeToElem(argTpe).asElem[Any]
        val xElem = elemToCostedElem(eArg)
        val f = fun { x: Rep[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), body)
        }(Lazy(xElem))
        val eRes = f.elem.eRange.eVal
        mkCostedFunc(f, opCost(Nil, costOf(node)), l.tpe.dataSize(SType.DummyValue), eArg, eRes)

      case col @ ConcreteCollection(InSeqUnzipped(vs, cs, ss), elemType) =>
        implicit val eAny = stypeToElem(elemType).asElem[Any]
        val values = colBuilder.fromItems(vs: _*)(eAny)
        val costs = colBuilder.fromItems(cs: _*)
        val sizes = colBuilder.fromItems(ss: _*)(sizeElement(eAny))
        RCCostedColl(values, costs, sizes, opCost(cs, costOf(col)))

      case sigmastate.Upcast(In(inputC), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        val res = upcast(inputC.value)(elem)
        withConstantSize(res, opCost(Seq(inputC.cost), costOf(node)))

      case sigmastate.Downcast(In(inputC), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        val res = downcast(inputC.value)(elem)
        withConstantSize(res, opCost(Seq(inputC.cost), costOf(node)))

      case LongToByteArray(In(input)) =>
        val inputC = asRep[Costed[Long]](input)
        val res = sigmaDslBuilder.longToByteArray(inputC.value)
        val cost = opCost(Seq(inputC.cost), costOf(node))
        mkCostedColl(res, 8, cost)

      case ByteArrayToLong(In(arr)) =>
        val arrC = asRep[Costed[Coll[Byte]]](arr)
        val value = sigmaDslBuilder.byteArrayToLong(arrC.value)
        val cost = opCost(Seq(arrC.cost), costOf(node))
        withConstantSize(value, cost)

      case Xor(InCollByte(l), InCollByte(r)) =>
        val values = colBuilder.xor(l.value, r.value)
        val sizes = r.sizes
        val len = sizes.length
        val costs = colBuilder.replicate(len, 0)
        val cost = opCost(Seq(l.cost, r.cost), perKbCostOf(node, len.toLong))
        RCCostedColl(values, costs, sizes, cost)

// TODO should be
//      case ErgoAddressToSigmaProp(input) =>
//        val inputC = evalNode(ctx, env, input)
//        withDefaultSize(inputC.value, inputC.cost + costOf(node))

// TODO why we need this here?
      case sigmastate.Values.ConstantPlaceholder(index, tpe) =>
        val elem = toLazyElem(stypeToElem(tpe))
        val res = constantPlaceholder(index)(elem)
        withConstantSize(res, costOf(node))

      case SubstConstants(InCollByte(bytes), InCollInt(positions), InCollAny(newValues)) =>
        val values = sigmaDslBuilder.substConstants(bytes.values, positions.values, newValues.values)(AnyElement)
        val len = bytes.size.dataSize + newValues.size.dataSize
        val cost = opCost(Seq(bytes.cost, positions.cost, newValues.cost), perKbCostOf(node, len))
        mkCostedColl(values, len.toInt, cost)

      case DecodePoint(InCollByte(bytes)) =>
        val res = sigmaDslBuilder.decodePoint(bytes.values)
        RCCostedPrim(res, opCost(Seq(bytes.cost), costOf(node)), SizeGroupElement)

//      case Terms.MethodCall(obj, method, args, _) if obj.tpe.isCollectionLike =>
//        val xsC = asRep[CostedColl[Any]](evalNode(ctx, env, obj))
//        val (argsVals, argsCosts) = args.map {
//          case sfunc: Value[SFunc]@unchecked if sfunc.tpe.isFunc =>
//            val funC = asRep[CostedFunc[Unit, Any, Any]](evalNode(ctx, env, sfunc)).func
//            val (calcF, costF) = splitCostedFunc2(funC, okRemoveIsValid = true)
//            val cost = xsC.values.zip(xsC.costs.zip(xsC.sizes)).map(costF).sum(intPlusMonoid)
//            (calcF, cost)
//          case a =>
//            val aC = eval(a)
//            (aC.value, aC.cost)
//        }.unzip
//        // todo add costOf(node)
//        val cost = argsCosts.foldLeft(xsC.cost)({ case (s, e) => s + e }) // + costOf(node)
//        val xsV = xsC.value
//        val value = (method.name, argsVals) match {
//          case (SCollection.IndexOfMethod.name, Seq(e, from)) => xsV.indexOf(e, asRep[Int](from))
//          case (SCollection.IndicesMethod.name, _) => xsV.indices
//          case (SCollection.FlatMapMethod.name, Seq(f)) => xsV.flatMap(asRep[Any => Coll[Any]](f))
//          case (SCollection.SegmentLengthMethod.name, Seq(f, from)) =>
//            xsV.segmentLength(asRep[Any => Boolean](f), asRep[Int](from))
//          case (SCollection.IndexWhereMethod.name, Seq(f, from)) =>
//            xsV.indexWhere(asRep[Any => Boolean](f), asRep[Int](from))
//          case (SCollection.LastIndexWhereMethod.name, Seq(f, end)) =>
//            xsV.lastIndexWhere(asRep[Any => Boolean](f), asRep[Int](end))
//          case (SCollection.ZipMethod.name, Seq(col2)) => xsV.zip(asRep[Coll[Any]](col2))
//          case (SCollection.PartitionMethod.name, Seq(f)) => xsV.partition(asRep[Any => Boolean](f))
//          case (SCollection.PatchMethod.name, Seq(from, col, repl)) =>
//            xsV.patch(asRep[Int](from), asRep[Coll[Any]](col), asRep[Int](repl))
//          case (SCollection.UpdatedMethod.name, Seq(index, elem)) =>
//            xsV.updated(asRep[Int](index), asRep[Any](elem))
//          case (SCollection.UpdateManyMethod.name, Seq(indexCol, elemCol)) =>
//            xsV.updateMany(asRep[Coll[Int]](indexCol), asRep[Coll[Any]](elemCol))
//          case _ => error(s"method $method is not supported")
//        }
//        withConstantSize(value, cost)
//
//      case Terms.MethodCall(obj, method, args, _) if obj.tpe.isOption =>
//        val optC = asRep[CostedOption[Any]](eval(obj))
//        val argsC = args.map(eval)
//        (method.name, argsC) match {
//          case (SOption.MapMethod.name, Seq(f)) => optC.map(asRep[Costed[Any => Any]](f))
//          case (SOption.FilterMethod.name, Seq(f)) => optC.filter(asRep[Costed[Any => Boolean]](f))
//          case _ => error(s"method $method is not supported in object $obj")
//        }
//
//      case Terms.MethodCall(obj, method, args, typeSubst) if obj.tpe.isBox =>
//        val boxC = asRep[CostedBox](eval(obj))
//        val argsC = args.map(eval)
//        (method.name, argsC) match {
//          case (SBox.getRegMethod.name, Seq(index)) =>
//            val tpe = typeSubst(SBox.tT)
//            implicit val elem = stypeToElem(tpe).asElem[Any]
//            boxC.getReg(asRep[Int](index.value))(elem)
//          case _ if method.objType.coster.isDefined =>
//            method.objType.coster.get(IR)(boxC, method, argsC)
//          case _ => error(s"method $method is not supported in object $obj")
//        }

      // fallback rule for MethodCall, should be the last case in the list
      case Terms.MethodCall(obj, method, args, _) if method.objType.coster.isDefined =>
        val objC = eval(obj)
        val argsC = args.map(eval)
        method.objType.coster.get(IR)(objC, method, argsC)

      case _ =>
        error(s"Don't know how to evalNode($node)", node.sourceContext.toOption)
    }
    val resC = asRep[Costed[T#WrappedType]](res)
    onTreeNodeCosted(ctx, env, node, resC)
    resC
  }

//  trait Sizable[T] {
//    def size(x: Rep[T]): RSize[T]
//  }
//  object Sizable {
//    def apply[T](sz: Sizable[T]): Sizable[T] = sz
//  }
//
//  def SizableColl[T: Sizable]: Sizable[Coll[T]] = new Sizable[Coll[T]] {
//    override def size(x: RColl[T]): RSize[Coll[T]] = x.map()
//  }
//  def sizeColl[T](coll: RColl[T]): RSize[Coll[T]] = {
//    coll.
//  }

  def buildCostedGraph[T](envVals: Map[Any, SValue], tree: SValue): Rep[Costed[Context] => Costed[T]] = {
    fun { ctxC: RCosted[Context] =>
      val env = envVals.mapValues(v => evalNode(ctxC, Map(), v))
      val res = asCosted[T](evalNode(ctxC, env, tree))
      res
    }
  }

  def cost[T](env: ScriptEnv, typed: SValue): Rep[Costed[Context] => Costed[T]] = {
    val cg = buildCostedGraph[T](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    cg
  }

  def error(msg: String) = throw new CosterException(msg, None)
  def error(msg: String, srcCtx: Option[SourceContext]) = throw new CosterException(msg, srcCtx)
}
