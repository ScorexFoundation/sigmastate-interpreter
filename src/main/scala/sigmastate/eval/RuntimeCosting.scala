package sigmastate.eval

import java.lang.reflect.Constructor

import scala.language.implicitConversions
import scala.language.existentials
import scalan.{Lazy, MutableLazy, Nullable, RType}
import scalan.util.CollectionUtil.TraversableOps
import org.ergoplatform._
import sigmastate._
import sigmastate.Values._
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.CosterException
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo._
import scalan.compilation.GraphVizConfig
import SType._
import org.ergoplatform.SigmaConstants._
import scalan.RType._
import scorex.crypto.hash.{Sha256, Blake2b256}
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.{Terms, SourceContext}
import sigma.types.PrimViewType
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.CollType
import special.Types._
import special.sigma.{GroupElementRType, AvlTreeRType, BigIntegerRType, BoxRType, ECPointRType, BigIntRType, SigmaPropRType}
import special.sigma.Extensions._
import org.ergoplatform.validation.ValidationRules._
import scalan.util.ReflectionUtil
import sigmastate.eval.ExactNumeric._
import spire.syntax.all.cfor
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


trait RuntimeCosting extends CostingRules { IR: IRContext =>
  import Context._;
  import Header._;
  import PreHeader._;
  import GroupElement._;
  import BigInt._;
  import WOption._
  import Coll._;
  import CollBuilder._;
  import SigmaProp._;
  import Box._
  import CCostedBuilder._
  import Size._;
  import SizeBox._;
  import SizeColl._;
  import SizeOption._;
  import SizePair._;
  import SizeContext._
  import CSizePrim._
  import CSizePair._
  import CSizeColl._
  import Costed._;
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
  import AvlTree._
  import WSpecialPredef._
  import CostModel._

  val okMeasureOperationTime: Boolean = false

  this.isInlineThunksOnForce = true  // this required for splitting of cost graph
  this.keepOriginalFunc = false  // original lambda of Lambda node contains invocations of evalNode and we don't want that
  this.useAlphaEquality = false
//  unfoldWithOriginalFunc = unfoldWithOrig

  /** Whether to print values of evaluated nodes of the graph. */
  val okPrintEvaluatedEntries: Boolean = false

  /** Whether to create CostOf nodes or substutute costs from CostTable as constants in the graph.
    * true - substitute; false - create CostOf nodes */
  var substFromCostTable: Boolean = true

  /** Whether to save calcF and costF graphs in the file given by ScriptNameProp environment variable */
  var saveGraphsInFile: Boolean = false

  /** Whether to output the cost value estimated for the script given by ScriptNameProp environment variable */
  var outputEstimatedCost: Boolean = false

  /** Whether to output the computed results of the script. */
  var outputComputedResults: Boolean = false

  /** Whether to perform extended checks of correctness, expected invariants and data consistency.
    * NOTE: Since it may add substantial overhead, set it to `false` before using in production. */
  val debugModeSanityChecks: Boolean = false

//  /** Pass configuration which is used by default in IRContext. */
//  val calcPass = new DefaultPass("calcPass", Pass.defaultPassConfig.copy(constantPropagation = true))
//
//  /** Pass configuration which is used during splitting cost function out of cost graph.
//    * @see `RuntimeCosting.split2` */
//  val costPass = new DefaultPass("costPass", Pass.defaultPassConfig.copy(constantPropagation = true))

/**  To enable specific configuration uncomment one of the lines above and use it in the beginPass below. */
//  beginPass(costPass)

  case class CostOf(opName: String, opType: SFunc) extends BaseDef[Int] {
    override def mirror(t: Transformer): Ref[Int] = self

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
  case class PerKbCostOf(operId: OperationId, size: Ref[Long]) extends BaseDef[Int] {
    override def transform(t: Transformer): Def[Int] = PerKbCostOf(operId, t(size))
    /** Cost rule which is used to compute operation cost, depending on dataSize.
      * Per kilobite cost of the oparation is obtained from CostTable and multiplied on
      * the data size in Kb. */
    def eval(dataSize: Long): Int = {
      val cost = CostTable.DefaultCosts(operId)
      ((dataSize / 1024L).toInt + 1) * cost
    }
  }

  def costOf(costOp: CostOf, doEval: Boolean): Ref[Int] = {
    val res = if (doEval) {
      val c = Const(costOp.eval)
      // optimized hot-spot: here we avoid rewriting which is done by reifyObject
      findOrCreateDefinition(c, c.self)
    }
    else {
      // optimized hot-spot: here we avoid rewriting which is done by reifyObject
      findOrCreateDefinition(costOp, costOp.self)
    }
    res
  }

  def costOf(opName: String, opType: SFunc, doEval: Boolean): Ref[Int] = {
    val costOp = CostOf(opName, opType)
    costOf(costOp, doEval)
  }

  def costOf(opName: String, opType: SFunc): Ref[Int] = {
    costOf(opName, opType, substFromCostTable)
  }

  def costOf(method: SMethod): Ref[Int] = {
    val methodTemplate = method.objType.methodById(method.methodId)
    val opId = methodTemplate.opId
    costOf(opId.name, opId.opType.copy(tpeParams = Nil), substFromCostTable)
  }

  def perKbCostOf(method: SMethod, dataSize: Ref[Long]): Ref[Int] = {
    val methodTemplate = method.objType.methodById(method.methodId)
    val opId = methodTemplate.opId
    perKbCostOf(opId.name, opId.opType.copy(tpeParams = Nil), dataSize)
  }

  val _costOfProveDlogEval = CostOf("ProveDlogEval", SFunc(SUnit, SSigmaProp))
  val _costOfProveDHTuple = CostOf("ProveDHTuple", SFunc(SUnit, SSigmaProp))

  def costOfSigmaTree(sigmaTree: SigmaBoolean): Int = sigmaTree match {
    case _: ProveDlog => _costOfProveDlogEval.eval
    case _: ProveDHTuple => _costOfProveDHTuple.eval
    case CAND(children) => children.map(costOfSigmaTree(_)).sum
    case COR(children)  => children.map(costOfSigmaTree(_)).sum
    case CTHRESHOLD(_, children)  => children.map(costOfSigmaTree(_)).sum
    case _ => CostTable.MinimalCost
  }

  def perKbCostOf(opName: String, opType: SFunc, dataSize: Ref[Long]): Ref[Int] = {
    val opNamePerKb = s"${opName}_per_kb"
    PerKbCostOf(OperationId(opNamePerKb, opType), dataSize)
  }

  def perKbCostOf(node: SValue, dataSize: Ref[Long]): Ref[Int] = {
    perKbCostOf(node.getClass.getSimpleName, node.opType, dataSize)
  }

  def perItemCostOf(node: SValue, arrLength: Ref[Int]) = {
    val opName = s"${node.getClass.getSimpleName}_per_item"
    costOf(opName, node.opType) * arrLength
  }

  def constCost(tpe: SType): Ref[Int] = tpe match {
    case _: SFunc =>
      costOf(s"Lambda", Constant[SType](SFunc.identity.asWrappedType, tpe).opType)
    case _ =>
      costOf(s"Const", Constant[SType](SType.DummyValue, tpe).opType)
  }

  def constCost[T: Elem]: Ref[Int] = {
    val tpe = elemToSType(element[T])
    constCost(tpe)
  }

  val UpcastBigIntOpType = SFunc(sigmastate.Upcast.tT, SBigInt)
  val DowncastBigIntOpType = SFunc(SBigInt, sigmastate.Upcast.tR)

  def costOf(v: SValue): Ref[Int] = v match {
    case l: Terms.Lambda =>
      constCost(l.tpe)
    case l: FuncValue =>
      constCost(l.tpe)
    case sigmastate.Upcast(_, SBigInt) =>
      costOf("Upcast", UpcastBigIntOpType)
    case sigmastate.Downcast(v, _) if v.tpe == SBigInt =>
      costOf("Downcast", DowncastBigIntOpType)
    case _ =>
      costOf(v.opName, v.opType)
  }

  trait SizeStruct extends Size[Struct] {
    def sizeFields: Ref[Struct]
  }
  case class SizeStructCtor(sizeFields: Ref[Struct]) extends SizeStruct {
    override def transform(t: Transformer) = SizeStructCtor(t(sizeFields))

    implicit val eVal: Elem[Struct] = {
      val fields = sizeFields.elem.fields.map { case (fn, cE) => (fn, cE.asInstanceOf[SizeElem[_, _]].eVal) }
      structElement(fields)
    }
    val resultType: Elem[Size[Struct]] = sizeElement(eVal)

    def dataSize: Ref[Long] = {
      val sizes = sizeFields.fields.map { case (_, cf: RSize[a]@unchecked) => cf.dataSize }
      val sizesColl = colBuilder.fromItems(sizes:_*)
      sizesColl.sum(longPlusMonoid)
    }
  }
  def RSizeStruct(sizeFields: Ref[Struct]): Ref[Size[Struct]] = SizeStructCtor(sizeFields)

  trait CostedStruct extends Costed[Struct] { }

  case class CostedStructCtor(costedFields: Ref[Struct], structCost: Ref[Int]) extends CostedStruct {
    override def transform(t: Transformer) = CostedStructCtor(t(costedFields), t(structCost))

    implicit val eVal: Elem[Struct] = {
      val fields = costedFields.elem.fields.map { case (fn, cE) => (fn, cE.asInstanceOf[CostedElem[_, _]].eVal) }
      structElement(fields)
    }
    val resultType: Elem[Costed[Struct]] = costedElement(eVal)

    def builder: Ref[CostedBuilder] = costedBuilder

    def value: Ref[Struct] = costedFields.mapFields { case cf: RCosted[a]@unchecked => cf.value }

    def cost: Ref[Int] = {
      val costs = costedFields.fields.map { case (_, cf: RCosted[a]@unchecked) => cf.cost }
      opCost(value, costs, structCost)
    }

    override def size: Ref[Size[Struct]] = {
      val sizeFields = costedFields.mapFields { case cf: RCosted[a]@unchecked => cf.size }
      SizeStructCtor(sizeFields)
    }
  }

  def RCostedStruct(costedFields: Ref[Struct], structCost: Ref[Int]): Ref[Costed[Struct]] = CostedStructCtor(costedFields, structCost)

  // SizeThunk =============================================
  trait SizeThunk[A] extends Size[Thunk[A]] { }

  case class SizeThunkCtor[A](sizeBlock: Ref[Thunk[Size[A]]]) extends SizeThunk[A] {
    override def transform(t: Transformer) = SizeThunkCtor(t(sizeBlock))
    implicit val eVal: Elem[Thunk[A]] = thunkElement(sizeBlock.elem.eItem.eVal)
    val resultType: Elem[Size[Thunk[A]]] = sizeElement(eVal)

    override def dataSize: Ref[Long] = sizeBlock.force().dataSize
  }

  def RSizeThunk[A](sizeBlock: Ref[Thunk[Size[A]]]): Ref[Size[Thunk[A]]] = SizeThunkCtor(sizeBlock)
  // ---------------------------------------------------------


  // CostedThunk =============================================
  trait CostedThunk[A] extends Costed[Thunk[A]] { }

  case class CostedThunkCtor[A](costedBlock: Ref[Thunk[Costed[A]]], thunkCost: Ref[Int]) extends CostedThunk[A] {
    override def transform(t: Transformer) = CostedThunkCtor(t(costedBlock), t(thunkCost))
    implicit val eVal: Elem[Thunk[A]] = thunkElement(costedBlock.elem.eItem.eVal)
    val resultType: Elem[Costed[Thunk[A]]] = costedElement(eVal)

    def builder: Ref[CostedBuilder] = costedBuilder
    def value: Ref[Thunk[A]] = Thunk { costedBlock.force().value }
    def cost: Ref[Int] = asRep[Int](ThunkForce(Thunk(costedBlock.force.cost))) + thunkCost
    override def size: RSize[Thunk[A]] = SizeThunkCtor(Thunk { costedBlock.force().size })
  }

  def RCostedThunk[A](costedBlock: Ref[Thunk[Costed[A]]], thunkCost: Ref[Int]): Ref[Costed[Thunk[A]]] = CostedThunkCtor(costedBlock, thunkCost)
  // ---------------------------------------------------------

  def typeSize(tpe: SType): Ref[Long] = {
    assert(tpe.isConstantSize, {
      s"Expected constant size type but was $tpe"
    })
    val size = tpe.dataSize(SType.DummyValue)
    toRep(size)
  }

  def typeSize[T: Elem]: Ref[Long] = {
    val tpe = elemToSType(element[T])
    typeSize(tpe)
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case CostOf(name, opType) => s"CostOf($name:$opType)"
    case GroupElementConst(p) => p.showToString
    case SigmaPropConst(sp) => sp.toString
    case _ => super.formatDef(d)
  }

  type RColl[T] = Ref[Coll[T]]
  type ROption[T] = Ref[WOption[T]]
  type RCostedColl[T] = Ref[CostedColl[T]]
  type RCostedOption[T] = Ref[CostedOption[T]]
  type RFuncCosted[A,B] = Ref[Costed[A] => Costed[B]]

  implicit class RFuncCostedOps[A,B](f: RFuncCosted[A,B]) {
    implicit val eA = f.elem.eDom.eVal
    /**NOTE: when removeIsValid == true the resulting type B may change from Boolean to SigmaProp
      * This should be kept in mind at call site */
    def sliceCalc(okRemoveIsProven: Boolean): Ref[A => Any] = {
      val _f = { x: Ref[A] => f(RCCostedPrim(x, IntZero, zeroSize(x.elem))).value }
      val res = if (okRemoveIsProven) fun(removeIsProven(_f)) else fun(_f)
      res
    }

    def sliceCalc: Ref[A => B] = fun { x: Ref[A] => f(RCCostedPrim(x, IntZero, zeroSize(x.elem))).value }

    def sliceCost: Ref[((Int,Size[A])) => Int] = fun { in: Ref[(Int, Size[A])] =>
      val Pair(c, s) = in
      f(RCCostedPrim(placeholder[A], c, s)).cost
    }

    def sliceCostEx: Ref[((A, (Int,Size[A]))) => Int] = fun { in: Ref[(A, (Int, Size[A]))] =>
      val Pair(ctx, Pair(c, s)) = in

      val oldFun = funUnderCosting
      try {
        funUnderCosting = f
        f(RCCostedPrim(ctx, c, s)).cost
      }
      finally {
        funUnderCosting = oldFun
      }
    }

    def sliceSize: Ref[Size[A] => Size[B]] = fun { in: Ref[Size[A]] =>
      val s = in
      val arg = RCCostedPrim(placeholder[A], IntZero, s)
      f(arg).size
    }
  }

  type CostedCollFunc[A,B] = Costed[A] => CostedColl[B]
  type CostedOptionFunc[A,B] = Costed[A] => CostedOption[B]
  type RCostedCollFunc[A,B] = Ref[CostedCollFunc[A, B]]
  type RCostedOptionFunc[A,B] = Ref[CostedOptionFunc[A, B]]

  implicit class RCostedCollFuncOps[A,B](f: RCostedCollFunc[A,B]) {
    implicit val eA = f.elem.eDom.eVal
    def sliceValues: Ref[A => Coll[B]] = fun { x: Ref[A] => f(RCCostedPrim(x, IntZero, zeroSize(x.elem))).values }
    def sliceCosts: Ref[((Int,Size[A])) => (Coll[Int], Int)] = fun { in: Ref[(Int, Size[A])] =>
      val Pair(c, s) = in
      val colC = f(RCCostedPrim(placeholder[A], c, s))
      Pair(colC.costs, colC.valuesCost)
    }
    def sliceSizes: Ref[Size[A] => Coll[Size[B]]] = fun { in: Ref[Size[A]] =>
      val s = in
      f(RCCostedPrim(placeholder[A], IntZero, s)).sizes
    }
  }

  implicit def extendCostedFuncElem[E,A,B](e: Elem[CostedFunc[E,A,B]]): CostedFuncElem[E,A,B,_] = e.asInstanceOf[CostedFuncElem[E,A,B,_]]

  implicit def extendSizeElem[A](elem: Elem[Size[A]]): SizeElem[A, Size[A]] =
    elem.asInstanceOf[SizeElem[A, Size[A]]]

  implicit def extendCostedElem[A](elem: Elem[Costed[A]]): CostedElem[A, Costed[A]] =
    elem.asInstanceOf[CostedElem[A, Costed[A]]]

  implicit def extendCostedCollElem[A](elem: Elem[CostedColl[A]]): CostedCollElem[A, CostedColl[A]] =
    elem.asInstanceOf[CostedCollElem[A, CostedColl[A]]]

  def splitCostedFunc2[A,B](f: RFuncCosted[A,B]): (Ref[A=>B], Ref[((Int, Size[A])) => Int]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc
    val costF = f.sliceCost
    (calcF, costF)
  }
  def splitCostedFunc2[A, B](f: RFuncCosted[A,B], okRemoveIsValid: Boolean): (Ref[A=>Any], Ref[((Int, Size[A])) => Int]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc(okRemoveIsValid)
    val costF = f.sliceCost
    (calcF, costF)
  }
  def splitCostedFunc[A,B](f: RFuncCosted[A,B]): (Ref[A=>B], Ref[((Int, Size[A])) => Int], Ref[Size[A] => Size[B]]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc
    val costF = f.sliceCost
    val sizeF = f.sliceSize
    (calcF, costF, sizeF)
  }

  def splitCostedCollFunc[A,B](f: RCostedCollFunc[A,B]): (Ref[A=>Coll[B]], Ref[((Int, Size[A])) => (Coll[Int], Int)], Ref[Size[A] => Coll[Size[B]]]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceValues
    val costF = f.sliceCosts
    val sizeF = f.sliceSizes
    (calcF, costF, sizeF)
  }

  object CostedFoldExtractors {
    type Result = (ROption[A], Th[B], RFunc[A, Costed[B]]) forSome {type A; type B}
  }

  object IsConstSizeCostedColl {
    def unapply(d: Def[_]): Nullable[Ref[Costed[Coll[A]]] forSome {type A}] = d.resultType match {
      case ce: CostedElem[_,_] if !ce.isInstanceOf[CostedCollElem[_, _]] =>
        ce.eVal match {
          case colE: CollElem[a,_] if colE.eItem.isConstantSize =>
            val res = d.self.asInstanceOf[Ref[Costed[Coll[A]]] forSome {type A}]
            Nullable(res)
          case _ => Nullable.None
        }
      case _ => Nullable.None
    }
  }

  object IsCostedPair {
    def unapply(d: Def[_]): Nullable[Ref[Costed[(A, B)]] forSome {type A; type B}] = d.resultType match {
      case ce: CostedElem[_,_] if !ce.isInstanceOf[CostedPairElem[_, _, _]] =>
        ce.eVal match {
          case _: PairElem[a,b]  =>
            val res = d.self.asInstanceOf[Ref[Costed[(A, B)]] forSome {type A; type B}]
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

  class ElemAccessor[T](prop: Ref[T] => Elem[_]) {
    def unapply(s: Sym): Nullable[Elem[_]] = { val sR = asRep[T](s); Nullable(prop(sR)) }
  }
  object ElemAccessor {
    def apply[T](prop: Ref[T] => Elem[_]): ElemAccessor[T] = new ElemAccessor(prop)
  }

  val EValOfSizeColl = ElemAccessor[Coll[Size[Any]]](_.elem.eItem.eVal)

  private val CBM      = CollBuilderMethods
  private val SigmaM   = SigmaPropMethods
  private val CCM      = CostedCollMethods
  private val CostedM  = CostedMethods
  private val WOptionM = WOptionMethods
  private val SDBM     = SigmaDslBuilderMethods
  private val SM       = SizeMethods

  def mkNormalizedOpCost(costedValue: Sym, costs: Seq[Ref[Int]]): Ref[Int] = {
    val (args, rests) = costs.partition(_.node.isInstanceOf[OpCost])
    val restSum =
      if (rests.isEmpty) IntZero
      else rests.reduce((x, y) => x + y)
    opCost(costedValue, args, restSum)
  }

  object AnyOf {
    def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[A]], Elem[A]) forSome {type A}] = d match {
      case SDBM.anyOf(_, xs) =>
        CBM.fromItems.unapply(xs)
      case _ => Nullable.None
    }
  }
  object AllOf {
    def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[A]], Elem[A]) forSome {type A}] = d match {
      case SDBM.allOf(_, xs) =>
        CBM.fromItems.unapply(xs)
      case _ => Nullable.None
    }
  }
  object AnyZk {
    def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])] = d match {
      case SDBM.anyZK(_, xs) =>
        CBM.fromItems.unapply(xs).asInstanceOf[Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])]]
      case _ => Nullable.None
    }
  }
  object AllZk {
    def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])] = d match {
      case SDBM.allZK(_, xs) =>
        CBM.fromItems.unapply(xs).asInstanceOf[Nullable[(Ref[CollBuilder], Seq[Ref[SigmaProp]], Elem[SigmaProp])]]
      case _ => Nullable.None
    }
  }
  object HasSigmas {
    def unapply(items: Seq[Sym]): Option[(Seq[Ref[Boolean]], Seq[Ref[SigmaProp]])] = {
      val bs = ArrayBuffer.empty[Ref[Boolean]]
      val ss = ArrayBuffer.empty[Ref[SigmaProp]]
      for (i <- items) {
        i match {
          case SigmaM.isValid(s) => ss += s
          case b => bs += asRep[Boolean](b)
        }
      }
      assert(items.length == bs.length + ss.length)
      if (ss.isEmpty) None
      else Some((bs,ss))
    }
  }

  /** For performance reasons the patterns are organized in special (non-declarative) way.
    * Unfortunately, this is less readable, but gives significant performance boost
    * Look at comments to understand the logic of the rules.
    *
    * @hotspot executed for each node of the graph, don't beautify.
    */
  override def rewriteDef[T](d: Def[T]): Ref[_] = {
    // First we match on node type, and then depending on it, we have further branching logic.
    // On each branching level each node type should be matched exactly once,
    // for the rewriting to be sound.
    d match {
      // Rule: cast(eTo, x) if x.elem <:< eTo  ==>  x
      case Cast(eTo: Elem[to], x) if eTo.getClass.isAssignableFrom(x.elem.getClass) =>
        x

      // Rule: ThunkDef(x, Nil).force => x
      case ThunkForce(Def(ThunkDef(root, sch))) if sch.isEmpty => root

      case SM.dataSize(size) => size.node match {
        case CSizeCollCtor(sizes) => sizes match {
          case CBM.replicate(_, n, s: RSize[a]@unchecked) => s.dataSize * n.toLong
          case EValOfSizeColl(eVal) if eVal.isConstantSize => sizes.length.toLong * typeSize(eVal)
          case _ => super.rewriteDef(d)
        }
        case _ => super.rewriteDef(d)
      }

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

      case SDBM.Colls(_) => colBuilder

      case SDBM.sigmaProp(_, SigmaM.isValid(p)) => p
      case SigmaM.isValid(SDBM.sigmaProp(_, bool)) => bool

      case CCM.foldCosted(xs: RCostedColl[a], zero: RCosted[b], _f) =>
        val f = asRep[Costed[(b,a)] => Costed[b]](_f)
        val (calcF/*: Ref[((b,a)) => b]*/,
             costF/*: Ref[((Int, Size[(b,a)])) => Int]*/,
             sizeF/*: Ref[Size[(b,a)] => Size[b]]*/) = splitCostedFunc[(b,a), b](f)

        val resV = xs.values.foldLeft(zero.value, calcF)

        implicit val eA: Elem[a] = xs.elem.eItem
        implicit val eB: Elem[b] = zero.elem.eVal
        val sizes = xs.sizes
        val len = sizes.length
        val zeros = colBuilder.replicate(len, IntZero)

        // in order to fail fast this line is computed first before the fold loop below
        val preFoldCost = opCost(resV, Array(xs.cost, zero.cost), len * CostTable.lambdaInvoke + CostTable.lambdaCost)

        val Pair(resS, resC) = sizes.foldLeft(Pair(zero.size, preFoldCost),
            fun { in: Ref[((Size[b], Int), Size[a])] =>
              val Pair(Pair(accSizeB, accCost), xSize) = in
              val sBA = RCSizePair(accSizeB, xSize)
              val size = sizeF(sBA)  // unfold sizeF
              val cost: Ref[Int] = opCost(size, Array(accCost), asRep[Int](Apply(costF, Pair(IntZero, sBA), false)) + CostTable.lambdaInvoke)
              val res = Pair(size, cost)
              res
            }
        )

        val cost = opCost(resV, Array(preFoldCost), resC)
        RCCostedPrim(resV, cost, resS)

      case CostedM.cost(costed) => costed.node match {
        case CCostedCollCtor(values, costs, _, accCost) =>
          accCost.node match {
            case _: OpCost =>
              opCost(values, Array(accCost), costs.sum(intPlusMonoid))    // OpCost should be in args position
            case _ =>
              opCost(values, Nil, costs.sum(intPlusMonoid) + accCost)
          }
        case CCostedOptionCtor(v, costOpt, _, accCost) =>
          accCost.node match {
            case _: OpCost =>
              opCost(v, Array(accCost), costOpt.getOrElse(Thunk(IntZero)))  // OpCost should be in args position
            case _ =>
              opCost(v, Nil, costOpt.getOrElse(Thunk(IntZero)) + accCost)
          }
        case CCostedPairCtor(l, r, accCost) =>
          val costs = Array(l.cost, r.cost, accCost)
          val v = Pair(l.value, r.value)
          mkNormalizedOpCost(v, costs)

        // Rule: opt.fold(default, f).cost ==> opt.fold(default.cost, x => f(x).cost)
        case WOptionM.fold(opt, _th @ Def(ThunkDef(_, _)), _f) =>
          implicit val eA: Elem[Any] = opt.elem.eItem.asInstanceOf[Elem[Any]]
          val th = asRep[Thunk[Costed[Any]]](_th)
          val f = asRep[Any => Costed[Any]](_f)
          opt.fold(Thunk(forceThunkByMirror(th).cost), fun { x: Ref[Any] => f(x).cost })
        case _ => super.rewriteDef(d)
      }

      case CostedM.value(costed) => costed.node match {
        case CCostedFuncCtor(_, func: RFuncCosted[a,b], _,_) => func.sliceCalc

        // Rule: opt.fold(default, f).value ==> opt.fold(default.value, x => f(x).value)
        case WOptionM.fold(opt, _th @ Def(ThunkDef(_, _)), _f) =>
          implicit val eA: Elem[Any] = opt.elem.eItem.asInstanceOf[Elem[Any]]
          val th = asRep[Thunk[Costed[Any]]](_th)
          val f = asRep[Any => Costed[Any]](_f)
          opt.fold(Thunk(forceThunkByMirror(th).value), fun { x: Ref[Any] => f(x).value })

        case _ => super.rewriteDef(d)
      }

      // Rule: opt.fold(default, f).size ==> opt.fold(default.size, x => f(x).size)
      case CostedM.size(WOptionM.fold(opt, _th @ Def(ThunkDef(_, _)), _f)) =>
        implicit val eA: Elem[Any] = opt.elem.eItem.asInstanceOf[Elem[Any]]
        val th = asRep[Thunk[Costed[Any]]](_th)
        val f = asRep[Any => Costed[Any]](_f)
        opt.fold(Thunk(forceThunkByMirror(th).size), fun { x: Ref[Any] => f(x).size })

      case CCostedPrimCtor(v, c, s) =>
        val res = v.elem.asInstanceOf[Elem[_]] match {
          case pe: PairElem[a,b] =>
            val p = asRep[(a,b)](v)
            costedPrimToPair(p, c, asRep[Size[(a,b)]](s))
          case ce: CollElem[a,_] =>
            val col = asRep[Coll[a]](v)
            costedPrimToColl(col, c, asRep[Size[Coll[a]]](s))
          case oe: WOptionElem[a,_] =>
            val opt = asRep[WOption[a]](v)
            costedPrimToOption(opt, c, asRep[Size[WOption[a]]](s))
          case _ => super.rewriteDef(d)
        }
        res

      case IsConstSizeCostedColl(col) if !d.isInstanceOf[MethodCall] => // see also rewriteNonInvokableMethodCall
        mkCostedColl(col.value, col.value.length, col.cost)

      case OpCost(_, id, args, cost) =>
        val zero = IntZero
        if (isCostingProcess && cost == zero && args.length == 1 && args(0).node.isInstanceOf[OpCost]) {
          args(0) // Rule: OpCode(_,_, Seq(x @ OpCode(...)), 0) ==> x,
        }
        else
        if (args.exists(_ == zero)) {
          val nonZeroArgs = args.filterNot(_ == zero)
          val res: Ref[Int] =
            if (cost == zero && nonZeroArgs.isEmpty) zero
            else {
              val lamVar = lambdaStack.head.x
              OpCost(lamVar, id, nonZeroArgs, cost)
            }
          res
        } else {
          if (debugModeSanityChecks) {
            if (args.contains(cost))
              !!!(s"Invalid OpCost($id, $args, $cost)")
          }
          super.rewriteDef(d)
        }

      case _ =>
        if (currentPass.config.constantPropagation) {
          // additional constant propagation rules (see other similar cases)
          d match {
            case AnyOf(_,items,_) if (items.forall(_.isConst)) =>
              val bs = items.map { case Def(Const(b: Boolean)) => b }
              toRep(bs.exists(_ == true))
            case AllOf(_,items,_) if (items.forall(_.isConst)) =>
              val bs = items.map { case Def(Const(b: Boolean)) => b }
              toRep(bs.forall(_ == true))
            case _ =>
              super.rewriteDef(d)
          }
        }
        else
          super.rewriteDef(d)
    }
  }

  def costedPrimToColl[A](coll: Ref[Coll[A]], c: Ref[Int], s: RSize[Coll[A]]): RCostedColl[A] = s.elem.asInstanceOf[Any] match {
    case se: SizeElem[_,_] if se.eVal.isInstanceOf[CollElem[_,_]] =>
      val sizes = asSizeColl(s).sizes
      val costs = colBuilder.replicate(sizes.length, IntZero)
      mkCostedColl(coll, costs, sizes, c)
    case _ =>
      !!!(s"Expected Size[Coll[A]] node but was $s -> ${s.node}")
  }

  def costedPrimToOption[A](opt: Ref[WOption[A]], c: Ref[Int], s: RSize[WOption[A]]) = s.elem.asInstanceOf[Any] match {
    case se: SizeElem[_,_] if se.eVal.isInstanceOf[WOptionElem[_,_]] =>
      val sizeOpt = asSizeOption(s).sizeOpt
      mkCostedOption(opt, SomeIntZero, sizeOpt, c)
    case _ =>
      !!!(s"Expected RCSizeOption node but was $s -> ${s.node}")
  }

  def costedPrimToPair[A,B](p: Ref[(A,B)], c: Ref[Int], s: RSize[(A,B)]) = s.elem.asInstanceOf[Any] match {
    case se: SizeElem[_,_] if se.eVal.isInstanceOf[PairElem[_,_]] =>
      val sPair = asSizePair(s)
      val zero = IntZero
      val l = RCCostedPrim(p._1, zero, sPair.l)
      val r = RCCostedPrim(p._2, zero, sPair.r)
      val newCost = if (c == zero) zero else opCost(Pair(l, r), Array(c), zero)
      RCCostedPair(l, r, newCost)
    case _ =>
      !!!(s"Expected RCSizePair node but was $s -> ${s.node}")
  }

  override def rewriteNonInvokableMethodCall(mc: MethodCall): Ref[_] = mc match {
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

  /** Should be specified in the final cake */
  val builder: sigmastate.lang.SigmaBuilder
  import builder._

  /** Lazy values, which are immutable, but can be reset, so that the next time they are accessed
    * the expression is re-evaluated. Each value should be reset in onReset() method. */
  private val _sigmaDslBuilder: LazyRep[SigmaDslBuilder] = MutableLazy(variable[SigmaDslBuilder])
  @inline def sigmaDslBuilder: Ref[SigmaDslBuilder] = _sigmaDslBuilder.value

  private val _colBuilder: LazyRep[CollBuilder] = MutableLazy(variable[CollBuilder])
  @inline def colBuilder: Ref[CollBuilder] = _colBuilder.value

//  private val _sizeBuilder: LazyRep[SizeBuilder] = MutableLazy(RCSizeBuilder())
//  @inline def sizeBuilder: Ref[SizeBuilder] = _sizeBuilder.value

  private val _costedBuilder: LazyRep[CostedBuilder] = MutableLazy(RCCostedBuilder())
  @inline def costedBuilder: Ref[CostedBuilder] = _costedBuilder.value

  private val _monoidBuilder: LazyRep[MonoidBuilder] = MutableLazy(variable[MonoidBuilder])
  @inline def monoidBuilder: Ref[MonoidBuilder] = _monoidBuilder.value

  private val _intPlusMonoid: LazyRep[Monoid[Int]] = MutableLazy(monoidBuilder.intPlusMonoid)
  @inline def intPlusMonoid: Ref[Monoid[Int]] = _intPlusMonoid.value

  private val _longPlusMonoid: LazyRep[Monoid[Long]] = MutableLazy(monoidBuilder.longPlusMonoid)
  @inline def longPlusMonoid: Ref[Monoid[Long]] = _longPlusMonoid.value

  private val _costedGlobal: LazyRep[Costed[SigmaDslBuilder]] =
    MutableLazy(RCCostedPrim(sigmaDslBuilder, IntZero, costedBuilder.mkSizePrim(1L, sigmaDslBuilderElement)))
  @inline def costedGlobal: RCosted[SigmaDslBuilder] = _costedGlobal.value

  private val _costOfProveDlog: LazyRep[Int] = MutableLazy(costOf(_costOfProveDlogEval, substFromCostTable))
  @inline def CostOfProveDlog: Ref[Int] = _costOfProveDlog.value

  private val _costOfDHTuple: LazyRep[Int] = MutableLazy(costOf(_costOfProveDHTuple, substFromCostTable))
  @inline def CostOfDHTuple: Ref[Int] = _costOfDHTuple.value   // see CostTable for how it relate to ProveDlogEval

  protected override def onReset(): Unit = {
    super.onReset()
    // WARNING: every lazy value should be listed here, otherwise bevavior after resetContext is undefined and may throw.
    Array(_sigmaDslBuilder, _colBuilder, _costedBuilder,
      _monoidBuilder, _intPlusMonoid, _longPlusMonoid, _costedGlobal,
      _costOfProveDlog, _costOfDHTuple)
        .foreach(_.reset())
    _contextDependantNodes = debox.Set.ofSize[Int](InitDependantNodes)
  }

  import Cost._

  def removeIsProven[T,R](f: Ref[T] => Ref[Any]): Ref[T] => Ref[Any] = { x: Ref[T] =>
    val y = f(x);
    val res = y match {
      case SigmaPropMethods.isValid(p) => p
      case v => v
    }
    res
  }

  private[sigmastate] var funUnderCosting: Sym = null
  def isCostingProcess: Boolean = funUnderCosting != null

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
  }).asInstanceOf[Elem[T#WrappedType]]

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
    (ByteElement, ByteIsExactNumeric),
    (ShortElement, ShortIsExactNumeric),
    (IntElement, IntIsExactNumeric),
    (LongElement, LongIsExactNumeric),
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

  def adaptSigmaBoolean(v: BoolValue) = v match {
    case sb: SigmaBoolean => sb.isProven
    case _ => v
  }

    /** Helper to create costed collection of some constant size type T */
  def mkCostedColl[T](col: Ref[Coll[T]], len: Ref[Int], cost: Ref[Int]): Ref[CostedColl[T]] = {
    // TODO optimize: the method should be specialized on T so that mkSizePrim is not used
    val eT = col.elem.eItem
    val costs = colBuilder.replicate(len, IntZero)
    val sizes = colBuilder.replicate(len, costedBuilder.mkSizePrim(typeSize(eT), eT): RSize[T])
    RCCostedColl(col, costs, sizes, cost)
  }

  @inline final def asCosted[T](x: Ref[_]): Ref[Costed[T]] = x.asInstanceOf[Ref[Costed[T]]]

  type CostingEnv = Map[Any, RCosted[_]]

  import sigmastate._

  protected def isOperationNode(v: SValue): Boolean = v match {
    case _: Block | _: BlockValue | _: TaggedVariableNode[_] | _: ValNode | _: ValDef | _: ValUse[_] | _: FuncValue => false
    case _ => true
  }

  protected def onTreeNodeCosted[T <: SType](
        ctx: RCosted[Context], env: CostingEnv,
        node: Value[T], costed: RCosted[T#WrappedType]): Unit = {
  }

  @inline def SigmaDsl = sigmaDslBuilderValue
  @inline def Colls = sigmaDslBuilderValue.Colls

  protected implicit def groupElementToECPoint(g: special.sigma.GroupElement): EcPointType = SigmaDsl.toECPoint(g).asInstanceOf[EcPointType]

  def constantTypeSize[T](implicit eT: Elem[T]): RSize[T] = RCSizePrim(typeSize(eT), eT)

  def withConstantSize[T](v: Ref[T], cost: Ref[Int]): RCosted[T] = RCCostedPrim(v, cost, constantTypeSize(v.elem))

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
  def attachCost[T](source: RCosted[T], accCost: Ref[Int], cost: Ref[Int]): RCosted[T] = asRep[Costed[T]] {
    def newCost(v: Sym, c: Ref[Int]) = opCost(v, Array(accCost, c), cost) // put cost in dependency list

    source.elem.eVal match {
      case e: CollElem[a, _] =>
        val xsC = asCostedColl[a](asCosted[Coll[a]](source))
        val v = xsC.values
        val c = xsC.cost
        RCCostedColl(v, xsC.costs, xsC.sizes, newCost(v, c))
      case e: PairElem[a,b] =>
        val pC = asCostedPair[a,b](asCosted[(a,b)](source))
        RCCostedPair(pC.l, pC.r, newCost(Pair(pC.l, pC.r), pC.cost))
      case e =>
        val c = source.cost  // this is a current cost of the value
        val v = source.value
        RCCostedPrim(v, newCost(v, c), source.size)
    }
  }

  /** Initial capacity of the hash set, large enough to avoid many rebuidings
    * and small enough to not consume too much memory. */
  private val InitDependantNodes = 10000

  /** Mutable IR context state, make sure it is reset in onReset() to its initial state. */
  private[this] var _contextDependantNodes = debox.Set.ofSize[Int](InitDependantNodes)

  @inline final def isContextDependant(sym: Sym): Boolean =
    if (sym.isConst) true
    else {
      _contextDependantNodes(sym.node.nodeId)
    }

  /** @hotspot don't beautify the code */
  @inline final def allContextDependant(syms: Array[Sym]): Boolean = {
    val len = syms.length
    cfor(0)(_ < len, _ + 1) { i =>
      if (!isContextDependant(syms(i))) return false
    }
    true
  }

  /** Here we hook into graph building process at the point where each new graph node is added to the graph.
    * First, we call `super.createDefinition`, which adds the new node `d` to the graph (`s` is the node's symbol).
    * Next, we update context dependence analysis information (see isSupportedIndexExpression)
    * The graph node is `context-dependent` if:
    * 1) it is the node of Context type
    * 2) all nodes it depends on are `context-dependent`
    *
    * @see super.createDefinition, isSupportedIndexExpression
    */
  override protected def createDefinition[T](optScope: Nullable[ThunkScope], s: Ref[T], d: Def[T]): Ref[T] = {
    val res = super.createDefinition(optScope, s, d)
    val d1 = res.node
    // the node is of Context type  => `context-dependent`
    // all arguments are `context-dependent`  =>  d is `context-dependent`
    val isDependent =
       d1.resultType.isInstanceOf[ContextElem[_]] ||
       allContextDependant(d1.deps)
    if (isDependent)
      _contextDependantNodes += (d1.nodeId)
    else false  // this is to avoid boxing in `then` branch
    res
  }

  /** Checks that index expression sub-graph (which root is `i`) consists of `context-dependent` nodes.
    * This is used in the validation rule for the costing of ByIndex operation.
    * @see RuntimeCosting, CheckIsSupportedIndexExpression */
  def isSupportedIndexExpression(i: Ref[Int]): Boolean = {
    isContextDependant(i)
  }

  // This type descriptors are used quite often here and there.
  // Explicit usage of this values saves lookup time in elemCache.
  val eCollByte = collElement(ByteElement)
  val ePairOfCollByte = pairElement(eCollByte, eCollByte)

  def costedBooleanTransformer[T](node: BooleanTransformer[_],
                                  xs: RCostedColl[T],
                                  condition: RCosted[T => SType#WrappedType],
                                  calcF: Ref[T => Any], accCost: Ref[Int]) = {
    val args: Seq[Ref[Int]] = Array(xs.cost, condition.cost)
    val res = calcF.elem.eRange.asInstanceOf[Elem[_]] match {
      case BooleanElement =>
        node match {
          case _: ForAll[_] =>
            val value = xs.values.forall(asRep[T => Boolean](calcF))
            val cost = opCost(value, args, accCost + costOf(SCollection.ForallMethod))
            withConstantSize(value, cost)
          case _: Exists[_] =>
            val value = xs.values.exists(asRep[T => Boolean](calcF))
            val cost = opCost(value, args, accCost + costOf(SCollection.ExistsMethod))
            withConstantSize(value, cost)
        }
      case _: SigmaPropElem[_] =>
        val children = xs.values.map(asRep[T => SigmaProp](calcF))
        val size = SizeSigmaProposition
        node match {
          case _: ForAll[_] =>
            val value = sigmaDslBuilder.allZK(children)
            val cost = opCost(value, args, accCost + costOf(SCollection.ForallMethod))
            RCCostedPrim(value, cost, size)
          case _: Exists[_] =>
            val value = sigmaDslBuilder.anyZK(children)
            val cost = opCost(value, args, accCost + costOf(SCollection.ExistsMethod))
            RCCostedPrim(value, cost, size)
        }
    }
    res
  }

  class CostingRuleStat(val node: SValue, val outerStart: Long, var innerTime: Long, val outerEnd: Long)

  var ruleStack: List[CostingRuleStat] = Nil

  protected def evalNode[T <: SType](ctx: RCosted[Context], env: CostingEnv, node: Value[T]): RCosted[T#WrappedType] = {
    import WOption._
    def eval[T <: SType](node: Value[T]): RCosted[T#WrappedType] = evalNode(ctx, env, node)
    object In { def unapply(v: SValue): Nullable[RCosted[Any]] = Nullable(asRep[Costed[Any]](evalNode(ctx, env, v))) }
    class InColl[T: Elem] { def unapply(v: SValue): Nullable[Ref[CostedColl[T]]] = Nullable(tryCast[CostedColl[T]](evalNode(ctx, env, v))) }
    val InCollByte = new InColl[Byte]; val InCollAny = new InColl[Any]()(AnyElement); val InCollInt = new InColl[Int]

    val InCollCollByte = new InColl[Coll[Byte]]()(eCollByte)
    val InPairCollByte = new InColl[(Coll[Byte], Coll[Byte])]()(ePairOfCollByte)

    object InSeq { def unapply(items: Seq[SValue]): Nullable[Seq[RCosted[Any]]] = {
      val res = items.map { x: SValue =>
        val xC = eval(x)
        asRep[Costed[Any]](xC)
      }
      Nullable(res)
    }}
    object InSeqUnzipped { def unapply(items: Seq[SValue]): Nullable[(Seq[Ref[Any]], Seq[Ref[Int]], Seq[RSize[Any]])] = {
      val res = items.mapUnzip { x: SValue =>
        val xC = eval(x)
        (asRep[Any](xC.value), xC.cost, asRep[Size[Any]](xC.size))
      }
      Nullable(res)
    }}

    if (okMeasureOperationTime) {
      val t = System.nanoTime()
      ruleStack = new CostingRuleStat(node, t, 0, t) :: ruleStack
    }

    val res: Ref[Any] = node match {
      case TaggedVariableNode(id, _) =>
        env.getOrElse(id, !!!(s"TaggedVariable $id not found in environment $env"))

      case c @ Constant(v, tpe) => v match {
        case p: SSigmaProp =>
          assert(tpe == SSigmaProp)
          val resV = liftConst(p)
          RCCostedPrim(resV, opCost(resV, Nil, costOfSigmaTree(p)), SizeSigmaProposition)
        case bi: SBigInt =>
          assert(tpe == SBigInt)
          val resV = liftConst(bi)
          withConstantSize(resV, opCost(resV, Nil, costOf(c)))
        case p: SGroupElement =>
          assert(tpe == SGroupElement)
          val resV = liftConst(p)
          withConstantSize(resV, opCost(resV, Nil, costOf(c)))
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
          RCCostedPrim(boxV, opCost(boxV, Nil, costOf(c)), sizeOfData(box))
        case tree: special.sigma.AvlTree =>
          val treeV = liftConst(tree)
          RCCostedPrim(treeV, opCost(treeV, Nil, costOf(c)), SizeAvlTree)
        case s: String =>
          val resV = toRep(s)(stypeToElem(tpe).asInstanceOf[Elem[String]])
          RCCostedPrim(resV, opCost(resV, Nil, costOf(c)), SizeString)
        case _ =>
          val resV = toRep(v)(stypeToElem(tpe))
          withConstantSize(resV, opCost(resV, Nil, costOf(c)))
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
        val resV: Ref[SigmaProp] = sigmaDslBuilder.proveDlog(vC.value)
        val cost = opCost(resV, Array(vC.cost), CostOfProveDlog)
        RCCostedPrim(resV, cost, SizeSigmaProposition)

      case CreateProveDHTuple(In(_gv), In(_hv), In(_uv), In(_vv)) =>
        val gvC = asRep[Costed[GroupElement]](_gv)
        val hvC = asRep[Costed[GroupElement]](_hv)
        val uvC = asRep[Costed[GroupElement]](_uv)
        val vvC = asRep[Costed[GroupElement]](_vv)
        val resV: Ref[SigmaProp] = sigmaDslBuilder.proveDHTuple(gvC.value, hvC.value, uvC.value, vvC.value)
        val cost = opCost(resV, Array(gvC.cost, hvC.cost, uvC.cost, vvC.cost), CostOfDHTuple)
        RCCostedPrim(resV, cost, SizeSigmaProposition)

      case sigmastate.Exponentiate(In(_l), In(_r)) =>
        val l = asRep[Costed[GroupElement]](_l)
        val r = asRep[Costed[BigInt]](_r)
        val value = l.value.exp(r.value)
        val cost = opCost(value, Array(l.cost, r.cost), costOf(node))
        RCCostedPrim(value, cost, SizeGroupElement)

      case sigmastate.MultiplyGroup(In(_l), In(_r)) =>
        val l = asRep[Costed[GroupElement]](_l)
        val r = asRep[Costed[GroupElement]](_r)
        val value = l.value.multiply(r.value)
        val cost = opCost(value, Array(l.cost, r.cost), costOf(node))
        RCCostedPrim(value, cost, SizeGroupElement)

      case Values.GroupGenerator =>
        SigmaDslBuilderCoster(costedGlobal, SGlobal.groupGeneratorMethod, Nil)

      case sigmastate.ByteArrayToBigInt(In(_arr)) =>
        val arrC = asRep[Costed[Coll[Byte]]](_arr)
        val arr = arrC.value
        val value = sigmaDslBuilder.byteArrayToBigInt(arr)
        val size = arrC.size.dataSize
        val cost = opCost(value, Array(arrC.cost), costOf(node) + costOf("new_BigInteger_per_item", node.opType) * size.toInt)
        RCCostedPrim(value, cost, SizeBigInt)

      case sigmastate.LongToByteArray(In(_x)) =>
        val xC = asRep[Costed[Long]](_x)
        val col = sigmaDslBuilder.longToByteArray(xC.value) // below we assume col.length == typeSize[Long]
        val cost = opCost(col, Array(xC.cost), costOf(node))
        LongBytesInfo.mkCostedColl(col, cost)

      // opt.get =>
      case utxo.OptionGet(In(_opt)) =>
        OptionCoster(_opt, SOption.GetMethod, Nil)

      // opt.isDefined =>
      case utxo.OptionIsDefined(In(_opt)) =>
        OptionCoster(_opt, SOption.IsDefinedMethod, Nil)

      // opt.getOrElse =>
      case utxo.OptionGetOrElse(In(_opt), In(_default)) =>
        OptionCoster(_opt, SOption.GetOrElseMethod, Array(_default))

      case SelectField(In(_tup), fieldIndex) =>
        val eTuple = _tup.elem.eVal.asInstanceOf[Elem[_]]
        CheckTupleType(IR)(eTuple)
        eTuple match {
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
// TODO soft-fork: implement similar to Pair case
//          case se: StructElem[_] =>
//            val tup = asRep[Costed[Struct]](_tup)
//            val fn = STuple.componentNameByIndex(fieldIndex - 1)
//            val v = tup.value.getUntyped(fn)
//            val c = opCost(v, Seq(tup.cost), costedBuilder.SelectFieldCost)
//            val s: RSize[Any] = ???
//            RCCostedPrim(v, c, s)
        }

      case Values.Tuple(InSeq(Seq(x, y))) =>
        val v = Pair(x, y)
        val costs = Array(x.cost, y.cost, CostTable.newPairValueCost: Ref[Int])
        val c = mkNormalizedOpCost(v, costs)
        RCCostedPair(x, y, c)

      case Values.Tuple(InSeq(items)) =>
        val fields = items.zipWithIndex.map { case (x, i) => (s"_${i+1}", x)}
        val value = struct(fields)
        val cost = opCost(value, items.map(_.cost), costedBuilder.ConstructTupleCost)
        RCostedStruct(value, cost)

      case node: BooleanTransformer[_] =>
        val tpeIn = node.input.tpe.elemType
        val eIn = stypeToElem(tpeIn)
        val xs = asRep[CostedColl[Any]](eval(node.input))
        val eAny = xs.elem.asInstanceOf[CostedElem[Coll[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val conditionC = asRep[CostedFunc[Unit, Any, SType#WrappedType]](evalNode(ctx, env, node.condition))
        val condC = conditionC.func
        val (calcF, costF) = splitCostedFunc2(condC, okRemoveIsValid = true)
        val sizeF = condC.sliceSize
        val sizes = xs.sizes
        val len = sizes.length
        val cost = if (tpeIn.isConstantSize) {
          val predicateCost: Ref[Int] = Apply(costF, Pair(IntZero, constantTypeSize(eAny)), false)
          len * (predicateCost + CostTable.lambdaInvoke)
        } else {
          colBuilder.replicate(len, IntZero).zip(sizes).map(costF).sum(intPlusMonoid) + len * CostTable.lambdaInvoke
        }
        val res = costedBooleanTransformer[Any](node, xs, conditionC, calcF, cost)
        res

      case MapCollection(input, sfunc) =>
        val inputC = evalNode(ctx, env, input)
        val mapper = evalNode(ctx, env, sfunc)
        val res = CollCoster(inputC, SCollection.MapMethod, Array(mapper))
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

          val foldOpC = fun { in: Ref[CostedPair[s, a]] =>
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
        RCCostedColl(vals, costs, sizes, opCost(vals, Array(inputC.valuesCost), costOf(op)))

      case Append(In(_col1), In(_col2)) =>
        val col1 = asRep[CostedColl[Any]](_col1)
        val col2 = asRep[CostedColl[Any]](_col2)
        val values = col1.values.append(col2.values)
        val costs = col1.costs.append(col2.costs)
        val sizes = col1.sizes.append(col2.sizes)
        RCCostedColl(values, costs, sizes, opCost(values, Array(col1.cost, col2.cost), costOf(node)))

      case Filter(input, p) =>
        val inputC = evalNode(ctx, env, input)
        val pC = evalNode(ctx, env, p)
        val res = CollCoster(inputC, SCollection.FilterMethod, Array(pC))
        res

      case Terms.Apply(f, Seq(x)) if f.tpe.isFunc =>
        val fC = asRep[CostedFunc[Unit, Any, Any]](evalNode(ctx, env, f))
        val xC = asRep[Costed[Any]](evalNode(ctx, env, x))
        f.tpe.asFunc.tRange match {
          case _: SCollectionType[_] =>
            val (calcF, costF, sizeF) = splitCostedCollFunc(asRep[CostedCollFunc[Any,Any]](fC.func))
            val value = xC.value
            val values: RColl[Any] = Apply(calcF, value, false)
            val costRes: Ref[(Coll[Int], Int)] = Apply(costF, Pair(xC.cost, xC.size), false)
            val sizes: RColl[Size[Any]] = Apply(sizeF, xC.size, false)
            RCCostedColl(values, costRes._1, sizes, costRes._2)
//          case optTpe: SOption[_] =>
//            val (calcF, costF, sizeF) = splitCostedOptionFunc(asRep[CostedOptionFunc[Any,Any]](fC.func))
//            val value = xC.value
//            val values: Ref[WOption[Any]] = Apply(calcF, value, false)
//            val costRes: Ref[(WOption[Int], Int)] = Apply(costF, Pair(value, Pair(xC.cost, xC.dataSize)), false)
//            val sizes: Ref[WOption[Long]]= Apply(sizeF, Pair(value, xC.dataSize), false)
//            RCCostedOption(values, costRes._1, sizes, costRes._2)
          case _ =>
            val calcF = fC.sliceCalc
            val costF = fC.sliceCost
            val sizeF = fC.sliceSize
            val value = xC.value
            val y: Ref[Any] = Apply(calcF, value, false)
            val c: Ref[Int] = opCost(y, Array(fC.cost, xC.cost), asRep[Int](Apply(costF, Pair(IntZero, xC.size), false)) + CostTable.lambdaInvoke)
            val s: Ref[Size[Any]]= Apply(sizeF, xC.size, false)
            RCCostedPrim(y, c, s)
        }

      case opt: OptionValue[_] =>
        error(s"Option constructors are not supported: $opt", opt.sourceContext.toOption)

      case CalcBlake2b256(In(input)) =>
        val bytesC = asRep[Costed[Coll[Byte]]](input)
        val res = sigmaDslBuilder.blake2b256(bytesC.value)
        val cost = opCost(res, Array(bytesC.cost), perKbCostOf(node, bytesC.size.dataSize))
        HashInfo.mkCostedColl(res, cost)
      case CalcSha256(In(input)) =>
        val bytesC = asRep[Costed[Coll[Byte]]](input)
        val res = sigmaDslBuilder.sha256(bytesC.value)
        val cost = opCost(res, Array(bytesC.cost), perKbCostOf(node, bytesC.size.dataSize))
        HashInfo.mkCostedColl(res, cost)

      case utxo.SizeOf(In(xs)) =>
        xs.elem.eVal.asInstanceOf[Any] match {
          case ce: CollElem[a,_] =>
            val xsC = asRep[Costed[Coll[a]]](xs)
            val v = xsC.value.length
            RCCostedPrim(v, opCost(v, Array(xsC.cost), costOf(node)), SizeInt)
          case se: StructElem[_] =>
            val xsC = asRep[Costed[Struct]](xs)
            val v = se.fields.length
            RCCostedPrim(v, opCost(v, Array(xsC.cost), costOf(node)), SizeInt)
          case pe: PairElem[a,b] =>
            val xsC = asRep[Costed[(a,b)]](xs)
            val v: Ref[Int] = 2
            RCCostedPrim(v, opCost(v, Array(xsC.cost), costOf(node)), SizeInt)
        }

      case ByIndex(xs, i, defaultOpt) =>
        val xsC = asRep[CostedColl[Any]](eval(xs))
        val iC = asRep[Costed[Int]](eval(i))
        val iV = iC.value
        val size = if (xs.tpe.elemType.isConstantSize)
            constantTypeSize(xsC.elem.eItem)
        else {
          CheckIsSupportedIndexExpression(IR)(xs, i, iV)
          xsC.sizes(iV)
        }

        defaultOpt match {
          case Some(defaultValue) =>
            val defaultC = asRep[Costed[Any]](eval(defaultValue))
            val default = defaultC.value
            val value = xsC.value.getOrElse(iV, default)
            val cost = opCost(value, Array(xsC.cost, iC.cost, defaultC.cost), costOf(node))
            RCCostedPrim(value, cost, size)
          case None =>
            val value = xsC.value(iV)
            RCCostedPrim(value, opCost(value, Array(xsC.cost, iC.cost), costOf(node)), size)
        }

      case SigmaPropIsProven(p) =>
        val pC = asRep[Costed[SigmaProp]](eval(p))
        val v = pC.value.isValid
        val c = opCost(v, Array(pC.cost), costOf(node))
        RCCostedPrim(v, c, SizeBoolean)
      case SigmaPropBytes(p) =>
        val pC = asRep[Costed[SigmaProp]](eval(p))
        val v = pC.value.propBytes
        SigmaPropBytesInfo.mkCostedColl(v, opCost(v, Array(pC.cost), costOf(node)))

      case utxo.ExtractId(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val id = boxC.value.id
        HashInfo.mkCostedColl(id, opCost(id, Array(boxC.cost), costOf(node)))
      case utxo.ExtractBytesWithNoRef(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val v = boxC.value.bytesWithoutRef
        BoxBytesWithoutRefsInfo.mkCostedColl(v, opCost(v, Array(boxC.cost), costOf(node)))
      case utxo.ExtractAmount(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val v = boxC.value.value
        val c = opCost(v, Array(boxC.cost), costOf(node))
        RCCostedPrim(v, c, SizeLong)
      case utxo.ExtractScriptBytes(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val bytes = boxC.value.propositionBytes
        BoxPropositionBytesInfo.mkCostedColl(bytes, opCost(bytes, Array(boxC.cost), costOf(node)))
      case utxo.ExtractBytes(In(box)) =>
        val boxC = asRep[Costed[Box]](box)
        val bytes = boxC.value.bytes
        BoxBytesInfo.mkCostedColl(bytes, opCost(bytes, Array(boxC.cost), costOf(node)))
      case utxo.ExtractCreationInfo(In(box)) =>
        BoxCoster(box, SBox.creationInfoMethod, Nil)
      case utxo.ExtractRegisterAs(In(box), regId, optTpe) =>
        implicit val elem = stypeToElem(optTpe.elemType).asInstanceOf[Elem[Any]]
        val i: RCosted[Int] = RCCostedPrim(regId.number.toInt, IntZero, SizeInt)
        BoxCoster(box, SBox.getRegMethod, Array(i), Array(liftElem(elem)))

      case BoolToSigmaProp(bool) =>
        val boolC = eval(bool)
        val value = sigmaDslBuilder.sigmaProp(boolC.value)
        val cost = opCost(value, Array(boolC.cost), costOf(node))
        RCCostedPrim(value, cost, SizeSigmaProposition)

      case AtLeast(bound, input) =>
        val inputC = asRep[CostedColl[SigmaProp]](evalNode(ctx, env, input))
        if (inputC.values.length.isConst) {
          val inputCount = valueFromRep(inputC.values.length)
          if (inputCount > AtLeast.MaxChildrenCount)
            error(s"Expected input elements count should not exceed ${AtLeast.MaxChildrenCount}, actual: $inputCount", node.sourceContext.toOption)
        }
        val boundC = eval(bound)
        val res = sigmaDslBuilder.atLeast(boundC.value, inputC.values)
        val cost = opCost(res, Array(boundC.cost, inputC.cost), costOf(node))
        RCCostedPrim(res, cost, SizeSigmaProposition)

      case op: ArithOp[t] if op.tpe == SBigInt =>
        import OpCodes._
        val xC = asRep[Costed[BigInt]](eval(op.left))
        val yC = asRep[Costed[BigInt]](eval(op.right))
        val opName = op.opName
        var v: Ref[BigInt] = null;
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
        val c = opCost(v, Array(xC.cost, yC.cost), costOf(op))
        RCCostedPrim(v, c, SizeBigInt)

      case op: ArithOp[t] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToEndoBinOp(op.opCode, et)
        val x = evalNode(ctx, env, op.left)
        val y = evalNode(ctx, env, op.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          val v = ApplyBinOp(binop, x.value, y.value)
          withConstantSize(v, opCost(v, Array(x.cost, y.cost), costOf(op)))
        }

      case LogicalNot(input) =>
        val inputC = evalNode(ctx, env, input)
        val v = ApplyUnOp(Not, inputC.value)
        withConstantSize(v, opCost(v, Array(inputC.cost), costOf(node)))

      case ModQ(input) =>
        val inputC = asRep[Costed[BigInt]](eval(input))
        val v = inputC.value.modQ
        RCCostedPrim(v, opCost(v, Array(inputC.cost), costOf(node)), SizeBigInt)

      case ModQArithOp(l, r, code) =>
        val lC = asRep[Costed[BigInt]](eval(l))
        val rC = asRep[Costed[BigInt]](eval(r))
        val v = code match {
          case OpCodes.PlusModQCode => lC.value.plusModQ(rC.value)
          case OpCodes.MinusModQCode => lC.value.minusModQ(rC.value)
          case code => error(s"unknown code for modular arithmetic op: $code")
        }
        RCCostedPrim(v, opCost(v, Array(lC.cost, rC.cost), costOf(node)), SizeBigInt)

      case OR(input) => input match {
        case ConcreteCollection(items, tpe) =>
          val itemsC = items.map(item => eval(adaptSigmaBoolean(item)))
          val res = sigmaDslBuilder.anyOf(colBuilder.fromItems(itemsC.map(_.value): _*))
          val costs = itemsC.map(_.cost)
          val nOps = costs.length - 1
          val cost = opCost(res, costs, perItemCostOf(node, nOps))
          withConstantSize(res, cost)
        case _ =>
          val inputC = asRep[CostedColl[Boolean]](eval(input))
          val res = sigmaDslBuilder.anyOf(inputC.value)
          val nOps = inputC.sizes.length - 1
          val cost = opCost(res, Array(inputC.cost), perItemCostOf(node, nOps))
          withConstantSize(res, cost)
      }

      case AND(input) => input match {
        case ConcreteCollection(items, tpe) =>
          val itemsC = items.map(item => eval(adaptSigmaBoolean(item)))
          val res = sigmaDslBuilder.allOf(colBuilder.fromItems(itemsC.map(_.value): _*))
          val costs = itemsC.map(_.cost)
          val nOps = costs.length - 1
          val cost = opCost(res, costs, perItemCostOf(node, nOps))
          withConstantSize(res, cost)
        case _ =>
          val inputC = tryCast[CostedColl[Boolean]](eval(input))
          val res = sigmaDslBuilder.allOf(inputC.value)
          val nOps = inputC.sizes.length - 1
          val cost = opCost(res, Array(inputC.cost), perItemCostOf(node, nOps))
          withConstantSize(res, cost)
      }

      case XorOf(input) => input match {
        case ConcreteCollection(items, tpe) =>
          val itemsC = items.map(item => eval(item))
          val res = sigmaDslBuilder.xorOf(colBuilder.fromItems(itemsC.map(_.value): _*))
          val costs = itemsC.map(_.cost)
          val nOps = costs.length - 1
          val cost = opCost(res, costs, perItemCostOf(node, nOps))
          withConstantSize(res, cost)
        case _ =>
          val inputC = tryCast[CostedColl[Boolean]](eval(input))
          val res = sigmaDslBuilder.xorOf(inputC.value)
          val nOps = inputC.sizes.length - 1
          val cost = opCost(res, Array(inputC.cost), perItemCostOf(node, nOps))
          withConstantSize(res, cost)
      }

      case BinOr(l, r) =>
        val lC = evalNode(ctx, env, l)
        val rC = RCostedThunk(Thunk(evalNode(ctx, env, r)), IntZero)
        val v = Or.applyLazy(lC.value, rC.value)
        val c = opCost(v, Array(lC.cost, rC.cost), costOf(node))
        withConstantSize(v, c)

      case BinAnd(l, r) =>
        val lC = evalNode(ctx, env, l)
        val rC = RCostedThunk(Thunk(evalNode(ctx, env, r)), IntZero)
        val v = And.applyLazy(lC.value, rC.value)
        val c = opCost(v, Array(lC.cost, rC.cost), costOf(node))
        withConstantSize(v, c)

      case BinXor(l, r) =>
        val lC = evalNode(ctx, env, l)
        val rC = evalNode(ctx, env, r)
        val v = BinaryXorOp.apply(lC.value, rC.value)
        val c = opCost(v, Array(lC.cost, rC.cost), costOf(node))
        withConstantSize(v, c)

      case neg: Negation[SNumericType]@unchecked =>
        val tpe = neg.input.tpe
        val et = stypeToElem(tpe)
        val op = NumericNegate(elemToNumeric(et))(et)
        val inputC = evalNode(ctx, env, neg.input)
        inputC match { case x: RCosted[a] =>
            val v = ApplyUnOp(op, x.value)
          withConstantSize(v, opCost(v, Array(x.cost), costOf(neg)))
        }

      case SigmaAnd(items) =>
        val itemsC = items.map(eval)
        val res = sigmaDslBuilder.allZK(colBuilder.fromItems(itemsC.map(s => asRep[SigmaProp](s.value)): _*))
        val costs = itemsC.map(_.cost)
        val cost = opCost(res, costs, perItemCostOf(node, costs.length))
        RCCostedPrim(res, cost, SizeSigmaProposition)

      case SigmaOr(items) =>
        val itemsC = items.map(eval)
        val res = sigmaDslBuilder.anyZK(colBuilder.fromItems(itemsC.map(s => asRep[SigmaProp](s.value)): _*))
        val costs = itemsC.map(_.cost)
        val cost = opCost(res, costs, perItemCostOf(node, costs.length))
        RCCostedPrim(res, cost, SizeSigmaProposition)

      case If(c, t, e) =>
        val cC = evalNode(ctx, env, c)
        def tC = evalNode(ctx, env, t)
        def eC = evalNode(ctx, env, e)
        val resV = IF (cC.value) THEN tC.value ELSE eC.value
        val resCost = opCost(resV, Array(cC.cost, tC.cost, eC.cost), costOf("If", SFunc(Vector(SBoolean, If.tT, If.tT), If.tT)))
        RCCostedPrim(resV, resCost, tC.size) // TODO costing: implement tC.size max eC.size

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
              opCost(value, Array(x.cost, y.cost), opcost)
            }
            else opCost(value, Array(x.cost, y.cost), perKbCostOf(node, x.size.dataSize + y.size.dataSize))
          val res = withConstantSize(value, cost)
          res
        }

      case l @ Terms.Lambda(_, Seq((n, argTpe)), tpe, Some(body)) =>
        val eArg = stypeToElem(argTpe).asInstanceOf[Elem[Any]]
        val eCostedArg = elemToCostedElem(eArg)
        val f = fun { x: Ref[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), body)
        }(Lazy(eCostedArg))
        val eRes = f.elem.eRange.eVal
        mkCostedFunc(f, opCost(f, Nil, costOf(node)), l.tpe.dataSize(SType.DummyValue), eArg, eRes)

      case l @ FuncValue(Seq((n, argTpe)), body) =>
        val eArg = stypeToElem(argTpe).asInstanceOf[Elem[Any]]
        val xElem = elemToCostedElem(eArg)
        val f = fun { x: Ref[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), body)
        }(Lazy(xElem))
        val eRes = f.elem.eRange.eVal
        mkCostedFunc(f, opCost(f, Nil, costOf(node)), l.tpe.dataSize(SType.DummyValue), eArg, eRes)

      case col @ ConcreteCollection(InSeqUnzipped(vs, cs, ss), elemType) =>
        implicit val eAny = stypeToElem(elemType).asInstanceOf[Elem[Any]]
        val values = colBuilder.fromItems(vs: _*)(eAny)
        val costs = colBuilder.replicate(cs.length, IntZero)
        val sizes = colBuilder.fromItems(ss: _*)(sizeElement(eAny))
//        val args = vs.zip(cs).map { case (v,c) =>
//          if (c.node.isInstanceOf[OpCost]) c else opCost(v, Nil, c)
//        }
        val args = mutable.ArrayBuilder.make[Ref[Int]]
        val uniqueArgs = scalan.AVHashMap[Ref[Any], (Ref[Any], Ref[Int])](10)
        vs.zip(cs).foreach { vc =>
          uniqueArgs.get(vc._1) match {
            case Nullable((v, c)) =>
              assert(c == vc._2, s"Inconsistent costed graph for $col: argument $v have different costs $c and ${vc._2}")
            case _ =>
              val v = vc._1
              val c = vc._2
              uniqueArgs.put(v, vc)
              val arg = if (c.node.isInstanceOf[OpCost]) c else opCost(v, Nil, c)
              args += arg
          }
        }
        val accCost = opCost(values, args.result(), costOf(col) + CostTable.concreteCollectionItemCost * ss.length)
        RCCostedColl(values, costs, sizes, accCost)

      case sigmastate.Upcast(In(inputC), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        val res = upcast(inputC.value)(elem)
        withConstantSize(res, opCost(res, Array(inputC.cost), costOf(node)))

      case sigmastate.Downcast(In(inputC), tpe) =>
        val elem = stypeToElem(tpe.asNumType)
        val res = downcast(inputC.value)(elem)
        withConstantSize(res, opCost(res, Array(inputC.cost), costOf(node)))

      case ByteArrayToLong(In(arr)) =>
        val arrC = asRep[Costed[Coll[Byte]]](arr)
        val value = sigmaDslBuilder.byteArrayToLong(arrC.value)
        val cost = opCost(value, Array(arrC.cost), costOf(node))
        RCCostedPrim(value, cost, SizeLong)

      case Xor(InCollByte(l), InCollByte(r)) =>
        val values = colBuilder.xor(l.value, r.value)
        val sizes = r.sizes
        val len = sizes.length
        val costs = colBuilder.replicate(len, IntZero)
        val cost = opCost(values, Array(l.cost, r.cost), perKbCostOf(node, len.toLong))
        RCCostedColl(values, costs, sizes, cost)

      case SubstConstants(InCollByte(bytes), InCollInt(positions), InCollAny(newValues)) =>
        val values = sigmaDslBuilder.substConstants(bytes.values, positions.values, newValues.values)(AnyElement)
        val len = bytes.size.dataSize + newValues.size.dataSize
        val cost = opCost(values, Array(bytes.cost, positions.cost, newValues.cost), perKbCostOf(node, len))
        mkCostedColl(values, len.toInt, cost)

      case DecodePoint(InCollByte(bytes)) =>
        val res = sigmaDslBuilder.decodePoint(bytes.values)
        RCCostedPrim(res, opCost(res, Array(bytes.cost), costOf(node)), SizeGroupElement)

      // fallback rule for MethodCall, should be the last case in the list
      case Terms.MethodCall(obj, method, args, typeSubst) if method.objType.coster.isDefined =>
        val objC = eval(obj)
        val argsC = args.map(eval)
        val elems = typeSubst.values.toSeq.map(tpe => liftElem(stypeToElem(tpe).asInstanceOf[Elem[Any]]))
        method.objType.coster.get(IR)(objC, method, argsC, elems)

      case _ =>
        error(s"Don't know how to evalNode($node)", node.sourceContext.toOption)
    }

    if (okMeasureOperationTime) {
      val t = System.nanoTime()

      val rule = ruleStack.head   // always non empty at this point
      ruleStack = ruleStack.tail  // pop current rule
      assert(rule.node.opCode == node.opCode, s"Inconsistent stack at ${rule :: ruleStack}")

      val ruleFullTime = t - rule.outerStart  // full time spent in this rule

      // add this time to parent's innerTime (if any parent)
      if (ruleStack.nonEmpty) {
        val parent = ruleStack.head
        parent.innerTime += ruleFullTime
      } else {
        // top level
//        println(s"Top time: $ruleFullTime")
      }
      
      val ruleSelfTime = ruleFullTime - rule.innerTime
      node match {
        case mc: Terms.MethodCall =>
          val m = mc.method
          ComplexityTableStat.addMcTime(m.objType.typeId, m.methodId, ruleSelfTime)
        case _ =>
          ComplexityTableStat.addOpTime(node.opCode, ruleSelfTime)
      }
    }
    val resC = asRep[Costed[T#WrappedType]](res)
    onTreeNodeCosted(ctx, env, node, resC)
    resC
  }

  def buildCostedGraph[T](envVals: Map[Any, SValue], tree: SValue): Ref[Costed[Context] => Costed[T]] = {
    try {
      assert(ruleStack.isEmpty)
      fun { ctxC: RCosted[Context] =>
        val env = envVals.mapValues(v => evalNode(ctxC, Map.empty, v))
        val res = asCosted[T](evalNode(ctxC, env, tree))
        res
      }
    }
    finally {
      // ensure leaving it in initial state
      ruleStack = Nil
    }
  }

  def cost[T](env: ScriptEnv, typed: SValue): Ref[Costed[Context] => Costed[T]] = {
    val cg = buildCostedGraph[T](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    cg
  }

  def error(msg: String) = throw new CosterException(msg, None)
  def error(msg: String, srcCtx: Option[SourceContext]) = throw new CosterException(msg, srcCtx)
}
