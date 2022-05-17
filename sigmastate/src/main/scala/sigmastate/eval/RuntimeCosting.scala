package sigmastate.eval

import scala.language.implicitConversions
import scala.language.existentials
import scalan.{ExactIntegral, ExactNumeric, ExactOrdering, Lazy, MutableLazy, Nullable, RType, SigmaLibrary}
import scalan.util.CollectionUtil.TraversableOps
import scalan.util.Extensions.ByteOps
import org.ergoplatform._
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.CosterException
import sigmastate.serialization.OpCodes
import sigmastate.utxo._
import scalan.compilation.GraphVizConfig
import SType._
import scalan.RType._
import sigmastate.interpreter.Interpreter.ScriptEnv
import sigmastate.lang.{SourceContext, Terms}
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.collection.CollType
import special.sigma.{AvlTreeRType, BigIntRType, BoxRType, GroupElementRType, SigmaPropRType}
import special.sigma.Extensions._
import org.ergoplatform.validation.ValidationRules._
import scalan.ExactIntegral._
import scalan.ExactNumeric._
import scalan.ExactOrdering.{ByteIsExactOrdering, IntIsExactOrdering, LongIsExactOrdering, ShortIsExactOrdering}
import spire.syntax.all.cfor

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


trait RuntimeCosting extends SigmaLibrary { IR: IRContext =>
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

//  /** Pass configuration which is used by default in IRContext. */
//  val calcPass = new DefaultPass("calcPass", Pass.defaultPassConfig.copy(constantPropagation = true))
//
//  /** Pass configuration which is used during splitting cost function out of cost graph.
//    * @see `RuntimeCosting.split2` */
//  val costPass = new DefaultPass("costPass", Pass.defaultPassConfig.copy(constantPropagation = true))

/**  To enable specific configuration uncomment one of the lines above and use it in the beginPass below. */
//  beginPass(costPass)

  case class CostOf(opName: String, opType: SFunc) extends BaseDef[Int] {
    override def mirror(t: Transformer): Ref[Int] = self // TODO no HF proof

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

  def perKbCostOf(opName: String, opType: SFunc, dataSize: Ref[Long]): Ref[Int] = {
    val opNamePerKb = s"${opName}_per_kb"
    PerKbCostOf(OperationId(opNamePerKb, opType), dataSize)
  }

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
    case GroupElementConst(p) => p.showToString
    case SigmaPropConst(sp) => sp.toString
    case _ => super.formatDef(d)
  }

  type RColl[T] = Ref[Coll[T]]
  type ROption[T] = Ref[WOption[T]]
  type RCostedColl[T] = Ref[CostedColl[T]]
  type RCostedOption[T] = Ref[CostedOption[T]]
  type RFuncCosted[A,B] = Ref[Costed[A] => Costed[B]]

  type CostedCollFunc[A,B] = Costed[A] => CostedColl[B]
  type CostedOptionFunc[A,B] = Costed[A] => CostedOption[B]
  type RCostedCollFunc[A,B] = Ref[CostedCollFunc[A, B]]
  type RCostedOptionFunc[A,B] = Ref[CostedOptionFunc[A, B]]

  implicit def extendCostedFuncElem[E,A,B](e: Elem[CostedFunc[E,A,B]]): CostedFuncElem[E,A,B,_] = e.asInstanceOf[CostedFuncElem[E,A,B,_]]

  implicit def extendSizeElem[A](elem: Elem[Size[A]]): SizeElem[A, Size[A]] =
    elem.asInstanceOf[SizeElem[A, Size[A]]]

  implicit def extendCostedElem[A](elem: Elem[Costed[A]]): CostedElem[A, Costed[A]] =
    elem.asInstanceOf[CostedElem[A, Costed[A]]]

  implicit def extendCostedCollElem[A](elem: Elem[CostedColl[A]]): CostedCollElem[A, CostedColl[A]] =
    elem.asInstanceOf[CostedCollElem[A, CostedColl[A]]]

  implicit class ElemOpsForCosting(e: Elem[_]) {
    def isConstantSize: Boolean = e.sourceType.isConstantSize
  }

  private val CBM      = CollBuilderMethods
  private val SigmaM   = SigmaPropMethods
  private val SDBM     = SigmaDslBuilderMethods

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
    * HOTSPOT: executed for each node of the graph, don't beautify.
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

  /** Should be specified in the final cake */
  val builder: sigmastate.lang.SigmaBuilder
  import builder._

  /** Lazy values, which are immutable, but can be reset, so that the next time they are accessed
    * the expression is re-evaluated. Each value should be reset in onReset() method. */
  private val _sigmaDslBuilder: LazyRep[SigmaDslBuilder] = MutableLazy(variable[SigmaDslBuilder])
  @inline def sigmaDslBuilder: Ref[SigmaDslBuilder] = _sigmaDslBuilder.value

  private val _colBuilder: LazyRep[CollBuilder] = MutableLazy(variable[CollBuilder])
  @inline def colBuilder: Ref[CollBuilder] = _colBuilder.value

  private val _costedBuilder: LazyRep[CostedBuilder] = MutableLazy(RCCostedBuilder())
  @inline def costedBuilder: Ref[CostedBuilder] = _costedBuilder.value

  private val _monoidBuilder: LazyRep[MonoidBuilder] = MutableLazy(costedBuilder.monoidBuilder)
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
  }

  def removeIsProven[T,R](f: Ref[T] => Ref[R]): Ref[T] => Ref[R] = { x: Ref[T] =>
    val y = f(x);
    val res = y match {
      case SigmaPropMethods.isValid(p) => p
      case v => v
    }
    asRep[R](res)
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
  private lazy val elemToExactNumericMap = Map[Elem[_], ExactNumeric[_]](
    (ByteElement, ByteIsExactIntegral),
    (ShortElement, ShortIsExactIntegral),
    (IntElement, IntIsExactIntegral),
    (LongElement, LongIsExactIntegral),
    (bigIntElement, BigIntIsExactIntegral)
  )
  private lazy val elemToExactIntegralMap = Map[Elem[_], ExactIntegral[_]](
    (ByteElement,   ByteIsExactIntegral),
    (ShortElement,  ShortIsExactIntegral),
    (IntElement,    IntIsExactIntegral),
    (LongElement,   LongIsExactIntegral)
  )
  protected lazy val elemToExactOrderingMap = Map[Elem[_], ExactOrdering[_]](
    (ByteElement,   ByteIsExactOrdering),
    (ShortElement,  ShortIsExactOrdering),
    (IntElement,    IntIsExactOrdering),
    (LongElement,   LongIsExactOrdering),
    (bigIntElement, BigIntIsExactOrdering)
  )

  def elemToExactNumeric [T](e: Elem[T]): ExactNumeric[T]  = elemToExactNumericMap(e).asInstanceOf[ExactNumeric[T]]
  def elemToExactIntegral[T](e: Elem[T]): ExactIntegral[T] = elemToExactIntegralMap(e).asInstanceOf[ExactIntegral[T]]
  def elemToExactOrdering[T](e: Elem[T]): ExactOrdering[T] = elemToExactOrderingMap(e).asInstanceOf[ExactOrdering[T]]

  def opcodeToEndoBinOp[T](opCode: Byte, eT: Elem[T]): EndoBinOp[T] = opCode match {
    case OpCodes.PlusCode => NumericPlus(elemToExactNumeric(eT))(eT)
    case OpCodes.MinusCode => NumericMinus(elemToExactNumeric(eT))(eT)
    case OpCodes.MultiplyCode => NumericTimes(elemToExactNumeric(eT))(eT)
    case OpCodes.DivisionCode => IntegralDivide(elemToExactIntegral(eT))(eT)
    case OpCodes.ModuloCode => IntegralMod(elemToExactIntegral(eT))(eT)
    case OpCodes.MinCode => OrderingMin(elemToExactOrdering(eT))(eT)
    case OpCodes.MaxCode => OrderingMax(elemToExactOrdering(eT))(eT)
    case _ => error(s"Cannot find EndoBinOp for opcode $opCode")
  }

  def opcodeToBinOp[A](opCode: Byte, eA: Elem[A]): BinOp[A,_] = opCode match {
    case OpCodes.EqCode  => Equals[A]()(eA)
    case OpCodes.NeqCode => NotEquals[A]()(eA)
    case OpCodes.GtCode  => OrderingGT[A](elemToExactOrdering(eA))
    case OpCodes.LtCode  => OrderingLT[A](elemToExactOrdering(eA))
    case OpCodes.GeCode  => OrderingGTEQ[A](elemToExactOrdering(eA))
    case OpCodes.LeCode  => OrderingLTEQ[A](elemToExactOrdering(eA))
    case _ => error(s"Cannot find BinOp for opcode newOpCode(${opCode.toUByte-OpCodes.LastConstantCode}) and type $eA")
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

  import sigmastate._

  @inline def SigmaDsl = sigmaDslBuilderValue
  @inline def Colls = sigmaDslBuilderValue.Colls

  protected implicit def groupElementToECPoint(g: special.sigma.GroupElement): EcPointType = SigmaDsl.toECPoint(g).asInstanceOf[EcPointType]

  def constantTypeSize[T](implicit eT: Elem[T]): RSize[T] = RCSizePrim(typeSize(eT), eT)

  def withConstantSize[T](v: Ref[T], cost: Ref[Int]): RCosted[T] = RCCostedPrim(v, cost, constantTypeSize(v.elem))

  def error(msg: String) = throw new CosterException(msg, None)
  def error(msg: String, srcCtx: Option[SourceContext]) = throw new CosterException(msg, srcCtx)
}
