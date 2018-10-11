package sigmastate.lang

import java.math.BigInteger
import scala.language.implicitConversions
import scala.language.existentials
import com.sun.org.apache.xml.internal.serializer.ToUnknownStream
import org.bouncycastle.math.ec.ECPoint

import scalan.{Lazy, SigmaLibrary}
import scalan.util.CollectionUtil.TraversableOps
import org.ergoplatform._
import scapi.sigma.ProveDiffieHellmanTuple
import sigmastate.SCollection.SByteArray
import sigmastate.Values.Value.Typed
import sigmastate._
import sigmastate.Values.{OptionValue, Constant, SValue, SigmaPropConstant, Value, ByteArrayConstant, TaggedVariableNode, SigmaBoolean, ConcreteCollection}
import sigmastate.interpreter.CryptoConstants
import sigmastate.lang.Terms._
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.exceptions.CosterException
import sigmastate.serialization.OpCodes
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo._
import sigmastate.eval.{OrderingOps, NumericOps, DataCosting}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scalan.compilation.GraphVizConfig
import SType._

trait Costing extends SigmaLibrary with DataCosting {
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
  import ConcreteCostedBuilder._
  import Costed._;
  import CostedContext._
  import CostedPrim._;
  import CostedFunc._;
  import CostedCol._;
  import CostedBox._;
  import CostedBuilder._;
  import CostedOption._;
  import CostedNone._
  import CostedSome._
  import ProveDlogEvidence._
  import SigmaDslBuilder._
  import TrivialSigma._
  import MonoidBuilderInst._
  
  def opcodeToArithOpName(opCode: Byte): String = opCode match {
    case OpCodes.PlusCode     => "+"
    case OpCodes.MinusCode    => "-"
    case OpCodes.MultiplyCode => "*"
    case OpCodes.DivisionCode => "/"
    case OpCodes.ModuloCode   => "%"
    case _ => error(s"Cannot find ArithOpName for opcode $opCode")
  }

  case class CostOf(opName: String, opType: SFunc) extends BaseDef[Int]

  def costOf(opName: String, opType: SFunc): Rep[Int] = CostOf(opName, opType)

  def constCost(tpe: SType): Rep[Int] = tpe match {
    case f: SFunc =>
      costOf(s"Lambda", Constant[SType](0.asWrappedType, tpe).opType)
    case _ =>
      costOf(s"Const", Constant[SType](0.asWrappedType, tpe).opType)
  }

  def constCost[T: Elem]: Rep[Int] = {
    val tpe = elemToSType(element[T])
    constCost(tpe)
  }

  def costOf(v: SValue): Rep[Int] = {
    val opTy = v.opType
    v match {
      case ArithOp(_, _, opCode) =>
        costOf(opcodeToArithOpName(opCode), opTy)
      case l: Terms.Lambda =>
        constCost(l.tpe)
      case c @ Constant(_, tpe) =>
        costOf(s"Const", opTy)
      case v =>
        val className = v.getClass.getSimpleName
        costOf(className, opTy)
    }
  }

  object ConstantSizeType {
    def unapply(e: Elem[_]): Option[SType] = {
      val tpe = elemToSType(e)
      if (tpe.isConstantSize) Some(tpe)
      else None
    }
  }

  override def sizeOf[T](value: Rep[T]): Rep[Long] = (value, value.elem) match {
    case (_, _: BoxElem[_]) =>
      value.asRep[Box].dataSize
    case (_xs, ce: ColElem[a,_]) =>
      val xs = _xs.asRep[Col[a]]
      implicit val eA = xs.elem.eItem
      val tpe = elemToSType(eA)
      if (tpe.isConstantSize)
        typeSize(tpe) * xs.length.toLong
      else
        xs.map(fun(sizeOf(_))).sum(costedBuilder.monoidBuilder.longPlusMonoid)
    case (_, ConstantSizeType(tpe)) =>
      typeSize(tpe)
    case _ =>
      super.sizeOf(value)
  }

  /** Graph node to represent computation of size for types with isConstantSize == true. */
  case class TypeSize(tpe: SType) extends BaseDef[Long] {
    assert(tpe.isConstantSize, s"Expected isConstantSize type but was TypeSize($tpe)")
  }

  def typeSize(tpe: SType): Rep[Long] = TypeSize(tpe)

  def typeSize[T: Elem]: Rep[Long] = {
    val tpe = elemToSType(element[T])
    assert(tpe.isConstantSize)
    typeSize(tpe)
  }

  def showECPoint(p: ECPoint) = {
    val rawX = p.getRawXCoord.toString.substring(0, 6)
    val rawY = p.getRawYCoord.toString.substring(0, 6)
    s"ECPoint($rawX,$rawY,...)"
  }

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case CostOf(name, opType) => s"CostOf($name:$opType)"
    case WECPointConst(p) => showECPoint(p)
    case _ => super.formatDef(d)
  }

  type RCostedCol[T] = Rep[CostedCol[T]]
  type RCostedFunc[A,B] = Rep[Costed[A] => Costed[B]]

  implicit class RCostedFuncOps[A,B](f: RCostedFunc[A,B]) {
    implicit val eA = f.elem.eDom.eVal
    def sliceCalc: Rep[A => B] = fun { x: Rep[A] => f(RCostedPrim(x, 0, 0L)).value }
    def sliceCost: Rep[((Int,Long)) => Int] = fun { x: Rep[(Int, Long)] => f(RCostedPrim(variable[A], x._1, x._2)).cost }
    def sliceSize: Rep[Long => Long] = fun { x: Rep[Long] => f(RCostedPrim(variable[A], 0, x)).dataSize }
  }

  implicit def extendCostedElem[A](e: Elem[Costed[A]]): CostedElem[A,_] = e.asInstanceOf[CostedElem[A,_]]
  implicit def extendCostedFuncElem[E,A,B](e: Elem[CostedFunc[E,A,B]]): CostedFuncElem[E,A,B] = e.asInstanceOf[CostedFuncElem[E,A,B]]

  def splitCostedFunc[A,B](f: RCostedFunc[A,B]): (Rep[A=>B], Rep[((Int, Long)) => Int], Rep[Long => Long]) = {
    implicit val eA = f.elem.eDom.eVal
    val calcF = f.sliceCalc
    val costF = f.sliceCost
    val sizeF = f.sliceSize
    (calcF, costF, sizeF)
  }

  type RWOption[T] = Rep[WOption[T]]

  object CostedFoldExtractors {
    val CM = CostedMethods
    val COM = CostedOptionMethods
    val WOM = WOptionMethods
    type Result = (RWOption[A], Th[B], RFunc[A, Costed[B]]) forSome {type A; type B}

    object IsGetCost {
      def unapply(d: Def[_]): Option[Result] = d match {
        case CM.cost(COM.get(WOM.fold(opt, th, f))) =>
          Some((opt, th, f)).asInstanceOf[Option[Result]]
        case _ => None
      }
    }
    object IsGetDataSize {
      def unapply(d: Def[_]): Option[Result] = d match {
        case CM.dataSize(COM.get(WOM.fold(opt, th, f))) =>
          Some((opt, th, f)).asInstanceOf[Option[Result]]
        case _ => None
      }
    }
    object IsGet {
      def unapply(d: Def[_]): Option[Result] = d match {
        case COM.get(WOM.fold(opt, th, f)) =>
          Some((opt, th, f)).asInstanceOf[Option[Result]]
        case _ => None
      }
    }
  }
  override val performViewsLifting = false

  type CostedThunk[T] = Th[Costed[T]]

  override def rewriteDef[T](d: Def[T]): Rep[_] = {
    val CBM = ColBuilderMethods
    val SigmaM = SigmaPropMethods
    val CCM = CostedColMethods
    val CostedM = CostedMethods
    val CostedOptionM = CostedOptionMethods
    val WOptionM = WOptionMethods

    d match {
      case ApplyBinOpLazy(op, l, Def(ThunkDef(root @ SigmaM.isValid(prop), sch))) if l.elem == BooleanElement =>
        val l1 = RTrivialSigma(l.asRep[Boolean]).asRep[SigmaProp]
        // don't need new Thunk because sigma logical ops always strict
        val res = if (op == And)
          l1 && prop
        else
          l1 || prop
        res.isValid

      case TrivialSigmaCtor(SigmaM.isValid(p)) => p

      case CCM.mapCosted(xs: RCostedCol[a], _f: RCostedFunc[_, b]) =>
        val f = _f.asRep[Costed[a] => Costed[b]]
        val (calcF, costF, sizeF) = splitCostedFunc[a, b](f)
        val vals = xs.values.map(calcF)
        val costs = xs.costs.zip(xs.sizes).map(costF)
        val sizes = xs.sizes.map(sizeF)
        RCostedCol(vals, costs, sizes, xs.valuesCost)

//      case CCM.filterCosted(xs: RCostedCol[a], _f: RCostedFunc[_,_]) =>
//        val f = _f.asRep[Costed[a] => Costed[Boolean]]
//        val (calcF, costF, _) = splitCostedFunc[a, Boolean](f)
//        val vals = xs.values.filter(calcF)
//        val costs = xs.costs.zip(xs.sizes).map(costF)  // TODO how to filter our sizes and costs
//        val sizes = colBuilder.replicate(xs.sizes.length, 1L)
//        RCostedCol(vals, costs, sizes, xs.valuesCost)

      case CostedM.value(Def(CostedFuncCtor(_, func: RCostedFunc[a,b], _,_))) =>
        func.sliceCalc

//      case CostedFoldExtractors.IsGetCost(opt: RWOption[a], th: CostedThunk[b]@unchecked, f) =>
//        implicit val eA = opt.elem.eItem
//        opt.fold(Thunk { forceThunkByMirror(th).cost }, fun { x: Rep[a] => f.asRep[a => Costed[b]](x).cost })
//
//      case CostedFoldExtractors.IsGetDataSize(opt: RWOption[a], th: CostedThunk[b]@unchecked, f) =>
//        implicit val eA = opt.elem.eItem
//        opt.fold(Thunk { forceThunkByMirror(th).dataSize }, fun { x: Rep[a] => f.asRep[a => Costed[b]](x).dataSize })

      case CostedFoldExtractors.IsGet(opt: RWOption[a], _th, _f) =>
        implicit val eA = opt.elem.eItem
        val th = _th.asRep[Thunk[CostedOption[Any]]]
        val f = _f.asRep[a => CostedOption[Any]]
        f(opt.get).get

      case _ => super.rewriteDef(d)
    }
  }

  lazy val BigIntegerElement: Elem[WBigInteger] = wBigIntegerElement

  override def toRep[A](x: A)(implicit eA: Elem[A]):Rep[A] = eA match {
    case BigIntegerElement => Const(x)
    case _ => super.toRep(x)
  }

  var defCounter = 0
  var defTime: Long = 0
//  override def def_unapply[T](e: Rep[T]) = {
//    defCounter += 1
//    val start = System.nanoTime()
//    val res = super.def_unapply(e)
//    val end = System.nanoTime()
//    defTime += (end - start)
//    res
//  }

  /** Should be specified in the final cake */
  val builder: sigmastate.lang.SigmaBuilder
  import builder._
  lazy val compiler = new SigmaCompiler(builder)

  val colBuilder: Rep[ColBuilder] = RColOverArrayBuilder()
  val costedBuilder = RConcreteCostedBuilder()
  val intPlusMonoid = costedBuilder.monoidBuilder.intPlusMonoid
  val longPlusMonoid = costedBuilder.monoidBuilder.longPlusMonoid
  import Cost._

  def splitFast[T,R](f: Rep[T => Costed[R]]): Rep[(T => R, T => Int)] = {
    implicit val eT = f.elem.eDom
    f.getLambda
    val calc = fun { x: Rep[T] =>
      val y = f(x);
      val res = y.value match {
        case SigmaPropMethods.isValid(p) => p
        case v => v
      }
      res.asRep[R]
    }
    val cost = fun { x: Rep[T] => f(x).cost }
    Pair(calc, cost)
  }

  def split2[T,R](f: Rep[T => Costed[R]]): Rep[(T => R, T => Int)] = {
    implicit val eT = f.elem.eDom
    val calc = fun { x: Rep[T] =>
      val y = f(x);
      val res = y.value match {
        case SigmaPropMethods.isValid(p) => p
        case v => v
      }
      res.asRep[R]
    }
    val cost = fun { x: Rep[T] => f(x).cost }
    Pair(calc, cost)
  }

  def split3[T,R](f: Rep[T => Costed[R]]): Rep[(T => R, (T => Int, T => Long))] = {
    implicit val eT = f.elem.eDom
    val calc = fun { x: Rep[T] =>
      val y = f(x);
      val res = y.value match {
        case SigmaPropMethods.isValid(p) => p
        case v => v
      }
      res.asRep[R]
    }
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
    case SBigInt => wBigIntegerElement
    case SBox => boxElement
    case SGroupElement => wECPointElement
    case SSigmaProp => sigmaPropElement
    case c: SCollection[a] => colElement(stypeToElem(c.elemType))
    case _ => error(s"Don't know how to convert SType $t to Elem")
  }).asElem[T#WrappedType]

  def elemToSType[T](e: Elem[T]): SType = (e match {
    case BooleanElement => SBoolean
    case ByteElement => SByte
    case ShortElement => SShort
    case IntElement => SInt
    case LongElement => SLong
    case _: WBigIntegerElem[_] => SBigInt
    case _: WECPointElem[_] => SGroupElement
    case oe: WOptionElem[_,_] => sigmastate.SOption(elemToSType(oe.eItem))
    case _: BoxElem[_] => SBox
    case _: SigmaPropElem[_] => SSigmaProp
    case ce: ColElem[_,_] => SCollection(elemToSType(ce.eItem))
    case fe: FuncElem[_,_] => SFunc(elemToSType(fe.eDom), elemToSType(fe.eRange))
    case _ => error(s"Don't know how to convert Elem $e to SType")
  })

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
  private val elemToNumericMap = Map[Elem[_], Numeric[_]](
    (ByteElement, numeric[Byte]),
    (ShortElement, numeric[Short]),
    (IntElement, numeric[Int]),
    (LongElement, numeric[Long]),
    (BigIntegerElement, numeric[BigInteger])
  )
  private val elemToIntegralMap = Map[Elem[_], Integral[_]](
    (ByteElement, integral[Byte]),
    (ShortElement, integral[Short]),
    (IntElement, integral[Int]),
    (LongElement, integral[Long]),
    (BigIntegerElement, integral[BigInteger])
  )
  private val elemToOrderingMap = Map[Elem[_], Ordering[_]](
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
    RCostedPrim(v, c, s)
  }

  implicit class RepOps[T](x: Rep[T]) {
    def dataSize: Rep[Long] = sizeOf(x)
  }

  type Env = Map[String, Sym]

  import sigmastate._
  import scapi.sigma.{DLogProtocol, DiffieHellmanTupleProtocol}

  private def evalNode[T <: SType](ctx: Rep[CostedContext], env: Map[String, RCosted[_]], node: Value[T]): RCosted[T#WrappedType] = {
    import MonoidBuilderInst._; import WOption._; import WSpecialPredef._
    def eval[T <: SType](node: Value[T]): RCosted[T#WrappedType] = evalNode(ctx, env, node)
    def withDefaultSize[T](v: Rep[T], cost: Rep[Int]): RCosted[T] = CostedPrimRep(v, cost, v.dataSize)
    object In { def unapply(v: SValue): Option[RCosted[Any]] = Some(evalNode(ctx, env, v).asRep[Costed[Any]]) }
    val res: Rep[Any] = node match {
      case Ident(n, _) =>
        env.getOrElse(n, !!!(s"Variable $n not found in environment $env"))

      case _: DLogProtocol.ProveDlog | _: ProveDiffieHellmanTuple =>
        eval(SigmaPropConstant(node.asSigmaBoolean))

      case c @ Constant(v, tpe) => v match {
        case p: DLogProtocol.ProveDlog =>
          val ge = evalNode(ctx, env, p.value).asRep[Costed[WECPoint]]
          val resV: Rep[SigmaProp] = RProveDlogEvidence(ge.value)
          withDefaultSize(resV, ge.cost + costOf(SigmaPropConstant(p)))
        case bi: BigInteger =>
          assert(tpe == SBigInt)
          val resV = liftConst(bi)
          withDefaultSize(resV, costOf(c))
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
              RCostedCol(resVals, resCosts, resSizes, costOf(c))
          }
        case _ =>
          val resV = toRep(v)(stypeToElem(tpe))
          withDefaultSize(resV, costOf(c))
      }

      case Height  => ctx.HEIGHT
      case Inputs  => ctx.INPUTS
      case Outputs => ctx.OUTPUTS
      case Self    => ctx.SELF
      case LastBlockUtxoRootHash => ctx.LastBlockUtxoRootHash

      case op @ TaggedVariableNode(id, tpe) =>
        val resV = ctx.getVar(id)(stypeToElem(tpe))
        withDefaultSize(resV, costOf(op))

      case Terms.Block(binds, res) =>
        var curEnv = env
        for (Val(n, _, b) <- binds) {
          if (curEnv.contains(n)) error(s"Variable $n already defined ($n = ${curEnv(n)}")
          val bC = evalNode(ctx, curEnv, b)
//          val bC = valDef(n, Thunk(evalNode(ctx, curEnv, b)))
          curEnv = curEnv + (n -> bC)
        }
        val res1 = evalNode(ctx, curEnv, res)
        res1

      // Rule: allOf(arr) --> AND(arr)
      case Terms.Apply(AllSym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
        eval(mkAND(arr))

      // Rule: anyOf(arr) --> OR(arr)
      case Terms.Apply(AnySym, Seq(arr: Value[SCollection[SBoolean.type]]@unchecked)) =>
        eval(mkOR(arr))

      case Terms.Apply(Blake2b256Sym, Seq(arg: Value[SByteArray]@unchecked)) =>
        eval(mkCalcBlake2b256(arg))

      case Terms.Apply(Sha256Sym, Seq(arg: Value[SByteArray]@unchecked)) =>
        eval(mkCalcSha256(arg))

      case Terms.Apply(IsMemberSym, Seq(tree: Value[SAvlTree.type]@unchecked, key: Value[SByteArray]@unchecked, proof: Value[SByteArray]@unchecked)) =>
        eval(mkIsMember(tree, key, proof))

      case Terms.Apply(ProveDlogSym, Seq(g: Value[SGroupElement.type]@unchecked)) =>
        eval(mkProveDlog(g))

      case Terms.Apply(LongToByteArraySym, Seq(arg: Value[SLong.type]@unchecked)) =>
        eval(mkLongToByteArray(arg))

      case sigmastate.Upcast(Constant(value, tpe), toTpe: SNumericType) =>
        eval(mkConstant(toTpe.upcast(value.asInstanceOf[AnyVal]), toTpe))

      // Rule: col.size --> SizeOf(col)
      case Select(obj, "size", _) =>
        if (obj.tpe.isCollectionLike)
          eval(mkSizeOf(obj.asValue[SCollection[SType]]))
        else
          error(s"The type of $obj is expected to be Collection to select 'size' property")

      // Rule: proof.isValid --> IsValid(proof)
      case Select(p, SSigmaProp.IsValid, _) if p.tpe == SSigmaProp =>
        eval(SigmaPropIsValid(p.asSigmaProp))

      // Rule: proof.propBytes --> ProofBytes(proof)
      case Select(p, SSigmaProp.PropBytes, _) if p.tpe == SSigmaProp =>
        eval(SigmaPropBytes(p.asSigmaProp))

      // box.regName[regType].isDefined =>
      case sel @ Select(Select(Typed(box, SBox), regName, _), sigmastate.SOption.IsDefinedMethod.name, Some(regType)) =>
        val regId = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel"))
        val boxC = evalNode(ctx, env, box).asRep[Costed[Box]]
        val elem = stypeToElem(regType)
        val valueOpt = boxC.value.getReg(regId.number.toInt)(elem)
        val baseCost = boxC.cost + Cost.ExtractRegister
        val v = valueOpt.isDefined
        withDefaultSize(v, baseCost)

      case sel @ Select(Select(Typed(box, SBox), regName, _), sigmastate.SOption.Get, Some(regType)) =>
        val reg = ErgoBox.registerByName.getOrElse(regName,
          error(s"Invalid register name $regName in expression $sel"))
        eval(mkExtractRegisterAs(box.asBox, reg, sigmastate.SOption(regType)))

//      case sel @ Terms.Apply(Select(Select(Typed(box, SBox), regName, _), sigmastate.SOption.GetOrElseMethod.name, Some(_)), Seq(arg)) =>
//        val reg = ErgoBox.registerByName.getOrElse(regName,
//          error(s"Invalid register name $regName in expression $sel"))
//        eval(mkExtractRegisterAs(box.asBox, reg, arg.tpe, Some(arg)))

      case sel @ Select(obj, field, _) if obj.tpe == SBox =>
        (obj.asValue[SBox.type], field) match {
          case (box, SBox.Value) => eval(mkExtractAmount(box))
          case (box, SBox.PropositionBytes) => eval(mkExtractScriptBytes(box))
          case (box, SBox.Id) => eval(mkExtractId(box))
          case (box, SBox.Bytes) => eval(mkExtractBytes(box))
          case (box, SBox.BytesWithNoRef) => eval(mkExtractBytesWithNoRef(box))
          case _ => error(s"Invalid access to Box property in $sel: field $field is not found")
        }

      case Select(obj: SigmaBoolean, field, _) =>
        field match {
          case SigmaBoolean.PropBytes => eval(SigmaPropBytes(SigmaPropConstant(obj)))
          case SigmaBoolean.IsValid => eval(SigmaPropIsValid(SigmaPropConstant(obj)))
        }

      case Select(obj, "value", Some(SLong)) if obj.tpe == SBox =>
        eval(mkExtractAmount(obj.asValue[SBox.type]))

      case Select(tuple, fn, _) if tuple.tpe.isTuple && fn.startsWith("_") =>
        val index = fn.substring(1).toByte
        eval(mkSelectField(tuple.asTuple, index))

      case Terms.Apply(Select(col, "slice", _), Seq(from, until)) =>
        eval(mkSlice(col.asValue[SCollection[SType]], from.asIntValue, until.asIntValue))

      case op @ Slice(In(input), In(from), In(until)) =>
        val inputC = input.asRep[CostedCol[Any]]
        val fromC = from.asRep[Costed[Int]]
        val untilC = until.asRep[Costed[Int]]
        val f = fromC.value
        val u = untilC.value
        val vals = inputC.values.slice(f, u)
        val costs = inputC.costs.slice(f, u)
        val sizes = inputC.sizes.slice(f, u)
        RCostedCol(vals, costs, sizes, inputC.valuesCost + costOf(op))

      case Terms.Apply(Select(col, "where", _), Seq(Terms.Lambda(_, Seq((n, t)), _, Some(body)))) =>
        val input = col.asValue[SCollection[SType]]
        val cond = body.asValue[SBoolean.type]
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(ctx, env, input).asRep[CostedCol[Any]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val condC = fun { x: Rep[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), cond)
        }
        val res = inputC.filterCosted(condC)
        res

      case Terms.Apply(Select(col, method @ (SCollection.ExistsMethod.name | SCollection.ForallMethod.name), _),
                       Seq(Terms.Lambda(_, Seq((n, t)), _, Some(body)))) =>
        val input = col.asValue[SCollection[SType]]
        val cond = body.asValue[SBoolean.type]
        val eItem = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(ctx, env, input).asRep[Costed[Col[Any]]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eItem == eAny, s"Types should be equal: but $eItem != $eAny")
        val Pair(condCalc, condCost) = split2(fun { x: Rep[Any] =>
          evalNode(ctx, env + (n -> CostedPrimRep(x, 0, x.dataSize)), cond)
        })
        val inputV = inputC.value
        val res = method match {
          case SCollection.ExistsMethod.name => inputV.exists(condCalc)
          case SCollection.ForallMethod.name => inputV.forall(condCalc)
        }
        val cost = inputC.cost + inputV.map(condCost).sum(costedBuilder.monoidBuilder.intPlusMonoid)
        withDefaultSize(res, cost)

      case Terms.Apply(Select(col,"map", _), Seq(Terms.Lambda(_, Seq((n, t)), _, Some(mapper)))) =>
        val input = col.asValue[SCollection[SType]]
        val eIn = stypeToElem(input.tpe.elemType)
        val inputC = evalNode(ctx, env, input).asRep[CostedCol[Any]]
        implicit val eAny = inputC.elem.asInstanceOf[CostedElem[Col[Any],_]].eVal.eA
        assert(eIn == eAny, s"Types should be equal: but $eIn != $eAny")
        val mapperC = fun { x: Rep[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), mapper)
        }
        val res = inputC.mapCosted(mapperC)
        res

//      case Terms.Apply(Select(col,"fold", _), Seq(zero, Terms.Lambda(Seq((zeroArg, tZero), (opArg, tOp)), _, Some(body)))) =>
//        val taggedZero = mkTaggedVariable(21, tZero)
//        val taggedOp = mkTaggedVariable(22, tOp)
//        val env1 = env ++ Seq(zeroArg -> taggedZero, opArg -> taggedOp)
//        some(mkFold(col.asValue[SCollection[SType]], taggedZero.varId, zero, taggedOp.varId, body))(env1)

      case Terms.Apply(Select(col,"getOrElse", _), Seq(index, defaultValue)) =>
        val index1 = index.asValue[SInt.type]
        val defaultValue1 = defaultValue.asValue[SType]
        eval(mkByIndex(col.asValue[SCollection[SType]], index1, Some(defaultValue1)))

      case Terms.Apply(f, Seq(x)) if f.tpe.isFunc =>
        val fC = evalNode(ctx, env, f).asRep[CostedFunc[Unit, Any, Any]]
        val xC = evalNode(ctx, env, x).asRep[Costed[Any]]
        if (f.tpe.asFunc.tRange.isCollection) {
          ???
        }
        else {
          val (calcF, costF, sizeF) = splitCostedFunc(fC.func)
          val y: Rep[Any] = Apply(calcF, xC.value, false)
          val c: Rep[Int] = Apply(costF, Pair(xC.cost, xC.dataSize), false)
          val s: Rep[Long]= Apply(sizeF, xC.dataSize, false)
          RCostedPrim(y, c, s)
        }

      case opt: OptionValue[_] =>
        error(s"Option constructors are not supported: $opt")

      case utxo.SizeOf(xs) =>
        val xsC = evalNode(ctx, env, xs).asRep[Costed[Col[Any]]]
        val v = xsC.value.length
        withDefaultSize(v, xsC.cost + costOf(node))
      case ByIndex(xs, i, None) =>
        val xsC = evalNode(ctx, env, xs).asRep[CostedCol[Any]]
        val iC = evalNode(ctx, env, i).asRep[Costed[Int]]
        val iV = iC.value
        val size = xsC.sizes(iV)
        CostedPrimRep(xsC.value(iV), xsC.cost + iC.cost + costOf(node), size)

      case SigmaPropIsValid(p) =>
        val pC = evalNode(ctx, env, p).asRep[Costed[SigmaProp]]
        val v = pC.value.isValid
        val c = pC.cost + costOf(node)
        val s = pC.dataSize // NOTE: we pass SigmaProp's size, this is handled in buildCostedGraph
        CostedPrimRep(v, c, s)
      case SigmaPropBytes(p) =>
        val pC = evalNode(ctx, env, p).asRep[Costed[SigmaProp]]
        val v = pC.value.propBytes
        withDefaultSize(v, pC.cost + costOf(node))
      case utxo.ExtractAmount(box) =>
        val boxC = evalNode(ctx, env, box).asRep[Costed[Box]]
        withDefaultSize(boxC.value.value, boxC.cost + costOf(node))
      case utxo.ExtractScriptBytes(box) =>
        val boxC = evalNode(ctx, env, box).asRep[Costed[Box]]
        val bytes = boxC.value.propositionBytes
        withDefaultSize(bytes, boxC.cost + costOf(node))

      case utxo.ExtractRegisterAs(In(box), regId, optTpe) =>
        val boxC = box.asRep[CostedBox]
        implicit val elem = stypeToElem(optTpe.elemType).asElem[Any]
        val valueOpt = boxC.getReg(regId.number.toInt)(elem)
        val res = valueOpt.get
        res
//        if (default.isDefined) {
//          val d = evalNode(ctx, env, default.get)
//          val (v,c) = (RWSpecialPredef.optionGetOrElse(valueOpt, d.value), boxC.cost + d.cost + costOf(node))
//          costedBuilder.costedValue(v, RWSpecialPredef.some(c))
//        } else {
//          val c = boxC.cost + costOf(node)
//          val vC = costedBuilder.costedValue(valueOpt, RWSpecialPredef.some(c)).asRep[CostedOption[Any]]
//          vC.get
//        }

      case op: ArithOp[t] if op.tpe == SBigInt =>
        import OpCodes._
        op.opCode match {
          case PlusCode | MinusCode =>
            val x = evalNode(ctx, env, op.left).asRep[Costed[WBigInteger]]
            val y = evalNode(ctx, env, op.right).asRep[Costed[WBigInteger]]
            val resSize = x.dataSize.max(y.dataSize) + 1L  // according to algorithm in BigInteger.add()
            val isPlus = op.opCode == PlusCode
            val opName = if (isPlus) "+" else "-"
            val cost = x.cost + y.cost + costOf(op) + costOf(opName + "_per_item", op.opType) * resSize.toInt
            val resValue = if (isPlus) x.value.add(y.value) else x.value.subtract(y.value)
            RCostedPrim(resValue, cost, resSize)
//          case MinusCode => NumericMinus(elemToNumeric(eT))(eT)
//          case MultiplyCode => NumericTimes(elemToNumeric(eT))(eT)
//          case DivisionCode => IntegralDivide(elemToIntegral(eT))(eT)
//          case ModuloCode => IntegralMod(elemToIntegral(eT))(eT)
          case _ => error(s"Cannot perform Costing.evalNode($op)")
        }
      case op: ArithOp[t] =>
        val tpe = op.left.tpe
        val et = stypeToElem(tpe)
        val binop = opcodeToEndoBinOp(op.opCode, et)
        val x = evalNode(ctx, env, op.left)
        val y = evalNode(ctx, env, op.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          withDefaultSize(ApplyBinOp(binop, x.value, y.value), x.cost + y.cost + costOf(op))
        }
      case OR(input) => input.matchCase(
        cc => {
          val itemsC = cc.items.map(evalNode(ctx, env, _))
          val res = sigmaDslBuilder.anyOf(colBuilder.apply(itemsC.map(_.value): _*))
          val cost = itemsC.map(_.cost).reduce((x, y) => x + y) + costOf(node)
          withDefaultSize(res, cost)
//          val headC = evalNode(ctx, env, cc.items(0))
//          val tailC = cc.items.iterator.drop(1).map(x => evalCostedBlock(evalNode(ctx, env, x)))
//          val itemsC = (Iterator.single(headC) ++ tailC).toIndexedSeq
//          val res = sigmaDslBuilder.anyOf(colBuilder.apply(new mutable.WrappedArray.ofRef(itemsC.map(_.value).toArray):_*))
//          val cost = itemsC.map(_.cost).reduce((x, y) => x + y) + OrDeclaration
//          CostedPrimRep(res, cost)
        },
        const => ???,
        tup => ???
      )
      case AND(input) => input.matchCase(
        cc => {
          val itemsC = cc.items.map(evalNode(ctx, env, _))
          val res = sigmaDslBuilder.allOf(colBuilder.apply(itemsC.map(_.value): _*))
          val cost = itemsC.map(_.cost).reduce((x, y) => x + y) + costOf(node)
          withDefaultSize(res, cost)
        },
        const => ???,
        tup => ???
      )

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

      case op: Relation[t,_] if op.tpe == SBigInt =>
        import OpCodes._
        op.opCode match {
          case GtCode =>
            val x = evalNode(ctx, env, op.left).asRep[Costed[WBigInteger]]
            val y = evalNode(ctx, env, op.right).asRep[Costed[WBigInteger]]
            val resSize = x.dataSize.min(y.dataSize)
            val cost = x.cost + y.cost + costOf(op) + costOf(">_per_item", op.opType) * resSize.toInt
            RCostedPrim(x.value.compareTo(y.value) > 0, cost, resSize)
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
        val x = evalNode(ctx, env, rel.left)
        val y = evalNode(ctx, env, rel.right)
        (x, y) match { case (x: RCosted[a], y: RCosted[b]) =>
          withDefaultSize(
            binop.apply(x.value, y.value.asRep[t#WrappedType]),
            x.cost + y.cost + costOf(rel)
          )
        }

      case If(c, t, e) =>
        val cC = evalNode(ctx, env, c)
        def tC = evalNode(ctx, env, t)
        def eC = evalNode(ctx, env, e)
        val resV = IF (cC.value) THEN tC.value ELSE eC.value
        val resCost = cC.cost + (tC.cost max eC.cost) + costOf(node)
        withDefaultSize(resV, resCost)
        
      case l @ Terms.Lambda(_, Seq((n, argTpe)), tpe, Some(body)) =>
        implicit val eAny = stypeToElem(argTpe).asElem[Any]
        val f = fun { x: Rep[Costed[Any]] =>
          evalNode(ctx, env + (n -> x), body)
        }
        RCostedFunc(RCostedPrim((), 0, 0L), f, costOf(node), l.tpe.dataSize(0.asWrappedType))

      case col @ ConcreteCollection(items, elemTpe) =>
        val (vals, costs, sizes) = items.mapUnzip { x: SValue =>
          val xC = eval(x)
          (xC.value, xC.cost, xC.dataSize)
        }
        val resV = colBuilder.apply(vals: _*)
        val resC = colBuilder.apply(costs: _*).sum(intPlusMonoid) + costOf(col)
        withDefaultSize(resV, resC)
      case _ =>
        error(s"Don't know how to evalNode($node)")
    }
    res.asRep[Costed[T#WrappedType]]
  }


  def buildCostedGraph[T <: SType](envVals: Map[String, SValue], tree: Value[T]): Rep[Context => Costed[T#WrappedType]] = {
    fun { ctx: Rep[Context] =>
      val ctxC = RCostedContext(ctx)
      val env = envVals.mapValues(v => evalNode(ctxC, Map(), v))
      val res = evalNode(ctxC, env, tree)
      res
//      val res1 = res match {
//        case RCostedPrim(SigmaPropMethods.isValid(p), Def(ApplyBinOp(op, l, r)), s) if op.isInstanceOf[NumericPlus[_]] =>
//          RCostedPrim(p, l.asRep[Int], s)
//        case _ => res
//      }
//      res1.asRep[Costed[T#WrappedType]]
    }
  }

  def cost(env: Map[String, Any], code: String) = {
    val typed = compiler.typecheck(env, code)
    val cg = buildCostedGraph[SType](env.mapValues(builder.liftAny(_).get), typed)
    cg
  }

  def cost(env: Map[String, Any], typed: SValue) = {
    val cg = buildCostedGraph[SType](env.mapValues(builder.liftAny(_).get), typed)
    cg
  }

  def error(msg: String) = throw new CosterException(msg, None)
}
