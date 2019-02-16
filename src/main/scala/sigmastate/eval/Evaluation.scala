package sigmastate.eval

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform._
import sigmastate._
import sigmastate.Values.{Value, GroupElementConstant, SigmaBoolean, Constant}
import sigmastate.lang.Terms.OperationId
import sigmastate.utxo.CostTableStat

import scala.reflect.ClassTag
import scala.util.Try
import sigmastate.SType._
import sigmastate.interpreter.CryptoConstants.EcPointType
import special.sigma.InvalidType
import scalan.{Nullable, RType}
import scalan.RType._
import sigma.types.PrimViewType
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.{ProveDHTuple, DLogProtocol}
import special.sigma.Extensions._
import sigma.util.Extensions._

trait Evaluation extends RuntimeCosting { IR =>
  import Context._
  import SigmaProp._
  import Coll._
  import CReplColl._
  import Box._
  import AvlTree._
  import CollBuilder._
  import SigmaDslBuilder._
  import CostedBuilder._
  import MonoidBuilder._
  import WBigInteger._
  import WArray._
  import WOption._
  import GroupElement._
  import Liftables._

  val okPrintEvaluatedEntries: Boolean = false

  private val ContextM = ContextMethods
  private val SigmaM = SigmaPropMethods
  private val CollM = CollMethods
  private val BoxM = BoxMethods
  private val AvlM = AvlTreeMethods
  private val CBM = CollBuilderMethods
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
    case CollM.length(_) | CollM.map(_,_) | CollM.sum(_,_) | CollM.zip(_,_) | CollM.slice(_,_,_) | CollM.apply(_,_) | CollM.append(_,_) =>
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
  import special.sigma.{Context => SigmaContext}

  type ContextFunc[T <: SType] = SigmaContext => Value[T]

  val sigmaDslBuilderValue: CostingSigmaDslBuilder
  val costedBuilderValue: special.collection.CostedBuilder
  val monoidBuilderValue: special.collection.MonoidBuilder

  def getDataEnv: DataEnv = {
    val env = Map[Sym, AnyRef](
      sigmaDslBuilder -> sigmaDslBuilderValue,
      sigmaDslBuilder.Colls -> sigmaDslBuilderValue.Colls,
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
      case col: special.collection.Coll[_] => s"Coll(${trim(col.toArray).mkString(",")})"
      case p: SGroupElement => p.showToString
      case ProveDlog(GroupElementConstant(g)) => s"ProveDlog(${showECPoint(g)})"
      case ProveDHTuple(
              GroupElementConstant(g), GroupElementConstant(h), GroupElementConstant(u), GroupElementConstant(v)) =>
        s"ProveDHT(${showECPoint(g)},${showECPoint(h)},${showECPoint(u)},${showECPoint(v)})"
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
            val valueInCtx = invokeUnlifted(ctx.elem, mc, dataEnv)
            val data = valueInCtx match {
              case Some(Constant(v, `declaredTpe`)) =>
                Some(Evaluation.toDslData(v, declaredTpe, ctxObj.isCost)(IR))
              case opt @ Some(v) => opt
              case None => None
              case _ => throw new InvalidType(s"Expected Constant($declaredTpe) but found $valueInCtx")
            }
            out(data)
          case d @ BoxM.getReg(box, _, elem) =>
            val ctxObj = dataEnv(ctxSym).asInstanceOf[CostingDataContext]
            val mc = d.asInstanceOf[MethodCall]
            val declaredTpe = elemToSType(elem)
            val valueInReg = invokeUnlifted(box.elem, mc, dataEnv)
            val data = valueInReg match {
              case Some(Constant(v, `declaredTpe`)) =>
                Some(Evaluation.toDslData(v, declaredTpe, ctxObj.isCost)(IR))
              case Some(v) =>
                valueInReg
              case None => None
              case _ => throw new InvalidType(
                s"Expected Some(Constant($declaredTpe)) but found $valueInReg value of register: $d")
            }
            out(data)
          case Const(x) => out(x.asInstanceOf[AnyRef])

          case Tup(In(a), In(b)) => out((a,b))
          case First(In(p: Tuple2[_,_])) => out(p._1)
          case Second(In(p: Tuple2[_,_])) => out(p._2)
          case FieldApply(In(data), IsTupleFN(i)) => data match {
            case coll: special.collection.Coll[a] =>
              out(coll(i - 1))
            case tup: Product =>
              out(tup.productElement(i - 1))
          }
          case wc: LiftedConst[_,_] => out(wc.constValue)
          case _: SigmaDslBuilder | _: CollBuilder | _: CostedBuilder | _: IntPlusMonoid | _: LongPlusMonoid =>
            out(dataEnv.getOrElse(te.sym, !!!(s"Cannot resolve companion instance for $te")))
//          case SigmaM.propBytes(prop) =>
//            val sigmaBool = dataEnv(prop).asInstanceOf[SigmaBoolean]
//            out(sigmaDslBuilderValue.Colls.fromArray(sigmaBool.bytes))
          case SigmaM.isValid(In(prop: AnyRef)) =>
            out(prop)

//          case SigmaM.and_sigma_&&(In(l: SigmaBoolean), In(r: SigmaBoolean)) =>
//            out(CAND.normalized(Seq(l, r)))

//          case SigmaM.or_sigma_||(In(l: SigmaBoolean), In(r: SigmaBoolean)) =>
//            out(COR.normalized(Seq(l, r)))

//          case SigmaM.and_bool_&&(In(l: SigmaBoolean), In(b: Boolean)) =>
//            if (b) {
//              out(l)
//            } else
//              out(TrivialProp.FalseProp)
//
//          case SigmaM.or_bool_||(In(l: SigmaBoolean), In(b: Boolean)) =>
//            if (b)
//              out(TrivialProp.TrueProp)
//            else {
//              out(l)
//            }
//          case SigmaM.lazyAnd(In(l: SigmaBoolean), In(y)) =>
//            val th = y.asInstanceOf[() => SigmaBoolean]
//            out(AND(l, th()).function(null, null))
//          case SigmaM.lazyOr(In(l: SigmaBoolean), In(y)) =>
//            val th = y.asInstanceOf[() => SigmaBoolean]
//            out(OR(l, th()).function(null, null))

//          case SDBM.anyZK(_, In(items: special.collection.Coll[SigmaBoolean]@unchecked)) =>
//            out(COR.normalized(items.toArray.toSeq))
//          case SDBM.allZK(_, In(items: special.collection.Coll[SigmaBoolean]@unchecked)) =>
//            out(CAND.normalized(items.toArray.toSeq))
//          case SDBM.atLeast(dsl, In(bound: Int), In(children: special.collection.Coll[SigmaBoolean]@unchecked)) =>
//            out(AtLeast.reduce(bound, children.toArray.toSeq))
//          case SDBM.sigmaProp(_, In(b: Boolean)) =>
//            val res = sigmastate.TrivialProp(b)
//            out(res)
          case SDBM.substConstants(_,
            In(input: special.collection.Coll[Byte]@unchecked),
            In(positions: special.collection.Coll[Int]@unchecked),
            In(newVals: special.collection.Coll[Any]@unchecked), _) =>
            val typedNewVals = newVals.toArray.map(v => builder.liftAny(v) match {
              case Nullable(v) => v
              case _ => sys.error(s"Cannot evaluate substConstants($input, $positions, $newVals): cannot lift value $v")
            })
            val byteArray = SubstConstants.eval(input.toArray, positions.toArray, typedNewVals)
            out(sigmaDslBuilderValue.Colls.fromArray(byteArray))

          case AM.length(In(arr: Array[_])) => out(arr.length)
          case CBM.replicate(In(b: special.collection.CollBuilder), In(n: Int), xSym @ In(x)) =>
            out(b.replicate(n, x)(asType[Any](xSym.elem.sourceType)))

          // NOTE: This is a fallback rule which should be places AFTER all other MethodCall patterns
          case mc @ MethodCall(obj, m, args, _) =>
            val dataRes = invokeUnlifted(obj.elem, mc, dataEnv)
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
              val res = resEnv(y)
              res
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

          case SDBM.sigmaProp(_, In(isValid: Boolean)) =>
            val res = CostingSigmaProp(sigmastate.TrivialProp(isValid))
            out(res)
          case SDBM.proveDlog(_, In(g: EcPointType)) =>
            val res = CostingSigmaProp(DLogProtocol.ProveDlog(g))
            out(res)
          case SDBM.proveDHTuple(_, In(g: EcPointType), In(h: EcPointType), In(u: EcPointType), In(v: EcPointType)) =>
            val res = CostingSigmaProp(ProveDHTuple(g, h, u, v))
            out(res)

          case CReplCollCtor(valueSym @ In(value), In(len: Int)) =>
            val res = sigmaDslBuilderValue.Colls.replicate(len, value)(asType[Any](valueSym.elem.sourceType))
            out(res)
          case costOp: CostOf =>
            out(costOp.eval)
          case SizeOf(sym @ In(data)) =>
            val tpe = elemToSType(sym.elem)
            val size = tpe match {
              case SAvlTree =>
                data.asInstanceOf[special.sigma.AvlTree].dataSize
              case _ => data match {
                case w: WrapperOf[_] =>
                  tpe.dataSize(w.wrappedValue.asWrappedType)
                case _ =>
                  tpe.dataSize(data.asWrappedType)
              }
            }
            out(size)
          case TypeSize(tpe) =>
            assert(tpe.isConstantSize)
            val size = tpe.dataSize(SType.DummyValue)
            out(size)
          case Downcast(In(from), eTo) =>
            val tpe = elemToSType(eTo).asNumType
            if (tpe == SBigInt)
              out(sigmaDslBuilderValue.BigInt(SBigInt.downcast(from.asInstanceOf[AnyVal])))
            else
              out(tpe.downcast(from.asInstanceOf[AnyVal]))
          case Upcast(In(from), eTo) =>
            val tpe = elemToSType(eTo).asNumType
            if (tpe == SBigInt)
              out(sigmaDslBuilderValue.BigInt(SBigInt.upcast(from.asInstanceOf[AnyVal])))
            else
              out(tpe.upcast(from.asInstanceOf[AnyVal]))

          case SimpleStruct(_, fields) =>
            val items = fields.map { case (_, In(fieldValue)) => fieldValue }.toArray
            out(sigmaDslBuilderValue.Colls.fromArray(items)(AnyType))

          case _ =>
            !!!(s"Don't know how to evaluate($te)")
        }
        if (okMeasureOperationTime) {
          val endTime = System.nanoTime()
          val estimatedTime = endTime - startTime
          te.sym.getMetadata(OperationIdKey) match {
            case Some(opId: OperationId) =>
              if (opId.opType.tRange.isCollection) {
                val col = res._1(res._2).asInstanceOf[SColl[Any]]
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
        case x =>
          val eRes = f.elem.eRange
          val tpeRes = elemToSType(eRes)
          val tRes = Evaluation.stypeToRType(tpeRes)
          val treeType = Evaluation.toErgoTreeType(tRes)
          val constValue = Evaluation.fromDslData(x, treeType)(IR)
          builder.mkConstant[SType](constValue.asInstanceOf[SType#WrappedType], tpeRes)
      }
    }
    res.asInstanceOf[ContextFunc[T]]
  }
}

object Evaluation {
  import special.sigma._
  import special.collection._
  import ErgoLikeContext._
  
  case class GenericRType[T <: AnyRef](classTag : ClassTag[T]) extends RType[T]

  def AnyRefRType[T <: AnyRef: ClassTag]: RType[T] = GenericRType[T](scala.reflect.classTag[T])

  def stypeToRType[T <: SType](t: T): RType[T#WrappedType] = (t match {
    case SBoolean => BooleanType
    case SByte => ByteType
    case SShort => ShortType
    case SInt => IntType
    case SLong => LongType
    case SString => StringType
    case SAny => AnyType
    case SBigInt => BigIntRType
    case SBox => BoxRType
    case SGroupElement => GroupElementRType
    case SAvlTree => AvlTreeRType
    case SSigmaProp => SigmaPropRType
    case STuple(Seq(tpeA, tpeB)) =>
      pairRType(stypeToRType(tpeA), stypeToRType(tpeB))
    case STuple(items) =>
      val types = items.toArray
      tupleRType(types.map(t => stypeToRType(t).asInstanceOf[SomeType]))
    case c: SCollectionType[a] => collRType(stypeToRType(c.elemType))
    case _ => sys.error(s"Don't know how to convert SType $t to RType")
  }).asInstanceOf[RType[T#WrappedType]]

  def rtypeToSType[T](t: RType[T]): SType = t match {
    case BooleanType => SBoolean
    case ByteType => SByte
    case ShortType => SShort
    case IntType => SInt
    case LongType => SLong
    case StringType => SString
    case AnyType => SAny
    case BigIntegerRType => SBigInt
    case ECPointRType => SGroupElement
    case AvlTreeRType => SAvlTree
    case ot: OptionType[_] => sigmastate.SOption(rtypeToSType(ot.tA))
    case BoxRType => SBox
    case SigmaPropRType => SSigmaProp
    case tup: TupleType => STuple(tup.items.map(t => rtypeToSType(t)).toIndexedSeq)
//    case st: StructType =>
////      assert(st.fieldNames.zipWithIndex.forall { case (n,i) => n == s"_${i+1}" })
//      STuple(st.fieldTypes.map(rtypeToSType(_)).toIndexedSeq)
    case ct: CollType[_] => SCollection(rtypeToSType(ct.tItem))
    case ft: FuncType[_,_] => SFunc(rtypeToSType(ft.tDom), rtypeToSType(ft.tRange))
    case pt: PairType[_,_] => STuple(rtypeToSType(pt.tFst), rtypeToSType(pt.tSnd))
    case pvt: PrimViewType[_,_] => rtypeToSType(pvt.tVal)
    case _ => sys.error(s"Don't know how to convert RType $t to SType")
  }

  /** Tries to reconstruct RType of the given value.
    * If not successfull returns failure. */
  def rtypeOf(value: Any): Try[RType[_]] = Try { value match {
    case arr if arr.getClass.isArray =>
      val itemClass = arr.getClass.getComponentType
      if (itemClass.isPrimitive) {
        val itemTag = ClassTag[Any](itemClass)
        RType.fromClassTag(itemTag)
      } else
        sys.error(s"Cannot compute rtypeOf($value): non-primitive type of array items")

    case coll: Coll[_] => collRType(coll.tItem)
    
    // all primitive types
    case v: Boolean => BooleanType
    case v: Byte  => ByteType
    case v: Short => ShortType
    case v: Int   => IntType
    case v: Long  => LongType
    case v: Char  => CharType
    case v: Float  => FloatType
    case v: Double  => DoubleType
    case v: String  => StringType
    case v: Unit  => UnitType

    case v: BigInteger => BigIntegerRType
    case n: special.sigma.BigInt => BigIntRType

    case v: ECPoint => ECPointRType
    case ge: GroupElement => GroupElementRType

    case b: ErgoBox => ErgoBoxRType
    case b: Box => BoxRType

    case avl: AvlTreeData => AvlTreeDataRType
    case avl: AvlTree => AvlTreeRType

    case sb: SigmaBoolean => SigmaBooleanRType
    case p: SigmaProp => SigmaPropRType

    case _ =>
      sys.error(s"Don't know how to compute typeOf($value)")
  }}

  /** Generic translation of any ErgoDsl type to the corresponding type used in ErgoTree. */
  def toErgoTreeType(dslType: RType[_]): RType[_] = dslType match {
    case p: PrimitiveType[_] => p
    case w: WrapperType[_] =>
      w match {
        case BigIntRType => BigIntegerRType
        case GroupElementRType => ECPointRType
        case SigmaPropRType => SigmaBooleanRType
        case BoxRType => ErgoBoxRType
        case AvlTreeRType => AvlTreeDataRType
        case _ => sys.error(s"Unknown WrapperType: $w")
      }
    case p: ArrayType[_] => arrayRType(toErgoTreeType(p.tA))
    case p: OptionType[_] => optionRType(toErgoTreeType(p.tA))
    case p: CollType[_] => arrayRType(toErgoTreeType(p.tItem))
    case p: PairType[_,_] => pairRType(toErgoTreeType(p.tFst), toErgoTreeType(p.tSnd))
    case p: EitherType[_,_] => eitherRType(toErgoTreeType(p.tA), toErgoTreeType(p.tB))
    case p: FuncType[_,_] => funcRType(toErgoTreeType(p.tDom), toErgoTreeType(p.tRange))
    case t: TupleType => tupleRType(t.items.map(x => toErgoTreeType(x)))
    case AnyType | AnyRefType | NothingType | StringType => dslType
    case _ =>
      sys.error(s"Don't know how to toErgoTreeType($dslType)")
  }

  /** Generic converter from types used in ErgoDsl to types used in ErgoTree values. */
  def fromDslData[T](value: Any, tRes: RType[T])(implicit IR: Evaluation): T = {
    val dsl = IR.sigmaDslBuilderValue
    val res = (value, tRes) match {
      case (w: WrapperOf[_], _) => w.wrappedValue
      case (coll: Coll[a], tarr: ArrayType[a1]) =>
        val tItem = tarr.tA
        coll.map[a1](x => fromDslData(x, tItem))(tItem).toArray
      case _ => value
    }
    res.asInstanceOf[T]
  }

  /** Generic converter from types used in ErgoTree values to types used in ErgoDsl. */
  def toDslData(value: Any, tpe: SType, isCost: Boolean)(implicit IR: Evaluation): Any = {
    val dsl = IR.sigmaDslBuilderValue
    (value, tpe) match {
      case (c: Constant[_], tpe) => toDslData(c.value, c.tpe, isCost)
      case (_, STuple(Seq(tpeA, tpeB))) =>
        value match {
          case tup: Tuple2[_,_] =>
            val valA = toDslData(tup._1, tpeA, isCost)
            val valB = toDslData(tup._2, tpeB, isCost)
            (valA, valB)
          case arr: Array[Any] =>
            val valA = toDslData(arr(0), tpeA, isCost)
            val valB = toDslData(arr(1), tpeB, isCost)
            (valA, valB)
        }
      case (arr: Array[a], SCollectionType(elemType)) =>
        implicit val elemRType: RType[SType#WrappedType] = Evaluation.stypeToRType(elemType)
        elemRType.asInstanceOf[RType[_]] match {
          case _: CollType[_] | _: TupleType | _: PairType[_,_] | _: WrapperType[_] =>
            val testArr = arr.map(x => toDslData(x, elemType, isCost))
            dsl.Colls.fromArray(testArr.asInstanceOf[Array[SType#WrappedType]])
          case _ =>
            dsl.Colls.fromArray(arr.asInstanceOf[Array[SType#WrappedType]])
        }
      case (arr: Array[a], STuple(items)) =>
        val res = arr.zip(items).map { case (x, t) => toDslData(x, t, isCost)}
        dsl.Colls.fromArray(res)(RType.AnyType)
      case (b: ErgoBox, SBox) => b.toTestBox(isCost)
      case (n: BigInteger, SBigInt) =>
        dsl.BigInt(n)
      case (p: ECPoint, SGroupElement) => dsl.GroupElement(p)
      case (t: SigmaBoolean, SSigmaProp) => dsl.SigmaProp(t)
      case (t: AvlTreeData, SAvlTree) => CostingAvlTree(t)
      case (x, _) => x
    }
  }

}
