package sigmastate.eval

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.ValidationRules.CheckCostFuncOperation
import org.ergoplatform._
import sigmastate._
import sigmastate.Values.{Constant, GroupElementConstant, SigmaBoolean, Value}
import sigmastate.lang.Terms.OperationId
import sigmastate.utxo.CostTableStat

import scala.reflect.ClassTag
import scala.util.Try
import sigmastate.SType._
import sigmastate.interpreter.CryptoConstants.EcPointType
import scalan.{Nullable, RType}
import scalan.RType._
import sigma.types.PrimViewType
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.{DLogProtocol, ProveDHTuple}
import special.sigma.Extensions._
import sigmastate.lang.exceptions.CosterException
import sigmastate.serialization.OpCodes
import special.SpecialPredef
import special.Types._

import scala.collection.immutable.HashSet

/** This is a slice in IRContext cake which implements evaluation of graphs.
  */
trait Evaluation extends RuntimeCosting { IR: IRContext =>
  import Context._
  import SigmaProp._
  import Coll._
  import CReplColl._
  import PairOfCols._
  import AnyValue._
  import Box._
  import AvlTree._
  import CollBuilder._
  import SigmaDslBuilder._
  import WBigInteger._
  import WArray._
  import WOption._
  import WRType._
  import GroupElement._
  import Liftables._
  import WSpecialPredef._
  import Size._
  import CSizePrim._
  import SizePair._
  import CSizePair._
  import SizeColl._
  import CSizeColl._
  import SizeOption._
  import CSizeOption._
  import SizeFunc._
  import CSizeFunc._
  import SizeAnyValue._
  import CSizeAnyValue._
  import SizeSigmaProp._
  import SizeBox._
  import CSizeBox._
  import SizeContext._
  import CSizeContext._
  import OpCodes._

  val okPrintEvaluatedEntries: Boolean = false

  private val SCM = SizeContextMethods
  private val SBM = SizeBoxMethods
  private val SSPM = SizeSigmaPropMethods
  private val SAVM = SizeAnyValueMethods
  private val SizeM = SizeMethods
  private val SPairM = SizePairMethods
  private val SCollM = SizeCollMethods
  private val SOptM = SizeOptionMethods
  private val SFuncM = SizeFuncMethods
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
  private val SPCM = WSpecialPredefCompanionMethods

  private val _allowedOpCodesInCosting = HashSet[OpCode](
    Undefined,
    InputsCode,
    OutputsCode,
    TupleCode,
    SelectFieldCode,
    SizeOfCode,
    PlusCode,
    MinusCode,
    MultiplyCode,
    MaxCode,
    MinCode,
    DivisionCode,
    ModuloCode,
  )

  def isAllowedOpCodeInCosting(opCode: OpCode): Boolean = _allowedOpCodesInCosting.contains(opCode)

  private def getOpCode(d: Def[_]): OpCode = d match {
      // todo implement
//    case _: Const[_] => true
    case _: OpCost | _: PerKbCostOf | _: Cast[_] => Undefined
    case _: Tup[_, _] => TupleCode
    case _: First[_, _] | _: Second[_, _] => SelectFieldCode
//    case _: FieldApply[_] => true
//    case _: IntPlusMonoid => true
//    case _: Lambda[_, _] => true
//    case _: ThunkDef[_] => true
//    case ApplyUnOp(_: NumericToLong[_] | _: NumericToInt[_], _) => true
    case ApplyBinOp(op, _, _) => op match {
      case _: NumericPlus[_] => PlusCode
      case _: NumericMinus[_] => MinusCode
      case _: NumericTimes[_] => MultiplyCode
      case _: IntegralDivide[_] => DivisionCode
      case _: IntegralMod[_] => ModuloCode
      case _: OrderingMax[_] => MaxCode
      case _: OrderingMin[_] => MinCode
    }

    case SCM.inputs(_) => InputsCode
    case SCM.outputs(_) => OutputsCode

//         | SCM.outputs(_) | SCM.dataInputs(_) | SCM.selfBox(_) | SCM.lastBlockUtxoRootHash(_) | SCM.headers(_) |
//         SCM.preHeader(_) | SCM.getVar(_, _, _) => true
//    case SBM.propositionBytes(_) | SBM.bytes(_) | SBM.bytesWithoutRef(_) | SBM.registers(_) | SBM.getReg(_, _, _) |
//         SBM.tokens(_) => true
//    case SSPM.propBytes(_) => true
//    case SAVM.tVal(_) | SAVM.valueSize(_) => true
//    case SizeM.dataSize(_) => true
//    case SPairM.l(_) | SPairM.r(_) => true
    case SCollM.sizes(_) => Undefined
//    case SOptM.sizeOpt(_) => true
//    case SFuncM.sizeEnv(_) => true
//    case _: CSizePairCtor[_, _] | _: CSizeFuncCtor[_, _, _] | _: CSizeOptionCtor[_] | _: CSizeCollCtor[_] |
//         _: CSizeBoxCtor | _: CSizeContextCtor | _: CSizeAnyValueCtor => true
//    case ContextM.SELF(_) | ContextM.OUTPUTS(_) | ContextM.INPUTS(_) | ContextM.dataInputs(_) | ContextM.LastBlockUtxoRootHash(_) |
//         ContextM.getVar(_, _, _) => true
//    case SigmaM.propBytes(_) => true
//    case _: CReplCollCtor[_] | _: PairOfColsCtor[_, _] => true
    case CollM.length(_) => SizeOfCode
//    case CollM.map(_, _) | CollM.sum(_, _) | CollM.zip(_, _) | CollM.slice(_, _, _) | CollM.apply(_, _) |
//         CollM.append(_, _) | CollM.foldLeft(_, _, _) => true
//    case CBM.replicate(_, _, _) | CBM.fromItems(_, _, _) => true
//    case BoxM.propositionBytes(_) | BoxM.bytesWithoutRef(_) | BoxM.getReg(_, _, _) => true
//    case OM.get(_) | OM.getOrElse(_, _) | OM.fold(_, _, _) | OM.isDefined(_) => true
//    case _: CostOf | _: SizeOf[_] => true
//    case _: Upcast[_, _] => true
//    case _: Apply[_, _] => true
//    case SPCM.some(_) => true
    case _ => error(s"Unknown opCode for $d}")
  }

  /** Checks if the function (Lambda node) given by the symbol `costF` contains only allowed operations
    * in the schedule. */
  def verifyCostFunc(costF: Rep[Any => Int]): Try[Unit] = {
    val Def(Lambda(lam,_,_,_)) = costF
    Try {
      lam.scheduleAll.forall { te =>
        CheckCostFuncOperation(this)(getOpCode(te.rhs)) { true }
      }
    }
  }

  /** Finds SigmaProp.isProven method calls in the given Lambda `f` */
  def findIsProven[T](f: Rep[Context => T]): Option[Sym] = {
    val Def(Lambda(lam,_,_,_)) = f
    val s = lam.scheduleAll.find(te => te.rhs match {
      case SigmaM.isValid(_) => true
      case _ => false
    }).map(_.sym)
    s
  }

  /** Checks that if SigmaProp.isProven method calls exists in the given Lambda's schedule,
    * then it is the last operation. */
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

  /** Constructs a new data environment for evaluation of graphs using `compile` method.
    * This environment contains global variables. */
  def getDataEnv: DataEnv = {
    val env = Map[Sym, AnyRef](
      specialPredef   -> SpecialPredef,
      sigmaDslBuilder -> sigmaDslBuilderValue,
      colBuilder      -> sigmaDslBuilderValue.Colls,
      costedBuilder   -> costedBuilderValue,
      monoidBuilder   -> monoidBuilderValue,
      intPlusMonoid   -> monoidBuilderValue.intPlusMonoid,
      longPlusMonoid  -> monoidBuilderValue.longPlusMonoid
    )
    env
  }

  case class EvaluatedEntry(env: DataEnv, sym: Sym, value: AnyRef)

  protected def printEnvEntry(sym: Sym, value: AnyRef) = {
    def trim[A](arr: Array[A]) = arr.take(arr.length min 100)
    def show(x: Any) = x match {
      case arr: Array[_] => s"Array(${trim(arr).mkString(",")})"
      case col: special.collection.Coll[_] => s"Coll(${trim(col.toArray).mkString(",")})"
      case p: SGroupElement => p.showToString
      case ProveDlog(GroupElementConstant(g)) => s"ProveDlog(${g.showToString})"
      case ProveDHTuple(
              GroupElementConstant(g), GroupElementConstant(h), GroupElementConstant(u), GroupElementConstant(v)) =>
        s"ProveDHT(${g.showToString},${h.showToString},${u.showToString},${v.showToString})"
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

  def getFromEnv(dataEnv: DataEnv, s: Sym): Any = dataEnv.get(s) match {
    case Some(v) => v
    case _ => error(s"Cannot find value in environment for $s (dataEnv = $dataEnv)")
  }

  def msgCostLimitError(cost: Int, limit: Long) = s"Estimated expression complexity $cost exceeds the limit $limit"

  /** Incapsulate simple monotonic (add only) counter with reset. */
  class CostCounter(val initialCost: Int) {
    private var _currentCost: Int = initialCost

    @inline def += (n: Int) = {
      // println(s"${_currentCost} + $n")
      this._currentCost += n
    }
    @inline def currentCost: Int = _currentCost
    @inline def resetCost() = { _currentCost = initialCost }
  }

  /** Implements finite state machine with stack of graph blocks (scopes),
    * which correspond to lambdas and thunks.
    * It accepts messages: startScope(), endScope(), add(), reset()
    * At any time `totalCost` is the currently accumulated cost. */
  class CostAccumulator(initialCost: Int, costLimit: Option[Long]) {

    @inline private def initialStack() = List(new Scope(Set(), 0))
    private var _scopeStack: List[Scope] = initialStack

    @inline def currentVisited: Set[Sym] = _scopeStack.head.visited
    @inline def currentScope: Scope = _scopeStack.head
    @inline private def getCostFromEnv(dataEnv: DataEnv, s: Sym): Int = getFromEnv(dataEnv, s).asInstanceOf[Int]

    /** Represents a single scope during execution of the graph.
      * The lifetime of each instance is bound to scope execution.
      * When the evaluation enters a new scope (e.g. calling a lambda) a new Scope instance is created and pushed
      * to _scopeStack, then is starts receiving `add` method calls.
      * When the evaluation leaves the scope, the top is popped off the stack. */
    class Scope(visitiedOnEntry: Set[Sym], initialCost: Int) extends CostCounter(initialCost) {
      private var _visited: Set[Sym] = visitiedOnEntry
      @inline def visited: Set[Sym] = _visited
      @inline def add(s: Sym, op: OpCost, opCost: Int, dataEnv: DataEnv): Unit = {
        for (arg <- op.args) {
          if (!_visited.contains(arg)) {
            val argCost = getCostFromEnv(dataEnv, arg)
//            println(s"${this.currentCost} + $argCost ($arg <- $op)")
            this += argCost
            _visited += arg
          }
        }
        if (!_visited.contains(op.opCost)) {
//          println(s"${this.currentCost} + $opCost (${op.opCost} <- $op)")
          this += opCost
        }
        _visited += s
      }
    }

    /** Called once for each operation of a scope (lambda or thunk).
      * if costLimit is defined then delegates to currentScope. */
    def add(s: Sym, op: OpCost, dataEnv: DataEnv): Int = {
      val opCost = getFromEnv(dataEnv, op.opCost).asInstanceOf[Int]
      if (costLimit.isDefined) {
        currentScope.add(s, op, opCost, dataEnv)
        // check that we are still withing the limit
        val cost = currentScope.currentCost
        val limit = costLimit.get
        if (cost > limit)
          throw new CosterException(msgCostLimitError(cost, limit), None)
      }
      opCost
    }

    /** Called before any operation of a new scope (lambda or thunk)*/
    def startScope() = {
      _scopeStack = new Scope(currentVisited, currentScope.currentCost) :: _scopeStack
    }

    /** Called after all operations of a scope are executed (lambda or thunk)*/
    def endScope() = {
      val deltaCost = currentScope.currentCost - currentScope.initialCost
      _scopeStack = _scopeStack.tail
      _scopeStack.head += deltaCost
    }

    /** Resets this accumulator into initial state to be ready for new graph execution. */
    @inline def reset() = {
      _scopeStack = initialStack()
    }

    /** Returns total accumulated cost */
    @inline def totalCost: Int = currentScope.currentCost
  }


  /** Transform graph IR into the corresponding Scala function
    * @param f          simbol of the graph representing function from type A to B
    * @param costLimit  when Some(value) is specified, then OpCost nodes will be used to accumulate total cost of execution. */
  def compile[SA, SB, A, B](dataEnv: Map[Sym, AnyRef], f: Rep[A => B], costLimit: Option[Long] = None)
                           (implicit lA: Liftable[SA, A], lB: Liftable[SB, B]): SA => (SB, Int) =
  {
    val costAccumulator = new CostAccumulator(0, costLimit)

    def evaluate(te: TableEntry[_]): EnvRep[_] = EnvRep { dataEnv =>
      object In { def unapply(s: Sym): Option[Any] = Some(getFromEnv(dataEnv, s)) }
      def out(v: Any): (DataEnv, Sym) = { val vBoxed = v.asInstanceOf[AnyRef]; (dataEnv + (te.sym -> vBoxed), te.sym) }
      try {
        val startTime = if (okMeasureOperationTime) System.nanoTime() else 0L
        val res: (DataEnv, Sym) = te.rhs match {
          case d @ ContextM.getVar(ctx @ In(ctxObj: CostingDataContext), _, elem) =>
            val mc = d.asInstanceOf[MethodCall]
            val declaredTpe = elemToSType(elem)
            val valueInCtx = invokeUnlifted(ctx.elem, mc, dataEnv)
            out(valueInCtx)
          case d @ BoxM.getReg(box, _, elem) =>
            val mc = d.asInstanceOf[MethodCall]
            val declaredTpe = elemToSType(elem)
            val valueInReg = invokeUnlifted(box.elem, mc, dataEnv)
            out(valueInReg)
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

          case _: SigmaDslBuilder | _: CollBuilder | _: CostedBuilder | _: IntPlusMonoid | _: LongPlusMonoid |
               _: WSpecialPredefCompanion =>
            out(dataEnv.getOrElse(te.sym, !!!(s"Cannot resolve companion instance for $te")))

          case SigmaM.isValid(In(prop: AnyRef)) =>
            out(prop)

          case SDBM.substConstants(_,
            In(input: special.collection.Coll[Byte]@unchecked),
            In(positions: special.collection.Coll[Int]@unchecked),
            In(newVals: special.collection.Coll[Any]@unchecked), _) =>
            val typedNewVals = newVals.toArray.map(v => builder.liftAny(v) match {
              case Nullable(v) => v
              case _ => sys.error(s"Cannot evaluate substConstants($input, $positions, $newVals): cannot lift value $v")
            })
            val byteArray = SubstConstants.eval(input.toArray, positions.toArray, typedNewVals)(sigmaDslBuilderValue.validationSettings)
            out(sigmaDslBuilderValue.Colls.fromArray(byteArray))

          case AM.length(In(arr: Array[_])) => out(arr.length)
          case CBM.replicate(In(b: special.collection.CollBuilder), In(n: Int), xSym @ In(x)) =>
            out(b.replicate(n, x)(asType[Any](xSym.elem.sourceType)))

          case SPCM.some(In(v)) => out(Some(v))
          case SPCM.none(_) => out(None)

          // NOTE: This is a fallback rule which should be places AFTER all other MethodCall patterns
          case mc @ MethodCall(obj, m, args, _) =>
            val dataRes = obj.elem match {
              case _: CollElem[_, _] => mc match {
                case CollMethods.flatMap(xs, f) =>
                  val newMC = mc.copy(args = mc.args :+ f.elem.eRange.eItem)(mc.selfType, mc.isAdapterCall)
                  invokeUnlifted(obj.elem, newMC, dataEnv)
                case _ =>
                  invokeUnlifted(obj.elem, mc, dataEnv)
              }
              case _ =>
                invokeUnlifted(obj.elem, mc, dataEnv)
            }
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
              costAccumulator.startScope()
              val resEnv = l.schedule.foldLeft(dataEnv + (x -> ctx)) { (env, te) =>
                val (e, _) = evaluate(te).run(env)
                e
              }
              val res = resEnv(y)
              costAccumulator.endScope()
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
              costAccumulator.startScope()
              val resEnv = schedule.foldLeft(dataEnv) { (env, te) =>
                val (e, _) = evaluate(te).run(env)
                e
              }
              val res = resEnv(y)
              costAccumulator.endScope()
              res
            }
            out(th)

          case SDBM.sigmaProp(_, In(isValid: Boolean)) =>
            val res = CSigmaProp(sigmastate.TrivialProp(isValid))
            out(res)
          case SDBM.proveDlog(_, In(g: EcPointType)) =>
            val res = CSigmaProp(DLogProtocol.ProveDlog(g))
            out(res)
          case SDBM.proveDHTuple(_, In(g: EcPointType), In(h: EcPointType), In(u: EcPointType), In(v: EcPointType)) =>
            val res = CSigmaProp(ProveDHTuple(g, h, u, v))
            out(res)
          case SDBM.avlTree(_, In(flags: Byte),
                           In(digest: SColl[Byte]@unchecked), In(keyLength: Int),
                           In(valueLengthOpt: Option[Int]@unchecked)) =>
            val res = sigmaDslBuilderValue.avlTree(flags, digest, keyLength, valueLengthOpt)
            out(res)

          case CReplCollCtor(valueSym @ In(value), In(len: Int)) =>
            val res = sigmaDslBuilderValue.Colls.replicate(len, value)(asType[Any](valueSym.elem.sourceType))
            out(res)

          case PairOfColsCtor(In(ls: SColl[a]@unchecked), In(rs: SColl[b]@unchecked)) =>
            val res = sigmaDslBuilderValue.Colls.pairColl(ls, rs)
            out(res)

          case CSizePrimCtor(In(dataSize: Long), tVal) =>
            val res = new special.collection.CSizePrim(dataSize, tVal.eA.sourceType)
            out(res)
          case CSizePairCtor(In(l: SSize[_]), In(r: SSize[_])) =>
            val res = new special.collection.CSizePair(l, r)
            out(res)
          case CSizeCollCtor(In(sizes: SColl[SSize[_]] @unchecked)) =>
            val res = new special.collection.CSizeColl(sizes)
            out(res)
          case CSizeOptionCtor(In(optSize: Option[SSize[_]] @unchecked)) =>
            val res = new special.collection.CSizeOption(optSize)
            out(res)
          case CSizeAnyValueCtor(tVal, In(valueSize: SSize[Any] @unchecked)) =>
            val res = new special.sigma.CSizeAnyValue(tVal.eA.sourceType.asInstanceOf[RType[Any]], valueSize)
            out(res)
          case CSizeBoxCtor(
                 In(propBytes: SSize[SColl[Byte]]@unchecked), In(bytes: SSize[SColl[Byte]]@unchecked),
                 In(bytesWithoutRef: SSize[SColl[Byte]]@unchecked), In(regs: SSize[SColl[Option[SAnyValue]]]@unchecked),
                 In(tokens: SSize[SColl[(SColl[Byte], Long)]]@unchecked)) =>
            val res = new EvalSizeBox(propBytes, bytes, bytesWithoutRef, regs, tokens)
            out(res)

          case costOp: CostOf =>
            out(costOp.eval)
          case op @ PerKbCostOf(_,In(size: Long)) =>
            out(op.eval(size))
          case op: OpCost =>
            val c = costAccumulator.add(te.sym, op, dataEnv)
            out(c)
          case SizeOf(sym @ In(data)) =>
            val tpe = elemToSType(sym.elem)
            val size = tpe match {
//              case SAvlTree =>
//                data.asInstanceOf[special.sigma.AvlTree].dataSize
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
          case c @ Cast(eTo, In(v)) =>
            if (!eTo.sourceType.classTag.runtimeClass.isAssignableFrom(v.getClass)) {
              error(s"Invalid cast $c: ${eTo.sourceType.classTag.runtimeClass} is not assignable from ${v.getClass}")
            }
            out(v)
          case Downcast(In(from), eTo) =>
            val tpe = elemToSType(eTo).asNumType
            if (tpe == SBigInt)
              out(SBigInt.downcast(from.asInstanceOf[AnyVal]))
            else
              out(tpe.downcast(from.asInstanceOf[AnyVal]))
          case Upcast(In(from), eTo) =>
            val tpe = elemToSType(eTo).asNumType
            if (tpe == SBigInt)
              out(SBigInt.upcast(from.asInstanceOf[AnyVal]))
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
          !!!(s"Error in Evaluation.compile.evaluate($te)", e)
      }
    }

    val res = (x: SA) => {
      costAccumulator.reset() // reset accumulator to initial state
      val g = new PGraph(f)
      val xSym = f.getLambda.x
      val resEnv = g.schedule.foldLeft(dataEnv + (xSym -> x.asInstanceOf[AnyRef])) { (env, te) =>
        val (e, _) = evaluate(te).run(env)
        e
      }
      val fun = resEnv(f).asInstanceOf[SA => SB]
      val y = fun(x)
      (y, costAccumulator.totalCost)
    }
    res
  }
}

object Evaluation {
  import special.sigma._
  import special.collection._

  /** Transforms a serializable ErgoTree type descriptor to the corresponding RType descriptor of SigmaDsl,
    * which is used during evaluation.
    */
  def stypeToRType[T <: SType](t: T): RType[T#WrappedType] = (t match {  // TODO optimize using memoization
    case SBoolean => BooleanType
    case SByte => ByteType
    case SShort => ShortType
    case SInt => IntType
    case SLong => LongType
    case SString => StringType
    case SAny => AnyType
    case SUnit => UnitType
    case SBigInt => BigIntRType
    case SBox => BoxRType
    case SContext => ContextRType
    case SGlobal => SigmaDslBuilderRType
    case SHeader => HeaderRType
    case SPreHeader => PreHeaderRType
    case SGroupElement => GroupElementRType
    case SAvlTree => AvlTreeRType
    case SSigmaProp => SigmaPropRType
    case STuple(Seq(tpeA, tpeB)) =>
      pairRType(stypeToRType(tpeA), stypeToRType(tpeB))
    case STuple(items) =>
      val types = items.toArray
      tupleRType(types.map(t => stypeToRType(t).asInstanceOf[SomeType]))
    case c: SCollectionType[a] => collRType(stypeToRType(c.elemType))
    case o: SOption[a] => optionRType(stypeToRType(o.elemType))
    case SFunc(Seq(tpeArg), tpeRange, Nil) => funcRType(stypeToRType(tpeArg), stypeToRType(tpeRange))
    case _ => sys.error(s"Don't know how to convert SType $t to RType")
  }).asInstanceOf[RType[T#WrappedType]]

  /** Transforms RType descriptor of SigmaDsl, which is used during evaluation,
    * to the corresponding serializable ErgoTree type descriptor,
    */
  def rtypeToSType[T](t: RType[T]): SType = t match { // TODO optimize using memoization
    case BooleanType => SBoolean
    case ByteType => SByte
    case ShortType => SShort
    case IntType => SInt
    case LongType => SLong
    case StringType => SString
    case AnyType => SAny
    case UnitType => SUnit

    case BigIntegerRType => SBigInt
    case BigIntRType => SBigInt

    case ECPointRType => SGroupElement
    case GroupElementRType => SGroupElement

    case AvlTreeRType => SAvlTree
    case ot: OptionType[_] => sigmastate.SOption(rtypeToSType(ot.tA))
    case BoxRType => SBox
    case ContextRType => SContext
    case SigmaDslBuilderRType => SGlobal
    case HeaderRType => SHeader
    case PreHeaderRType => SPreHeader
    case SigmaPropRType => SSigmaProp
    case SigmaBooleanRType => SSigmaProp
    case tup: TupleType => STuple(tup.items.map(t => rtypeToSType(t)).toIndexedSeq)
    case at: ArrayType[_] => SCollection(rtypeToSType(at.tA))
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
    case _: Boolean => BooleanType
    case _: Byte  => ByteType
    case _: Short => ShortType
    case _: Int   => IntType
    case _: Long  => LongType
    case _: Char  => CharType
    case _: Float  => FloatType
    case _: Double  => DoubleType
    case _: String  => StringType
    case _: Unit  => UnitType

    case _: BigInteger => BigIntegerRType
    case _: special.sigma.BigInt => BigIntRType

    case _: ECPoint => ECPointRType
    case _: GroupElement => GroupElementRType

    case _: ErgoBox => ErgoBoxRType
    case _: Box => BoxRType

    case _: AvlTreeData => AvlTreeDataRType
    case _: AvlTree => AvlTreeRType

    case _: SigmaBoolean => SigmaBooleanRType
    case _: SigmaProp => SigmaPropRType
    case _: Context => ContextRType
    case _ =>
      sys.error(s"Don't know how to compute typeOf($value)")
  }}

  /** Convert SigmaDsl representation of tuple to ErgoTree serializable representation. */
  def fromDslTuple(value: Any, tupleTpe: STuple): Coll[Any] = value match {
    case t: Tuple2[_,_] => TupleColl(t._1, t._2)
    case a: Coll[Any]@unchecked if a.tItem == RType.AnyType => a
    case _ =>
      sys.error(s"Cannot execute fromDslTuple($value, $tupleTpe)")
  }

  /** Convert ErgoTree serializable representation of tuple to SigmaDsl representation. */
  def toDslTuple(value: Coll[Any], tupleTpe: STuple): Any = tupleTpe match {
    case t if t.items.length == 2 => (value(0), value(1))
    case _ => value
  }

}
