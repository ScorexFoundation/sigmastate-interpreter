package sigmastate.eval

import java.lang.Math
import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform._
import org.ergoplatform.validation.ValidationRules.{CheckLoopLevelInCostFunction, CheckCostFuncOperation}
import sigmastate._
import sigmastate.Values.{Value, GroupElementConstant, SigmaBoolean, Constant}
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
import sigmastate.basics.{ProveDHTuple, DLogProtocol}
import special.sigma.Extensions._
import sigmastate.lang.exceptions.CostLimitException
import sigmastate.serialization.OpCodes
import special.SpecialPredef
import special.Types._

import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** This is a slice in IRContext cake which implements evaluation of graphs.
  */
trait Evaluation extends RuntimeCosting { IR: IRContext =>
  import Context._
  import SigmaProp._
  import Coll._
  import AnyValue._
  import Box._
  import AvlTree._
  import CollBuilder._
  import SigmaDslBuilder._
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
//  import CSizeAnyValue._
  import SizeSigmaProp._
  import SizeBox._
//  import CSizeBox._
  import SizeContext._
//  import CSizeContext._
  import MonoidBuilder._
  import OpCodes._

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
  private val CBM = CollBuilderMethods
  private val SDBM = SigmaDslBuilderMethods
  private val OM = WOptionMethods
  private val SPCM = WSpecialPredefCompanionMethods
  private val MBM = MonoidBuilderMethods

  private val _allowedOpCodesInCosting: HashSet[OpCodeExtra] = HashSet[OpCode](
    AppendCode,
    ByIndexCode,
    ConstantCode,
    DivisionCode,
    DowncastCode,
    ExtractBytesWithNoRefCode,
    ExtractRegisterAs,
    ExtractScriptBytesCode,
    FoldCode,
    FuncApplyCode,
    FuncValueCode,
    GetVarCode,
    InputsCode,
    LastBlockUtxoRootHashCode,
    MapCollectionCode,
    FlatMapCollectionCode,
    MaxCode,
    MethodCallCode,
    MinCode,
    MinusCode,
    ModuloCode,
    MultiplyCode,
    OptionGetCode,
    OptionGetOrElseCode,
    OptionIsDefinedCode,
    OutputsCode,
    PlusCode,
    SelectFieldCode,
    SelfCode,
    SigmaPropBytesCode,
    SizeOfCode,
    SliceCode,
    TupleCode,
    UpcastCode,
  ).map(toExtra) ++ HashSet[OpCodeExtra](
    OpCostCode,
    PerKbCostOfCode,
    CastCode,
    IntPlusMonoidCode,
    ThunkDefCode,
    ThunkForceCode,
    SCMInputsCode,
    SCMOutputsCode,
    SCMDataInputsCode,
    SCMSelfBoxCode,
    SCMLastBlockUtxoRootHashCode,
    SCMHeadersCode,
    SCMPreHeaderCode,
    SCMGetVarCode,
    SBMPropositionBytesCode,
    SBMBytesCode,
    SBMBytesWithoutRefCode,
    SBMRegistersCode,
    SBMGetRegCode,
    SBMTokensCode,
    SSPMPropBytesCode,
    SAVMTValCode,
    SAVMValueSizeCode,
    SizeMDataSizeCode,
    SPairLCode,
    SPairRCode,
    SCollMSizesCode,
    SOptMSizeOptCode,
    SFuncMSizeEnvCode,
    CSizePairCtorCode,
    CSizeFuncCtorCode,
    CSizeOptionCtorCode,
    CSizeCollCtorCode,
    CSizeBoxCtorCode,
    CSizeContextCtorCode,
    CSizeAnyValueCtorCode,
    CReplCollCtorCode,
    PairOfColsCtorCode,
    CollMSumCode,
    CBMReplicateCode,
    CBMFromItemsCode,
    CostOfCode,
    UOSizeOfCode,
    SPCMSomeCode,
  )

  /** Returns a set of opCodeEx values (extended op codes) which are allowed in cost function.
    * This may include both ErgoTree codes (from OpCodes) and also additional non-ErgoTree codes
    * from OpCodesExtra.
    * Any IR graph node can be uniquely assigned to extended op code value
    * from OpCodes + OpCodesExtra combined range. (See getOpCodeEx) */
  protected def allowedOpCodesInCosting: HashSet[OpCodeExtra] = _allowedOpCodesInCosting

  def isAllowedOpCodeInCosting(opCode: OpCodeExtra): Boolean = allowedOpCodesInCosting.contains(opCode)

  /** Returns extended op code assigned to the given IR graph node.
    */
  def getOpCodeEx(d: Def[_]): OpCodeExtra = d match {
    case _: OpCost => OpCostCode
    case _: PerKbCostOf => PerKbCostOfCode
    case _: Cast[_] => CastCode
    case _: ThunkDef[_] => ThunkDefCode
    case _: ThunkForce[_] => ThunkForceCode
    case MBM.intPlusMonoid(_) => IntPlusMonoidCode
    case SCM.inputs(_) => SCMInputsCode
    case SCM.outputs(_) => SCMOutputsCode
    case SCM.dataInputs(_) => SCMDataInputsCode
    case SCM.selfBox(_) => SCMSelfBoxCode
    case SCM.lastBlockUtxoRootHash(_) => SCMLastBlockUtxoRootHashCode
    case SCM.headers(_) => SCMHeadersCode
    case SCM.preHeader(_) => SCMPreHeaderCode
    case SCM.getVar(_, _, _) => SCMGetVarCode
    case SBM.propositionBytes(_) => SBMPropositionBytesCode
    case SBM.bytes(_) => SBMBytesCode
    case SBM.bytesWithoutRef(_) => SBMBytesWithoutRefCode
    case SBM.registers(_) => SBMRegistersCode
    case SBM.getReg(_, _, _) => SBMGetRegCode
    case SBM.tokens(_) => SBMTokensCode
    case SSPM.propBytes(_) => SSPMPropBytesCode
    case SAVM.tVal(_) => SAVMTValCode
    case SAVM.valueSize(_) => SAVMValueSizeCode
    case SizeM.dataSize(_) => SizeMDataSizeCode
    case SPairM.l(_) => SPairLCode
    case SPairM.r(_) => SPairRCode
    case SCollM.sizes(_) => SCollMSizesCode
    case SOptM.sizeOpt(_) => SOptMSizeOptCode
    case SFuncM.sizeEnv(_) => SFuncMSizeEnvCode
    case _: CSizePairCtor[_, _] => CSizePairCtorCode
    case _: CSizeFuncCtor[_, _, _] => CSizeFuncCtorCode
    case _: CSizeOptionCtor[_] => CSizeOptionCtorCode
    case _: CSizeCollCtor[_] => CSizeCollCtorCode
//    case _: CSizeBoxCtor => CSizeBoxCtorCode
//    case _: CSizeContextCtor => CSizeContextCtorCode
//    case _: CSizeAnyValueCtor => CSizeAnyValueCtorCode
    case CollM.sum(_, _) => CollMSumCode
    case CBM.replicate(_, _, _) => CBMReplicateCode
    case CBM.fromItems(_, _, _) => CBMFromItemsCode
    case _: CostOf => CostOfCode
    case _: SizeOf[_] => UOSizeOfCode
    case SPCM.some(_) => SPCMSomeCode
    case dSer => toExtra(dSer match {
      case _: Const[_] => ConstantCode
      case _: Tup[_, _] => TupleCode
      case _: First[_, _] | _: Second[_, _] => SelectFieldCode
      case _: Lambda[_, _] => FuncValueCode
      case _: Apply[_, _] => FuncApplyCode
      case _: Upcast[_, _] => UpcastCode
      case _: Downcast[_, _] => DowncastCode
      case ApplyBinOp(op, _, _) => op match {
        case _: NumericPlus[_] => PlusCode
        case _: NumericMinus[_] => MinusCode
        case _: NumericTimes[_] => MultiplyCode
        case _: IntegralDivide[_] => DivisionCode
        case _: IntegralMod[_] => ModuloCode
        case _: OrderingMin[_] => MinCode
        case _: OrderingMax[_] => MaxCode
        case _: Equals[_] => EqCode
        case _: NotEquals[_] => NeqCode
        case _: OrderingGT[_] => GtCode
        case _: OrderingLT[_] => LtCode
        case _: OrderingGTEQ[_] => GeCode
        case _: OrderingLTEQ[_] => LeCode
      }
      case ApplyUnOp(op, _) => op match {
        case _: NumericToLong[_] => UpcastCode // it's either this or DowncastCode
        case _: NumericToInt[_] => DowncastCode // it's either this or UpcastCode
      }
      case _: IfThenElseLazy[_] => IfCode
      case ContextM.SELF(_) => SelfCode
      case ContextM.OUTPUTS(_) => OutputsCode
      case ContextM.INPUTS(_) => InputsCode
      case ContextM.dataInputs(_) => MethodCallCode
      case ContextM.LastBlockUtxoRootHash(_) => LastBlockUtxoRootHashCode
      case ContextM.getVar(_, _, _) => GetVarCode
      case ContextM.HEIGHT(_) => HeightCode
      case ContextM.minerPubKey(_) => MinerPubkeyCode
      case SigmaM.propBytes(_) => SigmaPropBytesCode
      case SigmaM.isValid(_) => SigmaPropIsProvenCode
      case CollM.length(_) => SizeOfCode
      case CollM.apply(_, _) => ByIndexCode
      case CollM.map(_, _) => MapCollectionCode
      case CollM.flatMap(_, _) => FlatMapCollectionCode
      case CollM.zip(_, _) => MethodCallCode
      case CollM.slice(_, _, _) => SliceCode
      case CollM.append(_, _) => AppendCode
      case CollM.foldLeft(_, _, _) => FoldCode
      case CollM.exists(_, _) => ExistsCode
      case CollM.forall(_, _) => ForAllCode
      case CollM.filter(_, _) => FilterCode
      case BoxM.propositionBytes(_) => ExtractScriptBytesCode
      case BoxM.bytesWithoutRef(_) => ExtractBytesWithNoRefCode
      case BoxM.getReg(_, _, _) => ExtractRegisterAs
      case BoxM.value(_) => ExtractAmountCode
      case BoxM.bytes(_) => ExtractBytesCode
      case BoxM.id(_) => ExtractIdCode
      case BoxM.creationInfo(_) => ExtractCreationInfoCode
      case OM.get(_) => OptionGetCode
      case OM.getOrElse(_, _) => OptionGetOrElseCode
      case OM.fold(_, _, _) => MethodCallCode
      case OM.isDefined(_) => OptionIsDefinedCode
      case SDBM.substConstants(_, _, _, _, _) => SubstConstantsCode
      case SDBM.longToByteArray(_, _) => LongToByteArrayCode
      case SDBM.byteArrayToBigInt(_, _) => ByteArrayToBigIntCode
      case SDBM.byteArrayToLong(_, _) => ByteArrayToLongCode
      case SDBM.groupGenerator(_) => GroupGeneratorCode
      case SDBM.allOf(_, _) => AndCode
      case SDBM.anyOf(_, _) => OrCode
      case SDBM.atLeast(_, _, _) => AtLeastCode
      case SDBM.anyZK(_, _) => SigmaOrCode
      case SDBM.allZK(_, _) => SigmaAndCode
      case SDBM.blake2b256(_, _) => CalcBlake2b256Code
      case SDBM.sha256(_, _) => CalcSha256Code
      case SDBM.proveDlog(_, _) => ProveDlogCode
      case SDBM.proveDHTuple(_, _, _, _, _) => ProveDHTupleCode
      case SDBM.sigmaProp(_, _) => BoolToSigmaPropCode
      case SDBM.decodePoint(_, _) => DecodePointCode
      case SDBM.xorOf(_, _) => XorOfCode
      case CBM.xor(_, _, _) => XorCode
      case _: MethodCall => MethodCallCode
      case _ => error(s"Unknown opCode for $d}")
    })
  }


  object LoopOperation {
    /** Recognize loop operation and extracts the body of the loop.
      * Every loop operation should be handled here.
      * NOTE: flatMap is handled specially. */
    def unapply(d: Def[_]): Option[AstGraph] = d match {
      case CollM.map(_, Def(lam: Lambda[_,_])) => Some(lam)
      case CollM.exists(_, Def(lam: Lambda[_,_])) => Some(lam)
      case CollM.forall(_, Def(lam: Lambda[_,_])) => Some(lam)
      case CollM.foldLeft(_, _, Def(lam: Lambda[_,_])) => Some(lam)
      case CollM.filter(_, Def(lam: Lambda[_,_])) => Some(lam)
      case _ => None
    }
  }

  /** Recursively traverse the hierarchy of loop operations. */
  private def traverseScope(scope: AstGraph, level: Int): Unit = {
    scope.schedule.foreach { te =>
      te.node match {
        case op @ LoopOperation(bodyLam) =>
          CheckCostFuncOperation(this)(getOpCodeEx(op))
          val nextLevel = level + 1
          CheckLoopLevelInCostFunction(nextLevel)
          traverseScope(bodyLam, nextLevel)
        case CollM.flatMap(_, Def(lam: Lambda[_,_])) =>
          traverseScope(lam, level) // special case because the body is limited (so don't increase level)
        case op =>
          CheckCostFuncOperation(this)(getOpCodeEx(op))
      }
    }
  }

  /** Checks if the function (Lambda node) given by the symbol `costF` contains only allowed operations
    * in the schedule. */
  def verifyCostFunc(costF: Ref[Any => Int]): Try[Unit] = {
    val Def(Lambda(lam,_,_,_)) = costF
    Try {
      traverseScope(lam, level = 0)
      if (debugModeSanityChecks) {
        val backDeps = mutable.HashMap.empty[Sym, ArrayBuffer[Sym]]
        lam.flatSchedule.foreach { sym =>
          sym.node.deps.foreach { usedSym =>
            val usages = backDeps.getOrElseUpdate(usedSym, new ArrayBuffer())
            usages += sym
          }
        }
        //      println(backDeps)
        lam.flatSchedule
            .filter {
              case Def(op: OpCost) =>
                assert(!op.args.contains(op.opCost), s"Invalid $op")
                true
              case _ => false
            }
            .foreach { sym =>
              val usages = backDeps.getOrElse(sym, new ArrayBuffer())
              usages.foreach { usageSym =>
                usageSym.node match {
                  case _: Lambda[_,_] => //ok
                  case _: ThunkDef[_] => //ok
                  case OpCost(_, _, args, _) if args.contains(sym) => //ok
                  case OpCost(_, _, _, opCost) if opCost == sym =>
                    println(s"INFO: OpCost usage of node $sym -> ${sym.node} in opCost poistion in $usageSym -> ${usageSym.node}")
                  //ok
                  case _ =>
                    !!!(s"Non OpCost usage of node $sym -> ${sym.node} in $usageSym -> ${usageSym.node}: ${usageSym.elem}: (usages = ${usages.map(_.node)})")
                }
              }
            }
      }
    }
  }

  /** Finds SigmaProp.isProven method calls in the given Lambda `f` */
  def findIsProven[T](f: Ref[Context => T]): Option[Sym] = {
    val Def(Lambda(lam,_,_,_)) = f
    val s = lam.flatSchedule.find(sym => sym.node match {
      case SigmaM.isValid(_) => true
      case _ => false
    })
    s
  }

  /** Checks that if SigmaProp.isProven method calls exists in the given Lambda's schedule,
    * then it is the last operation. */
  def verifyIsProven[T](f: Ref[Context => T]): Try[Unit] = {
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

  def msgCostLimitError(cost: Long, limit: Long) = s"Estimated execution cost $cost exceeds the limit $limit"

  /** Incapsulate simple monotonic (add only) counter with reset. */
  class CostCounter(val initialCost: Int) {
    private var _currentCost: Int = initialCost

    @inline def += (n: Int) = {
      // println(s"${_currentCost} + $n")
      this._currentCost = java.lang.Math.addExact(this._currentCost, n)
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

    /** New item is pushed before loop starts evaluating and popped after it finishes. */
    private var _loopStack: List[Loop] = Nil

    @inline def currentVisited: Set[Sym] = _scopeStack.head.visited
    @inline def currentScope: Scope = _scopeStack.head

    /** Describes cost information of the loop (map, fold, flatMap etc.)
      * This is used for fail-fast checks in `add` method.
      * @param body  symbol of the lambda representing loop body */
    class Loop(val body: Sym, var accumulatedCost: Int)

    /** Represents a single scope during execution of the graph.
      * The lifetime of each instance is bound to scope execution.
      * When the evaluation enters a new scope (e.g. calling a lambda) a new Scope instance is created and pushed
      * to _scopeStack, then is starts receiving `add` method calls.
      * When the evaluation leaves the scope, the top is popped off the stack. */
    class Scope(visitiedOnEntry: Set[Sym], initialCost: Int) extends CostCounter(initialCost) {
      private var _visited: Set[Sym] = visitiedOnEntry
      @inline def visited: Set[Sym] = _visited
      @inline def isVisited(s: Sym) = _visited.contains(s)

      /** The value of dependency arg is either cost formula or another OpCost.
        * In the latter case we expect is has already been accumulated. */
      @inline private def getArgCostFromEnv(op: OpCost, dataEnv: DataEnv, s: Sym): Int = {
        val res = getFromEnv(dataEnv, s).asInstanceOf[Int]
        assert(!isVisited(s), s"Unexpected visited arg $s -> ${s.node} of $op")
        assert(!s.node.isInstanceOf[OpCost], s"Unexpected not-visited OpCost arg $s -> ${s.node} of $op")
        res
      }

      @inline def add(s: Sym, op: OpCost, dataEnv: DataEnv): Unit = {
        if (!isVisited(op.opCost)) {
          // this is not a dependency arg, it is cost formula, so we take its value from env
          val opCost = getFromEnv(dataEnv, op.opCost).asInstanceOf[Int]
          this += opCost
          // NOTE: we don't mark op.opCost as visited, it allows to append the same constant
          // many times in different OpCost nodes.
          // This is the key semantic difference between `args` and `opCost` arguments of OpCost.
          // If some cost is added via `args` is it marked as visited and never added again
          // If it is add via `opCost` it is not marked and can be added again in different
          // OpCost node down below in the scope.
        }
        // we need to add accumulate costs of non-visited args
        for (arg <- op.args) {
          if (!isVisited(arg)) {
            val argCost = getArgCostFromEnv(op, dataEnv, arg)
            this += argCost

            // this arg has been accumulated, mark it as visited to avoid repeated accumulations
            _visited += arg
          }
        }

        _visited += s
      }

      /** Called by nested Scopes to communicate accumulated cost back to parent scope.
        * When current scope terminates, it communicated accumulated cost up to its parent scope.
        * This value is used at the root scope to obtain total accumulated scope.
        */
      private var _resultRegister: Int = 0
      @inline def childScopeResult: Int = _resultRegister
      @inline def childScopeResult_=(resultCost: Int): Unit = {
        _resultRegister = resultCost
      }

    }

    /** Called once for each operation of a scope (lambda or thunk).
      */
    def add(s: Sym, op: OpCost, dataEnv: DataEnv): Int = {
      currentScope.add(s, op, dataEnv)

      // the cost we accumulated so far
      val cost = currentScope.currentCost

      // check that we are still withing the limit
      if (costLimit.isDefined) {
        val limit = costLimit.get
        val loopCost = if (_loopStack.isEmpty) 0 else _loopStack.head.accumulatedCost
        val accumulatedCost = java.lang.Math.addExact(cost, loopCost)
        if (accumulatedCost > limit) {
//          if (cost < limit)
//            println(s"FAIL FAST in loop: $accumulatedCost > $limit")
          throw new CostLimitException(accumulatedCost, msgCostLimitError(accumulatedCost, limit), None)
        }
      }

      // each OpCost represents how much cost was added since beginning of the current scope
      val opCostResult = cost - currentScope.initialCost
      opCostResult
    }

    /** Called before any operation of a new scope (lambda or thunk)*/
    def startScope() = {
      _scopeStack = new Scope(currentVisited, currentScope.currentCost) :: _scopeStack
    }

    /** Called after all operations of a scope are executed (lambda or thunk)
      * @param body  symbol of Lambda or ThunkDef node of the scope
      */
    def endScope(body: Sym) = {
      val deltaCost = currentScope.currentCost - currentScope.initialCost
      _scopeStack = _scopeStack.tail
      _scopeStack.head.childScopeResult = deltaCost  // set Result register of parent scope

      if (_loopStack.nonEmpty && _loopStack.head.body == body) {
        // every time we exit the body of the loop we need to update accumulated cost
        val h = _loopStack.head
        h.accumulatedCost = java.lang.Math.addExact(h.accumulatedCost, deltaCost)
      }
    }

    def startLoop(body: Sym) = {
      _loopStack = new Loop(body, 0) :: _loopStack
    }

    def endLoop() = {
      _loopStack = _loopStack.tail
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
  def compile[SA, SB, A, B](dataEnv: Map[Sym, AnyRef], f: Ref[A => B], costLimit: Option[Long] = None)
                           (implicit lA: Liftable[SA, A], lB: Liftable[SB, B]): SA => (SB, Int) =
  {
    val costAccumulator = new CostAccumulator(0, costLimit)

    def evaluate(sym: Sym): EnvRep[_] = EnvRep { dataEnv =>
      object In { def unapply(s: Sym): Option[Any] = Some(getFromEnv(dataEnv, s)) }
      def out(v: Any): (DataEnv, Sym) = { val vBoxed = v.asInstanceOf[AnyRef]; (dataEnv + (sym -> vBoxed), sym) }
      try {
        val res: (DataEnv, Sym) = sym.node match {
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

          case wc: LiftedConst[_,_] => out(wc.constValue)

          case _: SigmaDslBuilder | _: CollBuilder | _: CostedBuilder |
               _: WSpecialPredefCompanion |
               MBM.intPlusMonoid(_) | MBM.longPlusMonoid(_) =>
            out(dataEnv.getOrElse(sym, !!!(s"Cannot resolve companion instance for $sym -> ${sym.node}")))

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

          case CBM.replicate(In(b: special.collection.CollBuilder), In(n: Int), xSym @ In(x)) =>
            out(b.replicate(n, x)(asType[Any](xSym.elem.sourceType)))

          case SPCM.some(In(v)) => out(Some(v))
          case SPCM.none(_) => out(None)

          // NOTE: This is a fallback rule which should be places AFTER all other MethodCall patterns
          case mc @ MethodCall(obj, m, args, _) =>
            val dataRes = obj.elem match {
              case _: CollElem[_, _] => mc match {
                case CollMethods.flatMap(_, f) =>
                  val newMC = mc.copy(args = mc.args :+ f.elem.eRange.eItem)(mc.resultType, mc.isAdapterCall)
                  costAccumulator.startLoop(f)
                  val res = invokeUnlifted(obj.elem, newMC, dataEnv)
                  costAccumulator.endLoop()
                  res

                case CollMethods.foldLeft(_, _, f) =>
                  costAccumulator.startLoop(f)
                  val res = invokeUnlifted(obj.elem, mc, dataEnv)
                  costAccumulator.endLoop()
                  res

                case CollMethods.map(_, f) =>
                  costAccumulator.startLoop(f)
                  val res = invokeUnlifted(obj.elem, mc, dataEnv)
                  costAccumulator.endLoop()
                  res

                case CollMethods.filter(_, p) =>
                  costAccumulator.startLoop(p)
                  val res = invokeUnlifted(obj.elem, mc, dataEnv)
                  costAccumulator.endLoop()
                  res

                case CollMethods.forall(_, p) =>
                  costAccumulator.startLoop(p)
                  val res = invokeUnlifted(obj.elem, mc, dataEnv)
                  costAccumulator.endLoop()
                  res

                case CollMethods.exists(_, p) =>
                  costAccumulator.startLoop(p)
                  val res = invokeUnlifted(obj.elem, mc, dataEnv)
                  costAccumulator.endLoop()
                  res

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
              // x is not yet in _visited set of the new scope, thus it will accumulated is necessary
              val resEnv = l.schedule.foldLeft(dataEnv + (x -> ctx)) { (env, sym) =>
                val (e, _) = evaluate(sym).run(env)
                e
              }
              val res = resEnv(y)
              costAccumulator.endScope(sym)
              res
            }
            out(f)
          case Apply(In(_f), In(x: AnyRef), _) =>
            val f = _f.asInstanceOf[AnyRef => Any]
            out(f(x))
          case ThunkDef(y, schedule) =>
            val th = () => {
              costAccumulator.startScope()
              val resEnv = schedule.foldLeft(dataEnv) { (env, sym) =>
                val (e, _) = evaluate(sym).run(env)
                e
              }
              val res = resEnv(y)
              costAccumulator.endScope(sym)
              res
            }
            out(th)

          case ThunkForce(In(t: ThunkData[Any])) =>
            out(t())
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

//          case CReplCollCtor(valueSym @ In(value), In(len: Int)) =>
//            val res = sigmaDslBuilderValue.Colls.replicate(len, value)(asType[Any](valueSym.elem.sourceType))
//            out(res)
//
//          case PairOfColsCtor(In(ls: SColl[a]@unchecked), In(rs: SColl[b]@unchecked)) =>
//            val res = sigmaDslBuilderValue.Colls.pairColl(ls, rs)
//            out(res)

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
//          case CSizeAnyValueCtor(tVal, In(valueSize: SSize[Any] @unchecked)) =>
//            val res = new special.sigma.CSizeAnyValue(tVal.eA.sourceType.asInstanceOf[RType[Any]], valueSize)
//            out(res)
//          case CSizeBoxCtor(
//                 In(propBytes: SSize[SColl[Byte]]@unchecked), In(bytes: SSize[SColl[Byte]]@unchecked),
//                 In(bytesWithoutRef: SSize[SColl[Byte]]@unchecked), In(regs: SSize[SColl[Option[SAnyValue]]]@unchecked),
//                 In(tokens: SSize[SColl[(SColl[Byte], Long)]]@unchecked)) =>
//            val res = new EvalSizeBox(propBytes, bytes, bytesWithoutRef, regs, tokens)
//            out(res)

          case costOp: CostOf =>
            out(costOp.eval)
          case op @ PerKbCostOf(_,In(size: Long)) =>
            out(op.eval(size))
          case op: OpCost =>
            val c = costAccumulator.add(sym, op, dataEnv)
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

          case _ =>
            !!!(s"Don't know how to evaluate($sym -> ${sym.node})")
        }
        onEvaluatedGraphNode(res._1, res._2, res._1(res._2))
        res
      }
      catch {
        case e: Throwable =>
          !!!(s"Error in Evaluation.compile.evaluate($sym -> ${sym.node})", e)
      }
    }

    val res = (x: SA) => {
      costAccumulator.reset() // reset accumulator to initial state
      val g = new PGraph(f)
      val xSym = f.getLambda.x
      val resEnv = g.schedule.foldLeft(dataEnv + (xSym -> x.asInstanceOf[AnyRef])) { (env, te) =>
        val (updatedEnv, _) = evaluate(te).run(env)
        updatedEnv
      }
      val fun = resEnv(f).asInstanceOf[SA => SB]
      val y = fun(x)
      costAccumulator.currentScope += costAccumulator.currentScope.childScopeResult
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
