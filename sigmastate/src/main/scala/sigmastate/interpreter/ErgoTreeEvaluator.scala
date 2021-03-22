package sigmastate.interpreter

import org.ergoplatform.ErgoLikeContext
import sigmastate.{SMethod, SType}
import sigmastate.Values._
import sigmastate.eval.Profiler
import sigmastate.interpreter.ErgoTreeEvaluator.{DataEnv, OperationCostInfo, MethodDesc, CompanionDesc, OperationDesc}
import sigmastate.interpreter.Interpreter.ReductionResult
import sigmastate.lang.exceptions.CostLimitException
import special.sigma.{Context, SigmaProp}
import scalan.util.Extensions._
import sigmastate.lang.Terms.MethodCall
import spire.syntax.all.cfor

import scala.collection.mutable
import scala.util.DynamicVariable

/** Configuration parameters of the evaluation run. */
case class EvalSettings(
  /** Used together with [[ErgoTreeEvaluator.profiler]] to measure individual operations timings. */
  isMeasureOperationTime: Boolean,
  /** Used together with [[ErgoTreeEvaluator.profiler]] to measure script timings. */
  isMeasureScriptTime: Boolean,
  /** Used by [[ErgoTreeEvaluator]] to conditionally perform debug mode operations. */
  isDebug: Boolean = false,
  /** Used by [[ErgoTreeEvaluator]] to conditionally emit log messages. */
  isLogEnabled: Boolean = false,
  /** Used by [[ErgoTreeEvaluator]] to conditionally build a trace of added costs.
    * @see Value.addCost
    */
  costTracingEnabled: Boolean = false,
  /** Profiler which, when defined, should be used in [[ErgoTreeEvaluator]] constructor. */
  profilerOpt: Option[Profiler] = None,
  /** Should be set to true, if evaluation is performed as part of test suite.
    * In such a case, additional operations may be performed (such as sanity checks). */
  isTestRun: Boolean = false,
  /** If true, then expected test vectors are pretty-printed. */
  printTestVectors: Boolean = false)

/** Implements a simple and fast direct-style interpreter of ErgoTrees.
  *
  * ### Motivation
  * [[ErgoTree]] is a simple declarative intermediate representation for Ergo contracts. It is
  * designed to be compact in serialized form and directly executable, i.e. no additional
  * transformation is necessary before it can be efficiently executed.
  *
  * This class implements a big-step recursive interpreter that works directly with
  * ErgoTree HOAS. Because of this the evaluator is very simple and follows denotational
  * semantics of ErgoTree (see https://ergoplatform.org/docs/ErgoTree.pdf). Or, the other
  * way around, this implementation of ErgoTreeEvaluator is purely functional with
  * immutable data structures and can be used as definition of ErgoTree's semantics.
  *
  * ### Implementation
  * ErgoTreeEvaluator takes ErgoTree directly as it is deserialized as part of a
  * transaction. No additional transformation is performed.
  * ErgoTree is interpreted directly and all the intermediate data is stored in the
  * runtime types.
  * The runtime types are such types as [[special.collection.Coll]],
  * [[special.sigma.SigmaProp]], [[special.sigma.AvlTree]], [[BigInt]], etc.
  * It also use immutable Map to keep current [[DataEnv]] of computed [[ValDef]]s, as
  * result only addition is used from the map, and deletion is essentially a garbage
  * collection.
  *
  * ### Performance
  * Since this interpreter directly works with SigmaDsl types (Coll, BigInt, SigmaProp
  * etc), it turns out to be very fast. Since it also does JIT style costing instead of
  * AOT style, it is 5-6x faster than existing implementation.
  *
  * @param context   Represents blockchain data context for ErgoTree evaluation
  * @param constants Segregated constants from ErgoTree, to lookup them from
  *                  [[ConstantPlaceholder]] evaluation.
  * @param coster    Accumulates computation costs.
  * @param profiler  Performs operations profiling and time measurements (if enabled in settings).
  * @param settings  Settings to be used during evaluation.
  */
class ErgoTreeEvaluator(
  val context: Context,
  val constants: Seq[Constant[SType]],
  protected val coster: CostAccumulator,
  val profiler: Profiler,
  val settings: EvalSettings)
{
  /** Evaluates the given expression in the given data environment. */
  def eval(env: DataEnv, exp: SValue): Any = {
    ErgoTreeEvaluator.currentEvaluator.withValue(this) {
      exp.evalTo[Any](env)(this)
    }
  }

  /** Evaluates the given expression in the given data environment. */
  def evalWithCost(env: DataEnv, exp: SValue): (Any, Int) = {
    val res = eval(env, exp)
    val cost = coster.totalCost
    (res, cost)
  }

  /** Trace of cost items accumulated during execution of `eval` method.
    * Call [[ArrayBuffer.clear()]] before each `eval` invocation. */
  val costTrace = {
    val b = mutable.ArrayBuilder.make[CostItem]
    b.sizeHint(1000)
    b
  }

  /** Adds the given cost to the `coster`. If tracing is enabled, associates the cost with
    * the given operation.
    *
    * @param costKind kind of the cost to be added to `coster`
    * @param opDesc   operation descriptor to associate the cost with (when costTracingEnabled)
    */
  final def addCost(costKind: FixedCost, opDesc: OperationDesc): Unit = {
    coster.add(costKind.cost)
    if (settings.costTracingEnabled) {
      costTrace += FixedCostItem(opDesc, costKind)
    }
  }

  @inline final def addCost(costInfo: OperationCostInfo[FixedCost]): Unit = {
    addCost(costInfo.costKind, costInfo.opDesc)
  }

  /** Add the cost given by the cost descriptor and the type to the accumulator and
    * associate it with this operation descriptor.
    *
    * @param costKind descriptor of the cost
    * @param tpe      specific type for which the cost should be computed by this descriptor
    *                 (see costFunc method)
    * @param opDesc   operation which is associated with this cost
    */
  @inline
  final def addTypeBasedCost[R](costKind: TypeBasedCost,
                             tpe: SType, opDesc: OperationDesc)(block: () => R): R = {
    var costItem: TypeBasedCostItem = null
    if (settings.costTracingEnabled) {
      costItem = TypeBasedCostItem(opDesc, costKind, tpe)
      costTrace += costItem
    }
    if (settings.isMeasureOperationTime && block != null) {
      if (costItem == null) {
        costItem = TypeBasedCostItem(opDesc, costKind, tpe)
      }
      val start = System.nanoTime()
      val cost = costKind.costFunc(tpe) // should be measured as part of the operation
      coster.add(cost)
      val res = block()
      val end = System.nanoTime()
      profiler.addCostItem(costItem, end - start)
      res
    } else {
      val cost = costKind.costFunc(tpe)
      coster.add(cost)
      if (block == null) null.asInstanceOf[R] else block()
    }
  }

  /** Adds the given cost to the `coster`. If tracing is enabled, associates the cost with
    * the given operation.
    * @param costKind kind of the cost to be added to `coster`
    * @param opDesc the operation descriptor to associate the cost with (when costTracingEnabled)
    * @param block  operation executed under the given cost
    * @hotspot don't beautify the code
    */
  final def addFixedCost(costKind: FixedCost, opDesc: OperationDesc)(block: => Unit): Unit = {
    var costItem: FixedCostItem = null
    if (settings.costTracingEnabled) {
      costItem = FixedCostItem(opDesc, costKind)
      costTrace += costItem
    }
    if (settings.isMeasureOperationTime) {
      if (costItem == null) {
        costItem = FixedCostItem(opDesc, costKind)
      }
      val start = System.nanoTime()
      coster.add(costKind.cost)
      val _ = block
      val end = System.nanoTime()
      profiler.addCostItem(costItem, end - start)
    } else {
      coster.add(costKind.cost)
      block
    }
  }

  @inline
  final def addFixedCost(costInfo: OperationCostInfo[FixedCost])(block: => Unit): Unit = {
    addFixedCost(costInfo.costKind, costInfo.opDesc)(block)
  }

  /** Adds the given cost to the `coster`. If tracing is enabled, creates a new cost item
    * with the given operation.
    *
    * @param costKind the cost to be added to `coster` for each item
    * @param nItems   the number of items
    * @param opDesc   the operation to associate the cost with (when costTracingEnabled)
    * @param block    operation executed under the given cost
    * @tparam R result type of the operation
    * @hotspot don't beautify the code
    */
  final def addSeqCost[R](costKind: PerItemCost, nItems: Int, opDesc: OperationDesc)(block: () => R): R = {
    var costItem: SeqCostItem = null
    if (settings.costTracingEnabled) {
      costItem = SeqCostItem(opDesc, costKind, nItems)
      costTrace += costItem
    }
    if (settings.isMeasureOperationTime && block != null) {
      if (costItem == null) {
        costItem = SeqCostItem(opDesc, costKind, nItems)
      }
      val start = System.nanoTime()
      val cost = costKind.cost(nItems) // should be measured as part of the operation
      coster.add(cost)
      val res = block()
      val end = System.nanoTime()
      profiler.addCostItem(costItem, end - start)
      res
    } else {
      val cost = costKind.cost(nItems)
      coster.add(cost)
      if (block == null) null.asInstanceOf[R] else block()
    }
  }

  @inline
  final def addSeqCost[R](costInfo: OperationCostInfo[PerItemCost], nItems: Int)
                         (block: () => R): R = {
    addSeqCost(costInfo.costKind, nItems, costInfo.opDesc)(block)
  }

  /** Adds the cost to the `coster`. If tracing is enabled, creates a new cost item with
    * the given operation descriptor and cost kind. If time measuring is enabled also
    * performs profiling.
    *
    * @param costKind the cost descriptor to be used to compute the cost based on the
    *                 actual number of items returned by the `block`
    * @param opDesc   the operation to associate the cost with (when costTracingEnabled)
    * @param block    operation executed under the given cost, returns number of items
    *                 processed
    * @hotspot don't beautify the code
    */
  final def addSeqCost(costKind: PerItemCost, opDesc: OperationDesc)(block: () => Int): Unit = {
    var costItem: SeqCostItem = null
    var nItems = 0
    if (settings.isMeasureOperationTime) {
      val start = System.nanoTime()
      nItems = block()
      val cost = costKind.cost(nItems) // should be measured as part of the operation
      coster.add(cost)
      val end = System.nanoTime()

      costItem = SeqCostItem(opDesc, costKind, nItems)
      profiler.addCostItem(costItem, end - start)
    } else {
      nItems = block()
      val cost = costKind.cost(nItems)
      coster.add(cost)
    }
    if (settings.costTracingEnabled) {
      if (costItem == null)
        costItem = SeqCostItem(opDesc, costKind, nItems)
      costTrace += costItem
    }
  }

  final def addSeqCost(costInfo: OperationCostInfo[PerItemCost])(block: () => Int): Unit = {
    addSeqCost(costInfo.costKind, costInfo.opDesc)(block)
  }


  final def addMethodCallCost[R](mc: MethodCall, obj: Any, args: Array[Any])
                                (block: => R): R = {
    val costDetails = MethodCallCostItem.calcCost(mc, obj, args)(this)
    if (settings.costTracingEnabled) {
      costTrace += MethodCallCostItem(costDetails)
    }

    if (settings.isMeasureOperationTime) {
      coster.add(costDetails.cost)
      // measure time
      val start = System.nanoTime()
      val res = block
      val end = System.nanoTime()
      val time = end - start

      val len = costDetails.trace.length
      val totalCost = costDetails.cost

      // spread the measured time between individial cost items
      cfor(0)(_ < len, _ + 1) { i =>
        val costItem = costDetails.trace(i)
        profiler.addCostItem(costItem, time * costItem.cost / totalCost)
      }
      res
    } else {
      coster.add(costDetails.cost)
      block
    }
  }
}

object ErgoTreeEvaluator {
  /** Immutable data environment used to assign data values to graph nodes. */
  type DataEnv = Map[Int, Any]

  /** Each ErgoTree operation is described either using [[ValueCompanion]] or using
   * [[SMethod]]. */
  abstract class OperationDesc
  case class CompanionDesc(companion: ValueCompanion) extends OperationDesc
  case class MethodDesc(method: SMethod) extends OperationDesc {
    override def toString: String = s"MethodDesc(${method.opName})"

    override def hashCode(): Int = (method.objType.typeId << 8) | method.methodId

    override def equals(obj: Any): Boolean =
      this.eq(obj.asInstanceOf[AnyRef]) || (obj != null && (obj match {
        case that: MethodDesc =>
          method.objType.typeId == that.method.objType.typeId &&
          method.methodId == that.method.methodId
        case _ => false
      }))
  }
  case class NamedDesc(name: String) extends OperationDesc

  /** Operation costing descriptors combined together. */
  case class OperationCostInfo[C <: CostKind](costKind: C, opDesc: OperationDesc)

  def operationName(opDesc: OperationDesc): String = opDesc match {
    case CompanionDesc(companion) => companion.typeName
    case MethodDesc(method) => method.opName
    case NamedDesc(name) => name
  }

  /** Size of data block in bytes. Used in JIT cost calculations.
    * @see [[sigmastate.NEQ]],
    */
  val DataBlockSize: Int = 512

  /** Empty data environment. */
  val EmptyDataEnv: DataEnv = Map.empty

  /** A profiler which is used by default if [[EvalSettings.isMeasureOperationTime]] is enabled. */
  val DefaultProfiler = new Profiler

  /** Default global [[EvalSettings]] instance. */
  val DefaultEvalSettings = EvalSettings(
    isMeasureOperationTime = false,
    isMeasureScriptTime = false)

  /** Evaluator currently is being executed on the current thread.
    * This variable is set in a single place, specifically in the `eval` method of
    * [[ErgoTreeEvaluator]].
    * @see getCurrentEvaluator
    */
  private val currentEvaluator = new DynamicVariable[ErgoTreeEvaluator](null)

  /** Returns a current evaluator for the current thread. */
  def getCurrentEvaluator: ErgoTreeEvaluator = currentEvaluator.value

  /** Evaluate the given [[ErgoTree]] in the given Ergo context using the given settings.
    * The given ErgoTree is evaluated as-is and is not changed during evaluation.
    *
    * @param context      [[ErgoLikeContext]] used for script execution
    * @param ergoTree     script represented as [[ErgoTree]]
    * @param evalSettings evaluation settings
    * @return a sigma protocol proposition (as [[SigmaBoolean]]) and accumulated JIT cost estimation.
    */
  def evalToCrypto(context: ErgoLikeContext, ergoTree: ErgoTree, evalSettings: EvalSettings): ReductionResult = {
    val (res, cost) = eval(context, ergoTree.constants, ergoTree.toProposition(replaceConstants = false), evalSettings)
    val sb = res match {
      case sp: SigmaProp =>
        sigmastate.eval.SigmaDsl.toSigmaBoolean(sp)
      case sb: SigmaBoolean => sb
      case _ => error(s"Expected SigmaBoolean but was: $res")
    }
    ReductionResult(sb, cost)
  }

  /** Evaluate the given expression in the given Ergo context using the given settings.
    * The given Value is evaluated as-is and is not changed during evaluation.
    *
    * @param context      [[ErgoLikeContext]] used for script execution
    * @param constants    collection of segregated constants which can be refered by
    *                     [[ConstantPlaceholder]]s in `exp`
    * @param exp          ErgoTree expression represented as [[Value]]
    * @param evalSettings evaluation settings
    * @return 1) the result of evaluating `exp` in a given context and
    *         2) an accumulated JIT cost estimation.
    */
  def eval(context: ErgoLikeContext,
           constants: Seq[Constant[SType]],
           exp: SValue,
           evalSettings: EvalSettings): (Any, Int) = {
    val costAccumulator = new CostAccumulator(context.initCost.toIntExact, Some(context.costLimit))
    val sigmaContext = context.toSigmaContext(isCost = false)
    eval(sigmaContext, costAccumulator, constants, exp, evalSettings)
  }

  /** Evaluate the given expression in the given Ergo context using the given settings.
    * The given Value is evaluated as-is and is not changed during evaluation.
    *
    * @param sigmaContext    [[special.sigma.Context]] instance used for script execution
    * @param costAccumulator [[CostAccumulator]] instance used for accumulating costs
    * @param constants       collection of segregated constants which can be refered by
    *                        [[ConstantPlaceholder]]s in `exp`
    * @param exp             ErgoTree expression represented as [[sigmastate.Values.Value]]
    * @param evalSettings    evaluation settings
    * @return 1) the result of evaluating `exp` in a given context and
    *         2) an accumulated JIT cost estimation.
    */
  def eval(sigmaContext: Context,
           costAccumulator: CostAccumulator,
           constants: Seq[Constant[SType]],
           exp: SValue,
           evalSettings: EvalSettings): (Any, Int) = {
    val evaluator = new ErgoTreeEvaluator(
      sigmaContext, constants, costAccumulator, DefaultProfiler, evalSettings)
    val res = evaluator.eval(Map(), exp)
    val cost = costAccumulator.totalCost
    (res, cost)
  }

  def error(msg: String) = sys.error(msg)

  def msgCostLimitError(cost: Long, limit: Long) = s"Estimated execution cost $cost exceeds the limit $limit"

}

/** Encapsulate simple monotonic (add only) counter with reset. */
class CostCounter(val initialCost: Int) {
  private var _currentCost: Int = initialCost

  @inline def += (n: Int) = {
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

  @inline private def initialStack() = List(new Scope(initialCost))
  private var _scopeStack: List[Scope] = initialStack

  @inline def currentScope: Scope = _scopeStack.head

  /** Represents a single scope during execution of the graph.
    * The lifetime of each instance is bound to scope execution.
    * When the evaluation enters a new scope (e.g. calling a lambda) a new Scope instance is created and pushed
    * to _scopeStack, then is starts receiving `add` method calls.
    * When the evaluation leaves the scope, the top is popped off the stack. */
  class Scope(initialCost: Int) extends CostCounter(initialCost) {


    @inline def add(opCost: Int): Unit = {
          this += opCost
    }

    /** Called by nested Scopes to communicate accumulated cost back to parent scope.
      * When current scope terminates, it communicates accumulated cost up to its parent scope.
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
  def add(opCost: Int): Unit = {
    currentScope.add(opCost)

    // check that we are still withing the limit
    if (costLimit.isDefined) {
      val limit = costLimit.get
      // the cost we accumulated so far
      val accumulatedCost = currentScope.currentCost
      if (accumulatedCost > limit) {
        throw new CostLimitException(
          accumulatedCost, ErgoTreeEvaluator.msgCostLimitError(accumulatedCost, limit), None)
      }
    }
  }

  /** Resets this accumulator into initial state to be ready for new graph execution. */
  @inline def reset() = {
    _scopeStack = initialStack()
  }

  /** Returns total accumulated cost */
  @inline def totalCost: Int = currentScope.currentCost
}

/** An item in the cost accumulation trace of a [[ErgoTreeEvaluator]]. */
abstract class CostItem {
  def opName: String
  def cost: Int
}

/** An item in the cost accumulation trace of a [[ErgoTreeEvaluator]].
  * Represents cost of simple operation.
  * Used for debugging, testing and profiling of costing.
  * @param opDesc   descriptor of the ErgoTree operation
  * @param costKind kind of the cost to be added to accumulator
  */
case class FixedCostItem(opDesc: OperationDesc, costKind: FixedCost) extends CostItem {
  override def opName: String = ErgoTreeEvaluator.operationName(opDesc)
  override def cost: Int = costKind.cost
}
object FixedCostItem {
  def apply(companion: FixedCostValueCompanion): FixedCostItem = {
    FixedCostItem(companion.opDesc, companion.costKind)
  }
  def apply(method: SMethod, costKind: FixedCost): FixedCostItem = {
    FixedCostItem(MethodDesc(method), costKind)
  }
}

/** An item in the cost accumulation trace of a [[ErgoTreeEvaluator]].
  * Represents cost of an operation which depends on type (e.g. type of arguments).
  * Used for debugging, testing and profiling of costing.
  * @param opDesc   descriptor of the ErgoTree operation
  * @param costKind type based cost descriptor added to accumulator
  * @param tpe      concrete type on this the operation is executed
  * @see [[sigmastate.LE]], [[sigmastate.GT]]
  */
case class TypeBasedCostItem(
    opDesc: OperationDesc,
    costKind: TypeBasedCost,
    tpe: SType) extends CostItem {
  override def opName: String = {
    val name = ErgoTreeEvaluator.operationName(opDesc)
    s"$name[$tpe]"
  }
  override def cost: Int = costKind.costFunc(tpe)
  override def equals(obj: Any): Boolean =
    (this eq obj.asInstanceOf[AnyRef]) || (obj != null && (obj match {
      case that: TypeBasedCostItem =>
        opDesc == that.opDesc && tpe == that.tpe
      case _ => false
    }))
  override def hashCode(): Int = 31 * opDesc.hashCode() + tpe.hashCode()
}
object TypeBasedCostItem {
  def apply(companion: ValueCompanion, tpe: SType): TypeBasedCostItem = {
    TypeBasedCostItem(companion.opDesc, companion.costKind.asInstanceOf[TypeBasedCost], tpe)
  }
}

/** An item in the cost accumulation trace of a [[ErgoTreeEvaluator]].
  * Represents cost of a sequence of operation.
  * Used for debugging, testing and profiling of costing.
  *
  * @param opDesc   descriptor of the ErgoTree operation
  * @param costKind descriptor of the cost added to accumulator
  * @param nItems   number of items in the sequence
  */
case class SeqCostItem(opDesc: OperationDesc, costKind: PerItemCost, nItems: Int)
    extends CostItem {
  override def opName: String = ErgoTreeEvaluator.operationName(opDesc)
  override def cost: Int = costKind.cost(nItems)
  /** How many data chunks in this cost item. */
  def chunks: Int = costKind.chunks(nItems)
}
object SeqCostItem {
  def apply(companion: PerItemCostValueCompanion, nItems: Int): SeqCostItem =
    SeqCostItem(companion.opDesc, companion.costKind, nItems)
}

/** An item in the cost accumulation trace of a [[ErgoTreeEvaluator]].
  * Represents cost of MethodCall operation.
  * Used for debugging, testing and profiling of costing.
  *
  * @param items cost details obtained as part of MethodCall evaluation
  */
case class MethodCallCostItem(items: CostDetails) extends CostItem {
  override def opName: String = MethodCall.typeName
  override def cost: Int = items.cost
}
object MethodCallCostItem {
  /** Helper method to compute cost details for the given method call. */
  def calcCost(mc: MethodCall, obj: Any, args: Array[Any])
              (implicit E: ErgoTreeEvaluator): CostDetails = {
    // add approximated cost of invoked method (if specified)
    val cost = mc.method.costFunc match {
      case Some(costFunc) => costFunc(E, mc, obj, args)
      case _ => CostDetails.ZeroCost // TODO v5.0: throw exception if not defined
    }
    cost
  }
}

/** Abstract representation of cost results obtained during evaluation. */
abstract class CostDetails {
  /** The total cost of evaluation. */
  def cost: Int
  /** The trace of costed operations performed during evaluation. */
  def trace: Seq[CostItem]
  /** Actual execution time (in nanoseconds) if defined. */
  def actualTimeNano: Option[Long]
}

/** Detailed results of cost evaluation represented by trace.
  * NOTE: the `trace` is obtained during execution of [[ErgoTreeEvaluator]])
  * @param trace accumulated trace of all cost items (empty for AOT costing)
  * @param actualTimeNano measured time of execution (if some)
  */
case class TracedCost(trace: Seq[CostItem],
                      actualTimeNano: Option[Long] = None) extends CostDetails {
  /** Total cost of all cost items. */
  def cost: Int = trace.foldLeft(0)(_ + _.cost)
}

/** Result of cost evaluation represented using simple given value.
  * Used to represent cost of AOT costing.
  * @param cost the given value of the total cost
  * @param actualTimeNano measured time of execution (if some)
  */
case class GivenCost(cost: Int,
                     actualTimeNano: Option[Long] = None) extends CostDetails {
  /** The trait is empty for this representation of CostDetails.
    */
  override def trace: Seq[CostItem] = mutable.WrappedArray.empty
}

object CostDetails {
  /** Empty sequence of cost items. Should be used whenever possible to avoid allocations. */
  val EmptyTrace: Seq[CostItem] = mutable.WrappedArray.empty

  /** CostDetails with empty trace have also zero total cost. */
  val ZeroCost = TracedCost(EmptyTrace)

  /** Helper factory method to create CostDetails from the given trace. */
  def apply(trace: Seq[CostItem]): CostDetails = TracedCost(trace)

  /** Helper recognizer to work with different representations of costs in patterns
   * uniformly.
   */
  def unapply(d: CostDetails): Option[(Int, Seq[CostItem])] = d match {
    case TracedCost(t, _) => Some((d.cost, t))
    case GivenCost(c, _) => Some((c, EmptyTrace))
    case _ => None
  }
}
