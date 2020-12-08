package sigmastate.interpreter

import org.ergoplatform.ErgoLikeContext
import sigmastate.SType
import sigmastate.Values._
import sigmastate.eval.Profiler
import sigmastate.interpreter.ErgoTreeEvaluator.{CostItem, DataEnv, SimpleCostItem, SeqCostItem}
import sigmastate.interpreter.Interpreter.ReductionResult
import sigmastate.lang.exceptions.CostLimitException
import special.sigma.Context
import scalan.util.Extensions._

import scala.collection.mutable.ArrayBuffer
import scala.util.DynamicVariable

/** Configuration parameters of the evaluation run. */
case class EvalSettings(
  /** Used by [[ErgoTreeEvaluator.profiler]] to measure operations timings. */
  isMeasureOperationTime: Boolean,
  /** Used by [[ErgoTreeEvaluator]] to conditionally perform debug mode operations. */
  isDebug: Boolean = false,
  /** Used by [[ErgoTreeEvaluator]] to conditionally emit log messages. */
  isLogEnabled: Boolean = false,
  /** Used by [[ErgoTreeEvaluator]] to conditionally build a trace of added costs.
    * @see Value.addCost
    */
  costTracingEnabled: Boolean = false)

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

  val costTrace = ArrayBuffer.empty[CostItem]

  /** Adds the given cost to the `coster`. If tracing is enabled, associates the cost with
    * the given node operation.
    * @param cost the cost to be added to `coster`
    * @param opNode the node to associate the cost with (when costTracingEnabled)
    */
  def addCost(cost: Int, opNode: SValue): this.type = {
    addCost(cost, opNode.opName)
  }

  /** Adds the given cost to the `coster`. If tracing is enabled, associates the cost with
    * the given operation.
    * @param cost the cost to be added to `coster`
    * @param opName the operation name to associate the cost with (when costTracingEnabled)
    */
  def addCost(cost: Int, opName: String): this.type = {
    coster.add(cost)
    if (settings.costTracingEnabled) {
      costTrace += SimpleCostItem(opName, cost)
    }
    this
  }

  /** Adds the given cost to the `coster`. If tracing is enabled, associates the cost with
    * the given operation.
    * @param perItemCost the cost to be added to `coster` for each item
    * @param nItems the number of items
    * @param opName the operation name to associate the cost with (when costTracingEnabled)
    */
  def addCost(perItemCost: Int, nItems: Int, opName: String): this.type = {
    val cost = Math.multiplyExact(perItemCost, nItems)
    coster.add(cost)
    if (settings.costTracingEnabled) {
      costTrace += SeqCostItem(opName, perItemCost, nItems)  // TODO v5.0: more accurate tracing
    }
    this
  }

  def addPerKbCostOf(node: SValue, dataSize: Int) = {
    val cost = Value.perKbCostOf(node, dataSize)
    coster.add(cost)
    if (settings.isLogEnabled)
      println(s"PerKbCostOf(${node.opName}) -> $cost")
  }
}

object ErgoTreeEvaluator {
  /** Immutable data environment used to assign data values to graph nodes. */
  type DataEnv = Map[Int, Any]

  /** Empty data environment. */
  val EmptyDataEnv: DataEnv = Map.empty

  /** A profiler which is used by default if [[EvalSettings.isMeasureOperationTime]] is enabled. */
  val DefaultProfiler = new Profiler

  /** Default global [[EvalSettings]] instance. */
  val DefaultEvalSettings = EvalSettings(isMeasureOperationTime = false)

  /** Evaluator currently is being executed on the current thread.
    * This variable is set in a single place, specifically in the `eval` method of
    * [[ErgoTreeEvaluator]].
    * @see getCurrentEvaluator
    */
  private val currentEvaluator = new DynamicVariable[ErgoTreeEvaluator](null)

  /** Returns a current evaluator for the current thread. */
  def getCurrentEvaluator: ErgoTreeEvaluator = currentEvaluator.value

  /** An item in the cost accumulation trace of a [[ErgoTreeEvaluator]]. */
  abstract class CostItem {
    def opName: String
  }

  /** An item in the cost accumulation trace of a [[ErgoTreeEvaluator]].
    * Represents cost of simple operation.
    * Used for debugging of costing.
    * @param opName  name of the ErgoTree operation
    * @param cost    cost added to accumulator
    */
  case class SimpleCostItem(opName: String, cost: Int) extends CostItem

  /** An item in the cost accumulation trace of a [[ErgoTreeEvaluator]].
    * Represents cost of a sequence of operation.
    * Used for debugging of costing.
    *
    * @param opName      name of the ErgoTree operation
    * @param perItemCost cost added to accumulator for each item of a collection
    * @param nItems      number of items in the collection
    */
  case class SeqCostItem(opName: String, perItemCost: Int, nItems: Int)
    extends CostItem

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
      case sb: SigmaBoolean => sb
      case _ => error(s"Expected SigmaBoolean but was: $res")
    }
    (sb, cost)
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

  @inline private def initialStack() = List(new Scope(0))
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

