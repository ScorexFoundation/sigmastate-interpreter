package sigmastate.interpreter

import org.ergoplatform.ErgoLikeContext
import sigmastate.SFunc
import sigmastate.Values._
import sigmastate.eval.Profiler
import sigmastate.interpreter.ErgoTreeEvaluator.DataEnv
import sigmastate.interpreter.Interpreter.ReductionResult
import sigmastate.lang.exceptions.CostLimitException
import special.sigma.Context

/** Configuration parameters of the evaluation run. */
case class EvalSettings(
  /** Used by [[ErgoTreeEvaluator.profiler]] to measure operations timings. */
  isMeasureOperationTime: Boolean,
  /** Used by [[ErgoTreeEvaluator]] to conditionally perform debug mode operations. */
  isDebug: Boolean = false)

/** Used as a wrapper around Sigma [[Context]] to be used in [[ErgoTreeEvaluator]].
  */
class EvalContext(val context: Context)

/** Implements a Simple Fast direct-style interpreter of ErgoTrees.
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
  * AOT style, it is 5-6x faster then existing implementation.
  */
class ErgoTreeEvaluator(
  /** Represents data context for ErgoTree evaluation */
  val evalContext: EvalContext,
  /** Accumulates computation costs. */
  val coster: CostAccumulator,
  /** Preforms operations profiling and time measurments (if enabled in settings). */
  val profiler: Profiler,
  /** Settings to be used during evaluation. */
  val settings: EvalSettings)
{
  /** */
  def eval(env: DataEnv, exp: SValue): Any = {
    exp.evalTo[Any](this, env)
  }

  def addCostOf(opName: String, opType: SFunc) = {
    val cost = Value.costOf(opName, opType)
    coster.add(cost)
//    println(s"CostOf($opName, $opType) -> $cost") // comment before commit and push
  }

  def addCostOf(node: SValue) = {
    val cost = Value.costOf(node)
    coster.add(cost)
//    println(s"CostOf(${node.opName}) -> $cost") // comment before commit and push
  }

  def addPerItemCostOf(node: SValue, arrLen: Int) = {
    val cost = Value.perItemCostOf(node, arrLen)
    coster.add(cost)
//    println(s"PerItemCostOf(${node.opName}) -> $cost") // comment before commit and push
  }

  def addPerKbCostOf(node: SValue, dataSize: Int) = {
    val cost = Value.perKbCostOf(node, dataSize)
    coster.add(cost)
//    println(s"PerKbCostOf(${node.opName}) -> $cost") // comment before commit and push
  }
}

object ErgoTreeEvaluator {
  /** Immutable data environment used to assign data values to graph nodes. */
  type DataEnv = Map[Int, Any]

  /** A profiler which is used by default if [[EvalSettings.isMeasureOperationTime]] is enabled. */
  val DefaultProfiler = new Profiler

  /** Default global [[EvalSettings]] instance. */
  val DefaultEvalSettings = EvalSettings(isMeasureOperationTime = false)

  /** Evaluate the given [[ErgoTree] in the given Ergo context using the given settings.
    *
    * @param context      [[ErgoLikeContext]] used for script execution
    * @param ergoTree     script represented as [[ErgoTree]]
    * @param evalSettings evaluation settings
    * @return a sigma protocol proposition (as [[SigmaBoolean]]) and JIT cost estimation.
    */
  def eval(context: ErgoLikeContext, ergoTree: ErgoTree, evalSettings: EvalSettings): ReductionResult = {
    val (res, cost) = eval(context, ergoTree.toProposition(false), evalSettings)
    val sb = res match {
      case sb: SigmaBoolean => sb
      case _ => error(s"Expected SigmaBoolean but was: $res")
    }
    (sb, cost)
  }

  def eval(context: ErgoLikeContext, exp: SValue, evalSettings: EvalSettings): (Any, Int) = {
    val costAccumulator = new CostAccumulator(0, Some(context.costLimit))
    val sigmaContext = context.toSigmaContext(isCost = false)

    val ctx = new EvalContext(sigmaContext)
    val evaluator = new ErgoTreeEvaluator(ctx, costAccumulator, DefaultProfiler, evalSettings)
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
        //          if (cost < limit)
        //            println(s"FAIL FAST in loop: $accumulatedCost > $limit")
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

