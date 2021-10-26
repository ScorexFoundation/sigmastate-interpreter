package sigmastate.interpreter

import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.SigmaConstants.ScriptCostLimit
import sigmastate.{PerItemCost, FixedCost, SType, TypeBasedCost}
import sigmastate.Values._
import sigmastate.eval.Profiler
import sigmastate.interpreter.ErgoTreeEvaluator.DataEnv
import sigmastate.interpreter.Interpreter.ReductionResult
import special.sigma.{Context, SigmaProp}
import scalan.util.Extensions._
import sigmastate.interpreter.EvalSettings._
import sigmastate.lang.Terms.MethodCall
import spire.syntax.all.cfor
import supertagged.TaggedType
import debox.{Buffer => DBuffer}
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
  printTestVectors: Boolean = false,
  evaluationMode: EvaluationMode = AotEvaluationMode)

object EvalSettings {
  /** Enumeration type of evaluation modes of [[Interpreter]].
    * This type can be removed in v5.x releases together with AOT implementation once v5.0
    * protocol is activated.
    */
  object EvaluationMode extends TaggedType[Int]
  type EvaluationMode = EvaluationMode.Type

  /** Evaluation mode when the interpreter is executing using AOT costing implementation
   * of v4.x protocol. */
  val AotEvaluationMode: EvaluationMode = EvaluationMode @@ 1  // first bit

  /** Evaluation mode when the interpreter is executing using JIT costing implementation
    * of v5.x protocol. */
  val JitEvaluationMode: EvaluationMode = EvaluationMode @@ 2  // second bit

  /** Evaluation mode when the interpreter is executing using both AOT and JIT costing
    * implementations and compare the results.
    * This mode should be used in tests and not supposed to be used in production.
    */
  val TestEvaluationMode: EvaluationMode = EvaluationMode @@ 3 // both bits
}

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
  val settings: EvalSettings) {
  
  /** Evaluates the given expression in the given data environment. */
  def eval(env: DataEnv, exp: SValue): Any = {
    ErgoTreeEvaluator.currentEvaluator.withValue(this) {
      exp.evalTo[Any](env)(this)
    }
  }

  /** Evaluates the given expression in the given data environment and accrue the cost
    * into the `coster` of this evaluator.
    * @return the value of the expression and the total accumulated cost in the coster.
    *         The returned cost includes the initial cost accumulated in the `coster`
    *         prior to calling this method. */
  def evalWithCost(env: DataEnv, exp: SValue): (Any, Int) = {
    val res = eval(env, exp)
    val cost = coster.totalCost
    (res, cost)
  }

  /** Trace of cost items accumulated during execution of `eval` method. Call
    * `clearTrace` method before each `eval` invocation. */
  private lazy val costTrace: DBuffer[CostItem] = {
    DBuffer.ofSize[CostItem](1000)
  }

  /** Returns the currently accumulated trace of cost items in this evaluator.
    * A new array is allocated and returned, the evaluator state is unaffected.
    */
  def getCostTrace(): Seq[CostItem] = {
    costTrace.toArray()
  }

  /** Clears the accumulated trace of this evaluator. */
  def clearTrace() = {
    costTrace.clear()
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
    if (settings.isMeasureOperationTime) {
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
      block()
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
    * @hotspot don't beautify the code
    */
  final def addSeqCostNoOp(costKind: PerItemCost, nItems: Int, opDesc: OperationDesc): Unit = {
    var costItem: SeqCostItem = null
    if (settings.costTracingEnabled) {
      costItem = SeqCostItem(opDesc, costKind, nItems)
      costTrace += costItem
    }
    val cost = costKind.cost(nItems)
    coster.add(cost)
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
    if (settings.isMeasureOperationTime) {
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
      block()
    }
  }

  /** Adds the cost to the `coster`. See the other overload for details. */
  @inline
  final def addSeqCost[R](costInfo: OperationCostInfo[PerItemCost], nItems: Int)
                         (block: () => R): R = {
    addSeqCost(costInfo.costKind, nItems, costInfo.opDesc)(block)
  }

  /** Adds the cost to the `coster`. If tracing is enabled, creates a new cost item with
    * the given operation descriptor and cost kind. If time measuring is enabled also
    * performs profiling.
    *
    * WARNING: The cost is accumulated AFTER the block is executed.
    * Each usage of this method should be accompanied with a proof of why this cannot lead
    * to unbounded execution (see all usages).
    *
    * @param costKind the cost descriptor to be used to compute the cost based on the
    *                 actual number of items returned by the `block`
    * @param opDesc   the operation to associate the cost with (when costTracingEnabled)
    * @param block    operation executed under the given cost descriptors, returns the
    *                 actual number of items processed
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

  /** Adds the cost to the `coster`. See the other overload for details. */
  final def addSeqCost(costInfo: OperationCostInfo[PerItemCost])(block: () => Int): Unit = {
    addSeqCost(costInfo.costKind, costInfo.opDesc)(block)
  }
}

object ErgoTreeEvaluator {
  /** Immutable data environment used to assign data values to graph nodes. */
  type DataEnv = Map[Int, Any]

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

  /** Evaluator currently is being executed on the current thread.
    * This variable is set in a single place, specifically in the `eval` method of
    * [[ErgoTreeEvaluator]].
    * @see getCurrentEvaluator
    */
  private[sigmastate] val currentEvaluator = new DynamicVariable[ErgoTreeEvaluator](null)

  /** Returns a current evaluator for the current thread. */
  def getCurrentEvaluator: ErgoTreeEvaluator = currentEvaluator.value

  /** Creates a new [[ErgoTreeEvaluator]] instance with the given profiler and settings.
    * The returned evaluator can be used to initialize the `currentEvaluator` variable.
    * As a result, cost-aware operations (code blocks) can be implemented, even when those
    * operations don't involve ErgoTree evaluation.
    * As an example, see methods in [[sigmastate.SigSerializer]] and
    * [[sigmastate.FiatShamirTree]] where cost-aware code blocks are used.
    */
  def forProfiling(profiler: Profiler, evalSettings: EvalSettings): ErgoTreeEvaluator = {
    val acc = new CostAccumulator(0, Some(ScriptCostLimit.value))
    new ErgoTreeEvaluator(
      context = null,
      constants = mutable.WrappedArray.empty,
      acc, profiler, evalSettings.copy(profilerOpt = Some(profiler)))
  }

  /** Executes [[FixedCost]] code `block` and use the given evaluator `E` to perform
    * profiling and cost tracing.
    * This helper method allows implementation of cost-aware code blocks by using
    * thread-local instance of [[ErgoTreeEvaluator]].
    * If the `currentEvaluator` [[DynamicVariable]] is not initialized (equals to null),
    * then the block is executed with minimal overhead.
    *
    * @param costInfo operation descriptor
    * @param block    block of code to be executed (given as lazy by-name argument)
    * @param E        evaluator to be used (or null if it is not available on the
    *                 current thread), in which case the method is equal to the
    *                 `block` execution.
    * @return result of code block execution
    * HOTSPOT: don't beautify the code
    * Note, `null` is used instead of Option to avoid allocations.
    */
  def fixedCostOp[R <: AnyRef](costInfo: OperationCostInfo[FixedCost])
                              (block: => R)(implicit E: ErgoTreeEvaluator): R = {
    if (E != null) {
      var res: R = null.asInstanceOf[R]
      E.addFixedCost(costInfo) {
        res = block
      }
      res
    } else
      block
  }

  /** Executes [[PerItemCost]] code `block` and use the given evaluator `E` to perform
    * profiling and cost tracing.
    * This helper method allows implementation of cost-aware code blocks by using
    * thread-local instance of [[ErgoTreeEvaluator]].
    * If the `currentEvaluator` [[DynamicVariable]] is not initialized (equals to null),
    * then the block is executed with minimal overhead.
    *
    * @param costInfo operation descriptor
    * @param nItems   number of data items in the operation
    * @param block    block of code to be executed (given as lazy by-name argument)
    * @param E        evaluator to be used (or null if it is not available on the
    *                 current thread), in which case the method is equal to the
    *                 `block` execution.
    * @return result of code block execution
    * HOTSPOT: don't beautify the code
    * Note, `null` is used instead of Option to avoid allocations.
    */
  def perItemCostOp[R](costInfo: OperationCostInfo[PerItemCost], nItems: Int)
                      (block: () => R)(implicit E: ErgoTreeEvaluator): R = {
    if (E != null) {
      E.addSeqCost(costInfo, nItems)(block)
    } else
      block()
  }

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
}


