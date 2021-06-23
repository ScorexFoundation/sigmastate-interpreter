package sigmastate.interpreter

import java.util.concurrent.ExecutionException
import java.util.concurrent.atomic.AtomicInteger

import com.google.common.cache.{CacheBuilder, RemovalNotification, RemovalListener, LoadingCache, CacheLoader, CacheStats}
import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.validation.SigmaValidationSettings
import org.ergoplatform.validation.ValidationRules.{CheckCostFunc, CheckCalcFunc, trySoftForkable}
import scalan.{AVHashMap, Nullable}
import sigmastate.Values
import sigmastate.Values.ErgoTree
import sigmastate.eval.{RuntimeIRContext, IRContext}
import sigmastate.interpreter.Interpreter.{ReductionResult, WhenSoftForkReductionResult}
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.utils.Helpers._
import spire.syntax.all.cfor

import scala.collection.mutable

/** A reducer which represents precompiled script reduction function.
  * The function takes script execution context and produces the [[ReductionResult]],
  * which contains both sigma proposition and the approximation of the cost taken by the
  * reduction.
  */
trait ScriptReducer {
  /** Reduce this pre-compiled script in the given context.
    * This is equivalent to reduceToCrypto, except that graph construction is
    * completely avoided.
    */
  def reduce(context: InterpreterContext): ReductionResult
}

/** Used as a fallback reducer when precompilation failed due to soft-fork condition. */
case object WhenSoftForkReducer extends ScriptReducer {
  override def reduce(context: InterpreterContext): ReductionResult = {
    WhenSoftForkReductionResult(context.initCost)
  }
}

/** This class implements optimized reduction of the given pre-compiled script.
  * Pre-compilation of the necessary graphs is performed as part of constructor and
  * the graphs are stored in the given IR instance.
  *
  * The code make the following assumptions:
  * 1) the given script doesn't contain both [[sigmastate.utxo.DeserializeContext]] and
  * [[sigmastate.utxo.DeserializeRegister]]
  *
  * The code should correspond to reduceToCrypto method, but some operations may be
  * optimized due to assumptions above.
  */
case class PrecompiledScriptReducer(scriptBytes: Seq[Byte])(implicit val IR: IRContext)
  extends ScriptReducer {

  /** The following operations create [[IR.RCostingResultEx]] structure for the given
    * `scriptBytes` and they should be the same as in `reduceToCrypto` method.
    * This can be viewed as ahead of time pre-compilation of the cost and calc graphs
    * which are reused over many invocations of the `reduce` method.
    */
  val costingRes: IR.RCostingResultEx[Any] = {
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(scriptBytes.toArray)
    val prop = tree.toProposition(tree.isConstantSegregation)
    val validProp = Interpreter.toValidScriptType(prop)
    val res = IR.doCostingEx(Interpreter.emptyEnv, validProp, true)
    val costF = res.costF
    CheckCostFunc(IR)(IR.asRep[Any => Int](costF))
    val calcF = res.calcF
    CheckCalcFunc(IR)(calcF)
    res
  }

  /** Reduce this pre-compiled script in the given context.
    * This is equivalent to reduceToCrypto, except that graph construction is
    * completely avoided.
    */
  def reduce(context: InterpreterContext): ReductionResult = {
    import IR._
    implicit val vs = context.validationSettings
    val maxCost = context.costLimit
    val initCost = context.initCost
    trySoftForkable[ReductionResult](whenSoftFork = WhenSoftForkReductionResult(initCost)) {
      val costF = costingRes.costF
      val costingCtx = context.toSigmaContext(isCost = true)
      val estimatedCost = IR.checkCostWithContext(costingCtx, costF, maxCost, initCost).getOrThrow

      // check calc
      val calcF = costingRes.calcF
      val calcCtx = context.toSigmaContext(isCost = false)
      val res = Interpreter.calcResult(IR)(calcCtx, calcF)
      ReductionResult(SigmaDsl.toSigmaBoolean(res), estimatedCost)
    }
  }
}

/** Represents keys in the cache of precompiled ErgoTrees for repeated evaluation.
  * Note, [[SigmaValidationSettings]] are part of the key, which is important, because
  * the output of compilation depends on validation settings.
  *
  * @param ergoTreeBytes serialized bytes of ErgoTree instance (returned by ErgoTreeSerializer)
  * @param vs validation settings which where used for soft-forkable compilation.
  */
case class CacheKey(ergoTreeBytes: Seq[Byte], vs: SigmaValidationSettings)

/** Settings to configure script processor.
  * @param predefScripts collection of scripts to ALWAYS pre-compile (each given by ErgoTree bytes)
  * @param maxCacheSize  maximum number of entries in the cache
  * @param recordCacheStats if true, then cache statistics is recorded
  * @param reportingInterval number of cache load operations between two reporting events
  */
case class ScriptProcessorSettings(
  predefScripts: Seq[CacheKey],
  maxCacheSize: Int = 1000,
  recordCacheStats: Boolean = false,
  reportingInterval: Int = 100
)

/** Statistics of ScriptProcessor operations.
  * @param cacheStats cache statistics such as hits and misses
  * @param predefHits one hit counter for each predefined script
  */
case class ProcessorStats(cacheStats: CacheStats, predefHits: Seq[Int])

/** Script processor which holds pre-compiled reducers for the given scripts.
  * This class is thread-safe.
  */
class PrecompiledScriptProcessor(val settings: ScriptProcessorSettings) {

  /** Creates a new instance of IRContex to be used in reducers.
    * The default implementation can be overriden in derived classes.
    */
  protected def createIR(): IRContext = new RuntimeIRContext

  /** Holds for each ErgoTree bytes the corresponding pre-compiled reducer.
    * Since [[AVHashMap]] is not thread-safe it should be immutable
    * after it is constructed. However the counters will be updated on each increase.
    */
  protected val predefReducers: AVHashMap[CacheKey, (ScriptReducer, AtomicInteger)] = {
    implicit val IR: IRContext = createIR()
    val predefScripts = settings.predefScripts
    val res = AVHashMap[CacheKey, (ScriptReducer, AtomicInteger)](predefScripts.length)
    predefScripts.foreach { s =>
      val r = PrecompiledScriptReducer(s.ergoTreeBytes)
      val old = res.put(s, (r, new AtomicInteger(0)))
      require(old == null, s"duplicate predefined script: '${ErgoAlgos.encode(s.ergoTreeBytes.toArray)}'")
    }
    res
  }

  /** Obtain hit counter for each predefined script. The order of counters corresponds to
    * the order of the scripts in `settings`.
    * */
  def getPredefStats(): Seq[Int] = {
    val scriptKeys = settings.predefScripts.toArray
    val nScripts = scriptKeys.length
    val res = new Array[Int](nScripts)
    cfor(0)(_ < nScripts, _ + 1) { i =>
      val key = scriptKeys(i)
      val counter = predefReducers.get(key).get._2
      res(i) = counter.get()
    }
    res
  }

  /** Returns accumulated statistics info. */
  def getStats(): ProcessorStats = ProcessorStats(cache.stats(), getPredefStats())

  /** Called when the cache entry is evicted. */
  protected def onEvictedCacheEntry(key: CacheKey): Unit = {
  }

  /** Called when a cache item is removed. */
  protected val cacheListener = new RemovalListener[CacheKey, ScriptReducer]() {
    override def onRemoval(notification: RemovalNotification[CacheKey, ScriptReducer]): Unit = {
      if (notification.wasEvicted()) {
        onEvictedCacheEntry(notification.getKey)
      }
    }
  }

  /** Called to report processor stats. */
  protected def onReportStats(stats: ProcessorStats) = {
  }

  /** Loader to be used on a cache miss. The loader creates a new [[ScriptReducer]] for
    * the given [[CacheKey]]. The default loader creates an instance of
    * [[PrecompiledScriptReducer]] which stores its own IRContext and compiles costF,
    * calcF graphs. */
  protected val cacheLoader = new CacheLoader[CacheKey, ScriptReducer]() {
    /** Internal counter of all load operations happening an different threads. */
    private val loadCounter = new AtomicInteger(1)

    override def load(key: CacheKey): ScriptReducer = {
      val r = trySoftForkable[ScriptReducer](whenSoftFork = WhenSoftForkReducer) {
        PrecompiledScriptReducer(key.ergoTreeBytes)(createIR())
      }(key.vs)

      val c = loadCounter.incrementAndGet()
      if (c > settings.reportingInterval) {
        if (loadCounter.compareAndSet(c, 1)) {
          // call reporting only if we was able to reset the counter
          // avoid double reporting
          onReportStats(ProcessorStats(cache.stats(), getPredefStats()))
        }
      }

      r
    }
  }

  /** The cache which stores MRU set of pre-compiled reducers. */
  val cache: LoadingCache[CacheKey, ScriptReducer] = {
    var b = CacheBuilder.newBuilder
      .maximumSize(settings.maxCacheSize)
      .removalListener(cacheListener)
    if (settings.recordCacheStats) {
      b = b.recordStats()
    }
    b.build(cacheLoader)
  }

  /** An overload version of the method which looks up reducer for the given ErgoTree
    * using its 'bytes' property. See also `getReducer(key)`.
    *
    * @param ergoTree a tree to lookup pre-compiled reducer.
    * @param vs       validation settings which are used to detect soft-fork condition
    * @return a reducer for the given tree
    * May throw an exception if error happens and no soft-fork condition detected in `vs`.
    */
  def getReducer(ergoTree: ErgoTree, vs: SigmaValidationSettings): ScriptReducer = {
    val key = CacheKey(ergoTree.bytes, vs)
    getReducer(key)
  }

  /** Looks up reducer for the given key using its 'ergoTreeBytes' property.
    * It first looks up for predefReducers, if not found it looks up in the cache.
    * If there is no cache entry, the `cacheLoader` is used to load a new `ScriptReducer`.
    *
    * @param key a script key to lookup pre-compiled reducer.
    * @return a reducer for the given script
    * May throw an exception if error happens and no soft-fork condition detected in `key.vs`.
    */
  def getReducer(key: CacheKey): ScriptReducer = {
    predefReducers.get(key) match {
      case Nullable(r) =>
        if (settings.recordCacheStats) {
          r._2.incrementAndGet()  // update hit counter
        }
        r._1
      case _ =>
        val r = try {
          cache.get(key)
        } catch {
          case e: ExecutionException =>
            throw e.getCause
        }
        r
    }
  }
}

object PrecompiledScriptProcessor {
  /** Default script processor which uses [[RuntimeIRContext]] to process graphs. */
  val Default = new PrecompiledScriptProcessor(
    ScriptProcessorSettings(mutable.WrappedArray.empty[CacheKey]))

}
