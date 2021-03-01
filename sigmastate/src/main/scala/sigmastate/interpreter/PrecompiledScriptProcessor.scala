package sigmastate.interpreter

import com.google.common.cache.{RemovalListener, CacheBuilder, RemovalNotification, CacheLoader}
import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.validation.ValidationRules.{CheckCostFunc, CheckCalcFunc, trySoftForkable}
import scalan.{AVHashMap, Nullable}
import sigmastate.TrivialProp
import sigmastate.Values.ErgoTree
import sigmastate.eval.{RuntimeIRContext, IRContext}
import sigmastate.interpreter.Interpreter.ReductionResult
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.utils.Helpers._

import scala.collection.mutable

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
case class PrecompiledScriptReducer(scriptBytes: Seq[Byte])(implicit val IR: IRContext) {

  /** The following operations create [[RCostingResultEx]] structure for the given
    * `scriptBytes` and they should be the same as in `reduceToCrypto` method.
    * This can be viewed as ahead of time pre-compilation of the cost and calc graphs
    * which are reused over many invocations of the `reduce` method.
    */
  val costingRes = {
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
    trySoftForkable[ReductionResult](whenSoftFork = TrivialProp.TrueProp -> 0) {
      val costF = costingRes.costF
      val costingCtx = context.toSigmaContext(isCost = true)
      val estimatedCost = IR.checkCostWithContext(costingCtx, costF, maxCost, initCost).getOrThrow

      // check calc
      val calcF = costingRes.calcF
      val calcCtx = context.toSigmaContext(isCost = false)
      val res = Interpreter.calcResult(IR)(calcCtx, calcF)
      SigmaDsl.toSigmaBoolean(res) -> estimatedCost
    }
  }
}

/** Script processor which holds pre-compiled reducers for the given scripts.
  * @param predefScripts collection of scripts to ALWAYS pre-compile (each given by ErgoTree bytes)
  */
class PrecompiledScriptProcessor(val predefScripts: Seq[Seq[Byte]]) {
  /** Convenience synonym */
  type PSR = PrecompiledScriptReducer

  /** Creates a new instance of IRContex to be used in reducers.
    * The default implementation can be overriden in derived classes.
    */
  protected def createIR(): IRContext = new RuntimeIRContext

  /** Holds for each ErgoTree bytes the corresponding pre-compiled reducer. */
  val reducers = {
    implicit val IR: IRContext = createIR()
    val res = AVHashMap[Seq[Byte], PSR](predefScripts.length)
    predefScripts.foreach { s =>
      val r = PrecompiledScriptReducer(s)
      val old = res.put(s, r)
      require(old == null, s"duplicate predefined script: '${ErgoAlgos.encode(s.toArray)}'")
    }
    res
  }

  private val CacheListener = new RemovalListener[Seq[Byte], PSR]() {
    override def onRemoval(notification: RemovalNotification[Seq[Byte], PSR]): Unit = {
      if (notification.wasEvicted()) {
        val scriptHex = ErgoAlgos.encode(notification.getKey.toArray)
        println(s"Evicted: ${scriptHex}")
      }
    }
  }

  val cache = {
    CacheBuilder.newBuilder
      .maximumSize(1000)
      .removalListener(CacheListener)
      .recordStats()
      .build(new CacheLoader[Seq[Byte], PSR]() {
        override def load(key: Seq[Byte]): PSR = {
          PrecompiledScriptReducer(scriptBytes = key)(createIR())
        }
      })
  }

  /** Looks up verifier for the given ErgoTree using its 'bytes' property.
    * @param ergoTree a tree to lookup pre-compiled verifier.
    * @return non-empty Nullable instance with verifier for the given tree, otherwise
    *         Nullable.None
    */
  def getReducer(ergoTree: ErgoTree): Nullable[PSR] = {
    val key: Seq[Byte] = ergoTree.bytes
    reducers.get(key) match {
      case Nullable.None =>
        val verifier = cache.get(key)
        Nullable(verifier)
      case v => v
    }
  }
}

object PrecompiledScriptProcessor {
  val Default = new PrecompiledScriptProcessor(mutable.WrappedArray.empty[Seq[Byte]])
}
