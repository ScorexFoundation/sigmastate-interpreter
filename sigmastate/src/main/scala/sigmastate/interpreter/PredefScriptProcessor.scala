package sigmastate.interpreter

import org.ergoplatform.validation.ValidationRules.{trySoftForkable, CheckCostFunc, CheckCalcFunc}
import scalan.{AVHashMap, Nullable}
import sigmastate.TrivialProp
import sigmastate.Values.ErgoTree
import sigmastate.eval.{RuntimeIRContext, IRContext}
import sigmastate.interpreter.Interpreter.ReductionResult
import sigmastate.serialization.ErgoTreeSerializer
import sigmastate.utils.Helpers._

/** This class implements optimized verification of the given predefined script.
  * Pre-compilation of the necessary graphs is performed as part of constructor and
  * the graphs are stored in the given IR instance.
  *
  * The code make the following assumptions:
  * 1) the given script doesn't contain [[sigmastate.utxo.DeserializeContext]] and
  * [[sigmastate.utxo.DeserializeRegister]]
  * 2) Soft-forkability checks are not performed in constructor, thus any exception is
  * propagated up the stack.
  *
  * The code should correspond to reduceToCrypto method, but some operations may be
  * optimized due to assumptions above.
  */
case class PredefScriptVerifier(scriptBytes: Seq[Byte])(implicit val IR: IRContext) {

  /** The following operations create [[RCostingResultEx]] structure for the given
    * `scriptBytes` and they should be the same as in `reduceToCrypto` method.
    * This can be viewed as ahead of time pre-compilation of the cost and calc graphs
    * which are reused many times in the `verify` method.
    */
  val costingRes = {
    val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(scriptBytes.toArray)
    val prop = tree.toProposition(tree.isConstantSegregation)
    val res = IR.doCostingEx(Interpreter.emptyEnv, prop, true)
    val costF = res.costF
    CheckCostFunc(IR)(IR.asRep[Any => Int](costF))
    val calcF = res.calcF
    CheckCalcFunc(IR)(calcF)
    res
  }

  /** Verify this pre-compiled script in the given context.
    * This is equivalent to reduceToCrypto, except that graph construction is
    * completely avoided.
    */
  def verify(context: InterpreterContext): ReductionResult = {
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

/** Script processor which holds pre-compiled verifiers for the given scripts.
  * @param predefScripts collection of scripts to pre-compile (each given by ErgoTree bytes)
  */
case class PredefScriptProcessor(predefScripts: Seq[Seq[Byte]]) {
  private implicit val IR: IRContext = new RuntimeIRContext

  /** Holds for each ErgoTree bytes the corresponding pre-compiled verifier. */
  val verifiers = {
    val res = AVHashMap[Seq[Byte], PredefScriptVerifier](predefScripts.length)
    predefScripts.foreach { s =>
      val verifier = PredefScriptVerifier(s)
      res.put(s, verifier)
    }
    res
  }

  /** Looks up verifier for the given ErgoTree using its 'bytes' property.
    * @param ergoTree a tree to lookup pre-compiled verifier.
    * @return non-empty Nullable instance with verifier for the given tree, otherwise
    *         Nullable.None
    */
  def getVerifier(ergoTree: ErgoTree): Nullable[PredefScriptVerifier] = {
    val key: Seq[Byte] = ergoTree.bytes
    verifiers.get(key)
  }
}
