package sigmastate.interpreter

import java.util

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule, strategy}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.validation.SigmaValidationSettings
import org.ergoplatform.validation.ValidationRules._
import sigmastate.basics.DLogProtocol.ProveDlog
import scorex.util.ScorexLogging
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.basics.DLogProtocol.{DLogInteractiveProver, FirstDLogProverMessage}
import sigmastate.basics._
import sigmastate.interpreter.Interpreter._
import sigmastate.lang.exceptions.InterpreterException
import sigmastate.serialization.{SigmaSerializer, ValueSerializer}
import sigmastate.utxo.DeserializeContext
import sigmastate.{SType, _}
import sigmastate.eval.{Evaluation, IRContext, Profiler}
import scalan.util.BenchmarkUtil
import sigmastate.FiatShamirTree._
import sigmastate.SigSerializer._
import sigmastate.eval.Evaluation.addCostChecked
import sigmastate.interpreter.ErgoTreeEvaluator.fixedCostOp
import sigmastate.interpreter.EvalSettings._
import sigmastate.utils.Helpers._
import sigmastate.lang.Terms.ValueOps
import spire.syntax.all.cfor

import scala.util.{Success, Try}

/** Base (verifying) interpreter of ErgoTrees.
  * Can perform:
  * - ErgoTree evaluation (aka reduction) to sigma proposition (aka
  *  SigmaBoolean) in the given context.
  * - verification of ErgoTree in the given context.
  *
  * NOTE: In version v5.0 this interpreter contains two alternative implementations.
  * 1) Old implementation from v4.x which is based on AOT costing
  * 2) New implementation added in v5.0 which is based on JIT costing (see methods
  *    with JITC suffix).
  *
  * Both implementations are equivalent in v5.0, but have different performance
  * as result they produce different cost estimations.
  *
  * The interpreter has evaluationMode which defines how it should execute scripts.
  * @see verify, fullReduction
  */
trait Interpreter extends ScorexLogging {

  import Interpreter.AotReductionResult

  type CTX <: InterpreterContext

  type ProofT = UncheckedTree

  protected val IR: IRContext
  import IR._

  /** Processor instance which is used by this interpreter to execute ErgoTrees that
    * contain neither [[DeserializeContext]] nor [[sigmastate.utxo.DeserializeRegister]]
    * operations.
    */
  protected def precompiledScriptProcessor: PrecompiledScriptProcessor

  /** Evaluation settings used by [[ErgoTreeEvaluator]] which is used by this
    * interpreter to perform fullReduction.
    */
  protected def evalSettings: EvalSettings = ErgoTreeEvaluator.DefaultEvalSettings

  /** Logs the given message string. Can be overridden in the derived interpreter classes
    * to redefine the default behavior. */
  protected def logMessage(msg: String) = {
    println(msg)
  }

  /** Deserializes given script bytes using ValueSerializer (i.e. assuming expression tree format).
    * It also measures tree complexity adding to the total estimated cost of script execution.
    * The new returned context contains increased `initCost` and should be used for further processing.
    *
    * The method SHOULD be called only inside trySoftForkable scope, to make deserialization soft-forkable.
    *
    * NOTE: While ErgoTree is always of type SigmaProp, ValueSerializer can serialize expression of any type.
    * So it cannot be replaced with ErgoTreeSerializer here.
    */
  protected def deserializeMeasured(context: CTX, scriptBytes: Array[Byte]): (CTX, Value[SType]) = {
    val r = SigmaSerializer.startReader(scriptBytes)
    r.complexity = 0
    val script = ValueSerializer.deserialize(r)  // Why ValueSerializer? read NOTE above
    val scriptComplexity = r.complexity

    val currCost = Evaluation.addCostChecked(context.initCost, scriptComplexity, context.costLimit)
    val ctx1 = context.withInitCost(currCost).asInstanceOf[CTX]
    (ctx1, script)
  }

  /** @param updateContext  call back to setup new context (with updated cost limit) to be passed next time */
  protected def substDeserialize(context: CTX, updateContext: CTX => Unit, node: SValue): Option[SValue] = node match {
    case d: DeserializeContext[_] =>
      if (context.extension.values.contains(d.id))
        context.extension.values(d.id) match {
          case eba: EvaluatedValue[SByteArray]@unchecked if eba.tpe == SByteArray =>
            val scriptBytes = eba.value.toArray
            val (ctx1, script) = deserializeMeasured(context, scriptBytes)
            updateContext(ctx1)

            CheckDeserializedScriptType(d, script)
            Some(script)
          case _ =>
            None
        }
      else
        None
    case _ => None
  }

  /** Extracts proposition for ErgoTree handing soft-fork condition.
    * @note soft-fork handler */
  protected def propositionFromErgoTree(ergoTree: ErgoTree, context: CTX): SigmaPropValue = {
    val validationSettings = context.validationSettings
    val prop = ergoTree.root match {
      case Right(_) =>
        ergoTree.toProposition(ergoTree.isConstantSegregation)
      case Left(UnparsedErgoTree(_, error)) if validationSettings.isSoftFork(error) =>
        TrueSigmaProp
      case Left(UnparsedErgoTree(_, error)) =>
        throw new InterpreterException(
          "Script has not been recognized due to ValidationException, and it cannot be accepted as soft-fork.", None, Some(error))
    }
    prop
  }

  /** Substitute Deserialize* nodes with deserialized subtrees
    * We can estimate cost of the tree evaluation only after this step.*/
  private def applyDeserializeContext(context: CTX, exp: Value[SType]): (BoolValue, CTX) = {
    val currContext = new MutableCell(context)
    val substRule = strategy[Any] { case x: SValue =>
      substDeserialize(currContext.value, { ctx: CTX => currContext.value = ctx }, x)
    }
    val Some(substTree: SValue) = everywherebu(substRule)(exp)
    val res = Interpreter.toValidScriptType(substTree)
    (res, currContext.value)
  }

  /** Same as applyDeserializeContext, but returns SigmaPropValue instead of BoolValue.
    * This is necessary because new interpreter, while ultimately produces the same
    * results as the old interpreter, it is implemented differently internally.
    */
  private def applyDeserializeContextJITC(context: CTX, exp: Value[SType]): (SigmaPropValue, CTX) = {
    val currContext = new MutableCell(context)
    val substRule = strategy[Any] { case x: SValue =>
      substDeserialize(currContext.value, { ctx: CTX => currContext.value = ctx }, x)
    }
    val Some(substTree: SValue) = everywherebu(substRule)(exp)
    val res = toValidScriptTypeJITC(substTree)
    (res, currContext.value)
  }

  /** This method is used in both prover and verifier to compute SigmaBoolean value.
    * As the first step the cost of computing the `exp` expression in the given context is estimated.
    * If cost is above limit then exception is returned and `exp` is not executed
    * else `exp` is computed in the given context and the resulting SigmaBoolean returned.
    *
    * @param context the context in which `exp` should be executed
    * @param env     environment of variables used by the interpreter internally.
    *                Note, this is not system environment variables.
    * @param exp     expression to be executed in the given `context`
    * @return result of script reduction
    * @see `ReductionResult`
    */
  protected def reduceToCrypto(context: CTX, env: ScriptEnv, exp: Value[SType]): Try[AotReductionResult] = Try {
    import IR._
    implicit val vs = context.validationSettings
    val maxCost = context.costLimit
    val initCost = context.initCost
    trySoftForkable[AotReductionResult](whenSoftFork = WhenSoftForkReductionResult(initCost)) {
      val costingRes = doCostingEx(env, exp)
      val costF = costingRes.costF
      IR.onCostingResult(env, exp, costingRes)

      CheckCostFunc(IR)(asRep[Any => Int](costF))

      val costingCtx = context.toSigmaContext(isCost = true)
      val estimatedCost = IR.checkCostWithContext(costingCtx, costF, maxCost, initCost).getOrThrow

      IR.onEstimatedCost(env, exp, costingRes, costingCtx, estimatedCost)

      // check calc
      val calcF = costingRes.calcF
      CheckCalcFunc(IR)(calcF)
      val calcCtx = context.toSigmaContext(isCost = false)
      val res = Interpreter.calcResult(IR)(calcCtx, calcF)
      AotReductionResult(SigmaDsl.toSigmaBoolean(res), estimatedCost)
    }
  }

  /** This method uses the new JIT costing with direct ErgoTree execution. It is used in
    * both prover and verifier to compute SigmaProp value.
    * As the first step the cost of computing the `exp` expression in the given context is
    * estimated.
    * If cost is above limit then exception is returned and `exp` is not executed
    * else `exp` is computed in the given context and the resulting SigmaBoolean returned.
    *
    * @param context        the context in which `exp` should be executed
    * @param env            environment of system variables used by the interpreter internally
    * @param exp            expression to be executed in the given `context`
    * @return result of script reduction
    * @see `ReductionResult`
    */
  protected def reduceToCryptoJITC(context: CTX, env: ScriptEnv, exp: SigmaPropValue): Try[JitReductionResult] = Try {
    implicit val vs = context.validationSettings
    trySoftForkable[JitReductionResult](whenSoftFork = WhenSoftForkJitReductionResult(context.initCost)) {

      val (resProp, cost) = {
        val ctx = context.asInstanceOf[ErgoLikeContext]
        ErgoTreeEvaluator.eval(ctx, ErgoTree.EmptyConstants, exp, evalSettings) match {
          case (p: special.sigma.SigmaProp, c) => (p, c)
          case (res, _) =>
            sys.error(s"Invalid result type of $res: expected SigmaProp when evaluating $exp")
        }
      }

      JitReductionResult(SigmaDsl.toSigmaBoolean(resProp), cost.toLong)
    }
  }

  /** Full reduction of contract proposition given in the ErgoTree form to a SigmaBoolean value
    * which encodes either a sigma-protocol proposition or a boolean (true or false) value.
    *
    * Works as follows:
    * 1) parse ErgoTree instance into a typed AST
    * 2) go bottom-up the tree to replace DeserializeContext nodes only
    * 3) estimate cost and reduce the AST to a SigmaBoolean instance (either sigma-tree or
    * trivial boolean value)
    *
    * @param ergoTree input ErgoTree expression to reduce
    * @param ctx      context used in reduction
    * @param env      script environment
    * @return reduction result as a pair of sigma boolean and the accumulated cost counter
    *         after reduction
    */
  def fullReduction(ergoTree: ErgoTree,
                    ctx: CTX,
                    env: ScriptEnv): FullReductionResult = {
    implicit val vs: SigmaValidationSettings = ctx.validationSettings
    val context = ctx.withErgoTreeVersion(ergoTree.version).asInstanceOf[CTX]
    VersionContext.withVersions(context.activatedScriptVersion, ergoTree.version) {
      val prop = propositionFromErgoTree(ergoTree, context)
      val evalMode = getEvaluationMode(context)

      val res = prop match {
        case SigmaPropConstant(p) =>
          val sb = SigmaDsl.toSigmaBoolean(p)

          var aotRes: AotReductionResult = null
          if (evalMode.okEvaluateAot) {
            val aotCost = SigmaBoolean.estimateCost(sb)
            val resAotCost = Evaluation.addCostChecked(context.initCost, aotCost, context.costLimit)
            aotRes = AotReductionResult(sb, resAotCost)
          }

          var jitRes: JitReductionResult = null
          if (evalMode.okEvaluateJit) {
            // NOTE, evaluator cost unit needs to be scaled to the cost unit of context
            val jitCost = Eval_SigmaPropConstant.costKind.cost.toBlockCost
            val resJitCost = Evaluation.addCostChecked(context.initCost, jitCost, context.costLimit)
            jitRes = JitReductionResult(sb, resJitCost)
          }
          FullReductionResult(aotRes, jitRes)
        case _ if !ergoTree.hasDeserialize =>
          var aotRes: AotReductionResult = null
          if (evalMode.okEvaluateAot) {
            val r = precompiledScriptProcessor.getReducer(ergoTree, context.validationSettings)
            aotRes = r.reduce(context)
          }

          var jitRes: JitReductionResult = null
          if (evalMode.okEvaluateJit) {
            val ctx = context.asInstanceOf[ErgoLikeContext]
            jitRes = VersionContext.withVersions(ctx.activatedScriptVersion, ergoTree.version) {
              ErgoTreeEvaluator.evalToCrypto(ctx, ergoTree, evalSettings)
            }
          }
          FullReductionResult(aotRes, jitRes)
        case _ =>
          reductionWithDeserialize(ergoTree, prop, context, env, evalMode)
      }
      res
    }
  }

  /** Full reduction of contract proposition given in the ErgoTree form to a SigmaBoolean value
    * which encodes either a sigma-protocol proposition or a boolean (true or false) value.
    * See other overload for details.
    */
  def fullReduction(ergoTree: ErgoTree, ctx: CTX): FullReductionResult = {
    fullReduction(ergoTree, ctx, Interpreter.emptyEnv)
  }

  /** Performs reduction of proposition which contains deserialization operations. */
  private def reductionWithDeserialize(ergoTree: ErgoTree,
                                       prop: SigmaPropValue,
                                       context: CTX,
                                       env: ScriptEnv,
                                       evalMode: EvaluationMode): FullReductionResult = {
    implicit val vs: SigmaValidationSettings = context.validationSettings
    var aotRes: AotReductionResult = null
    if (evalMode.okEvaluateAot) {
      val (propTree, context2) = trySoftForkable[(BoolValue, CTX)](whenSoftFork = (TrueLeaf, context)) {
        applyDeserializeContext(context, prop)
      }

      // here we assume that when `propTree` is TrueProp then `reduceToCrypto` always succeeds
      // and the rest of the verification is also trivial
      aotRes = reduceToCrypto(context2, env, propTree).getOrThrow
    }

    var jitRes: JitReductionResult = null
    if (evalMode.okEvaluateJit) {
      jitRes = VersionContext.withVersions(context.activatedScriptVersion, ergoTree.version) {
        val (propTree, context2) = trySoftForkable[(SigmaPropValue, CTX)](whenSoftFork = (TrueSigmaProp, context)) {
          applyDeserializeContextJITC(context, prop)
        }

        // here we assume that when `propTree` is TrueProp then `reduceToCrypto` always succeeds
        // and the rest of the verification is also trivial
        reduceToCryptoJITC(context2, env, propTree).getOrThrow
      }
    }

    FullReductionResult(aotRes, jitRes)
  }

  /** Adds the cost to verify sigma protocol proposition.
    * This is AOT part of JITC-based interpreter, it predicts the cost of crypto
    * verification, which is asymptotically much faster and protects from spam scripts.
    *
    * @param jitRes    result of JIT-based reduction
    * @param costLimit total cost limit to check and raise exception if exceeded
    * @return computed jitRes.cost + crypto verification cost
    */
  protected def addCryptoCost(jitRes: JitReductionResult, costLimit: Long) = {
    val cryptoCost = estimateCryptoVerifyCost(jitRes.value).toBlockCost // scale JitCost to tx cost

    // Note, jitRes.cost is already scaled in fullReduction
    val fullJitCost = addCostChecked(jitRes.cost, cryptoCost, costLimit)
    fullJitCost
  }

  /** Returns evaluation mode used by this interpreter in the given context.
    * By default evaluation mode is determined based on `context.activatedScriptVersion`
    * so that the interpreter works either as v4.x of v5.0.
    *
    * Alternatively, the required evaluation mode can be specified by giving Some(mode)
    * value in `evalSettings.evaluationMode`, in which case the interpreter works as
    * specified.
    */
  protected def getEvaluationMode(context: CTX): EvaluationMode = {
    evalSettings.evaluationMode.getOrElse {
      if (context.activatedScriptVersion < VersionContext.JitActivationVersion)
        AotEvaluationMode
      else
        JitEvaluationMode
    }
  }

  /** Checks the possible soft-fork condition.
    *
    * @param ergoTree contract which needs to be executed
    * @param context  evaluation context to use for detecting soft-fork condition
    * @return `None`, if no soft-fork has been detected and ErgoTree execution can proceed normally
    *         `Some(true -> context.initCost)`, if soft-fork has been detected, but we
    *         cannot proceed with ErgoTree, however can accept relying on 90% of upgraded
    *         nodes (due to activation has already been done).
    * @throws InterpreterException when cannot proceed and no activation yet.
    */
  protected def checkSoftForkCondition(ergoTree: ErgoTree, context: CTX): Option[VerificationResult] = {
    // TODO v6.0: the condition below should be revised if necessary
    // The following conditions define behavior which depend on the version of ergoTree
    // This works in addition to more fine-grained soft-forkability mechanism implemented
    // using ValidationRules (see trySoftForkable method call here and in reduceToCrypto).

    if (context.activatedScriptVersion > VersionContext.MaxSupportedScriptVersion) {
      // The activated protocol exceeds capabilities of this interpreter.
      // NOTE: this path should never be taken for validation of candidate blocks
      // in which case Ergo node should always pass Interpreter.MaxSupportedScriptVersion
      // as the value of ErgoLikeContext.activatedScriptVersion.
      // see also ErgoLikeContext ScalaDoc.

      // Currently more than 90% of nodes has already switched to a higher version,
      // thus we can accept without verification, but only if we cannot verify
      // the given ergoTree
      if (ergoTree.version > VersionContext.MaxSupportedScriptVersion) {
        // We accept the box spending and rely on 90% of all the other nodes.
        // Thus, the old node will stay in sync with the network.
        return Some(true -> context.initCost)
      }
      // otherwise, we can verify the box spending and thus, proceed normally

    } else {
      // activated version is within the supported range [0..MaxSupportedScriptVersion]
      // in addition, ErgoTree version should never exceed the currently activated protocol

      if (ergoTree.version > context.activatedScriptVersion) {
        throw new InterpreterException(
          s"ErgoTree version ${ergoTree.version} is higher than activated ${context.activatedScriptVersion}")
      }
    }
    None // proceed normally
  }

  /** Executes the script in a given context.
    * Step 1: Deserialize context variables
    * Step 2: Evaluate expression and produce SigmaProp value, which is zero-knowledge
    *         statement (see also `SigmaBoolean`).
    * Step 3: Verify that the proof is presented to satisfy SigmaProp conditions.
    *
    * NOTE, ergoTree.complexity is not added to the cost when v5.0 is activated
    *
    * @param env      environment of system variables used by the interpreter internally
    * @param ergoTree ErgoTree expression to execute in the given context and verify its
    *                 result
    * @param context  the context in which `exp` should be executed
    * @param proof    The proof of knowledge of the secrets which is expected by the
    *                 resulting SigmaProp
    * @param message  message bytes, which are used in verification of the proof
    * @return verification result or Exception.
    *         If if the estimated cost of execution of the `exp` exceeds the limit (given
    *         in `context`), then exception if thrown and packed in Try.
    *         If the first component is false, then:
    *         1) script executed to false or
    *         2) the given proof failed to validate resulting SigmaProp conditions.
    * @see `reduceToCrypto`
    */
  def verify(env: ScriptEnv,
             ergoTree: ErgoTree,
             context: CTX,
             proof: Array[Byte],
             message: Array[Byte]): Try[VerificationResult] = {
    val res = Try {
      checkSoftForkCondition(ergoTree, context) match {
        case Some(resWhenSoftFork) => return Success(resWhenSoftFork)
        case None => // proceed normally
      }
      VersionContext.withVersions(context.activatedScriptVersion, ergoTree.version) {
        val evalMode = getEvaluationMode(context)
        evalMode match {
          case AotEvaluationMode =>
            val complexityCost = ergoTree.complexity.toLong
            val initCost = Evaluation.addCostChecked(context.initCost, complexityCost, context.costLimit)
            val contextWithCost = context.withInitCost(initCost).asInstanceOf[CTX]

            val reduced = fullReduction(ergoTree, contextWithCost, env)
            reduced.value match {
              case TrivialProp.TrueProp => (true, reduced.cost)
              case TrivialProp.FalseProp => (false, reduced.cost)
              case _ =>
                val ok = if (evalSettings.isMeasureOperationTime) {
                  val E = ErgoTreeEvaluator.forProfiling(verifySignatureProfiler, evalSettings)
                  verifySignature(reduced.value, message, proof)(E)
                } else {
                  verifySignature(reduced.value, message, proof)(null)
                }
                (ok, reduced.cost)
            }

          case JitEvaluationMode =>
            // NOTE, ergoTree.complexity is not acrued to the cost in v5.0
            val reduced = fullReduction(ergoTree, context, env)
            reduced.value match {
              case TrivialProp.TrueProp => (true, reduced.cost)
              case TrivialProp.FalseProp => (false, reduced.cost)
              case _ =>
                val fullJitCost = addCryptoCost(reduced.jitRes, context.costLimit)

                val ok = if (evalSettings.isMeasureOperationTime) {
                  val E = ErgoTreeEvaluator.forProfiling(verifySignatureProfiler, evalSettings)
                  verifySignature(reduced.value, message, proof)(E)
                } else {
                  verifySignature(reduced.value, message, proof)(null)
                }
                (ok, fullJitCost)
            }
        }
      }
    }
    res
  }

  // Perform Verifier Steps 4-6
  private def checkCommitments(sp: UncheckedSigmaTree, message: Array[Byte])(implicit E: ErgoTreeEvaluator): Boolean = {
    // Perform Verifier Step 4
    val newRoot = computeCommitments(sp).get.asInstanceOf[UncheckedSigmaTree]
    val bytes = concatArrays(FiatShamirTree.toBytes(newRoot), message)
    /**
      * Verifier Steps 5-6: Convert the tree to a string `s` for input to the Fiat-Shamir hash function,
      * using the same conversion as the prover in 7
      * Accept the proof if the challenge at the root of the tree is equal to the Fiat-Shamir hash of `s`
      * (and, if applicable,  the associated data). Reject otherwise.
      */
    val expectedChallenge = CryptoFunctions.hashFn(bytes)
    util.Arrays.equals(newRoot.challenge, expectedChallenge)
  }

  /**
    * Verifier Step 4: For every leaf node, compute the commitment a from the challenge e and response $z$,
    * per the verifier algorithm of the leaf's Sigma-protocol.
    * If the verifier algorithm of the Sigma-protocol for any of the leaves rejects, then reject the entire proof.
    */
  private[sigmastate] val computeCommitments: Strategy = everywherebu(rule[Any] {
    case c: UncheckedConjecture => c // Do nothing for internal nodes

    case sn: UncheckedSchnorr =>
      implicit val E = ErgoTreeEvaluator.getCurrentEvaluator
      fixedCostOp(ComputeCommitments_Schnorr) {
        val a = DLogInteractiveProver.computeCommitment(sn.proposition, sn.challenge, sn.secondMessage)
        sn.copy(commitmentOpt = Some(FirstDLogProverMessage(a)))
      }

    case dh: UncheckedDiffieHellmanTuple =>
      implicit val E = ErgoTreeEvaluator.getCurrentEvaluator
      fixedCostOp(ComputeCommitments_DHT) {
        val (a, b) = DiffieHellmanTupleInteractiveProver.computeCommitment(dh.proposition, dh.challenge, dh.secondMessage)
        dh.copy(commitmentOpt = Some(FirstDiffieHellmanTupleProverMessage(a, b)))
      }

    case _: UncheckedSigmaTree => ???
  })

  def verify(ergoTree: ErgoTree,
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension).asInstanceOf[CTX]
    verify(Interpreter.emptyEnv, ergoTree, ctxv, proverResult.proof, message)
  }

  def verify(env: ScriptEnv,
             ergoTree: ErgoTree,
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension).asInstanceOf[CTX]
    verify(env, ergoTree, ctxv, proverResult.proof, message)
  }

  def verify(ergoTree: ErgoTree,
             context: CTX,
             proof: ProofT,
             message: Array[Byte]): Try[VerificationResult] = {
    verify(Interpreter.emptyEnv, ergoTree, context, SigSerializer.toProofBytes(proof), message)
  }

  /**
    * Verify a signature on given (arbitrary) message for a given public key.
    *
    * @param sigmaTree public key (represented as a tree)
    * @param message   message
    * @param signature signature for the message
    * @param E         optional evaluator (can be null) which is used for profiling of operations.
    *                  When `E` is `null`, then profiling is turned-off and has no effect on
    *                  the execution.
    * @return whether signature is valid or not
    */
  def verifySignature(sigmaTree: SigmaBoolean,
                      message: Array[Byte],
                      signature: Array[Byte])(implicit E: ErgoTreeEvaluator): Boolean = {
    // Perform Verifier Steps 1-3
    try {
      SigSerializer.parseAndComputeChallenges(sigmaTree, signature) match {
        case NoProof => false
        case sp: UncheckedSigmaTree =>
          // Perform Verifier Steps 4-6
          checkCommitments(sp, message)
      }
    } catch {
      case t: Throwable =>
        // TODO cover with tests
        //  NOTE, property("handle improper signature") doesn't lead to exception
        //  because the current implementation of parseAndComputeChallenges doesn't throw
        //  an exception
        log.warn("Improper signature: ", t);
        false
    }
  }

}

object Interpreter {
  /** Result of Box.ergoTree verification procedure (see `verify` method).
    * The first component is the value of Boolean type which represents a result of
    * SigmaProp condition verification via sigma protocol.
    * The second component is the estimated cost of contract execution. */
  type VerificationResult = (Boolean, Long)

  /** Result of ErgoTree reduction procedure (see `fullReduction`) */
  abstract class ReductionResult {
    /** The value of SigmaProp type which represents a logical statement verifiable via
      * sigma protocol.
      */
    def value: SigmaBoolean

    /** Estimated cost of the contract execution.*/
    def cost: Long
  }

  /** Result of ErgoTree reduction procedure (see `fullReduction`).
    *
    * @param value the value of SigmaProp type which represents a logical statement
    *              verifiable via sigma protocol.
    * @param cost  the estimated cost of the contract execution.
    */
  case class AotReductionResult(value: SigmaBoolean, cost: Long) extends ReductionResult

  /** Result of ErgoTree reduction procedure by JIT-based interpreter (see
    * `reduceToCrypto` and friends).
    *
    * @param value the value of SigmaProp type which represents a logical statement
    *              verifiable via sigma protocol.
    * @param cost  the estimated cost of the contract execution (in block's scale).
    */
  case class JitReductionResult(value: SigmaBoolean, cost: Long) extends ReductionResult

  /** Result of fullReduction to sigma tree with costing. */
  case class FullReductionResult(
    private[sigmastate] val aotRes: AotReductionResult,
    private[sigmastate] val jitRes: JitReductionResult
  ) extends ReductionResult {
    require(aotRes != null ^ jitRes != null, s"Either AOT or JIT result must be defined, but not both: $this")
    override def value: SigmaBoolean = if (aotRes != null) aotRes.value else jitRes.value
    override def cost: Long = if (aotRes != null) aotRes.cost else jitRes.cost
  }

  /** Represents properties of interpreter invocation. */
  type ScriptEnv = Map[String, Any]

  /** Empty interpreter properties. */
  val emptyEnv: ScriptEnv = Map.empty[String, Any]

  /** Property name used to store script name. */
  val ScriptNameProp = "ScriptName"

  /** The result of script reduction when soft-fork condition is detected by the old node,
    * in which case the script is reduced to the trivial true proposition and takes up 0 cost.
    */
  def WhenSoftForkReductionResult(cost: Long): AotReductionResult = AotReductionResult(TrivialProp.TrueProp, cost)

  /** The result of script reduction when soft-fork condition is detected by the old node,
    * in which case the script is reduced to the trivial true proposition and takes up 0 cost.
    */
  def WhenSoftForkJitReductionResult(cost: Long): JitReductionResult = JitReductionResult(TrivialProp.TrueProp, cost)

  /** Represents the cost of computing DLogInteractiveProver.computeCommitment. */
  final val ComputeCommitments_Schnorr = OperationCostInfo(
    FixedCost(JitCost(3400)), NamedDesc("ComputeCommitments_Schnorr"))

  /** Represents the cost of computing DiffieHellmanTupleInteractiveProver.computeCommitment. */
  final val ComputeCommitments_DHT = OperationCostInfo(
    FixedCost(JitCost(6450)), NamedDesc("ComputeCommitments_DHT"))

  /** Represents the cost spent by JIT evaluator on a simple ErgoTree containing
    * SigmaPropConstant.
    * It doesn't include cost of crypto verification.
    */
  final val Eval_SigmaPropConstant = OperationCostInfo(
    FixedCost(JitCost(50)), NamedDesc("Eval_SigmaPropConstant"))

  /** Verification cost of each ProveDlog node of SigmaBoolean proposition tree. */
  final val ProveDlogVerificationCost =
    ParseChallenge_ProveDlog.costKind.cost +
    ComputeCommitments_Schnorr.costKind.cost +
    ToBytes_Schnorr.costKind.cost

  /** Verification cost of each ProveDHTuple node of SigmaBoolean proposition tree. */
  final val ProveDHTupleVerificationCost =
    ParseChallenge_ProveDHT.costKind.cost +
    ComputeCommitments_DHT.costKind.cost +
    ToBytes_DHT.costKind.cost

  /** Computes the estimated cost of verification of sigma proposition.
    * The cost is estimated ahead of time, without actually performing expencive crypto
    * operations.
    * @param sb sigma proposition
    * @return estimated cost of verification of the given proposition in JIT scale
    */
  def estimateCryptoVerifyCost(sb: SigmaBoolean): JitCost = {
    /** Recursively compute the total cost of the given children. */
    def childrenCost(children: Seq[SigmaBoolean]): JitCost = {
      val childrenArr = children.toArray
      val nChildren = childrenArr.length
      var sum = JitCost(0)
      cfor(0)(_ < nChildren, _ + 1) { i =>
        val c = estimateCryptoVerifyCost(childrenArr(i))
        sum = sum + c
      }
      sum
    }
    sb match {
      case _: ProveDlog => ProveDlogVerificationCost
      case _: ProveDHTuple => ProveDHTupleVerificationCost

      case and: CAND =>
        val nodeC = ToBytes_ProofTreeConjecture.costKind.cost
        val childrenC = childrenCost(and.children)
        nodeC + childrenC

      case or: COR =>
        val nodeC = ToBytes_ProofTreeConjecture.costKind.cost
        val childrenC = childrenCost(or.children)
        nodeC + childrenC

      case th: CTHRESHOLD =>
        val nChildren = th.children.length
        val nCoefs = nChildren - th.k
        val parseC = ParsePolynomial.costKind.cost(nCoefs)
        val evalC = EvaluatePolynomial.costKind.cost(nCoefs) * nChildren
        val nodeC = ToBytes_ProofTreeConjecture.costKind.cost
        val childernC = childrenCost(th.children)
        parseC + evalC + nodeC + childernC
      case _ =>
        JitCost(0)  // the cost of trivial proposition
    }
  }

  /** An instance of profiler used to measure cost parameters of verifySignature
    * operations.
    */
  val verifySignatureProfiler = new Profiler

  /** Executes the given `calcF` graph in the given context.
    * @param IR      container of the graph (see [[IRContext]])
    * @param context script execution context (built from [[org.ergoplatform.ErgoLikeContext]])
    * @param calcF   graph which represents a reduction function from Context to SigmaProp.
    * @return a reduction result
    */
  def calcResult(IR: IRContext)
                (context: special.sigma.Context,
                 calcF: IR.Ref[IR.Context => Any]): special.sigma.SigmaProp = {
    import IR._
    import IR.Context._
    import IR.SigmaProp._
    import IR.Liftables._
    val res = calcF.elem.eRange.asInstanceOf[Any] match {
      case _: SigmaPropElem[_] =>
        val valueFun = compile[IR.Context.SContext, IR.SigmaProp.SSigmaProp, IR.Context, IR.SigmaProp](
          getDataEnv,
          IR.asRep[IR.Context => IR.SigmaProp](calcF))(LiftableContext, LiftableSigmaProp)
        val (sp, _) = valueFun(context)
        sp
      case BooleanElement =>
        val valueFun = compile[SContext, Boolean, IR.Context, Boolean](
          IR.getDataEnv,
          asRep[IR.Context => Boolean](calcF))(LiftableContext, BooleanIsLiftable)
        val (b, _) = valueFun(context)
        sigmaDslBuilderValue.sigmaProp(b)
    }
    res
  }

  /** Special helper function which converts the given expression to expression returning
    * boolean or throws an exception if the conversion is not defined. */
  def toValidScriptType(exp: SValue): BoolValue = exp match {
    case v: Value[SBoolean.type]@unchecked if v.tpe == SBoolean => v
    case p: SValue if p.tpe == SSigmaProp => p.asBoolValue
    case x =>
      // This case is not possible, due to exp is always of Boolean/SigmaProp type.
      // In case it will ever change, leave it here to throw an explaining message.
      throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean or SigmaProp but was $x")
  }

  // TODO after HF: merge with old version (`toValidScriptType`)
  private def toValidScriptTypeJITC(exp: SValue): SigmaPropValue = exp match {
    case v: Value[SBoolean.type]@unchecked if v.tpe == SBoolean => v.toSigmaProp
    case p: SValue if p.tpe == SSigmaProp => p.asSigmaProp
    case x => throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean or SigmaProp but was $x")
  }

  /** Helper method to throw errors from Interpreter. */
  def error(msg: String) = throw new InterpreterException(msg)

}