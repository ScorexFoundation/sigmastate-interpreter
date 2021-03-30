package sigmastate.interpreter

import java.lang.{Math => JMath}
import java.util

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule, strategy}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.ergoplatform.validation.SigmaValidationSettings
import org.ergoplatform.validation.ValidationRules._
import sigmastate.basics.DLogProtocol.{DLogInteractiveProver, FirstDLogProverMessage}
import org.ergoplatform.ErgoLikeContext
import scorex.util.ScorexLogging
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.basics.DLogProtocol.{DLogInteractiveProver, FirstDLogProverMessage}
import sigmastate.basics._
import sigmastate.interpreter.Interpreter.{ReductionResult, ScriptEnv, VerificationResult, WhenSoftForkReductionResult}
import sigmastate.lang.exceptions.{CostLimitException, InterpreterException}
import sigmastate.serialization.{SigmaSerializer, ValueSerializer}
import sigmastate.utxo.{CostTable, DeserializeContext}
import sigmastate.{SType, _}
import sigmastate.eval.{Evaluation, IRContext, Profiler}
import scalan.{MutableLazy, Nullable}
import scalan.util.BenchmarkUtil
import sigmastate.interpreter.ErgoTreeEvaluator.{NamedDesc, OperationCostInfo, fixedCostOp}
import sigmastate.utils.Helpers._
import sigmastate.lang.Terms.ValueOps

import scala.util.{Success, Try}

trait Interpreter extends ScorexLogging {

  import Interpreter.ReductionResult

  type CTX <: InterpreterContext

  type ProofT = UncheckedTree

  val IR: IRContext
  import IR._

  /** Processor instance which is used by this interpreter to execute ErgoTrees that
    * contain neither [[DeserializeContext]] nor [[sigmastate.utxo.DeserializeRegister]]
    * operations.
    */
  def precompiledScriptProcessor: PrecompiledScriptProcessor

  /** Evaluation settings used by [[ErgoTreeEvaluator]] */
  def evalSettings: EvalSettings = ErgoTreeEvaluator.DefaultEvalSettings

  /** Specifies which cost to use as result of evaluation.
    * Used only in tests when evalSettings.isDebug == true.
    * The meaning of values:
    * - when Some(true) return AOT based cost
    * - when Some(false) return JIT based cost
    * - when None return AOT cost (but in addition compare jit and aot costs e.g. (jitCost -
    * treeComplexity) / 2 <= aotCost)
    */
  def returnAOTCost: Option[Boolean] = None

  /** Deserializes given script bytes using ValueSerializer (i.e. assuming expression tree format).
    * It also measures tree complexity adding to the total estimated cost of script execution.
    * The new returned context contains increased `initCost` and should be used for further processing.
    *
    * The method SHOULD be called only inside trySoftForkable scope, to make deserialization soft-forkable.
    *
    * NOTE: While ErgoTree is always of type SigmaProp, ValueSerializer can serialize expression of any type.
    * So it cannot be replaced with ErgoTreeSerializer here.
    */
  def deserializeMeasured(context: CTX, scriptBytes: Array[Byte]): (CTX, Value[SType]) = {
    val r = SigmaSerializer.startReader(scriptBytes)
    r.complexity = 0
    val script = ValueSerializer.deserialize(r)  // Why ValueSerializer? read NOTE above
    val scriptComplexity = r.complexity

    val currCost = Evaluation.addCostChecked(context.initCost, scriptComplexity, context.costLimit)
    val ctx1 = context.withInitCost(currCost).asInstanceOf[CTX]
    (ctx1, script)
  }

  /** @param updateContext  call back to setup new context (with updated cost limit) to be passed next time */
  def substDeserialize(context: CTX, updateContext: CTX => Unit, node: SValue): Option[SValue] = node match {
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

  // TODO after HF: merge with old version (`toValidScriptType`)
  private def toValidScriptTypeJITC(exp: SValue): SigmaPropValue = exp match {
    case v: Value[SBoolean.type]@unchecked if v.tpe == SBoolean => v.toSigmaProp
    case p: SValue if p.tpe == SSigmaProp => p.asSigmaProp
    case x => throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean or SigmaProp but was $x")
  }

  class MutableCell[T](var value: T)

  /** Extracts proposition for ErgoTree handing soft-fork condition.
    * @note soft-fork handler */
  def propositionFromErgoTree(ergoTree: ErgoTree, context: CTX): SigmaPropValue = {
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
  def applyDeserializeContext(context: CTX, exp: Value[SType]): (BoolValue, CTX) = {
    val currContext = new MutableCell(context)
    val substRule = strategy[Any] { case x: SValue =>
      substDeserialize(currContext.value, { ctx: CTX => currContext.value = ctx }, x)
    }
    val Some(substTree: SValue) = everywherebu(substRule)(exp)
    val res = Interpreter.toValidScriptType(substTree)
    (res, currContext.value)
  }

  // TODO after HF: merge with old version (`applyDeserializeContext`)
  /** Same as applyDeserializeContext, but returns SigmaPropValue instead of BoolValue. */
  def applyDeserializeContextJITC(context: CTX, exp: Value[SType]): (SigmaPropValue, CTX) = {
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
    * @param context        the context in which `exp` should be executed
    * @param env            environment of system variables used by the interpreter internally
    * @param exp            expression to be executed in the given `context`
    * @return result of script reduction
    * @see `ReductionResult`
    */
  def reduceToCrypto(context: CTX, env: ScriptEnv, exp: Value[SType]): Try[ReductionResult] = Try {
    import IR._
    implicit val vs = context.validationSettings
    val maxCost = context.costLimit
    val initCost = context.initCost
    trySoftForkable[ReductionResult](whenSoftFork = WhenSoftForkReductionResult(initCost)) {
      val costingRes = doCostingEx(env, exp, true)
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
      ReductionResult(SigmaDsl.toSigmaBoolean(res), estimatedCost)
    }
  }

  /** Reduces `exp` to SigmaProp under the default (empty) environment. */
  def reduceToCrypto(context: CTX, exp: Value[SType]): Try[ReductionResult] =
    reduceToCrypto(context, Interpreter.emptyEnv, exp)

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
    * @param treeComplexity amount of cost added to context.initCost as result of
    *                       applyDeserializeContext
    * @return result of script reduction
    * @see `ReductionResult`
    */
  def reduceToCryptoJITC(context: CTX, env: ScriptEnv, exp: SigmaPropValue): Try[ReductionResult] = Try {
    implicit val vs = context.validationSettings
    trySoftForkable[ReductionResult](whenSoftFork = WhenSoftForkReductionResult(context.initCost)) {

      val (res, cost) = {
        val ctx = context.asInstanceOf[ErgoLikeContext]
          .withInitCost(context.initCost * 10)   // adjust for Evaluator cost units scale
          .withCostLimit(context.costLimit * 10)
        ErgoTreeEvaluator.eval(ctx, ErgoTree.EmptyConstants, exp, evalSettings) match {
          case (p: special.sigma.SigmaProp, c) => (p, c)
          case (res, _) =>
            sys.error(s"Invalid result type of $res: expected SigmaProp when evaluating $exp")
        }
      }

      ReductionResult(SigmaDsl.toSigmaBoolean(res), cost.toLong / 10)  // scale back
    }
  }

  def compareResults(oldRes: Try[ReductionResult], newRes: Try[ReductionResult]) = {
//    assert(resNew == res, s"The new Evaluator result differ from the old: $resNew != $res")
//
//    def costErr = s"The JIT cost differ from the AOT: $costNew != $cost"
//
//    val resCost = returnAOTCost match {
//      case Some(true) => // AOT costing requested
//        if (costNew != cost) println(s"WARNING: $costErr")
//        cost
//      case Some(false) => // JIT costing requested
//        if (costNew != cost) println(s"WARNING: $costErr")
//        costNew
//      case None => // nothing special was requested
//        assert((costNew - treeComplexity) / 2 <= cost, s"The JIT cost is larger than the AOT: $costNew > $cost")
//        cost
//    }
//    resCost
  }

  /** Transforms ErgoTree into Value by replacing placeholders with constants and then
   * delegates to the main implementation method.
   */
  def reduceToCryptoJITC(context: CTX, tree: ErgoTree): Try[ReductionResult] =
    reduceToCryptoJITC(context, Interpreter.emptyEnv, tree.toProposition(tree.isConstantSegregation))

  /**
    * Full reduction of initial expression given in the ErgoTree form to a SigmaBoolean value
    * (which encodes whether a sigma-protocol proposition or a boolean value, so true or false).
    *
    * Works as follows:
    * 1) parse ErgoTree instance into a typed AST
    * 2) go bottom-up the tree to replace DeserializeContext nodes only
    * 3) estimate cost and reduce the AST to a SigmaBoolean instance (so sigma-tree or trivial boolean value)
    *
    *
    * @param ergoTree - input ErgoTree expression to reduce
    * @param context - context used in reduction
    * @param env - script environment
    * @return reduction result as a pair of sigma boolean and the accumulated cost counter after reduction
    */
  def fullReduction(ergoTree: ErgoTree,
                    context: CTX,
                    env: ScriptEnv): ReductionResult = {
    implicit val vs: SigmaValidationSettings = context.validationSettings
    val prop = propositionFromErgoTree(ergoTree, context)
    prop match {
      case SigmaPropConstant(p) =>
        val sb = SigmaDsl.toSigmaBoolean(p)
        val cost = SigmaBoolean.estimateCost(sb)
        val resCost = Evaluation.addCostChecked(context.initCost, cost, context.costLimit)
        val jitCost = SigmaBoolean.estimateCostJit(sb)
        ReductionResult(sb, resCost)
      case _ if !ergoTree.hasDeserialize =>
        val r = precompiledScriptProcessor.getReducer(ergoTree, context.validationSettings)
        val res = r.reduce(context)
        val ctx = context.asInstanceOf[ErgoLikeContext]
          .withInitCost(context.initCost * 10)    // adjust for Evaluator cost units scale
          .withCostLimit(context.costLimit * 10)
        val ReductionResult(v, c) = ErgoTreeEvaluator.evalToCrypto(ctx, ergoTree, evalSettings)
        val jitRes = ReductionResult(v, c / 10) // scale cost back
        checkResults(ergoTree, res, jitRes)
        res
      case _ =>
        reductionWithDeserialize(ergoTree, prop, context, env)
    }
  }

  private def checkResults(ergoTree: ErgoTree, res: ReductionResult, jitRes: ReductionResult) = {
    val oldValue = res.value
    val newValue = jitRes.value
    if (oldValue != newValue) {
      val msg =
        s"""Wrong JIT result: -----------------------------------------
          |ErgoTree: ${ergoTree.bytesHex}
          |Old result: $oldValue
          |New result: $newValue
          |------------------------------------------------------------""".stripMargin
      if (evalSettings.isTestRun) {
        error(msg)
      }
      else
        println(msg)
    }
    val oldCost = res.cost
    val newCost = jitRes.cost
    if (oldCost < newCost) {
      val msg =
        s"""Wrong JIT cost: -----------------------------------------
          |ErgoTree: ${ergoTree.bytesHex}
          |Old cost: $oldCost
          |New cost: $newCost
          |------------------------------------------------------------""".stripMargin
      if (evalSettings.isTestRun) {
        error(msg)
      }
      else
        println(msg)
    }
  }

  /** Performs reduction of proposition which contains deserialization operations. */
  private def reductionWithDeserialize(ergoTree: ErgoTree,
                                       prop: SigmaPropValue,
                                       context: CTX,
                                       env: ScriptEnv) = {
    implicit val vs: SigmaValidationSettings = context.validationSettings
    val res = {
      val (propTree, context2) = trySoftForkable[(BoolValue, CTX)](whenSoftFork = (TrueLeaf, context)) {
        applyDeserializeContext(context, prop)
      }

      // here we assume that when `propTree` is TrueProp then `reduceToCrypto` always succeeds
      // and the rest of the verification is also trivial
      reduceToCrypto(context2, env, propTree).getOrThrow
    }
    val jitRes = {
      val (propTree, context2) = trySoftForkable[(SigmaPropValue, CTX)](whenSoftFork = (TrueSigmaProp, context)) {
        applyDeserializeContextJITC(context, prop)
      }

      // here we assume that when `propTree` is TrueProp then `reduceToCrypto` always succeeds
      // and the rest of the verification is also trivial
      reduceToCryptoJITC(context2, env, propTree).getOrThrow
    }
    checkResults(ergoTree, res, jitRes)
    res
  }

  /** Executes the script in a given context.
    * Step 1: Deserialize context variables
    * Step 2: Evaluate expression and produce SigmaProp value, which is zero-knowledge
    *         statement (see also `SigmaBoolean`).
    * Step 3: Verify that the proof is presented to satisfy SigmaProp conditions.
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
    *         If left component is false, then:
    *         1) script executed to false or
    *         2) the given proof failed to validate resulting SigmaProp conditions.
    * @see `reduceToCrypto`
    */
  def verify(env: ScriptEnv,
             ergoTree: ErgoTree,
             context: CTX,
             proof: Array[Byte],
             message: Array[Byte]): Try[VerificationResult] = {
    val (res, t) = BenchmarkUtil.measureTime(Try {
      // TODO v5.0: the condition below should be revised if necessary
      // The following conditions define behavior which depend on the version of ergoTree
      // This works in addition to more fine-grained soft-forkability mechanism implemented
      // using ValidationRules (see trySoftForkable method call here and in reduceToCrypto).
      if (context.activatedScriptVersion > Interpreter.MaxSupportedScriptVersion) {
        // > 90% has already switched to higher version, accept without verification
        // NOTE: this path should never be taken for validation of candidate blocks
        // in which case Ergo node should always pass Interpreter.MaxSupportedScriptVersion
        // as the value of ErgoLikeContext.activatedScriptVersion.
        // see also ErgoLikeContext ScalaDoc.
        return Success(true -> context.initCost)
      } else {
        // activated version is within the supported range [0..MaxSupportedScriptVersion]
        // however
        if (ergoTree.version > context.activatedScriptVersion) {
          throw new InterpreterException(
            s"ErgoTree version ${ergoTree.version} is higher than activated ${context.activatedScriptVersion}")
        }
        // else proceed normally
      }

      val complexityCost = ergoTree.complexity.toLong
      val initCost = Evaluation.addCostChecked(context.initCost, complexityCost, context.costLimit)
      val contextWithCost = context.withInitCost(initCost).asInstanceOf[CTX]

      val ReductionResult(cProp, cost) = fullReduction(ergoTree, contextWithCost, env)

      val res = cProp match {
        case TrivialProp.TrueProp => (true, cost)
        case TrivialProp.FalseProp => (false, cost)
        case _ =>
          // TODO v5.0: add AOT estimation of sigma proposition verification
          //  Math.addExact(cost, SigmaBoolean.estimateCostJit(cProp))
          val fullCost = cost
          if (fullCost > context.costLimit) {
            // TODO cover with tests (2h)
            throw new CostLimitException(
              estimatedCost = fullCost,
              message = Evaluation.msgCostLimitError(fullCost, context.costLimit),
              cause = None)
          }
          val ok = if (evalSettings.isMeasureOperationTime) {
            val E = ErgoTreeEvaluator.forProfiling(Interpreter.verifySignatureProfiler, evalSettings)
            ErgoTreeEvaluator.currentEvaluator.withValue(E) {
              verifySignature(cProp, message, proof)
            }
          } else
            verifySignature(cProp, message, proof)
          (ok, fullCost)
      }
      res
    })
    if (outputComputedResults) {
      res.foreach { case (_, cost) =>
        val scaledCost = cost * 1 // this is the scale factor of CostModel with respect to the concrete hardware
        val timeMicro = t * 1000  // time in microseconds
        val error = if (scaledCost > timeMicro) {
          val error = ((scaledCost / timeMicro.toDouble - 1) * 100d).formatted(s"%10.3f")
          error
        } else {
          val error = (-(timeMicro.toDouble / scaledCost.toDouble - 1) * 100d).formatted(s"%10.3f")
          error
        }
        val name = "\"" + env.getOrElse(Interpreter.ScriptNameProp, "") + "\""
        println(s"Name-Time-Cost-Error\t$name\t$timeMicro\t$scaledCost\t$error")
      }
    }
    res
  }

  /** This is fast version of verification based on direct ErgoTree evaluator.
    * It produces different costs and cannot be used in Ergo v3.x
    */
  def verifyJit(env: ScriptEnv,
                ergoTree: ErgoTree,
                context: CTX,
                proof: Array[Byte],
                message: Array[Byte]): Try[VerificationResult] = {
    val (res, t) = BenchmarkUtil.measureTime(Try {

      val initCost = context.initCost
      val remainingLimit = context.costLimit - initCost
      if (remainingLimit <= 0)
        throw new CostLimitException(initCost, Evaluation.msgCostLimitError(initCost, context.costLimit), None)

      val context1 = context.withInitCost(initCost).asInstanceOf[CTX]
      val prop = propositionFromErgoTree(ergoTree, context1)

      implicit val vs = context1.validationSettings
      val (propTree, context2) = trySoftForkable[(SigmaPropValue, CTX)](whenSoftFork = (TrueLeaf, context1)) {
        applyDeserializeContextJITC(context1, prop)
      }

      // here we assume that when `propTree` is TrueProp then `reduceToCrypto` always succeeds
      // and the rest of the verification is also trivial
      // new JIT costing with direct ErgoTree execution
      val (cProp, cost) = {
        val ctx = context2.asInstanceOf[ErgoLikeContext]
        val (res, cost) = ErgoTreeEvaluator.eval(ctx, ErgoTree.EmptyConstants, propTree, evalSettings) match {
          case (p: special.sigma.SigmaProp, c) => (p, c)
          case (b: Boolean, c) => (SigmaDsl.sigmaProp(b), c)
          case (res, _) => sys.error(s"Invalid result type of $res: expected Boolean or SigmaProp when evaluating $propTree")
        }
        val scaledCost = JMath.multiplyExact(cost, CostTable.costFactorIncrease) / CostTable.costFactorDecrease
        val totalCost = JMath.addExact(initCost.toInt, scaledCost)
        (SigmaDsl.toSigmaBoolean(res), totalCost.toLong)
      }


      val checkingResult = cProp match {
        case TrivialProp.TrueProp => true
        case TrivialProp.FalseProp => false
        case cProp => verifySignature(cProp, message, proof)
      }
      checkingResult -> cost
    })
    if (outputComputedResults) {
      res.foreach { case (_, cost) =>
        val scaledCost = cost * 1 // this is the scale factor of CostModel with respect to the concrete hardware
        val timeMicro = t * 1000  // time in microseconds
        val error = if (scaledCost > timeMicro) {
          val error = ((scaledCost / timeMicro.toDouble - 1) * 100d).formatted(s"%10.3f")
          error
        } else {
          val error = (-(timeMicro.toDouble / scaledCost.toDouble - 1) * 100d).formatted(s"%10.3f")
          error
        }
        val name = "\"" + env.getOrElse(Interpreter.ScriptNameProp, "") + "\""
        println(s"Name-Time-Cost-Error\t$name\t$timeMicro\t$scaledCost\t$error")
      }
    }
    res
  }

  // Perform Verifier Steps 4-6
  private def checkCommitments(sp: UncheckedSigmaTree, message: Array[Byte]): Boolean = {
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

  final val ComputeCommitments_Schnorr = OperationCostInfo(
    FixedCost(3200), NamedDesc("ComputeCommitments_Schnorr"))

  final val ComputeCommitments_DHT = OperationCostInfo(
    FixedCost(6450), NamedDesc("ComputeCommitments_DHT"))

  /**
    * Verifier Step 4: For every leaf node, compute the commitment a from the challenge e and response $z$,
    * per the verifier algorithm of the leaf's Sigma-protocol.
    * If the verifier algorithm of the Sigma-protocol for any of the leaves rejects, then reject the entire proof.
    */
  val computeCommitments: Strategy = everywherebu(rule[Any] {
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
    * @param sigmaTree - public key (represented as a tree)
    * @param message - message
    * @param signature - signature for the message
    * @return - whether signature is valid or not
    */
  def verifySignature(sigmaTree: SigmaBoolean,
                      message: Array[Byte],
                      signature: Array[Byte]): Boolean = {
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
        // TODO coverage: property("handle improper signature") doesn't lead to exception
        //  because the current implementation of parseAndComputeChallenges doesn't check
        //  signature format
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

  /** Result of ErgoTree reduction procedure (see `reduceToCrypto` and friends).
    * The first component is the value of SigmaProp type which represents a statement
    * verifiable via sigma protocol.
    * The second component is the estimated cost of consumed by the contract execution. */
  case class ReductionResult(value: SigmaBoolean, cost: Long)

  type ScriptEnv = Map[String, Any]
  val emptyEnv: ScriptEnv = Map.empty[String, Any]
  val ScriptNameProp = "ScriptName"

  /** Maximum version of ErgoTree supported by this interpreter release.
    * See version bits in `ErgoTree.header` for more details.
    * This value should be increased with each new protocol update via soft-fork.
    * The following values are used for current and upcoming forks:
    * - version 3.x this value must be 0
    * - in v4.0 must be 1
    * - in v5.x must be 2
    * etc.
    */
  val MaxSupportedScriptVersion: Byte = 1 // supported versions 0 and 1

  /** The result of script reduction when soft-fork condition is detected by the old node,
    * in which case the script is reduced to the trivial true proposition and takes up 0 cost.
    */
  def WhenSoftForkReductionResult(cost: Long): ReductionResult = ReductionResult(TrivialProp.TrueProp, cost)

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
    case p: SValue if p.tpe == SSigmaProp => p.asSigmaProp.isProven
    case x =>
      // This case is not possible, due to exp is always of Boolean/SigmaProp type.
      // In case it will ever change, leave it here to throw an explaining message.
      throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean or SigmaProp but was $x")
  }

  def error(msg: String) = throw new InterpreterException(msg)

}