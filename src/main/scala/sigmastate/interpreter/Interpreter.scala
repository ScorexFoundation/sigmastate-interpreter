package sigmastate.interpreter

import java.util

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{strategy, rule, everywherebu}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import sigmastate.basics.DLogProtocol.{FirstDLogProverMessage, DLogInteractiveProver}
import scorex.util.ScorexLogging
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.eval.{IRContext, Sized}
import sigmastate.lang.Terms.ValueOps
import sigmastate.basics._
import sigmastate.interpreter.Interpreter.{VerificationResult, ScriptEnv}
import sigmastate.lang.exceptions.InterpreterException
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.DeserializeContext
import sigmastate.{SType, _}
import org.ergoplatform.validation.ValidationRules._

import scala.util.Try

trait Interpreter extends ScorexLogging {

  import Interpreter.ReductionResult

  type CTX <: InterpreterContext

  type ProofT = UncheckedTree

  def substDeserialize(context: CTX, node: SValue): Option[SValue] = node match {
    case d: DeserializeContext[_] =>
      if (context.extension.values.contains(d.id))
        context.extension.values(d.id) match {
          case eba: EvaluatedValue[SByteArray]@unchecked if eba.tpe == SByteArray =>
            val script = ValueSerializer.deserialize(eba.value.toArray)
            CheckDeserializedScriptType(d, script) {
              Some(script)
            }
          case _ => None
        }
      else
        None
    case _ => None
  }

  val IR: IRContext
  import IR._

  def toValidScriptType(exp: SValue): BoolValue = exp match {
    case v: Value[SBoolean.type]@unchecked if v.tpe == SBoolean => v
    case p: SValue if p.tpe == SSigmaProp => p.asSigmaProp.isProven
    case x => throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean or SigmaProp but was $x")
  }

  /** Substitute Deserialize* nodes with deserialized subtrees
    * We can estimate cost of the tree evaluation only after this step.*/
  def applyDeserializeContext(context: CTX, exp: Value[SType]): BoolValue = {
    val substRule = strategy[Value[_ <: SType]] { case x => substDeserialize(context, x) }
    val Some(substTree: SValue) = everywherebu(substRule)(exp)
    val res = toValidScriptType(substTree)
    res
  }

  def checkCost(context: CTX, exp: Value[SType], costF: Rep[((Int, IR.Size[IR.Context])) => Int]): Int = {
    import IR.Size._
    import IR.Context._;
    val costingCtx = context.toSigmaContext(IR, isCost = true)
    val maxCost = context.costLimit
    val costFun = IR.compile[(Int, SSize[SContext]), Int, (Int, Size[Context]), Int](IR.getDataEnv, costF, Some(maxCost))
    val (_, estimatedCost) = costFun((0, Sized.sizeOf(costingCtx)))
    if (estimatedCost > maxCost) {
      throw new Error(s"Estimated expression complexity $estimatedCost exceeds the limit $maxCost in $exp")
    }
    estimatedCost
  }

  def calcResult(context: special.sigma.Context, calcF: Rep[IR.Context => Any]): special.sigma.SigmaProp = {
    import IR._
    import Context._
    import SigmaProp._
    val res = calcF.elem.eRange.asInstanceOf[Any] match {
      case _: SigmaPropElem[_] =>
        val valueFun = compile[SContext, SSigmaProp, Context, SigmaProp](getDataEnv, asRep[Context => SigmaProp](calcF))
        val (sp, _) = valueFun(context)
        sp
      case BooleanElement =>
        val valueFun = compile[SContext, Boolean, IR.Context, Boolean](IR.getDataEnv, asRep[Context => Boolean](calcF))
        val (b, _) = valueFun(context)
        sigmaDslBuilderValue.sigmaProp(b)
    }
    res
  }

  /** This method is used in both prover and verifier to compute SigmaProp value.
    * As the first step the cost of computing the `exp` expression in the given context is estimated.
    * If cost is above limit
    *   then exception is returned and `exp` is not executed
    *   else `exp` is computed in the given context and the resulting SigmaBoolean returned.
    *
    * @param context   the context in which `exp` should be executed
    * @param env       environment of system variables used by the interpreter internally
    * @param exp       expression to be executed in the given `context`
    * @return          result of script reduction
    * @see `ReductionResult`
    */
  def reduceToCrypto(context: CTX, env: ScriptEnv, exp: Value[SType]): Try[ReductionResult] = Try {
    import IR._
    implicit val vs = context.validationSettings
    val maxCost = context.costLimit
    trySoftForkable[ReductionResult](whenSoftFork = TrivialProp.TrueProp -> 0) {
      val costingRes @ Pair(calcF, costF) = doCostingEx(env, exp, true)
      IR.onCostingResult(env, exp, costingRes)

      CheckCostFunc(IR)(asRep[Any => Int](costF)) { }

      CheckCalcFunc(IR)(calcF) { }

      val costingCtx = context.toSigmaContext(IR, isCost = true)
      val estimatedCost = IR.checkCostWithContext(costingCtx, exp, costF, maxCost)
              .fold(t => throw t, identity)

      IR.onEstimatedCost(env, exp, costingRes, costingCtx, estimatedCost)

      // check calc
      val calcCtx = context.toSigmaContext(IR, isCost = false)
      val res = calcResult(calcCtx, calcF)
      SigmaDsl.toSigmaBoolean(res) -> estimatedCost
    }
  }

  def reduceToCrypto(context: CTX, exp: Value[SType]): Try[ReductionResult] =
    reduceToCrypto(context, Interpreter.emptyEnv, exp)

  /** Extracts proposition for ErgoTree handing soft-fork condition.
    * @note soft-fork handler */
  def propositionFromErgoTree(tree: ErgoTree, ctx: CTX): SigmaPropValue = {
    val prop = tree.root match {
      case Right(_) =>
        tree.toProposition(tree.isConstantSegregation)
      case Left(UnparsedErgoTree(_, error)) if ctx.validationSettings.isSoftFork(error) =>
        TrueSigmaProp
      case Left(UnparsedErgoTree(_, error)) =>
        throw new InterpreterException(
          "Script has not been recognized due to ValidationException, and it cannot be accepted as soft-fork.", None, Some(error))
    }
    prop
  }

  /** Executes the script in a given context.
    * Step 1: Deserialize context variables
    * Step 2: Evaluate expression and produce SigmaProp value, which is zero-knowledge statement (see also `SigmaBoolean`).
    * Step 3: Verify that the proof is presented to satisfy SigmaProp conditions.
    *
    * @param env       environment of system variables used by the interpreter internally
    * @param tree      ErgoTree to execute in the given context and verify its result
    * @param context   the context in which `exp` should be executed
    * @param proof     The proof of knowledge of the secrets which is expected by the resulting SigmaProp
    * @param message   message bytes, which are used in verification of the proof
    *
    * @return          verification result or Exception.
    *                   If if the estimated cost of execution of the `tree` exceeds the limit (given in `context`),
    *                   then exception if thrown and packed in Try.
    *                   If left component is false, then:
    *                    1) script executed to false or
    *                    2) the given proof faild to validate resulting SigmaProp conditions.
    * @see `reduceToCrypto`
    */
  def verify(env: ScriptEnv, tree: ErgoTree,
             context: CTX,
             proof: Array[Byte],
             message: Array[Byte]): Try[VerificationResult] = Try {
    val prop = propositionFromErgoTree(tree, context)
    implicit val vs = context.validationSettings
    val propTree = trySoftForkable[BoolValue](whenSoftFork = TrueLeaf) {
      applyDeserializeContext(context, prop)
    }

    // here we assume that when `propTree` is TrueProp then `reduceToCrypto` always succeeds
    // and the rest of the verification is also trivial
    val (cProp, cost) = reduceToCrypto(context, env, propTree).fold(t => throw t, identity)

    val checkingResult = cProp match {
      case TrivialProp.TrueProp => true
      case TrivialProp.FalseProp => false
      case _ =>
        // Perform Verifier Steps 1-3
        SigSerializer.parseAndComputeChallenges(cProp, proof) match {
          case NoProof => false
          case sp: UncheckedSigmaTree =>
            // Perform Verifier Step 4
            val newRoot = computeCommitments(sp).get.asInstanceOf[UncheckedSigmaTree]

            /**
              * Verifier Steps 5-6: Convert the tree to a string `s` for input to the Fiat-Shamir hash function,
              * using the same conversion as the prover in 7
              * Accept the proof if the challenge at the root of the tree is equal to the Fiat-Shamir hash of `s`
              * (and, if applicable,  the associated data). Reject otherwise.
              */
            val expectedChallenge = CryptoFunctions.hashFn(FiatShamirTree.toBytes(newRoot) ++ message)
            util.Arrays.equals(newRoot.challenge, expectedChallenge)
        }
    }
    checkingResult -> cost
  }

  /**
    * Verifier Step 4: For every leaf node, compute the commitment a from the challenge e and response $z$,
    * per the verifier algorithm of the leaf's Sigma-protocol.
    * If the verifier algorithm of the Sigma-protocol for any of the leaves rejects, then reject the entire proof.
    */
  val computeCommitments: Strategy = everywherebu(rule[UncheckedSigmaTree] {
    case c: UncheckedConjecture => c // Do nothing for internal nodes

    case sn: UncheckedSchnorr =>
      val a = DLogInteractiveProver.computeCommitment(sn.proposition, sn.challenge, sn.secondMessage)
      sn.copy(commitmentOpt = Some(FirstDLogProverMessage(a)))

    case dh: UncheckedDiffieHellmanTuple =>
      val (a, b) = DiffieHellmanTupleInteractiveProver.computeCommitment(dh.proposition, dh.challenge, dh.secondMessage)
      dh.copy(commitmentOpt = Some(FirstDiffieHellmanTupleProverMessage(a, b)))

    case _ => ???
  })

  def verify(exp: ErgoTree,
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension).asInstanceOf[CTX]
    verify(Interpreter.emptyEnv, exp, ctxv, proverResult.proof, message)
  }

  def verify(env: ScriptEnv,
             exp: ErgoTree,
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension).asInstanceOf[CTX]
    verify(env, exp, ctxv, proverResult.proof, message)
  }


  def verify(exp: ErgoTree,
             context: CTX,
             proof: ProofT,
             message: Array[Byte]): Try[VerificationResult] = {
    verify(Interpreter.emptyEnv, exp, context, SigSerializer.toBytes(proof), message)
  }

}

object Interpreter {
  /** Result of Box.ergoTree verification procedure (see `verify` method).
    * The first component is the value of Boolean type which represents a result of
    * SigmaProp condition verification via sigma protocol.
    * The second component is the estimated cost of contract execution. */
  type VerificationResult = (Boolean, Long)

  /** Result of ErgoTree reduction procedure (see `reduceToCrypto`).
    * The first component is the value of SigmaProp type which represents a statement
    * verifiable via sigma protocol.
    * The second component is the estimated cost of contract execution */
  type ReductionResult = (SigmaBoolean, Long)

  type ScriptEnv = Map[String, Any]
  val emptyEnv: ScriptEnv = Map.empty[String, Any]
  val ScriptNameProp = "ScriptName"

  def error(msg: String) = throw new InterpreterException(msg)

}
