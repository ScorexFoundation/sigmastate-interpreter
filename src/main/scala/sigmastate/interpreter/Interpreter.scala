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
import sigmastate.lang.exceptions.{InterpreterException, CosterException}
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.DeserializeContext
import sigmastate.{SType, _}

import scala.util.Try


trait Interpreter extends ScorexLogging {

  import Interpreter.ReductionResult

  type CTX <: InterpreterContext

  type ProofT = UncheckedTree //todo:  ProofT <: UncheckedTree ?

  final val MaxByteArrayLength = 10000

  /**
    * Max cost of a script interpreter can accept
    */
  def maxCost: Long

  def substDeserialize(context: CTX, node: SValue): Option[SValue] = node match {
    case d: DeserializeContext[_] =>
      if (context.extension.values.contains(d.id))
        context.extension.values(d.id) match {
          case eba: EvaluatedValue[SByteArray]@unchecked if eba.tpe == SByteArray =>
            val script = ValueSerializer.deserialize(eba.value.toArray)
            if (d.tpe != script.tpe)
              throw new InterpreterException(s"Failed context deserialization of $d: expected deserialized script to have type ${d.tpe}; got ${script.tpe}")
            else
              Some(script)
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
    import IR.Size._; import IR.Context._;
    val costingCtx = context.toSigmaContext(IR, isCost = true)
    val costFun = IR.compile[(Int, SSize[SContext]), Int, (Int, Size[Context]), Int](IR.getDataEnv, costF, Some(maxCost))
    val (_, estimatedCost) = costFun((0, Sized.sizeOf(costingCtx)))
    if (estimatedCost > maxCost) {
      throw new Error(s"Estimated expression complexity $estimatedCost exceeds the limit $maxCost in $exp")
    }
    estimatedCost
  }

  def calcResult(context: special.sigma.Context, calcF: Rep[IR.Context => Any]): special.sigma.SigmaProp = {
    import IR._; import Context._; import SigmaProp._
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
  /**
    * As the first step both prover and verifier are applying context-specific transformations and then estimating
    * cost of the intermediate expression. If cost is above limit, abort. Otherwise, both prover and verifier are
    * reducing the expression in the same way.
    *
    * @param exp
    * @param context
    * @return
    */
  def reduceToCrypto(context: CTX, env: ScriptEnv, exp: Value[SType]): Try[ReductionResult] = Try {
    import IR._; import Size._; import Context._; import SigmaProp._
    val costingRes @ Pair(calcF, costF) = doCostingEx(env, exp, true)
    IR.onCostingResult(env, exp, costingRes)

    verifyCostFunc(asRep[Any => Int](costF)).fold(t => throw t, x => x)

    verifyIsProven(calcF).fold(t => throw t, x => x)

    val costingCtx = context.toSigmaContext(IR, isCost = true)
    val estimatedCost = checkCostWithContext(costingCtx, exp, costF, maxCost)
      .fold(t => throw new CosterException(
        s"Script cannot be executed $exp: ", exp.sourceContext.toList.headOption, Some(t)), identity)

//    println(s"reduceToCrypto: estimatedCost: $estimatedCost")
    
    // check calc
    val calcCtx = context.toSigmaContext(IR, isCost = false)
    val res = calcResult(calcCtx, calcF)
    SigmaDsl.toSigmaBoolean(res) -> estimatedCost
  }

  def reduceToCrypto(context: CTX, exp: Value[SType]): Try[ReductionResult] =
    reduceToCrypto(context, Interpreter.emptyEnv, exp)

  def verify(env: ScriptEnv, exp: ErgoTree,
             context: CTX,
             proof: Array[Byte],
             message: Array[Byte]): Try[VerificationResult] = Try {
    val propTree = applyDeserializeContext(context, exp.proposition)
    val (cProp, cost) = reduceToCrypto(context, env, propTree).get

    val checkingResult = cProp match {
      case TrueLeaf => true
      case FalseLeaf => false
      case cProp: SigmaBoolean =>
        cProp match {
          case TrivialProp.TrueProp => true
          case TrivialProp.FalseProp => false
          case _ =>
            // Perform Verifier Steps 1-3
            SigSerializer.parseAndComputeChallenges(cProp, proof) match {
              case NoProof => false
              case sp: UncheckedSigmaTree =>
                // Perform Verifier Step 4
                val newRoot = computeCommitments(sp).get.asInstanceOf[UncheckedSigmaTree] // todo: is this "asInstanceOf" necessary?

                /**
                  * Verifier Steps 5-6: Convert the tree to a string s for input to the Fiat-Shamir hash function,
                  * using the same conversion as the prover in 7
                  * Accept the proof if the challenge at the root of the tree is equal to the Fiat-Shamir hash of s
                  * (and, if applicable,  the associated data). Reject otherwise.
                  */
                val expectedChallenge = CryptoFunctions.hashFn(FiatShamirTree.toBytes(newRoot) ++ message)
                util.Arrays.equals(newRoot.challenge, expectedChallenge)
            }
        }
//      case _: Value[_] => false
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

  def verify(env: ScriptEnv, exp: ErgoTree,
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension).asInstanceOf[CTX]
    verify(env, exp, ctxv, proverResult.proof, message)
  }


  //todo: do we need the method below?
  def verify(exp: ErgoTree,
             context: CTX,
             proof: ProofT,
             message: Array[Byte]): Try[VerificationResult] = {
    verify(Interpreter.emptyEnv, exp, context, SigSerializer.toBytes(proof), message)
  }

}

object Interpreter {
  type VerificationResult = (Boolean, Long)
  type ReductionResult = (SigmaBoolean, Long)

  type ScriptEnv = Map[String, Any]
  val emptyEnv: ScriptEnv = Map.empty[String, Any]
  val ScriptNameProp = "ScriptName"

  def error(msg: String) = throw new InterpreterException(msg)

}
