package sigmastate.interpreter

import java.util
import java.util.Objects

import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{strategy, rule, everywherebu}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.math.ec.custom.djb.Curve25519Point
import scapi.sigma.DLogProtocol.{FirstDLogProverMessage, DLogInteractiveProver}
import scapi.sigma._
import scorex.crypto.authds.avltree.batch.{Lookup, Operation}
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.crypto.hash.Blake2b256
import scorex.util.ScorexLogging
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{ByteArrayConstant, _}
import sigmastate.eval.IRContext
import sigmastate.interpreter.Interpreter.{VerificationResult, ScriptEnv}
import sigmastate.lang.exceptions.InterpreterException
import sigmastate.lang.Terms.ValueOps
import sigmastate.serialization.{ValueSerializer, OpCodes, Serializer, OperationSerializer}
import sigmastate.utils.Extensions._
import sigmastate.utils.Helpers
import sigmastate.utxo.{GetVar, DeserializeContext, Transformer}
import sigmastate.{SType, _}
import special.sigma.InvalidType

import scala.util.{Success, Failure, Try}


object CryptoConstants {
  type EcPointType = Curve25519Point

  val dlogGroup: BcDlogFp[EcPointType] = Curve25519
  lazy val secureRandom = dlogGroup.secureRandom

  def secureRandomBytes(howMany: Int) = {
    val bytes = new Array[Byte](howMany)
    secureRandom.nextBytes(bytes)
    bytes
  }

  /** Size of the binary representation of any group element (2 ^ groupSizeBits == <number of elements in a group>) */
  val groupSizeBits: Int = 256

  /** Number of bytes to represent any group element as byte array */
  val groupSize: Int = 256 / 8 //32 bytes

  //size of challenge in Sigma protocols, in bits
  //if this anything but 192, threshold won't work, because we have polynomials over GF(2^192) and no others
  //so DO NOT change the value without implementing polynomials over GF(2^soundnessBits) first
  //and changing code that calls on GF2_192 and GF2_192_Poly classes!!!
  implicit val soundnessBits: Int = 192.ensuring(_ < groupSizeBits, "2^t < q condition is broken!")
}

object CryptoFunctions {
  lazy val soundnessBytes = CryptoConstants.soundnessBits / 8

  def hashFn(input: Array[Byte]): Array[Byte] = {
    Blake2b256.hash(input).take(soundnessBytes)
  }

  def showECPoint(p: ECPoint) = {
    val rawX = p.getRawXCoord.toString.substring(0, 6)
    val rawY = p.getRawYCoord.toString.substring(0, 6)
    s"ECPoint($rawX,$rawY,...)"
  }
}

trait Interpreter extends ScorexLogging {

  import CryptoConstants._
  import Interpreter.ReductionResult

  type CTX <: Context

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
            Some(ValueSerializer.deserialize(eba.value))
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
    case p: SValue if p.tpe == SSigmaProp => p.asSigmaProp.isValid
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
    val costingRes @ IR.Pair(calcF, costF) = doCosting(env, exp)
    IR.onCostingResult(env, exp, costingRes)

    IR.verifyCostFunc(costF).fold(t => throw t, x => x)

    IR.verifyIsValid(calcF).fold(t => throw t, x => x)

    // check cost
    val costingCtx = context.toSigmaContext(IR, isCost = true)
    val costFun = IR.compile[SInt.type](IR.getDataEnv, costF)
    val IntConstant(estimatedCost) = costFun(costingCtx)
    if (estimatedCost > maxCost) {
      throw new Error(s"Estimated expression complexity $estimatedCost exceeds the limit $maxCost in $exp")
    }
    // check calc
    val calcCtx = context.toSigmaContext(IR, isCost = false)
    val valueFun = IR.compile[SBoolean.type](IR.getDataEnv, calcF.asRep[IR.Context => SBoolean.WrappedType])
    val res = valueFun(calcCtx)
    val resValue = res match {
      case SigmaPropConstant(sb) => sb
      case _ => res
    }
    resValue -> estimatedCost
  }

  def reduceToCrypto(context: CTX, exp: Value[SType]): Try[ReductionResult] =
    reduceToCrypto(context, Interpreter.emptyEnv, exp)

  def verify(env: ScriptEnv, exp: Value[SBoolean.type],
             context: CTX,
             proof: Array[Byte],
             message: Array[Byte]): Try[VerificationResult] = Try {
    val propTree = applyDeserializeContext(context, exp)
    val (cProp, cost) = reduceToCrypto(context, env, propTree).get

    val checkingResult = cProp match {
      case TrueLeaf => true
      case FalseLeaf => false
      case cProp: SigmaBoolean =>
        cProp match {
          case TrivialProp.TrueProp => true
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
      case _: Value[_] => false
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

  def verify(exp: Value[SBoolean.type],
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension).asInstanceOf[CTX]
    verify(Interpreter.emptyEnv, exp, ctxv, proverResult.proof, message)
  }

  def verify(env: ScriptEnv, exp: Value[SBoolean.type],
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension).asInstanceOf[CTX]
    verify(env, exp, ctxv, proverResult.proof, message)
  }


  //todo: do we need the method below?
  def verify(exp: Value[SBoolean.type],
             context: CTX,
             proof: ProofT,
             message: Array[Byte]): Try[VerificationResult] = {
    verify(Interpreter.emptyEnv, exp, context, SigSerializer.toBytes(proof), message)
  }
}

object Interpreter {
  type VerificationResult = (Boolean, Long)
  type ReductionResult = (Value[SBoolean.type], Long)

  type ScriptEnv = Map[String, Any]
  val emptyEnv: ScriptEnv = Map()
  val ScriptNameProp = "ScriptName"

  def error(msg: String) = throw new InterpreterException(msg)
}
