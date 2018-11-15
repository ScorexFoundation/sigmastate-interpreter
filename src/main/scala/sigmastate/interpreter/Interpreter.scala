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

  val groupSizeBits: Int = 256
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

  /** Implements single reduction step by matching given tree against rewriting rules.
    * Context-specific transformations should be defined in descendants.
    *
    * This is the only function to define all the tree rewrites, except of deserializations.
    *
    * @param context a context instance
    * @param tree    to be rewritten
    * @return a new rewritten tree or `null` if `tree` cannot be rewritten.
    */
  def evaluateNode(context: CTX, tree: SValue): SValue = tree match {
    case t: TaggedVariable[_] =>
      if (context.extension.values.contains(t.varId))
        context.extension.values(t.varId)
      else
        null

    case t: GetVar[_] =>
      val elemType = t.tpe.elemType
      if (context.extension.values.contains(t.varId)) {
        val v = context.extension.values(t.varId)
        if (v.tpe != elemType)
          throw new InvalidType(s"Invalid value type ${v.tpe} in context variable with id ${t.varId}, expected ${t.tpe.elemType}")
        else
          SomeValue(v)
      }
      else
        NoneValue(elemType)

    case GroupGenerator =>
      GroupElementConstant(GroupGenerator.value)

    //Byte Arith operations
    case ArithOp(ByteConstant(l), ByteConstant(r), OpCodes.PlusCode) =>
      ByteConstant(l.addExact(r))

    case ArithOp(ByteConstant(l), ByteConstant(r), OpCodes.MinusCode) =>
      ByteConstant(l.subtractExact(r))

    case ArithOp(ByteConstant(l), ByteConstant(r), OpCodes.MultiplyCode) =>
      ByteConstant(l.multiplyExact(r))

    case ArithOp(ByteConstant(l), ByteConstant(r), OpCodes.ModuloCode) =>
      ByteConstant((l % r).toByte)

    case ArithOp(ByteConstant(l), ByteConstant(r), OpCodes.DivisionCode) =>
      ByteConstant((l / r).toByte)

    case ArithOp(ByteConstant(l), ByteConstant(r), OpCodes.MinCode) =>
      ByteConstant(math.min(l, r).toByte)

    case ArithOp(ByteConstant(l), ByteConstant(r), OpCodes.MaxCode) =>
      ByteConstant(math.max(l, r).toByte)

    //Short Arith operations
    case ArithOp(ShortConstant(l), ShortConstant(r), OpCodes.PlusCode) =>
      ShortConstant(l.addExact(r))

    case ArithOp(ShortConstant(l), ShortConstant(r), OpCodes.MinusCode) =>
      ShortConstant(l.subtractExact(r))

    case ArithOp(ShortConstant(l), ShortConstant(r), OpCodes.MultiplyCode) =>
      ShortConstant(l.multiplyExact(r))

    case ArithOp(ShortConstant(l), ShortConstant(r), OpCodes.ModuloCode) =>
      ShortConstant((l % r).toShort)

    case ArithOp(ShortConstant(l), ShortConstant(r), OpCodes.DivisionCode) =>
      ShortConstant((l / r).toShort)

    case ArithOp(ShortConstant(l), ShortConstant(r), OpCodes.MinCode) =>
      ShortConstant(math.min(l, r).toShort)

    case ArithOp(ShortConstant(l), ShortConstant(r), OpCodes.MaxCode) =>
      ShortConstant(math.max(l, r).toShort)

    //Int Arith operations
    case ArithOp(IntConstant(l), IntConstant(r), OpCodes.PlusCode) =>
      IntConstant(Math.addExact(l, r))

    case ArithOp(IntConstant(l), IntConstant(r), OpCodes.MinusCode) =>
      IntConstant(Math.subtractExact(l, r))

    case ArithOp(IntConstant(l), IntConstant(r), OpCodes.MultiplyCode) =>
      IntConstant(Math.multiplyExact(l, r))

    case ArithOp(IntConstant(l), IntConstant(r), OpCodes.ModuloCode) =>
      IntConstant(l % r)

    case ArithOp(IntConstant(l), IntConstant(r), OpCodes.DivisionCode) =>
      IntConstant(l / r)

    case ArithOp(IntConstant(l), IntConstant(r), OpCodes.MinCode) =>
      IntConstant(math.min(l, r))

    case ArithOp(IntConstant(l), IntConstant(r), OpCodes.MaxCode) =>
      IntConstant(math.max(l, r))

    //Long Arith operations
    case ArithOp(LongConstant(l), LongConstant(r), OpCodes.PlusCode) =>
      LongConstant(Math.addExact(l, r))

    case ArithOp(LongConstant(l), LongConstant(r), OpCodes.MinusCode) =>
      LongConstant(Math.subtractExact(l, r))

    case ArithOp(LongConstant(l), LongConstant(r), OpCodes.MultiplyCode) =>
      LongConstant(Math.multiplyExact(l, r))

    case ArithOp(LongConstant(l), LongConstant(r), OpCodes.ModuloCode) =>
      LongConstant(l % r)

    case ArithOp(LongConstant(l), LongConstant(r), OpCodes.DivisionCode) =>
      LongConstant(l / r)

    case ArithOp(LongConstant(l), LongConstant(r), OpCodes.MinCode) =>
      LongConstant(math.min(l, r).toLong)

    case ArithOp(LongConstant(l), LongConstant(r), OpCodes.MaxCode) =>
      LongConstant(math.max(l, r).toLong)

    //BigInt Arith operations
    case ArithOp(BigIntConstant(l), BigIntConstant(r), OpCodes.PlusCode) =>
      BigIntConstant(l.add(r))

    case ArithOp(BigIntConstant(l), BigIntConstant(r), OpCodes.MinusCode) =>
      BigIntConstant(l.subtract(r))

    case ArithOp(BigIntConstant(l), BigIntConstant(r), OpCodes.MultiplyCode) =>
      BigIntConstant(l.multiply(r))

    case ArithOp(BigIntConstant(l), BigIntConstant(r), OpCodes.ModuloCode) =>
      BigIntConstant(l.mod(r))

    case ArithOp(BigIntConstant(l), BigIntConstant(r), OpCodes.DivisionCode) =>
      BigIntConstant(l.divide(r))

    case ArithOp(BigIntConstant(l), BigIntConstant(r), OpCodes.MinCode) =>
      BigIntConstant(l.min(r))

    case ArithOp(BigIntConstant(l), BigIntConstant(r), OpCodes.MaxCode) =>
      BigIntConstant(l.max(r))

    case Xor(ByteArrayConstant(l), ByteArrayConstant(r)) =>
      assert(l.length == r.length)
      ByteArrayConstant(Helpers.xor(l, r))

    case Exponentiate(GroupElementConstant(l), BigIntConstant(r)) =>
      GroupElementConstant(dlogGroup.exponentiate(l, r))

    case MultiplyGroup(l: GroupElementConstant, r: GroupElementConstant) =>
      GroupElementConstant(dlogGroup.multiplyGroupElements(l.value, r.value))

    //relations
    case EQ(l: EvaluatedValue[_], r: EvaluatedValue[_]) =>
      BooleanConstant.fromBoolean(Objects.deepEquals(l.value, r.value))
    case NEQ(l: EvaluatedValue[_], r: EvaluatedValue[_]) =>
      BooleanConstant.fromBoolean(!Objects.deepEquals(l.value, r.value))

    case GT(ByteConstant(l), ByteConstant(r)) =>
      BooleanConstant.fromBoolean(l > r)
    case GE(ByteConstant(l), ByteConstant(r)) =>
      BooleanConstant.fromBoolean(l >= r)
    case LT(ByteConstant(l), ByteConstant(r)) =>
      BooleanConstant.fromBoolean(l < r)
    case LE(ByteConstant(l), ByteConstant(r)) =>
      BooleanConstant.fromBoolean(l <= r)

    case GT(ShortConstant(l), ShortConstant(r)) =>
      BooleanConstant.fromBoolean(l > r)
    case GE(ShortConstant(l), ShortConstant(r)) =>
      BooleanConstant.fromBoolean(l >= r)
    case LT(ShortConstant(l), ShortConstant(r)) =>
      BooleanConstant.fromBoolean(l < r)
    case LE(ShortConstant(l), ShortConstant(r)) =>
      BooleanConstant.fromBoolean(l <= r)

    case GT(IntConstant(l), IntConstant(r)) =>
      BooleanConstant.fromBoolean(l > r)
    case GE(IntConstant(l), IntConstant(r)) =>
      BooleanConstant.fromBoolean(l >= r)
    case LT(IntConstant(l), IntConstant(r)) =>
      BooleanConstant.fromBoolean(l < r)
    case LE(IntConstant(l), IntConstant(r)) =>
      BooleanConstant.fromBoolean(l <= r)

    case GT(LongConstant(l), LongConstant(r)) =>
      BooleanConstant.fromBoolean(l > r)
    case GE(LongConstant(l), LongConstant(r)) =>
      BooleanConstant.fromBoolean(l >= r)
    case LT(LongConstant(l), LongConstant(r)) =>
      BooleanConstant.fromBoolean(l < r)
    case LE(LongConstant(l), LongConstant(r)) =>
      BooleanConstant.fromBoolean(l <= r)

    case GT(BigIntConstant(l), BigIntConstant(r)) =>
      BooleanConstant.fromBoolean(l.compareTo(r) > 0)
    case GE(BigIntConstant(l), BigIntConstant(r)) =>
      BooleanConstant.fromBoolean(l.compareTo(r) >= 0)
    case LT(BigIntConstant(l), BigIntConstant(r)) =>
      BooleanConstant.fromBoolean(l.compareTo(r) < 0)
    case LE(BigIntConstant(l), BigIntConstant(r)) =>
      BooleanConstant.fromBoolean(l.compareTo(r) <= 0)

    case TreeModifications(tree: EvaluatedValue[AvlTreeData]@unchecked, ops: EvaluatedValue[SByteArray], proof: EvaluatedValue[SByteArray]) =>
      def invalidArg(value: EvaluatedValue[SByteArray]) = Interpreter.error(s"Collection expected but found $value")

      val operationsBytes = ops.matchCase(cc => cc.value, c => c.value, _ => invalidArg(ops))
      val proofBytes = proof.matchCase(cc => cc.value, c => c.value, _ => invalidArg(proof))
      val bv = tree.asInstanceOf[AvlTreeConstant].createVerifier(SerializedAdProof @@ proofBytes)
      val opSerializer = new OperationSerializer(bv.keyLength, bv.valueLengthOpt)
      val operations: Seq[Operation] = opSerializer.parseSeq(Serializer.startReader(operationsBytes, 0))
      operations.foreach(o => bv.performOneOperation(o))
      bv.digest match {
        case Some(v) => SomeValue(v)
        case _ => NoneValue[SByteArray](SByteArray)
      }

    case TreeLookup(tree: EvaluatedValue[AvlTreeData]@unchecked, key: EvaluatedValue[SByteArray], proof: EvaluatedValue[SByteArray]) =>
      def invalidArg(value: EvaluatedValue[SByteArray]) = Interpreter.error(s"Collection expected but found $value")

      val keyBytes = key.matchCase(cc => cc.value, c => c.value, _ => invalidArg(key))
      val proofBytes = proof.matchCase(cc => cc.value, c => c.value, _ => invalidArg(proof))
      val bv = tree.asInstanceOf[AvlTreeConstant].createVerifier(SerializedAdProof @@ proofBytes)
      bv.performOneOperation(Lookup(ADKey @@ keyBytes)) match {
        case Failure(_) => Interpreter.error(s"Tree proof is incorrect")
        case Success(r) => r match {
          case Some(v) => SomeValue(v)
          case _ => NoneValue[SByteArray](SByteArray)
        }
      }

    case If(cond: EvaluatedValue[SBoolean.type], trueBranch, falseBranch) =>
      if (cond.value) trueBranch else falseBranch

    case t: Transformer[_, _] if t.transformationReady => t.function(this, context)

    case _ => null
  }

  def reduceUntilConverged[T <: SType](context: CTX, tree: Value[T]): Value[T] = {
    // The interpreter checks whether any nodes were rewritten during last rewriting, and aborts if no rewritings.
    // Because each rewriting reduces size of the tree the process terminates with number of steps <= substTree.cost.
    var wasRewritten = false
    val rules: Strategy = strategy[Value[_ <: SType]] { case node =>
      val rewritten = evaluateNode(context, node)
      if (rewritten != null) {
        wasRewritten = true
        Some(rewritten)
      }
      else
        None
    }
    var currTree = tree
    do {
      wasRewritten = false
      currTree = everywherebu(rules)(currTree).get.asInstanceOf[Value[T]]
    } while (wasRewritten)
    currTree
  }

  val IR: IRContext
  import IR._

  /**
    * As the first step both prover and verifier are applying context-specific transformations and then estimating
    * cost of the intermediate expression. If cost is above limit, abort. Otherwise, both prover and verifier are
    * reducing the expression in the same way.
    *
    * @param exp
    * @param context
    * @return
    */
  def reduceToCrypto(context: CTX, env: ScriptEnv, exp: Value[SBoolean.type]): Try[ReductionResult] = Try {
    // Substitute Deserialize* nodes with deserialized subtrees
    // We can estimate cost of the tree evaluation only after this step.
    val substRule = strategy[Value[_ <: SType]] { case x => substDeserialize(context, x) }

    val substTree = everywherebu(substRule)(exp) match {
      case Some(v: Value[SBoolean.type]@unchecked) if v.tpe == SBoolean => v
      case Some(p: SValue) if p.tpe == SSigmaProp => p.asSigmaProp.isValid
      case x => throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean of SigmaProp but was $x")
    }
    val costingRes @ IR.Pair(calcF, costF) = doCosting(env, substTree)
    IR.onCostingResult(env, substTree, costingRes)

    IR.verifyCostFunc(costF).fold(t => throw t, x => x)

    IR.verifyIsValid(calcF).fold(t => throw t, x => x)

    // check cost
    val costingCtx = context.toSigmaContext(IR, isCost = true)
    val costFun = IR.compile[SInt.type](IR.getDataEnv, costF)
    val IntConstant(estimatedCost) = costFun(costingCtx)
    if (estimatedCost > maxCost) {
      throw new Error(s"Estimated expression complexity $estimatedCost exceeds the limit $maxCost in $substTree")
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

  def reduceToCrypto(context: CTX, exp: Value[SBoolean.type]): Try[ReductionResult] =
    reduceToCrypto(context, Interpreter.emptyEnv, exp)

  def verify(env: ScriptEnv, exp: Value[SBoolean.type],
             context: CTX,
             proof: Array[Byte],
             message: Array[Byte]): Try[VerificationResult] = Try {
    val (cProp, cost) = reduceToCrypto(context, env, exp).get

    val checkingResult = cProp match {
      case TrueLeaf => true
      case FalseLeaf => false
      case cProp: SigmaBoolean =>
        cProp match {
          case TrivialProof.TrueProof => true
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

  implicit class InterpreterOps(I: Interpreter) {
    def eval[T <: SType](ctx: Context, ev: Value[T]): Value[T] = {
      val reduced = I.reduceUntilConverged(ctx.asInstanceOf[I.CTX], ev)
      reduced
    }
  }

  def error(msg: String) = throw new InterpreterException(msg)
}
