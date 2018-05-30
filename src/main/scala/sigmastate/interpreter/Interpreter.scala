package sigmastate.interpreter

import java.math.BigInteger
import java.util

import org.bitbucket.inkytonik.kiama.relation.Tree
import sigmastate.Values.{ByteArrayConstant, _}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{and, everywherebu, log, rule, strategy}
import org.bouncycastle.math.ec.custom.djb.Curve25519Point
import org.bouncycastle.math.ec.custom.sec.SecP384R1Point
import scapi.sigma.DLogProtocol.FirstDLogProverMessage
import scapi.sigma._
import scorex.crypto.authds.avltree.batch.Lookup
import sigmastate.SCollection.SByteArray
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.crypto.hash.Blake2b256
import sigmastate.Values._
import sigmastate.interpreter.Interpreter.VerificationResult
import sigmastate.serialization.{OpCodes, ValueSerializer}
import sigmastate.utils.Helpers
import sigmastate.utils.Extensions._
import sigmastate.utxo.{CostTable, DeserializeContext, Transformer}
import sigmastate.{SType, _}

import scala.util.Try


object CryptoConstants {
  type EcPointType = SecP384R1Point

  val dlogGroup: BcDlogFp[EcPointType] = SecP384R1
  val groupSizeBits: Int = 384
  val groupSize: Int = 384 / 8 //48 bytes

  //size of challenge in Sigma protocols, in bits
  implicit val soundnessBits: Int = 256.ensuring(_ < groupSizeBits, "2^t < q condition is broken!")
}

object CryptoFunctions {
  lazy val soundnessBytes = CryptoConstants.soundnessBits / 8

  def hashFn(input: Array[Byte]): Array[Byte] = {
    Blake2b256.hash(input).take(soundnessBytes)
  }
}

trait Interpreter {

  import CryptoConstants._
  import Interpreter.ReductionResult

  type CTX <: Context[CTX]

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
          case eba: EvaluatedValue[SByteArray] @unchecked if eba.tpe == SByteArray => Some(ValueSerializer.deserialize(eba.value))
          case _ => None
        }
      else
        None
    case _ => None
  }

  /** First, both the prover and the verifier are making context-dependent tree transformations
    * (usually, context variables substitutions).
    * Context-specific transformations should be defined in descendants.
    *
    * This is the only function to define all the tree rewrites, except of deserializations.
    *
    * @param context a context instance
    * @param tree    to be rewritten
    * @return a new rewritten tree or `null` if `tree` cannot be rewritten.
    */
  def evaluateNode(context: CTX, tree: SValue): SValue = tree match {
    case GroupGenerator =>
      GroupElementConstant(GroupGenerator.value)

    //Int Arith operations
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

    case Xor(ByteArrayConstant(l), ByteArrayConstant(r)) =>
      assert(l.length == r.length)
      ByteArrayConstant(Helpers.xor(l, r))

    case c: CalcHash if c.input.evaluated => c.function(c.input.asInstanceOf[EvaluatedValue[SByteArray]])

    case Exponentiate(GroupElementConstant(l), BigIntConstant(r)) =>
      GroupElementConstant(dlogGroup.exponentiate(l, r))

    case MultiplyGroup(l: GroupElementConstant, r: GroupElementConstant) =>
      GroupElementConstant(dlogGroup.multiplyGroupElements(l.value, r.value))

    //relations
    case EQ(l: Value[_], r: Value[_]) if l.evaluated && r.evaluated =>
      BooleanConstant.fromBoolean(l == r)
    case NEQ(l: Value[_], r: Value[_]) if l.evaluated && r.evaluated =>
      BooleanConstant.fromBoolean(l != r)
    case GT(LongConstant(l), LongConstant(r)) =>
      BooleanConstant.fromBoolean(l > r)
    case GE(LongConstant(l), LongConstant(r)) =>
      BooleanConstant.fromBoolean(l >= r)
    case LT(LongConstant(l), LongConstant(r)) =>
      BooleanConstant.fromBoolean(l < r)
    case LE(LongConstant(l), LongConstant(r)) =>
      BooleanConstant.fromBoolean(l <= r)
    case GT(ByteConstant(l), ByteConstant(r)) =>
      BooleanConstant.fromBoolean(l > r)
    case GE(ByteConstant(l), ByteConstant(r)) =>
      BooleanConstant.fromBoolean(l >= r)
    case LT(ByteConstant(l), ByteConstant(r)) =>
      BooleanConstant.fromBoolean(l < r)
    case LE(ByteConstant(l), ByteConstant(r)) =>
      BooleanConstant.fromBoolean(l <= r)
    case GT(BigIntConstant(l), BigIntConstant(r)) =>
      BooleanConstant.fromBoolean(l.compareTo(r) > 0)
    case GE(BigIntConstant(l), BigIntConstant(r)) =>
      BooleanConstant.fromBoolean(l.compareTo(r) >= 0)
    case LT(BigIntConstant(l), BigIntConstant(r)) =>
      BooleanConstant.fromBoolean(l.compareTo(r) < 0)
    case LE(BigIntConstant(l), BigIntConstant(r)) =>
      BooleanConstant.fromBoolean(l.compareTo(r) <= 0)
    case IsMember(tree: AvlTreeConstant, ByteArrayConstant(key), ByteArrayConstant(proof)) =>
      val bv = tree.createVerifier(SerializedAdProof @@ proof)
      val res = bv.performOneOperation(Lookup(ADKey @@ key))
      BooleanConstant.fromBoolean(res.isSuccess) // TODO should we also check res.get.isDefined
    case If(cond: EvaluatedValue[SBoolean.type], trueBranch, falseBranch) =>
      if (cond.value) trueBranch else falseBranch

    //conjectures
    case a@AND(children) if a.transformationReady =>
      a.function(children.asInstanceOf[EvaluatedValue[SCollection[SBoolean.type]]])

    case o@OR(children) if o.transformationReady =>
      o.function(children.asInstanceOf[EvaluatedValue[SCollection[SBoolean.type]]])

    case t: Transformer[_, _] if t.transformationReady => t.function()

    case _ => null
  }

  // new reducer: 1 phase only which is constantly being repeated until non-reducible,
  // reduction state carried between reductions is number of transformations done.
  // when it becomes zero, it means that it is time to stop (tree becomes irreducible)
  //todo: should we limit number of steps?
  case class ReductionState(numberOfTransformations: Int) {
    def recordTransformation(): ReductionState = ReductionState(numberOfTransformations + 1)
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
  def reduceToCrypto(context: CTX, exp: Value[SBoolean.type]): Try[ReductionResult] = Try {
    require(new Tree(exp).nodes.length < CostTable.MaxExpressions,
      s"Too long expression, contains ${new Tree(exp).nodes.length} nodes, " +
        s"allowed maximum is ${CostTable.MaxExpressions}")

    // Substitute Deserialize* nodes with deserialized subtrees
    // We can estimate cost of the tree evaluation only after this step.
    val substRule = strategy[Value[_ <: SType]] { case x => substDeserialize(context, x) }

    val substTree = everywherebu(substRule)(exp) match {
      case Some(v: Value[SBoolean.type]@unchecked) if v.tpe == SBoolean => v
      case x => throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean but was $x")
    }

    val cost = substTree.cost(context)

    if (cost > maxCost) {
      throw new Error(s"Estimated expression complexity $cost exceeds the limit $maxCost in $substTree")
    }

    // After performing deserializations and checking cost of the resulting tree, both the prover
    // and the verifier are evaluating the tree by applying rewriting rules, until no rules trigger during tree
    // traversal (so interpreter checks whether any nodes were rewritten during last rewriting, and aborts if so).
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
    var currTree = substTree
    do {
      wasRewritten = false
      currTree = everywherebu(rules)(currTree).get.asInstanceOf[Value[SBoolean.type]]
    } while (wasRewritten)

    currTree -> cost
  }

  /**
    * Verifier steps:
    * 1. Place received challenges "e" and responses "z" into every leaf.
    * 2. Bottom-up: compute commitments at every leaf according to a = g^z/h^e. At every COR and CAND node, compute
    * the commitment as the union of the children's commitments. At every COR node, compute the challenge as the XOR of
    * the children's challenges. At every CAND node, verify that the children's challenges are all equal. (Note that
    * there is an opportunity for small savings here, because we don't need to send all the challenges for a CAND --
    * but let's save that optimization for later.)
    * 3. Check that the root challenge is equal to the hash of the root commitment and other inputs.
    */

  def verify(exp: Value[SBoolean.type],
             context: CTX,
             proof: Array[Byte],
             message: Array[Byte]): Try[VerificationResult] = Try {
    val (cProp, cost) = reduceToCrypto(context, exp).get

    val checkingResult = cProp match {
      case TrueLeaf => true
      case FalseLeaf => false
      case b: Value[SBoolean.type] if b.evaluated =>
        SigSerializer.parse(cProp, proof) match {
          case NoProof => false
          case sp: UncheckedSigmaTree[_] =>
            assert(sp.proposition == cProp)

            val newRoot = checks(sp).get.asInstanceOf[UncheckedTree]
            val (challenge, rootCommitments) = newRoot match {
              case u: UncheckedConjecture[_] => (u.challengeOpt.get, u.commitments)
              case sn: UncheckedSchnorr => (sn.challenge, sn.firstMessageOpt.toSeq)
              case dh: UncheckedDiffieHellmanTuple => (dh.challenge, dh.firstMessageOpt.toSeq)
              case _ =>
                Interpreter.error(s"Unknown type of root after 'checks' $newRoot")
            }

            val expectedChallenge = CryptoFunctions.hashFn(Helpers.concatBytes(rootCommitments.map(_.bytes) :+ message))
            util.Arrays.equals(challenge, expectedChallenge)
        }
      case _: Value[_] => false
    }
    checkingResult -> cost
  }

  /**
    * 2. Bottom-up: compute commitments at every leaf according to a = g^z/h^e. At every COR and CAND node, compute
    * the commitment as the union of the children's commitments. At every COR node, compute the challenge as the XOR of
    * the children's challenges. At every CAND node, verify that the children's challenges are all equal. (Note that
    * there is an opportunity for small savings here, because we don't need to send all the challenges for a CAND --
    * but let's save that optimization for later.)
    */
  val checks: Strategy = everywherebu(rule[UncheckedTree] {
    case and: CAndUncheckedNode =>
      //todo: reduce boilerplate below

      val challenges: Seq[Array[Byte]] = and.leafs.map {
        case u: UncheckedConjecture[_] => u.challengeOpt.get
        case sn: UncheckedSchnorr => sn.challenge
        case dh: UncheckedDiffieHellmanTuple => dh.challenge
      }

      val commitments: Seq[FirstProverMessage[_]] = and.leafs.flatMap {
        case u: UncheckedConjecture[_] => u.commitments

        //todo: reduce boilerplate below, replace additive notation w. multiplicative
        case sn: UncheckedSchnorr => sn.firstMessageOpt.toSeq

        case dh: UncheckedDiffieHellmanTuple => dh.firstMessageOpt.toSeq
      }

      val challenge = challenges.head

      assert(challenges.tail.forall(util.Arrays.equals(_, challenge)))

      and.copy(challengeOpt = Some(challenge), commitments = commitments)

    case or: COrUncheckedNode =>
      val challenges = or.leafs map {
        case u: UncheckedConjecture[_] => u.challengeOpt.get
        case sn: UncheckedSchnorr => sn.challenge
        case dh: UncheckedDiffieHellmanTuple => dh.challenge
        case a: Any => println(a); ???
      }

      val commitments = or.leafs flatMap {
        case u: UncheckedConjecture[_] => u.commitments
        case sn: UncheckedSchnorr => sn.firstMessageOpt.toSeq
        case dh: UncheckedDiffieHellmanTuple => dh.firstMessageOpt.toSeq
        case _ => ???
      }

      or.copy(challengeOpt = Some(Helpers.xor(challenges: _*)), commitments = commitments)

    case sn: UncheckedSchnorr =>

      val dlog = CryptoConstants.dlogGroup
      val g = dlog.generator
      val h = sn.proposition.h

      val a = dlog.multiplyGroupElements(
        dlog.exponentiate(g, sn.secondMessage.z.underlying()),
        dlog.getInverse(dlog.exponentiate(h, new BigInteger(1, sn.challenge))))

      sn.copy(firstMessageOpt = Some(FirstDLogProverMessage(a)))

    //todo: check that g,h belong to the group
    //g^z = a*u^e, h^z = b*v^e  => a = g^z/u^e, b = h^z/v^e
    case dh: UncheckedDiffieHellmanTuple =>
      val dlog = CryptoConstants.dlogGroup

      val g = dh.proposition.g
      val h = dh.proposition.h
      val u = dh.proposition.u
      val v = dh.proposition.v

      val z = dh.secondMessage.z

      val e = new BigInteger(1, dh.challenge)

      val gToZ = dlog.exponentiate(g, z)
      val hToZ = dlog.exponentiate(h, z)

      val uToE = dlog.exponentiate(u, e)
      val vToE = dlog.exponentiate(v, e)

      val a = dlog.multiplyGroupElements(gToZ, dlog.getInverse(uToE))
      val b = dlog.multiplyGroupElements(hToZ, dlog.getInverse(vToE))
      dh.copy(firstMessageOpt = Some(FirstDiffieHellmanTupleProverMessage(a, b)))

    case _ => ???
  })

  def verify(exp: Value[SBoolean.type],
             context: CTX,
             proverResult: SerializedProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension)
    verify(exp, ctxv, proverResult.proofBytes.toArray, message)
  }


  //below are two sugaric methods for tests

  def verify(exp: Value[SBoolean.type],
             context: CTX,
             proverResult: ProverResult,
             message: Array[Byte]): Try[VerificationResult] = {
    val ctxv = context.withExtension(proverResult.extension)
    verify(exp, ctxv, proverResult.proof, message)
  }

  def verify(exp: Value[SBoolean.type],
             context: CTX,
             proof: ProofT,
             message: Array[Byte]): Try[VerificationResult] = {
    verify(exp, context, SigSerializer.toBytes(proof), message)
  }
}

object Interpreter {
  type VerificationResult = (Boolean, Long)

  type ReductionResult = (Value[SBoolean.type], Long)

  def error(msg: String) = throw new InterpreterException(msg)
}

class InterpreterException(msg: String) extends Exception(msg)
