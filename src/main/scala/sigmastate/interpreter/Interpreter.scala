package sigmastate.interpreter

import java.math.BigInteger

import org.bitbucket.inkytonik.kiama.relation.Tree
import scorex.crypto.hash.Blake2b256
import sigmastate.{SType, _}
import sigmastate.utils.Helpers
import sigmastate.Values._

import scala.util.Try
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{and, rule, everywherebu, log, strategy}
import org.bouncycastle.math.ec.custom.djb.Curve25519Point
import scapi.sigma.DLogProtocol.FirstDLogProverMessage
import scapi.sigma._
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.crypto.authds.avltree.batch.Lookup
import sigmastate.utxo.{CostTable, Transformer, Height}

import scala.annotation.tailrec

object GroupSettings {
  type EcPointType = Curve25519Point
  val dlogGroup: BcDlogFp[EcPointType] = Curve25519

  implicit val soundness: Int = 256
}

trait Interpreter {

  import GroupSettings._

  type CTX <: Context[CTX]

  type ProofT = UncheckedTree //todo:  ProofT <: UncheckedTree ?

  final val MaxByteArrayLength = 10000

  /**
    * Max cost of a script interpreter can accept
    */
  def maxCost: Int

  /** First, both the prover and the verifier are making context-dependent tree transformations
    * (usually, context variables substitutions).
    * This method defines implementation-specific tree reductions, to be extended in descendants
    * using stackable-override pattern.
    * No rewriting is defined on this abstract level.
    *
    * @param context a context instance
    * @param tree to be rewritten
    * @return a new rewritten tree or `null` if `tree` cannot be rewritten.
    */
  def specificTransformations(context: CTX, tree: SValue): SValue = null

  def evaluateNode(context: CTX, node: SValue): SValue = node match {
    case GroupGenerator =>
      GroupElementConstant(dlogGroup.generator) // TODO should we use GroupGenerator.value instead

    //operations
    case Plus(l: IntConstant, r: IntConstant) => IntConstant(l.value + r.value)
    case Minus(l: IntConstant, r: IntConstant) => IntConstant(l.value - r.value)
    case Xor(l: ByteArrayConstant, r: ByteArrayConstant) =>
      assert(l.value.length == r.value.length)
      ByteArrayConstant(Helpers.xor(l.value, r.value))

    case AppendBytes(ByteArrayConstant(l), ByteArrayConstant(r)) =>
      require(l.length + r.length < MaxByteArrayLength)
      ByteArrayConstant(l ++ r)

    case c: CalcHash if c.input.evaluated => c.function(c.input.asInstanceOf[EvaluatedValue[SByteArray.type]])

    case Exponentiate(GroupElementConstant(l), BigIntConstant(r)) =>
      GroupElementConstant(dlogGroup.exponentiate(l, r))

    case MultiplyGroup(l: GroupElementConstant, r: GroupElementConstant) =>
      GroupElementConstant(dlogGroup.multiplyGroupElements(l.value, r.value))

    //relations
    case EQ(l: Value[_], r: Value[_]) if l.evaluated && r.evaluated =>
      BooleanConstant.fromBoolean(l == r)
    case NEQ(l: Value[_], r: Value[_]) if l.evaluated && r.evaluated =>
      BooleanConstant.fromBoolean(l != r)
    case GT(l: IntConstant, r: IntConstant) =>
      BooleanConstant.fromBoolean(l.value > r.value)
    case GE(l: IntConstant, r: IntConstant) =>
      BooleanConstant.fromBoolean(l.value >= r.value)
    case LT(l: IntConstant, r: IntConstant) =>
      BooleanConstant.fromBoolean(l.value < r.value)
    case LE(l: IntConstant, r: IntConstant) =>
      BooleanConstant.fromBoolean(l.value <= r.value)
    case IsMember(tree: AvlTreeConstant, key: ByteArrayConstant, proof: ByteArrayConstant) =>
      val bv = tree.createVerifier(SerializedAdProof @@ proof.value)
      val res = bv.performOneOperation(Lookup(ADKey @@ key.value))
      BooleanConstant.fromBoolean(res.isSuccess) // TODO should we also check res.get.isDefined
    case If(cond: EvaluatedValue[SBoolean.type], trueBranch, falseBranch) =>
      if (cond.value) trueBranch else falseBranch

    //conjectures
    case a @ AND(children) if a.transformationReady =>
      a.function(children.asInstanceOf[EvaluatedValue[SCollection[SBoolean.type]]])

    case o @ OR(children) if o.transformationReady =>
      o.function(children.asInstanceOf[EvaluatedValue[SCollection[SBoolean.type]]])

    case t: Transformer[_, _] if t.transformationReady => t.function()

    case _ => null  // this means the node cannot be evaluated
  }

  // new reducer: 1 phase only which is constantly being repeated until non-reducible,
  // reduction state carried between reductions is number of transformations done.
  // when it becomes zero, it means that it is time to stop (tree becomes irreducible)
  case class ReductionState(numberOfTransformations: Int) {
    def recordTransformation(): ReductionState = ReductionState(numberOfTransformations + 1)
  }

  //todo: return cost as well
  /**
    * As the first step both prover and verifier are applying context-specific transformations and then estimating
    * cost of the intermediate expression. If cost is above limit, abort. Otherwise, both prover and verifier are
    * reducing the expression in the same way.
    *
    * @param exp
    * @param context
    * @return
    */
  def reduceToCrypto(context: CTX, exp: Value[SBoolean.type]) = Try {
    require(new Tree(exp).nodes.length < CostTable.MaxExpressions)

    // Make context-dependent tree transformations (substitute references to a context
    // with data values from the context).
    // We can estimate cost of the tree evaluation only after this step.
    val substRule = strategy[Value[_ <: SType]] { case x => Option(specificTransformations(context, x)) }

    //todo: controversial .asInstanceOf?
    val substTree = everywherebu(substRule)(exp) match {
      case Some(v: Value[SBoolean.type]@unchecked) if v.tpe == SBoolean => v
      case x => throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean but was $x")
    }
    if (substTree.cost > maxCost) throw new Error("Estimated expression complexity exceeds the limit")

    // After performing context-dependent transformations and checking cost of the resulting tree, both the prover
    // and the verifier are evaluating the tree by applying rewriting rules, until no rules trigger during tree
    // traversal (so interpreter checks whether any nodes were rewritten during last rewriting, and aborts if so).
    // Because each rewriting reduces size of the tree the process terminates with number of steps <= substTree.cost.
    var wasRewritten = false
    val rules: Strategy = strategy[Value[_ <: SType]] { case node =>
      var rewritten = evaluateNode(context, node)
      if (rewritten == null) {
        rewritten = specificTransformations(context, node)
      }
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

    currTree
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
             proof: UncheckedTree,
             message: Array[Byte]): Try[Boolean] = Try {
    val cProp = reduceToCrypto(context, exp).get

    cProp match {
      case TrueLeaf => true
      case FalseLeaf => false
      case b: Value[SBoolean.type] if b.evaluated =>
        proof match {
          case NoProof => false
          case sp: UncheckedSigmaTree[_] =>
            assert(sp.proposition == cProp)

            val newRoot = checks(sp).get.asInstanceOf[UncheckedTree]
            val (challenge, rootCommitments) = newRoot match {
              case u: UncheckedConjecture[_] => (u.challengeOpt.get, u.commitments)
              case sn: SchnorrNode => (sn.challenge, sn.firstMessageOpt.toSeq)
              case dh: DiffieHellmanTupleUncheckedNode => (dh.challenge, dh.firstMessageOpt.toSeq)
            }

            val expectedChallenge = Blake2b256(rootCommitments.map(_.bytes).reduce(_ ++ _) ++ message)
            challenge.sameElements(expectedChallenge)
        }
      case _: Value[_] => false
    }
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
        case sn: SchnorrNode => sn.challenge
        case dh: DiffieHellmanTupleUncheckedNode => dh.challenge
      }

      val commitments: Seq[FirstProverMessage[_]] = and.leafs.flatMap {
        case u: UncheckedConjecture[_] => u.commitments
        case sn: SchnorrNode => sn.firstMessageOpt.toSeq
        case dh: DiffieHellmanTupleUncheckedNode => dh.firstMessageOpt.toSeq
      }

      val challenge = challenges.head

      assert(challenges.tail.forall(_.sameElements(challenge)))

      and.copy(challengeOpt = Some(challenge), commitments = commitments)

    case or: COrUncheckedNode =>
      val challenges = or.children map {
        case u: UncheckedConjecture[_] => u.challengeOpt.get
        case sn: SchnorrNode => sn.challenge
        case dh: DiffieHellmanTupleUncheckedNode => dh.challenge
        case a: Any => println(a); ???
      }

      val commitments = or.children flatMap {
        case u: UncheckedConjecture[_] => u.commitments
        case sn: SchnorrNode => sn.firstMessageOpt.toSeq
        case dh: DiffieHellmanTupleUncheckedNode => dh.firstMessageOpt.toSeq
        case _ => ???
      }

      or.copy(challengeOpt = Some(Helpers.xor(challenges: _*)), commitments = commitments)

    case sn: SchnorrNode =>

      val dlog = GroupSettings.dlogGroup
      val g = dlog.generator
      val h = sn.proposition.h

      val a = dlog.multiplyGroupElements(
        dlog.exponentiate(g, sn.secondMessage.z.underlying()),
        dlog.getInverse(dlog.exponentiate(h, new BigInteger(1, sn.challenge))))

      sn.copy(firstMessageOpt = Some(FirstDLogProverMessage(a)))

    //todo: check that g,h belong to the group
    //g^z = a*u^e, h^z = b*v^e  => a = g^z/u^e, b = h^z/v^e
    case dh: DiffieHellmanTupleUncheckedNode =>
      val dlog = GroupSettings.dlogGroup

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
             proverResult: ProverResult[ProofT],
             message: Array[Byte]): Try[Boolean] = {
    val ctxv = context.withExtension(proverResult.extension)
    verify(exp, ctxv, proverResult.proof, message)
  }
}

class InterpreterException(msg: String) extends Exception(msg)

object Interpreter {
  def error(msg: String) = throw new InterpreterException(msg)
}