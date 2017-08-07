package sigmastate.interpreter

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.{DlogGroup, ECElementSendableData}
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import scorex.crypto.hash.Blake2b256
import sigmastate._
import sigmastate.utils.Helpers

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}
import scapi.sigma.DLogProtocol.FirstDLogProverMessage
import scapi.sigma.FirstDiffieHellmanTupleProverMessage
import scapi.sigma.rework.FirstProverMessage


trait Interpreter {
  type CTX <: Context[CTX]
  type StateT <: StateTree
  type SigmaT <: SigmaTree

  type ProofT = UncheckedTree //todo:  ProofT <: UncheckedTree ?

  val dlogGroup: DlogGroup = new BcDlogECFp()

  def maxDepth: Int

  /**
    * implementation-specific tree reductions, to be defined in descendants
    *
    * @param tree - a tree to process-
    * @param ctx  - context instance
    * @return - processed tree
    */
  def specificPhases(tree: SigmaStateTree, ctx: CTX, cost: CostAccumulator): SigmaStateTree

  protected def contextSubst(ctx: CTX, cost: CostAccumulator): Strategy = {
    everywherebu(rule[SigmaStateTree] {
      case CustomByteArray(tag: Int) if ctx.extension.values.contains(tag) =>
        val value = ctx.extension.values(tag)
        cost.addCost(value.cost).ensuring(_.isRight)
        value
    })
  }

  protected val relations: Strategy = everywherebu(rule[SigmaStateTree] {
    case EQ(l: Value, r: Value) => BooleanConstantNode.fromBoolean(l == r)
    case NEQ(l: Value, r: Value) => BooleanConstantNode.fromBoolean(l != r)
    case GT(l: IntLeaf, r: IntLeaf) => BooleanConstantNode.fromBoolean(l.value > r.value)
    case GE(l: IntLeaf, r: IntLeaf) => BooleanConstantNode.fromBoolean(l.value >= r.value)
    case LT(l: IntLeaf, r: IntLeaf) => BooleanConstantNode.fromBoolean(l.value < r.value)
    case LE(l: IntLeaf, r: IntLeaf) => BooleanConstantNode.fromBoolean(l.value <= r.value)
  })

  protected val operations: Strategy = everywherebu(rule[SigmaStateTree] {
    case Plus(l: IntLeaf, r: IntLeaf) => IntLeaf(l.value + r.value)
    case Minus(l: IntLeaf, r: IntLeaf) => IntLeaf(l.value - r.value)
    case Xor(l: ByteArrayLeaf, r: ByteArrayLeaf) =>
      assert(l.value.length == r.value.length)
      ByteArrayLeaf(Helpers.xor(l.value, r.value))
    case Append(l: ByteArrayLeaf, r: ByteArrayLeaf) =>
      require(l.value.length + r.value.length < 10000) //todo: externalize this maximum intermediate value length limit
      ByteArrayLeaf(l.value ++ r.value)
    case CalcBlake2b256(l: ByteArrayLeaf) => ByteArrayLeaf(Blake2b256(l.value))
  })

  protected val conjs: Strategy = everywherebu(rule[SigmaStateTree] {

    case AND(children) =>

      @tailrec
      def iterChildren(children: Seq[SigmaStateTree],
                       currentBuffer: mutable.Buffer[SigmaStateTree]): mutable.Buffer[SigmaStateTree] = {
        if (children.isEmpty) currentBuffer else children.head match {
          case FalseConstantNode => mutable.Buffer(FalseConstantNode)
          case TrueConstantNode => iterChildren(children.tail, currentBuffer)
          case s: SigmaStateTree => iterChildren(children.tail, currentBuffer += s)
        }
      }

      val reduced = iterChildren(children, mutable.Buffer())

      reduced.size match {
        case i: Int if i == 0 => TrueConstantNode
        case i: Int if i == 1 => reduced.head
        case _ =>
          if (reduced.forall(_.isInstanceOf[SigmaTree]))
            CAND(reduced.map(_.asInstanceOf[SigmaTree]))
          else AND(reduced)
      }


    case OR(children) =>
      @tailrec
      def iterChildren(children: Seq[SigmaStateTree],
                       currentBuffer: mutable.Buffer[SigmaStateTree]): mutable.Buffer[SigmaStateTree] = {
        if (children.isEmpty) currentBuffer else children.head match {
          case TrueConstantNode => mutable.Buffer(TrueConstantNode)
          case FalseConstantNode => iterChildren(children.tail, currentBuffer)
          case s: SigmaStateTree => iterChildren(children.tail, currentBuffer += s)
        }
      }

      val reduced = iterChildren(children, mutable.Buffer())

      reduced.size match {
        case i: Int if i == 0 => FalseConstantNode
        case i: Int if i == 1 => reduced.head
        case _ =>
          if (reduced.forall(_.isInstanceOf[SigmaTree])) COR(reduced.map(_.asInstanceOf[SigmaTree]))
          else OR(reduced)
      }
  })

  @specialized
  case class CostAccumulator(initialValue: Int, limit: Int) {
    require(initialValue <= limit)
    private var value: Int = initialValue

    def addCost(delta: Int): Either[Int, Int] = {
      value = value + delta
      if (value <= limit) Right(limit) else Left(limit)
    }
  }

  //todo: cost analysis
  def reduceToCrypto(exp: SigmaStateTree, context: CTX): Try[SigmaStateTree] = Try({
    val additionalCost = CostAccumulator(exp.cost, 1000000)

    val afterContextSubst = contextSubst(context, additionalCost)(exp).get.asInstanceOf[SigmaStateTree]
    val afterSpecific = specificPhases(afterContextSubst, context, additionalCost)
    val afterOps = operations(afterSpecific).get.asInstanceOf[SigmaStateTree]
    val afterRels = relations(afterOps).get.asInstanceOf[SigmaStateTree]
    conjs(afterRels).get
  }.asInstanceOf[SigmaStateTree])

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

  def verify(exp: SigmaStateTree,
             context: CTX,
             proof: UncheckedTree,
             message: Array[Byte]): Try[Boolean] = Try {
    val cProp = reduceToCrypto(exp, context).get
    cProp match {
      case TrueConstantNode => true
      case FalseConstantNode => false
      case _ =>
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

    case or: COr2UncheckedNode =>
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

      or.copy(
        challengeOpt = Some(Helpers.xor(challenges: _*)),
        commitments = commitments)

    case sn: SchnorrNode =>

      val dlog = sn.proposition.dlogGroup
      val g = dlog.getGenerator
      val h = sn.proposition.h

      val a = dlog.multiplyGroupElements(
        dlog.exponentiate(g, sn.secondMessage.z.underlying()),
        dlog.getInverse(dlog.exponentiate(h, new BigInteger(1, sn.challenge))))

      sn.copy(firstMessageOpt = Some(FirstDLogProverMessage(a)))

    //todo: check that g,h belong to the group
    //g^z = a*u^e, h^z = b*v^e  => a = g^z/u^e, b = h^z/v^e
    case dh: DiffieHellmanTupleUncheckedNode =>
      val dlog = dh.proposition.dlogGroup

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

      val a = dlog.multiplyGroupElements(gToZ, dlog.getInverse(uToE)).generateSendableData().asInstanceOf[ECElementSendableData]
      val b = dlog.multiplyGroupElements(hToZ, dlog.getInverse(vToE)).generateSendableData().asInstanceOf[ECElementSendableData]
      dh.copy(firstMessageOpt = Some(FirstDiffieHellmanTupleProverMessage(a, b)))

    case _ => ???
  })

  def verify(exp: SigmaStateTree,
             context: CTX,
             proverResult: ProverResult[ProofT],
             message: Array[Byte]): Try[Boolean] = {
    val ctxv = context.withExtension(proverResult.extension)
    verify(exp, ctxv, proverResult.proof, message)
  }
}