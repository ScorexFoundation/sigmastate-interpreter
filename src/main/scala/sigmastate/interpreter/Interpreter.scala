package sigmastate.interpreter

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.DlogGroup
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import scorex.crypto.hash.Blake2b256
import sigmastate._
import sigmastate.utils.Helpers

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, rule}
import scapi.sigma.rework.DLogProtocol.FirstDLogProverMessage


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
  def specificPhases(tree: SigmaStateTree, ctx: CTX): SigmaStateTree

  protected def contextSubst(ctx: CTX): Strategy = everywherebu(rule[SigmaStateTree] {
    case CustomByteArray(tag: Int) if ctx.extension.values.contains(tag) => ctx.extension.values(tag)
  })

  protected val rels: Strategy = everywherebu(rule[SigmaStateTree] {
    case EQ(l: Value, r: Value) => BooleanConstantNode.fromBoolean(l == r)
    case NEQ(l: Value, r: Value) => BooleanConstantNode.fromBoolean(l != r)
    case GT(l: IntLeaf, r: IntLeaf) => BooleanConstantNode.fromBoolean(l.value > r.value)
    case GE(l: IntLeaf, r: IntLeaf) => BooleanConstantNode.fromBoolean(l.value >= r.value)
    case LT(l: IntLeaf, r: IntLeaf) => BooleanConstantNode.fromBoolean(l.value < r.value)
    case LE(l: IntLeaf, r: IntLeaf) => BooleanConstantNode.fromBoolean(l.value <= r.value)
  })

  protected val ops: Strategy = everywherebu(rule[SigmaStateTree] {
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
        case i: Int if i == 2 =>
          if (reduced.forall(_.isInstanceOf[SigmaTree]))
            COR2(reduced.head.asInstanceOf[SigmaTree], reduced.tail.head.asInstanceOf[SigmaTree])
          else OR(reduced)
        case _ =>
          if (reduced.forall(_.isInstanceOf[SigmaTree]))
            ??? //todo: COR for > 2 args
          else OR(reduced)
      }
  })

  //todo: cost analysis
  def reduceToCrypto(exp: SigmaStateTree, context: CTX): Try[SigmaStateTree] = Try({
    val afterContextSubst = contextSubst(context)(exp).get.asInstanceOf[SigmaStateTree]
    val afterSpecific = specificPhases(afterContextSubst, context)
    val afterOps = ops(afterSpecific).get.asInstanceOf[SigmaStateTree]
    val afterRels = rels(afterOps).get.asInstanceOf[SigmaStateTree]
    conjs(afterRels).get
  }.asInstanceOf[SigmaStateTree])

  /**
    * Verifier steps:
    * 1. Place received challenges "e" and responses "z"  into every leaf.
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
              case sn: SchnorrNode => (sn.challenge, Seq(sn.firstMessageOpt.get))
            }
            challenge.sameElements(Blake2b256(rootCommitments.map(_.bytes).reduce(_ ++ _) ++ message))

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
      }

      val commitments: Seq[FirstDLogProverMessage] = and.leafs.flatMap {
        case u: UncheckedConjecture[_] => u.commitments
        case sn: SchnorrNode => Seq(sn.firstMessageOpt.get)
      }

      val challenge = challenges.head

      assert(challenges.tail.forall(_.sameElements(challenge)))

      and.copy(challengeOpt = Some(challenge), commitments = commitments)

    case or: COr2UncheckedNode =>
      //todo: refactor boilerplate
      val (challengeLeft, commitmentsLeft) = or.leftChild match {
        case u: UncheckedConjecture[_] => (u.challengeOpt.get, u.commitments)
        case sn: SchnorrNode => (sn.challenge, Seq(sn.firstMessageOpt.get))
        case a: Any => println(a); ???
      }
      val (challengeRight, commitmentsRight) = or.rightChild match {
        case u: UncheckedConjecture[_] => (u.challengeOpt.get, u.commitments)
        case sn: SchnorrNode => (sn.challenge, Seq(sn.firstMessageOpt.get))
        case _ => ???
      }
      or.copy(
        challengeOpt = Some(Helpers.xor(challengeLeft, challengeRight)),
        commitments = commitmentsLeft ++ commitmentsRight)

    case sn: SchnorrNode =>

      val dlog = sn.proposition.dlogGroup
      val g = dlog.getGenerator
      val h = sn.proposition.h

      val a = dlog.multiplyGroupElements(
        dlog.exponentiate(g, sn.secondMessage.z.underlying()),
        dlog.getInverse(dlog.exponentiate(h, new BigInteger(1, sn.challenge))))

      sn.copy(firstMessageOpt = Some(FirstDLogProverMessage(a)))
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




