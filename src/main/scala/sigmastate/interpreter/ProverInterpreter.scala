package sigmastate.interpreter

import java.util
import java.util.Objects

import org.bitbucket.inkytonik.kiama.attribution.AttributionCore
import org.bitbucket.inkytonik.kiama.relation.Tree
import scapi.sigma.SigmaProtocolPrivateInput
import scapi.sigma.DLogProtocol._
import sigmastate._
import sigmastate.utils.Helpers
import sigmastate.utils.Extensions._
import Values._
import com.google.common.primitives.Shorts
import sigmastate.lang.Terms._

import scala.util.Try
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{rule, everywheretd, everywherebu}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scapi.sigma._
import scorex.utils.Random
import sigmastate.serialization.Serializer
import sigmastate.serialization.Serializer.{Position, Consumed}

/**
  * Proof generated by a prover along with possible context extensions
  */
class ProverResult(val proof: Array[Byte], val extension: ContextExtension) {
  override def hashCode(): Consumed = util.Arrays.hashCode(proof) * 31 + extension.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case obj: ProverResult =>
      util.Arrays.equals(proof, obj.proof) && extension == obj.extension
    case _ => false
  }
}

object ProverResult {

  def apply(proof: Array[Byte], extension: ContextExtension): ProverResult =
    new ProverResult(proof, extension)

  object serializer extends Serializer[ProverResult, ProverResult] {
    override def toBytes(pr: ProverResult): Array[Byte] = {
      val ceBytes = ContextExtension.serializer.toBytes(pr.extension)
      val sigBytesCount = pr.proof.length.toShort

      Shorts.toByteArray(sigBytesCount) ++ pr.proof ++ ceBytes
    }

    override def parseBytes(bytes: Array[Byte]): Try[ProverResult] = Try {
      parseBody(bytes, 0)._1
    }

    override def parseBody(bytes: Array[Byte], pos: Position): (ProverResult, Consumed) = {
      val sigBytesCount = Shorts.fromByteArray(bytes.slice(pos, pos + 2))
      val proofBytes = bytes.slice(pos + 2, pos + 2 + sigBytesCount)
      val (ce, ceConsumed) = ContextExtension.serializer.parseBody(bytes, pos + 2 + sigBytesCount)
      ProverResult(proofBytes, ce) -> (2 + sigBytesCount + ceConsumed)
    }
  }
}

case class CostedProverResult(override val proof: Array[Byte],
                              override val extension: ContextExtension,
                              cost: Long) extends ProverResult(proof, extension)

/**
  * Interpreter with enhanced functionality to prove statements.
  */
trait ProverInterpreter extends Interpreter with AttributionCore {
  import Interpreter._

  override type ProofT = UncheckedTree

  val secrets: Seq[SigmaProtocolPrivateInput[_, _]]

  def contextExtenders: Map[Byte, EvaluatedValue[_ <: SType]] = Map()

  val knownExtensions = ContextExtension(contextExtenders)

  /**
    * "Prover steps:
    * *
    * (markSimulated)
    *1. bottom-up: mark every node real or simulated, according to the following rule.
    *  DLogNode: if you know the DL, then real, else simulated.
    *  DHTuple: if you know the DHT, then real, else simulated.
    *  COR: if at least one child real, then real; else simulated.
    *  CAND: if at least one child simulated, then simulated; else real.
    *
    *  Note that all descendants of a simulated node will be later simulated, even
    *  if they were marked as real. This is what the next step will do.
    *
    *  Root should end up real according to this rule -- else you won't be able to carry out the proof in the end.
    *
    * (polishSimulated)
    *2. top-down: mark every child of a simulated node "simulated." If two or more children of a real COR are real,
    *  mark all but one simulated.
    *
    * (challengeSimulated)
    *3. top-down: compute a challenge for every simulated child of every COR and CAND, according to the following rules.
    *  If COR, then every simulated child gets a fresh random challenge.
    *  If CAND (which means CAND itself is simulated, and all its children are),
    *  then every child gets the same challenge as the CAND.
    *
    * (simulations)
    *4. bottom-up: For every simulated leaf, simulate a response and a commitment (i.e., first and second prover message)
    *  according to the Schnorr simulator. For every real leaf, compute the commitment (i.e., first prover message) according
    *  to the Schnorr protocol. For every COR/CAND node, let the commitment be the union (as a set) of commitments below it.
    *
    *5. Compute the Schnorr challenge as the hash of the commitment of the root (plus other inputs -- probably the tree
    *  being proven and the message).
    *
    * (challengesReal, proving)
    *6. top-down: compute the challenge for every real child of every real COR and CAND, as follows. If COR, then the
    *  challenge for the one real child of COR is equal to the XOR of the challenge of COR and the challenges for all the
    *  simulated children of COR. If CAND, then the challenge for every real child of CAND is equal to the the challenge of
    *  the CAND. Note that simulated CAND and COR have only simulated descendants, so no need to recurse down from them."
    */
  protected def prove(unprovenTree: UnprovenTree, message: Array[Byte]): ProofT = {
    val step1 = markSimulated(unprovenTree).get.asInstanceOf[UnprovenTree]
    assert(step1.real, s"Tree root should be real but was $step1")

    val step2 = polishSimulated(step1).get.asInstanceOf[UnprovenTree]
    val step3 = challengeSimulated(step2).get.asInstanceOf[UnprovenTree]
    val step4 = simulations(step3).get.asInstanceOf[UnprovenTree]

    //step 5 - compute root challenge
    val rootChallenge = CryptoFunctions.hashFn(FiatShamirTree.toBytes(step4) ++ message)
    val step5 = step4.withChallenge(rootChallenge)

    val step6 = proving(step5).get.asInstanceOf[ProofTree]

    convertToUnchecked(step6)
  }

  def prove(exp: Value[SBoolean.type], context: CTX, message: Array[Byte]): Try[ProverResult] = Try {
    val (reducedProp, cost) = reduceToCrypto(context.withExtension(knownExtensions), exp).get
    val proofTree = reducedProp match {
      case bool: BooleanConstant =>
        bool match {
          case TrueLeaf => NoProof
          case FalseLeaf => ???
        }
      case _ =>
        val ct = convertToUnproven(reducedProp.asInstanceOf[SigmaBoolean])
        prove(ct, message)
    }
    val proof = SigSerializer.toBytes(proofTree)
    CostedProverResult(proof, knownExtensions, cost)
  }

  /**
    * 1. bottom-up: mark every node real or simulated, according to the following rule.
    * DLogNode: if you know the DL, then real, else simulated.
    * DHTuple: if you know the DHT, then real, else simulated.
    * COR: if at least one child real, then real; else simulated.
    * CAND: if at least one child simulated, then simulated; else real.
    *
    * Note that all descendants of a simulated node will be later simulated, even
    * if they were marked as real. This is what the next step will do.
    */
  val markSimulated: Strategy = everywherebu(rule[UnprovenTree] {
    case and: CAndUnproven =>
      val simulated = and.children.exists(_.asInstanceOf[UnprovenTree].simulated)
      and.copy(simulated = simulated)
    case or: COrUnproven =>
      val simulated = or.children.forall(_.asInstanceOf[UnprovenTree].simulated)
      or.copy(simulated = simulated)
    case su: UnprovenSchnorr =>
      val secretKnown = secrets.exists {
        case in: DLogProverInput => in.publicImage == su.proposition
        case _ => false
      }
      su.copy(simulated = !secretKnown)
    case dhu: UnprovenDiffieHellmanTuple =>
      val secretKnown = secrets.exists {
        case in: DiffieHellmanTupleProverInput => in.publicImage == dhu.proposition
        case _ => false
      }
      dhu.copy(simulated = !secretKnown)
    case t =>
      error(s"Don't know how to markSimulated($t)")
  })

  /**
    * 2. top-down: mark every child of a simulated node "simulated."
    * If two or more children of a real COR are real, mark all but one simulated.
    */
  val polishSimulated: Strategy = everywheretd(rule[UnprovenTree] {
    case and: CAndUnproven =>
      if (and.simulated) and.copy(children = and.children.map(_.asInstanceOf[UnprovenTree].withSimulated(true)))
      else and
    case or: COrUnproven =>
      if (or.simulated) {
        or.copy(children = or.children.map(_.asInstanceOf[UnprovenTree].withSimulated(true)))
      } else {
        val newChildren = or.children.foldLeft((Seq[UnprovenTree](), false)) { case ((children, realFound), child) =>
          val cut = child.asInstanceOf[UnprovenTree]
          (realFound, cut.real) match {
            case (true, true) => (children :+ cut.withSimulated(true), true)
            case (true, false) => (children :+ cut, true)
            case (false, true) => (children :+ cut, true)
            case (false, false) => (children :+ cut, false)
          }
        }._1
        or.copy(children = newChildren)
      }
    case su: UnprovenSchnorr => su
    case dhu: UnprovenDiffieHellmanTuple => dhu
    case _ => ???
  })

  /**
    * 3. top-down: compute a challenge for every simulated child of every COR and CAND, according to the following rules.
    * If COR, then every simulated child gets a fresh random challenge. If CAND (which means CAND itself is simulated, and
    * all its children are), then every child gets the same challenge as the CAND.
    */
  val challengeSimulated: Strategy = everywheretd(rule[UnprovenTree] {
    case and: CAndUnproven if and.simulated =>
      assert(and.challengeOpt.isDefined)
      val challenge = and.challengeOpt.get
      val newChildren = and.children.cast[UnprovenTree].map(_.withChallenge(challenge))
      and.copy(children = newChildren)

    case and: CAndUnproven if and.real => and

    case or: COrUnproven if or.real =>
      val newChildren = or.children.cast[UnprovenTree].map(c =>
        if (c.real) c
        else c.withChallenge(Random.randomBytes(CryptoFunctions.soundnessBytes))
      )
      or.copy(children = newChildren)

    case or: COrUnproven if or.simulated =>
      assert(or.challengeOpt.isDefined)
      val unprovenChildren = or.children.cast[UnprovenTree]
      val t = unprovenChildren.tail.map(_.withChallenge(Random.randomBytes(CryptoFunctions.soundnessBytes)))
      val toXor: Seq[Array[Byte]] = or.challengeOpt.get +: t.map(_.challengeOpt.get)
      val xoredChallenge = Helpers.xor(toXor: _*)
      val h = unprovenChildren.head.withChallenge(xoredChallenge)
      or.copy(children = h +: t)

    case su: UnprovenSchnorr => su
    case dhu: UnprovenDiffieHellmanTuple => dhu

    case a: Any => error(s"Don't know how to challengeSimulated($a)")
  })

  /**
    * 4. bottom-up: For every simulated leaf, simulate a response and a commitment (i.e., second and first prover
    * message) according to the Schnorr simulator. For every real leaf, compute the commitment (i.e., first prover
    * message) according to the Schnorr protocol. For every COR/CAND node, let the commitment be the union (as a set)
    * of commitments below it.
    */
  val simulations: Strategy = everywherebu(rule[ProofTree] {
    case and: CAndUnproven =>
      val commitments = and.children.flatMap {
        case ul: UnprovenLeaf => ul.commitmentOpt.toSeq
        case uc: UnprovenConjecture => uc.childrenCommitments
        case sn: UncheckedSchnorr => sn.commitmentOpt.toSeq
        case dh: UncheckedDiffieHellmanTuple => dh.commitmentOpt.toSeq
        case _ => ???
      }
      and.copy(childrenCommitments = commitments)

    case or: COrUnproven =>
      val commitments = or.children.flatMap {
        case ul: UnprovenLeaf => ul.commitmentOpt.toSeq
        case uc: UnprovenConjecture => uc.childrenCommitments
        case sn: UncheckedSchnorr => sn.commitmentOpt.toSeq
        case dh: UncheckedDiffieHellmanTuple => dh.commitmentOpt.toSeq
        case a: Any => ???
      }
      or.copy(childrenCommitments = commitments)

    case su: UnprovenSchnorr =>
      if (su.simulated) {
        assert(su.challengeOpt.isDefined)
        SchnorrSigner(su.proposition, None).prove(su.challengeOpt.get)
      } else {
        val (r, commitment) = DLogInteractiveProver.firstMessage(su.proposition)
        su.copy(commitmentOpt = Some(commitment), randomnessOpt = Some(r))
      }

    case dhu: UnprovenDiffieHellmanTuple =>
      if (dhu.simulated) {
        assert(dhu.challengeOpt.isDefined)
        val prover = new DiffieHellmanTupleInteractiveProver(dhu.proposition, None)
        val (fm, sm) = prover.simulate(Challenge(dhu.challengeOpt.get))
        UncheckedDiffieHellmanTuple(dhu.proposition, Some(fm), dhu.challengeOpt.get, sm)
      } else {
        val (r, fm) = DiffieHellmanTupleInteractiveProver.firstMessage(dhu.proposition)
        dhu.copy(commitmentOpt = Some(fm), randomnessOpt = Some(r))
      }

    case _ => ???
  })

  def extractChallenge(pt: ProofTree): Option[Array[Byte]] = pt match {
    case upt: UnprovenTree => upt.challengeOpt
    case sn: UncheckedSchnorr => Some(sn.challenge)
    case dh: UncheckedDiffieHellmanTuple => Some(dh.challenge)
    case _ => ???
  }

  /**
    * (proving)
    * 6. top-down: compute the challenge for every real child of every real COR and CAND, as follows. If COR, then the
    * challenge for the one real child of COR is equal to the XOR of the challenge of COR and the challenges for all the
    * simulated children of COR. If CAND, then the challenge for every real child of CAND is equal to the the challenge of
    * the CAND. Note that simulated CAND and COR have only simulated descendants, so no need to recurse down from them."
    **/
  val proving: Strategy = everywheretd(rule[ProofTree] {
    case and: CAndUnproven if and.real =>
      assert(and.challengeOpt.isDefined)
      val andChallenge = and.challengeOpt.get
      and.copy(children = and.children.map(_.asInstanceOf[UnprovenTree].withChallenge(andChallenge)))

    case or: COrUnproven if or.real =>
      assert(or.challengeOpt.isDefined)
      val rootChallenge = or.challengeOpt.get
      val challenge = Helpers.xor(rootChallenge +: or.children.flatMap(extractChallenge): _*)

      or.copy(children = or.children.map {
        case r: UnprovenTree if r.real => r.withChallenge(challenge)
        case p: ProofTree => p
      })

    case su: UnprovenSchnorr if su.real =>
      assert(su.challengeOpt.isDefined)
      val privKey = secrets
        .filter(_.isInstanceOf[DLogProverInput])
        .find(_.asInstanceOf[DLogProverInput].publicImage == su.proposition)
        .get.asInstanceOf[DLogProverInput]
      val z = DLogInteractiveProver.secondMessage(privKey, su.randomnessOpt.get, Challenge(su.challengeOpt.get))
      UncheckedSchnorr(su.proposition, None, su.challengeOpt.get, z)

    case dhu: UnprovenDiffieHellmanTuple if dhu.real =>
      assert(dhu.challengeOpt.isDefined)
      val privKey = secrets
        .filter(_.isInstanceOf[DiffieHellmanTupleProverInput])
        .find(_.asInstanceOf[DiffieHellmanTupleProverInput].publicImage == dhu.proposition)
        .get.asInstanceOf[DiffieHellmanTupleProverInput]
      val z = DiffieHellmanTupleInteractiveProver.secondMessage(privKey, dhu.randomnessOpt.get, Challenge(dhu.challengeOpt.get))
      UncheckedDiffieHellmanTuple(dhu.proposition, None, dhu.challengeOpt.get, z)


    case sn: UncheckedSchnorr => sn

    case dh: UncheckedDiffieHellmanTuple => dh

    case ut: UnprovenTree => ut

    case a: Any => println(a); ???
  })


  //converts SigmaTree => UnprovenTree
  val convertToUnproven: SigmaBoolean => UnprovenTree = attr {
    case CAND(sigmaTrees) =>
      CAndUnproven(CAND(sigmaTrees), Seq(), None, simulated = false, sigmaTrees.map(convertToUnproven))
    case COR(children) =>
      COrUnproven(COR(children), Seq(), None, simulated = false, children.map(convertToUnproven))
    case ci: ProveDlog =>
      UnprovenSchnorr(ci, None, None, None, simulated = false)
    case dh: ProveDiffieHellmanTuple =>
      UnprovenDiffieHellmanTuple(dh, None, None, None, simulated = false)
  }

  //converts ProofTree => UncheckedTree
  val convertToUnchecked: ProofTree => UncheckedTree = attr {
    case and: CAndUnproven =>
      CAndUncheckedNode(and.challengeOpt, and.childrenCommitments, and.children.map(convertToUnchecked))
    case or: COrUnproven =>
      COrUncheckedNode(or.challengeOpt, or.childrenCommitments, or.children.map(convertToUnchecked))
    case s: UncheckedSchnorr => s
    case d: UncheckedDiffieHellmanTuple => d
    case _ => ???
  }
}
