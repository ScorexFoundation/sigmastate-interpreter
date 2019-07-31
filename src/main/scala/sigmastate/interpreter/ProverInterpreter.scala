package sigmastate.interpreter

import java.util

import io.circe._
import io.circe.syntax._
import gf2t.{GF2_192, GF2_192_Poly}
import org.bitbucket.inkytonik.kiama.attribution.AttributionCore
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, everywheretd, rule}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.ergoplatform.JsonCodecs
import org.ergoplatform.settings.Algos
import scalan.util.CollectionUtil._
import sigmastate.Values._
import sigmastate._
import sigmastate.basics.DLogProtocol._
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.basics.{DiffieHellmanTupleInteractiveProver, DiffieHellmanTupleProverInput, ProveDHTuple, SigmaProtocolPrivateInput}
import sigmastate.lang.exceptions.CostLimitException
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{Helpers, SigmaByteReader, SigmaByteWriter}

import scala.util.Try

/**
  * Proof of correctness of tx spending
  *
  * @param proof     - proof that satisfies final sigma proposition
  * @param extension - user-defined variables to be put into context
  */
class ProverResult(val proof: Array[Byte], val extension: ContextExtension) {
  override def hashCode(): Int = util.Arrays.hashCode(proof) * 31 + extension.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case obj: ProverResult =>
      util.Arrays.equals(proof, obj.proof) && extension == obj.extension
    case _ => false
  }

  override def toString: Idn = s"ProverResult(${Algos.encode(proof)},$extension)"
}

object ProverResult extends JsonCodecs {
  val empty: ProverResult = ProverResult(Array[Byte](), ContextExtension.empty)

  def apply(proof: Array[Byte], extension: ContextExtension): ProverResult =
    new ProverResult(proof, extension)

  object serializer extends SigmaSerializer[ProverResult, ProverResult] {

    override def serialize(obj: ProverResult, w: SigmaByteWriter): Unit = {
      w.putUShort(obj.proof.length)
      w.putBytes(obj.proof)
      ContextExtension.serializer.serialize(obj.extension, w)
    }

    override def parse(r: SigmaByteReader): ProverResult = {
      val sigBytesCount = r.getUShort()
      val proofBytes = r.getBytes(sigBytesCount)
      val ce = ContextExtension.serializer.parse(r)
      ProverResult(proofBytes, ce)
    }
  }

  implicit val jsonEncoder: Encoder[ProverResult] = { v =>
    Json.obj(
      "proofBytes" -> v.proof.asJson,
      "extension" -> v.extension.asJson
    )
  }

  implicit val jsonDecoder: Decoder[ProverResult] = { cursor =>
    for {
      proofBytes <- cursor.downField("proofBytes").as[Array[Byte]]
      extMap <- cursor.downField("extension").as[Map[Byte, EvaluatedValue[SType]]]
    } yield ProverResult(proofBytes, ContextExtension(extMap))
  }

}

case class CostedProverResult(override val proof: Array[Byte],
                              override val extension: ContextExtension,
                              cost: Long) extends ProverResult(proof, extension)

/**
  * Interpreter with enhanced functionality to prove statements.
  */
trait ProverInterpreter extends Interpreter with AttributionCore {

  import CryptoConstants.secureRandomBytes
  import Interpreter._

  override type ProofT = UncheckedTree

  val secrets: Seq[SigmaProtocolPrivateInput[_, _]]

  /**
    * The comments in this section are taken from the algorithm for the
    * Sigma-protocol prover as described in the white paper
    *
    */
  // todo: if we are concerned about timing attacks against the prover, we should make sure that this code
  // todo: takes the same amount of time regardless of which nodes are real and which nodes are simulated
  // todo: In particular, we should avoid the use of exists and forall, because they short-circuit the evaluation
  // todo: once the right value is (or is not) found. We should also make all loops look similar, the same
  // todo: amount of copying is done regardless of what's real or simulated,
  // todo: real vs. simulated computations take the same time, etc.
  protected def prove(unprovenTree: UnprovenTree, message: Array[Byte]): ProofT = {

    // Prover Step 1: Mark as real everything the prover can prove
    val step1 = markReal(unprovenTree).get.asInstanceOf[UnprovenTree]

    // Prover Step 2: If the root of the tree is marked "simulated" then the prover does not have enough witnesses
    // to perform the proof. Abort.
    assert(step1.real, s"Tree root should be real but was $step1")

    // Prover Step 3: Change some "real" nodes to "simulated" to make sure each node
    // has the right number of simulated children.
    val step3 = polishSimulated(step1).get.asInstanceOf[UnprovenTree]

    // Prover Steps 4, 5, and 6 together: find challenges for simulated nodes; simulate simulated leaves;
    // compute commitments for real leaves
    val step6 = simulateAndCommit(step3).get.asInstanceOf[UnprovenTree]

    // Prover Steps 7: convert the relevant information in the tree (namely, tree structure, node types,
    // the statements being proven and commitments at the leaves)
    // to a string
    val s = FiatShamirTree.toBytes(step6)

    // Prover Step 8: compute the challenge for the root of the tree as the Fiat-Shamir hash of s
    // and the message being signed.
    val rootChallenge = Challenge @@ CryptoFunctions.hashFn(s ++ message)
    val step8 = step6.withChallenge(rootChallenge)

    // Prover Step 9: complete the proof by computing challenges at real nodes and additionally responses at real leaves
    val step9 = proving(step8).get.asInstanceOf[ProofTree]

    // Syntactic step that performs a type conversion only
    convertToUnchecked(step9)
  }

  def prove(exp: ErgoTree, context: CTX, message: Array[Byte]): Try[CostedProverResult] =
    prove(emptyEnv, exp, context, message)

  def prove(env: ScriptEnv, tree: ErgoTree, ctx: CTX, message: Array[Byte]): Try[CostedProverResult] = Try {
    import TrivialProp._

    val initCost = tree.complexity + ctx.initCost
    val remainingLimit = ctx.costLimit - initCost
    if (remainingLimit <= 0)
      throw new CostLimitException(initCost,
        s"Estimated execution cost $initCost exceeds the limit ${ctx.costLimit}", None)

    val ctxUpdInitCost = ctx.withInitCost(initCost).asInstanceOf[CTX]

    val prop = propositionFromErgoTree(tree, ctxUpdInitCost)
    val (propTree, _) = applyDeserializeContext(ctxUpdInitCost, prop)
    val tried = reduceToCrypto(ctxUpdInitCost, env, propTree)
    val (reducedProp, cost) = tried.fold(t => throw t, identity)

    def errorReducedToFalse = error("Script reduced to false")

    val proofTree = reducedProp match {
      case TrueProp => NoProof
      case FalseProp => errorReducedToFalse
      case sigmaTree =>
        val unprovenTree = convertToUnproven(sigmaTree)
        prove(unprovenTree, message)
    }
    // Prover Step 10: output the right information into the proof
    val proof = SigSerializer.toBytes(proofTree)
    CostedProverResult(proof, ctxUpdInitCost.extension, cost)
  }

  /**
    * Prover Step 1: This step will mark as "real" every node for which the prover can produce a real proof.
    * This step may mark as "real" more nodes than necessary if the prover has more than the minimal
    * necessary number of witnesses (for example, more than one child of an OR).
    * This will be corrected in the next step.
    * In a bottom-up traversal of the tree, do the following for each node:
    */
  val markReal: Strategy = everywherebu(rule[UnprovenTree] {
    case and: CAndUnproven =>
      // If the node is AND, mark it "real" if all of its children are marked real; else mark it "simulated"
      val simulated = and.children.exists(_.asInstanceOf[UnprovenTree].simulated)
      and.copy(simulated = simulated)
    case or: COrUnproven =>
      // If the node is OR, mark it "real" if at least one child is marked real; else mark it "simulated"
      val simulated = or.children.forall(_.asInstanceOf[UnprovenTree].simulated)
      or.copy(simulated = simulated)
    case t: CThresholdUnproven =>
      // If the node is TRESHOLD(k), mark it "real" if at least k of its children are marked real; else mark it "simulated"
      val c = t.children.foldLeft(0) { (count, child) =>
        count + (if (child.asInstanceOf[UnprovenTree].simulated) 0 else 1)
      }
      t.copy(simulated = c < t.k)
    case su: UnprovenSchnorr =>
      // If the node is a leaf, mark it "real'' if the witness for it is available; else mark it "simulated"
      val secretKnown = secrets.exists {
        case in: DLogProverInput => in.publicImage == su.proposition
        case _ => false
      }
      su.copy(simulated = !secretKnown)
    case dhu: UnprovenDiffieHellmanTuple =>
      // If the node is a leaf, mark it "real" if the witness for it is available; else mark it "simulated"
      val secretKnown = secrets.exists {
        case in: DiffieHellmanTupleProverInput => in.publicImage == dhu.proposition
        case _ => false
      }
      dhu.copy(simulated = !secretKnown)
    case t =>
      error(s"Don't know how to markReal($t)")
  })

  /**
    * Prover Step 3: This step will change some "real" nodes to "simulated" to make sure each node has
    * the right number of simulated children.
    * In a top-down traversal of the tree, do the following for each node:
    */
  val polishSimulated: Strategy = everywheretd(rule[UnprovenTree] {
    case and: CAndUnproven =>
      // If the node is marked "simulated", mark all of its children "simulated"
      if (and.simulated) and.copy(children = and.children.map(_.asInstanceOf[UnprovenTree].withSimulated(true)))
      else and
    case or: COrUnproven =>
      // If the node is marked "simulated", mark all of its children "simulated"
      if (or.simulated) {
        or.copy(children = or.children.map(_.asInstanceOf[UnprovenTree].withSimulated(true)))
      } else {
        // If the node is OR marked "real",  mark all but one of its children "simulated"
        // (the node is guaranteed by step 1 to have at least one "real" child).
        // Which particular child is left "real" is not important for security;
        // the choice can be guided by efficiency or convenience considerations.
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
    case t: CThresholdUnproven =>
      // If the node is marked "simulated", mark all of its children "simulated"
      if (t.simulated) t.copy(children = t.children.map(_.asInstanceOf[UnprovenTree].withSimulated(true)))
      else {
        // If the node is THRESHOLD(k) marked "real", mark all but k of its children "simulated"
        // (the node is guaranteed, by the previous step, to have at least k "real" children).
        // Which particular ones are left "real" is not important for security;
        // the choice can be guided by efficiency or convenience considerations.
        //
        // We'll mark the first k real ones real
        val newChildren = t.children.foldLeft((Seq[UnprovenTree](), 0)) { case ((children, countOfReal), child) =>
          val kid = child.asInstanceOf[UnprovenTree]
          val (newKid, newCountOfReal) = kid.real match {
            case false => (kid, countOfReal)
            case true => ( {
              if (countOfReal >= t.k) kid.withSimulated(true) else kid
            }, countOfReal + 1)
          }
          (children :+ newKid, newCountOfReal)
        }._1
        t.copy(children = newChildren)
      }
    case su: UnprovenSchnorr => su
    case dhu: UnprovenDiffieHellmanTuple => dhu
    case _ => ???
  })

  /**
    * Prover Step 4: In a top-down traversal of the tree, compute the challenges e for simulated children of every node
    * Prover Step 5: For every leaf marked "simulated", use the simulator of the Sigma-protocol for that leaf
    * to compute the commitment $a$ and the response z, given the challenge e that is already stored in the leaf.
    * Prover Step 6: For every leaf marked "real", use the first prover step of the Sigma-protocol for that leaf to
    * compute the commitment a.
    */
  val simulateAndCommit: Strategy = everywheretd(rule[ProofTree] {
    // Step 4 part 1: If the node is marked "real", then each of its simulated children gets a fresh uniformly
    // random challenge in {0,1}^t.
    case and: CAndUnproven if and.real => and // A real AND node has no simulated children

    //real OR or Threshold case
    case uc: UnprovenConjecture if uc.real =>
      val newChildren = uc.children.cast[UnprovenTree].map(c =>
        if (c.real) c
        else c.withChallenge(Challenge @@ secureRandomBytes(CryptoFunctions.soundnessBytes))
      )
      uc match {
        case or: COrUnproven => or.copy(children = newChildren)
        case t: CThresholdUnproven => t.copy(children = newChildren)
        case _ => ???
      }

    // Step 4 part 2: If the node is marked "simulated", let e_0 be the challenge computed for it.
    // All of its children are simulated, and thus we compute challenges for all
    // of them, as follows:
    case and: CAndUnproven if and.simulated =>
      // If the node is AND, then all of its children get e_0 as the challenge
      assert(and.challengeOpt.isDefined)
      val challenge = and.challengeOpt.get
      val newChildren = and.children.cast[UnprovenTree].map(_.withChallenge(challenge))
      and.copy(children = newChildren)

    case or: COrUnproven if or.simulated =>
      // If the node is OR, then each of its children except one gets a fresh uniformly random
      // challenge in {0,1}^t. The remaining child gets a challenge computed as an XOR of the challenges of all
      // the other children and e_0.
      assert(or.challengeOpt.isDefined)
      val unprovenChildren = or.children.cast[UnprovenTree]
      val t = unprovenChildren.tail.map(_.withChallenge(Challenge @@ secureRandomBytes(CryptoFunctions.soundnessBytes)))
      val toXor: Seq[Array[Byte]] = or.challengeOpt.get +: t.map(_.challengeOpt.get)
      val xoredChallenge = Challenge @@ Helpers.xor(toXor: _*)
      val h = unprovenChildren.head.withChallenge(xoredChallenge)
      or.copy(children = h +: t)

    case t: CThresholdUnproven if t.simulated =>
      // The faster algorithm is as follows. Pick n-k fresh uniformly random values
      // q_1, ..., q_{n-k} from {0,1}^t and let q_0=e_0.
      // Viewing 1, 2, ..., n and q_0, ..., q_{n-k} as elements of GF(2^t),
      // evaluate the polynomial Q(x) = sum {q_i x^i} over GF(2^t) at points 1, 2, ..., n
      // to get challenges for child 1, 2, ..., n, respectively.
      assert(t.challengeOpt.isDefined)
      val n = t.children.length
      val unprovenChildren = t.children.cast[UnprovenTree]
      val q = GF2_192_Poly.fromByteArray(t.challengeOpt.get, secureRandomBytes(CryptoFunctions.soundnessBytes * (n - t.k)))

      val newChildren = unprovenChildren.foldLeft((Seq[UnprovenTree](), 1)) {
        case ((childSeq, childIndex), child) =>
          (childSeq :+ child.withChallenge(Challenge @@ q.evaluate(childIndex.toByte).toByteArray), childIndex + 1)
      }._1
      t.withPolynomial(q).copy(children = newChildren)

    // The algorithm with better resistance to timing attacks is as follows.
    // Pick n-k fresh uniformly random values e_1, ..., e_{n-k}
    // as challenges for the children number 1, ..., n-k.
    // Let i_0 = 0. Viewing 0, 1, 2, ..., n and e_0, ..., e_{n-k} as elements of GF(2^t),
    // find (via polynomial interpolation) the
    // lowest-degree polynomial Q(x)=sum_{i=0}^{n-k} a_i x^i  over GF(2^t) that is equal to e_j at j for each j
    // from 0 to n-k (this polynomial will have n-k+1 coefficients, and the lowest coefficient will be e_0).
    // Set the challenge at child j for n-k<j<= n to equal Q(j).

    /* **** Uncomment this and comment out the above algorithm if you want better resistance to timing attacks
    assert(t.challengeOpt.isDefined)
    val n = t.children.length
    val unprovenChildren = t.children.cast[UnprovenTree]
    val childrenWithRandomChallenges = unprovenChildren.slice(0, n-t.k).map(_.withChallenge(Challenge @@ secureRandomBytes(CryptoFunctions.soundnessBytes)))
    val (points, values, _) = childrenWithRandomChallenges.foldLeft(((Array[Byte](), Array[GF2_192](),1))) {
      case ((p, v, count), child) =>
        val (newPoints, newValues) =
          if (count <= n - t.k) {
            (p :+ count.toByte, v :+ new GF2_192(child.challengeOpt.get))
          }
          else (p, v)
        (newPoints, newValues, count + 1)
    }
    val q = GF2_192_Poly.interpolate(points, values, new GF2_192(t.challengeOpt.get))

    val newChildren = unprovenChildren.slice(n-t.k, n).foldLeft((childrenWithRandomChallenges, n-t.k+1)) {
      case ((childSeq, childIndex), child) =>
        (childSeq :+ child.withChallenge(Challenge @@ q.evaluate(childIndex.toByte).toByteArray), childIndex + 1)
    }._1
    t.withPolynomial(q).copy(children=newChildren)
    */



    case su: UnprovenSchnorr =>
      if (su.simulated) {
        // Step 5 (simulated leaf -- complete the simulation)
        assert(su.challengeOpt.isDefined)
        val (fm, sm) = DLogInteractiveProver.simulate(su.proposition, su.challengeOpt.get)
        UncheckedSchnorr(su.proposition, Some(fm), su.challengeOpt.get, sm)
      } else {
        // Step 6 (real leaf -- compute the commitment a)
        val (r, commitment) = DLogInteractiveProver.firstMessage(su.proposition)
        su.copy(commitmentOpt = Some(commitment), randomnessOpt = Some(r))
      }

    case dhu: UnprovenDiffieHellmanTuple =>
      if (dhu.simulated) {
        // Step 5 (simulated leaf -- complete the simulation)
        assert(dhu.challengeOpt.isDefined)
        val (fm, sm) = DiffieHellmanTupleInteractiveProver.simulate(dhu.proposition, dhu.challengeOpt.get)
        UncheckedDiffieHellmanTuple(dhu.proposition, Some(fm), dhu.challengeOpt.get, sm)
      } else {
        // Step 6 (real leaf -- compute the commitment a)
        val (r, fm) = DiffieHellmanTupleInteractiveProver.firstMessage(dhu.proposition)
        dhu.copy(commitmentOpt = Some(fm), randomnessOpt = Some(r))
      }

    case a: Any => error(s"Don't know how to challengeSimulated($a)")
  })

  def extractChallenge(pt: ProofTree): Option[Array[Byte]] = pt match {
    case upt: UnprovenTree => upt.challengeOpt
    case sn: UncheckedSchnorr => Some(sn.challenge)
    case dh: UncheckedDiffieHellmanTuple => Some(dh.challenge)
    case _ => error(s"Cannot extractChallenge($pt)")
  }

  /**
    * Prover Step 9: Perform a top-down traversal of only the portion of the tree marked "real" in order to compute
    * the challenge e for every node marked "real" below the root and, additionally, the response z for every leaf
    * marked "real"
    */
  val proving: Strategy = everywheretd(rule[ProofTree] {
    // If the node is a non-leaf marked real whose challenge is e_0, proceed as follows:
    case and: CAndUnproven if and.real =>
      assert(and.challengeOpt.isDefined)
      // If the node is AND, let each of its children have the challenge e_0
      val andChallenge = and.challengeOpt.get
      and.copy(children = and.children.map(_.asInstanceOf[UnprovenTree].withChallenge(andChallenge)))

    case or: COrUnproven if or.real =>
      // If the node is OR, it has only one child marked "real".
      // Let this child have the challenge equal to the XOR of the challenges of all the other children and e_0
      assert(or.challengeOpt.isDefined)
      val rootChallenge = or.challengeOpt.get
      val challenge = Challenge @@ Helpers.xor(rootChallenge +: or.children.flatMap(extractChallenge): _*)

      or.copy(children = or.children.map {
        case r: UnprovenTree if r.real => r.withChallenge(challenge)
        case p: ProofTree => p
      })

    case t: CThresholdUnproven if t.real =>
      // If the node is THRESHOLD(k), number its children from 1 to no. Let i_1,..., i_{n-k}
      // be the indices of the children marked `"simulated" and e_1, ...,  e_{n-k} be their corresponding challenges.
      // Let i_0 = 0. Viewing 0, 1, 2, ..., n and e_0, ..., e_{n-k} as elements of GF(2^t),
      // find (via polynomial interpolation) the lowest-degree polynomial
      // Q(x)=sum_{i=0}^{n-k} a_i x^i  over GF(2^t) that is equal to e_j at i_j for each f from 0 to n-k
      // (this polynomial will have n-k+1 coefficients, and the lowest coefficient will be e_0). For child number
      // i of the node, if the child is marked "real", compute its challenge as Q(i) (if the child is marked
      // "simulated", its challenge is already Q(i), by construction of Q).
      assert(t.challengeOpt.isDefined)
      val (points, values, _) = t.children.foldLeft(Array[Byte](), Array[GF2_192](), 1) {
        case ((p, v, count), child) =>
          val (newPoints, newValues) = {
            // This is the easiest way to find out whether a child is simulated -- just to check if it alread
            // has a challenge. Other ways are more of a pain because the children can be of different types
            val challengeOpt = extractChallenge(child)
            if (challengeOpt.isEmpty) (p, v)
            else (p :+ count.toByte, v :+ new GF2_192(challengeOpt.get))

          }
          (newPoints, newValues, count + 1)
      }
      val q = GF2_192_Poly.interpolate(points, values, new GF2_192(t.challengeOpt.get))
      val newChildren = t.children.foldLeft(Seq[ProofTree](), 1) {
        case ((s, count), child) =>
          val newChild = child match {
            case r: UnprovenTree if r.real => r.withChallenge(Challenge @@ q.evaluate(count.toByte).toByteArray())
            case p: ProofTree => p
          }
          (s :+ newChild, count + 1)
      }._1
      t.withPolynomial(q).copy(children = newChildren)

    // If the node is a leaf marked "real", compute its response according to the second prover step
    // of the Sigma-protocol given the commitment, challenge, and witness
    case su: UnprovenSchnorr if su.real =>
      assert(su.challengeOpt.isDefined, s"Real UnprovenTree $su should have challenge defined")
      val privKey = secrets
        .filter(_.isInstanceOf[DLogProverInput])
        .find(_.asInstanceOf[DLogProverInput].publicImage == su.proposition)
        .get.asInstanceOf[DLogProverInput]
      val z = DLogInteractiveProver.secondMessage(privKey, su.randomnessOpt.get, su.challengeOpt.get)
      UncheckedSchnorr(su.proposition, None, su.challengeOpt.get, z)

    case dhu: UnprovenDiffieHellmanTuple if dhu.real =>
      assert(dhu.challengeOpt.isDefined)
      val privKey = secrets
        .filter(_.isInstanceOf[DiffieHellmanTupleProverInput])
        .find(_.asInstanceOf[DiffieHellmanTupleProverInput].publicImage == dhu.proposition)
        .get.asInstanceOf[DiffieHellmanTupleProverInput]
      val z = DiffieHellmanTupleInteractiveProver.secondMessage(privKey, dhu.randomnessOpt.get, dhu.challengeOpt.get)
      UncheckedDiffieHellmanTuple(dhu.proposition, None, dhu.challengeOpt.get, z)


    case sn: UncheckedSchnorr => sn

    case dh: UncheckedDiffieHellmanTuple => dh

    case ut: UnprovenTree => ut

    case a: Any => log.warn("Wrong input in prove(): ", a); ???
  })


  //converts SigmaTree => UnprovenTree
  def convertToUnproven(sigmaTree: SigmaBoolean): UnprovenTree = sigmaTree match {
    case and@CAND(sigmaTrees) =>
      CAndUnproven(and, None, simulated = false, sigmaTrees.map(convertToUnproven))
    case or@COR(children) =>
      COrUnproven(or, None, simulated = false, children.map(convertToUnproven))
    case threshold@CTHRESHOLD(k, children) =>
      CThresholdUnproven(threshold, None, simulated = false, k, children.map(convertToUnproven), None)
    case ci: ProveDlog =>
      UnprovenSchnorr(ci, None, None, None, simulated = false)
    case dh: ProveDHTuple =>
      UnprovenDiffieHellmanTuple(dh, None, None, None, simulated = false)
    case _ =>
      error(s"Cannot convertToUnproven($sigmaTree)")
  }

  //converts ProofTree => UncheckedSigmaTree
  val convertToUnchecked: ProofTree => UncheckedSigmaTree = attr {
    case and: CAndUnproven =>
      CAndUncheckedNode(and.challengeOpt.get, and.children.map(convertToUnchecked))
    case or: COrUnproven =>
      COrUncheckedNode(or.challengeOpt.get, or.children.map(convertToUnchecked))
    case t: CThresholdUnproven =>
      CThresholdUncheckedNode(t.challengeOpt.get, t.children.map(convertToUnchecked), t.k, t.polynomialOpt)
    case s: UncheckedSchnorr => s
    case d: UncheckedDiffieHellmanTuple => d
    case _ => ???
  }
}
