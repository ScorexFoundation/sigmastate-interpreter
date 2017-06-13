package sigmastate.interpreter

import scapi.sigma.rework.DLogProtocol
import scapi.sigma.rework.DLogProtocol.DLogNode
import sigmastate._
import sigmastate.utils.Helpers
import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, everywheretd, rule}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy


trait DLogProverInterpreter extends ProverInterpreter {
  override type SigmaT = SigmaTree
  override type ProofT = UncheckedTree

  val secrets: Seq[DLogProtocol.DLogProverInput]

  //to be applied bottom up, marks whether simulation is needed for a sigma-protocol
  val markSimulated: Strategy = rule[UnprovenTree] {
    case su: SchnorrUnproven =>
      val secretKnown = secrets.exists(_.publicImage.h == su.proposition.h)
      su.copy(simulated = !secretKnown)
  }

  //to be applied down from the top node
  val challengeDisperse: Strategy = rule[UnprovenTree] {
    case cand: CAndUnproven if cand.challengeOpt.isDefined =>
      val challenge = cand.challengeOpt.get
      cand.copy(children = cand.children.map(_.withChallenge(challenge)))

    case cor: COr2Unproven if cor.challengeOpt.isDefined =>
      ???
      /*
      val rootChallenge = cor.challengeOpt.get
      val challengeSize = rootChallenge.length
      val randomChallenges = cor.children.tail.map(_ => Random.randomBytes(challengeSize))
      val realChallenge = Helpers.xor(rootChallenge +: randomChallenges: _*)
      val childrenChallenges = realChallenge +: randomChallenges
      assert(childrenChallenges.size == cor.children.size)

      cor.copy(children = cor.children.zip(childrenChallenges).map { case (ut, ch) => ut.setChallenge(ch) })*/
  }

  override def normalizeUnprovenTree(unprovenTree: UnprovenTree): UnprovenTree = {
    val t = everywherebu(markSimulated)(unprovenTree).get.asInstanceOf[UnprovenTree]
    everywheretd(challengeDisperse)(t).get.asInstanceOf[UnprovenTree]
  }

  override def prove(unproven: UnprovenTree): ProofT = TreeConversion.proving(secrets)(unproven)
}



object TreeConversion extends Attribution {

  //to be applied bottom up, converts SigmaTree => UnprovenTree
  val convertToUnproven: SigmaTree => UnprovenTree = attr {
    case CAND(sigmaTrees) => CAndUnproven(CAND(sigmaTrees), None, sigmaTrees.map(convertToUnproven))
    case COR2(left, right) => COr2Unproven(COR2(left, right), None, convertToUnproven(left), convertToUnproven(right))
    case ci: DLogNode => SchnorrUnproven(None, simulated = false, ci)
  }

  val proving: Seq[DLogProtocol.DLogProverInput] => UnprovenTree => UncheckedTree = paramAttr { secrets => {
    case SchnorrUnproven(Some(challenge), simulated, proposition) =>
      if (simulated) {
        SchnorrSigner(proposition.asInstanceOf[DLogNode],None).prove(challenge)
      } else {
        val privKey = secrets.find(_.publicImage.h == proposition.h).get
        SchnorrSigner.generate(privKey).prove(challenge)
      }

    case CAndUnproven(proposition, Some(challenge), children) =>
      val proven = children.map(proving(secrets))
      CAndUncheckedNode(proposition, challenge, proven)

    case COr2Unproven(proposition, Some(challenge), leftChild, rightChild) =>
      assert(Helpers.xor(leftChild.challengeOpt.get, rightChild.challengeOpt.get).sameElements(challenge))

      COr2UncheckedNode(proposition, challenge, proving(secrets)(leftChild), proving(secrets)(rightChild))
    case _ => ???
  }
  }
}
