package sigmastate.interpreter

import scapi.sigma.rework.DLogProtocol
import sigmastate._
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

  override def prove(unproven: UnprovenTree): ProofT = proving(secrets)(unproven)
}