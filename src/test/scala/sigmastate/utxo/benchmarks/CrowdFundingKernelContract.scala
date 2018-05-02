package sigmastate.utxo.benchmarks

import java.math.BigInteger

import scapi.sigma.Challenge
import scapi.sigma.DLogProtocol.{FirstDLogProverMessage, DLogInteractiveProver, ProveDlog, DLogProverInput}
import scorex.crypto.hash.Blake2b256
import sigmastate._
import sigmastate.helpers.ErgoProvingInterpreter
import sigmastate.interpreter.GroupSettings
import sigmastate.utils.Helpers
import sigmastate.utxo.ErgoContext

import scala.util.Try

class CrowdFundingKernelContract(
    timeout: Long,
    minToRaise: Long,
    override val backerProver: ErgoProvingInterpreter,
    override val projectProver: ErgoProvingInterpreter
) extends CrowdFundingContract(timeout, minToRaise, backerProver, projectProver) {

  def isValid(pubKey: ProveDlog, message: Array[Byte]): projectProver.ProofT = {
    import projectProver._
    var su = UnprovenSchnorr(pubKey, None, None, None, simulated = false)
    val secretKnown = secrets.exists {
      case in: DLogProverInput => in.publicImage == pubKey
      case _ => false
    }
    val simulated = !secretKnown
    val step4: UnprovenTree = if (simulated) {
      assert(su.challengeOpt.isDefined)
      SchnorrSigner(su.proposition, None).prove(su.challengeOpt.get).asInstanceOf[UnprovenTree]
    } else {
      val (r, commitment) = DLogInteractiveProver.firstMessage(pubKey)
      UnprovenSchnorr(pubKey, Some(commitment), Some(r), None, simulated = false)
    }

    val commitments = step4 match {
      case ul: UnprovenLeaf => ul.commitmentOpt.toSeq
      case uc: UnprovenConjecture => uc.childrenCommitments
    }

    val rootChallenge = Blake2b256(Helpers.concatBytes(commitments.map(_.bytes) :+ message))

    su = step4.withChallenge(rootChallenge).asInstanceOf[UnprovenSchnorr]
    assert(su.challengeOpt.isDefined)
    val privKey = secrets
        .filter(_.isInstanceOf[DLogProverInput])
        .find(_.asInstanceOf[DLogProverInput].publicImage == su.proposition)
        .get.asInstanceOf[DLogProverInput]
    val z = DLogInteractiveProver.secondMessage(privKey, su.randomnessOpt.get, Challenge(su.challengeOpt.get))
    UncheckedSchnorr(su.proposition, None, su.challengeOpt.get, z)
  }

  def prove(ctx: ErgoContext, message: Array[Byte]): projectProver.ProofT = {
    val c1 = ctx.currentHeight >= timeout //&& isValid(backerPubKey, fakeMessage)
    val c2 = Array(
      ctx.currentHeight < timeout,
      ctx.spendingTransaction.outputs.exists(out => {
        out.value >= minToRaise && (out.propositionBytes sameElements projectPubKey.propBytes)
      })
    ).forall(identity)
    var proof: projectProver.ProofT = null
    c1 || (c2 && { proof = isValid(projectPubKey, message); true})
    proof
  }

  def verify(proof: projectProver.ProofT, ctx: ErgoContext, message: Array[Byte]): Try[Boolean] = Try {
    var sn = proof.asInstanceOf[UncheckedSchnorr]
    val dlog = GroupSettings.dlogGroup
    val g = dlog.generator
    val h = sn.proposition.h

    val a = dlog.multiplyGroupElements(
      dlog.exponentiate(g, sn.secondMessage.z.underlying()),
      dlog.getInverse(dlog.exponentiate(h, new BigInteger(1, sn.challenge))))

    sn = sn.copy(firstMessageOpt = Some(FirstDLogProverMessage(a)))

    val (challenge, rootCommitments) = (sn.challenge, sn.firstMessageOpt.toSeq)

    val expectedChallenge = Blake2b256(rootCommitments.map(_.bytes).reduce(_ ++ _) ++ message)
    challenge.sameElements(expectedChallenge)
  }
}
