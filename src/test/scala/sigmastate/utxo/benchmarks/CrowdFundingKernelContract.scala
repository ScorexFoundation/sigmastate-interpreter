package sigmastate.utxo.benchmarks

import java.math.BigInteger
import java.util

import org.ergoplatform.ErgoLikeContext
import scapi.sigma.Challenge
import scapi.sigma.DLogProtocol.{DLogInteractiveProver, DLogProverInput, FirstDLogProverMessage, ProveDlog}
import scorex.crypto.hash.Blake2b256
import sigmastate._
import sigmastate.helpers.ErgoLikeProvingInterpreter
import sigmastate.interpreter.{CryptoConstants, Interpreter}
import sigmastate.utils.Helpers

import scala.util.Try

class CrowdFundingKernelContract(
                                  timeout: Long,
                                  minToRaise: Long,
                                  override val backerProver: ErgoLikeProvingInterpreter,
                                  override val projectProver: ErgoLikeProvingInterpreter
) extends CrowdFundingContract(timeout, minToRaise, backerProver, projectProver) {

  def isValid(pubKey: ProveDlog, message: Array[Byte]): projectProver.ProofT = {
    import projectProver._
    var su = UnprovenSchnorr(pubKey, None, None, None, simulated = false)
    val secret = secrets.find {
      case in: DLogProverInput => in.publicImage == pubKey
      case _ => false
    }
    val secretKnown = secret.isDefined
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
      case _ => ???
      /*case uc: UnprovenConjecture => uc.childrenCommitments*/ // can't do this anymore because internal nodes no longer have commitments
    }

    val rootChallenge = Blake2b256(Helpers.concatBytes(commitments.map(_.bytes) :+ message))

    su = step4.asInstanceOf[UnprovenSchnorr]
    val privKey = secret.get.asInstanceOf[DLogProverInput]
    val z = DLogInteractiveProver.secondMessage(privKey, su.randomnessOpt.get, Challenge(rootChallenge))
    UncheckedSchnorr(su.proposition, None, rootChallenge, z)
  }

  def prove(ctx: ErgoLikeContext, message: Array[Byte]): Array[Byte] = {
    val c1 = ctx.currentHeight >= timeout //&& isValid(backerPubKey, fakeMessage)
    val c2 = Array(
      ctx.currentHeight < timeout,
      ctx.spendingTransaction.outputs.exists(out => {
        out.value >= minToRaise && util.Arrays.equals(out.propositionBytes, projectPubKey.bytes)
      })
    ).forall(identity)
    var proof: projectProver.ProofT = null
    c1 || (c2 && { proof = isValid(projectPubKey, message); true})
    SigSerializer.toBytes(proof)
  }

  def verify(proof: Array[Byte],
             ctx: ErgoLikeContext,
             message: Array[Byte]): Try[Interpreter.VerificationResult] = Try {
    var sn = proof.asInstanceOf[UncheckedSchnorr]
    val dlog = CryptoConstants.dlogGroup
    val g = dlog.generator
    val h = sn.proposition.h

    val a = dlog.multiplyGroupElements(
      dlog.exponentiate(g, sn.secondMessage.z.underlying()),
      dlog.getInverse(dlog.exponentiate(h, new BigInteger(1, sn.challenge))))

    val rootCommitment = FirstDLogProverMessage(a)

    val expectedChallenge = Blake2b256(Helpers.concatBytes(Seq(rootCommitment.bytes, message)))
    util.Arrays.equals(sn.challenge, expectedChallenge) -> 0
  }
}
