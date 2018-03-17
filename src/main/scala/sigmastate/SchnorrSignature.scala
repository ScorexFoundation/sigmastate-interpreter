package sigmastate

import scapi.sigma.{Challenge, NonInteractiveProver}
import scapi.sigma.DLogProtocol._
import scorex.crypto.hash.Blake2b256
import sigmastate.interpreter.GroupSettings


// TODO: make implementation corresponding to RFC-8032 standard for EdDSA signatures

object SchnorrSignature {
  implicit val soundness = 256

  implicit val hf = Blake2b256
}

case class SchnorrSigner(override val publicInput: ProveDlog, privateInputOpt: Option[DLogProverInput])
  extends NonInteractiveProver[DLogSigmaProtocol, DLogProverInput, ProveDlog, SchnorrNode] {

  def prove(challenge: Array[Byte]): SchnorrNode = {
    val prover = new DLogInteractiveProver(publicInput, privateInputOpt)

    val (fm, sm) = privateInputOpt.isDefined match {
      //real proving
      case true =>
        val firstMsg = prover.firstMessage
        val e = Blake2b256(firstMsg.ecData.getEncoded(true) ++ challenge)
        firstMsg -> prover.secondMessage(Challenge(e))
      //simulation
      case false => prover.simulate(Challenge(challenge))
    }

    //val sb = SchnorrSigner.serialize(fm, sm)

    SchnorrNode(publicInput, Some(fm), challenge, sm)
  }
}

object SchnorrSigner {

  import GroupSettings.dlogGroup

  def serialize(fm: FirstDLogProverMessage, sm: SecondDLogProverMessage): Array[Byte] = {
    val grec = fm.ecData
    val z = sm.z

    val grb = grec.getEncoded(true)
    val zb = z.toByteArray
    //todo: is byte enough for any group?
    Array(grb.length.toByte, zb.length.toByte) ++ grb  ++ zb
  }

  def generate(privateInput: DLogProverInput): SchnorrSigner = {
    val publicInput: ProveDlog = {
      val g = dlogGroup.generator
      val gw = dlogGroup.exponentiate(g, privateInput.w)

      ProveDlog(gw)
    }

    SchnorrSigner(publicInput, Some(privateInput))
  }
}