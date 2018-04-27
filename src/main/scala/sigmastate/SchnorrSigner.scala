package sigmastate

import scapi.sigma.{Challenge, NonInteractiveProver}
import scapi.sigma.DLogProtocol._
import scorex.crypto.hash.Blake2b256



case class SchnorrSigner(override val publicInput: ProveDlog, privateInputOpt: Option[DLogProverInput])
  extends NonInteractiveProver[DLogSigmaProtocol, DLogProverInput, ProveDlog, SchnorrNode] {

  def prove(challenge: Array[Byte]): SchnorrNode = {
    val prover = new DLogInteractiveProver(publicInput, privateInputOpt)

    val (fm, sm) = if (privateInputOpt.isDefined) {
      val firstMsg = prover.firstMessage
      val e = Blake2b256(firstMsg.ecData.getEncoded(true) ++ challenge)
      firstMsg -> prover.secondMessage(Challenge(e))
    } else {
      prover.simulate(Challenge(challenge))
    }

    //val sb = SchnorrSigner.serialize(fm, sm)

    SchnorrNode(publicInput, Some(fm), challenge, sm)
  }
}