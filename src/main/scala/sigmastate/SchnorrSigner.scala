package sigmastate

import scapi.sigma.{Challenge, NonInteractiveProver}
import scapi.sigma.DLogProtocol._
import sigmastate.interpreter.CryptoFunctions


case class SchnorrSigner(override val publicInput: ProveDlog, privateInputOpt: Option[DLogProverInput])
  extends NonInteractiveProver[DLogSigmaProtocol, DLogProverInput, ProveDlog, UncheckedSchnorr] {

  def prove(challenge: Array[Byte]): UncheckedSchnorr = {
    val prover = new DLogInteractiveProver(publicInput, privateInputOpt)

    val (fm, sm) = if (privateInputOpt.isDefined) {
      val firstMsg = prover.firstMessage
      val e = CryptoFunctions.hashFn(firstMsg.ecData.getEncoded(true) ++ challenge)
      firstMsg -> prover.secondMessage(Challenge(e))
    } else {
      prover.simulate(Challenge(challenge))
    }

    UncheckedSchnorr(publicInput, Some(fm), challenge, sm)
  }
}
