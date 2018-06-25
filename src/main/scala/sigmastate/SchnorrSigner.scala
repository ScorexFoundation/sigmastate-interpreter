package sigmastate

import scapi.sigma.{Challenge, NonInteractiveProver}
import scapi.sigma.DLogProtocol._
import sigmastate.interpreter.CryptoFunctions


// todo: this whole file can go away

case class SchnorrSigner(override val publicInput: ProveDlog, privateInputOpt: Option[DLogProverInput])
  extends NonInteractiveProver[DLogSigmaProtocol, DLogProverInput, ProveDlog, UncheckedSchnorr] {

  def prove(challenge: Array[Byte]): UncheckedSchnorr = {
    val prover = new DLogInteractiveProver(publicInput, privateInputOpt)

    val (fm, sm) = if (privateInputOpt.isDefined) {
      // todo: this code is unused, so remove it
      val firstMsg = prover.firstMessage
      val e = CryptoFunctions.hashFn(firstMsg.ecData.getEncoded(true) ++ challenge)
      firstMsg -> prover.secondMessage(Challenge(e))
    } else {
      // this is the only used code, so it has been put directly into the two places it's used
      prover.simulate(Challenge(challenge))
    }

    UncheckedSchnorr(publicInput, Some(fm), challenge, sm)
  }
}
