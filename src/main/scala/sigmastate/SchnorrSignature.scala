package sigmastate

import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import edu.biu.scapi.primitives.dlog.DlogGroup
import scapi.sigma.{Challenge, NonInteractiveProver}
import scapi.sigma.DLogProtocol._
import scorex.crypto.hash.Blake2b256
import sigmastate.SchnorrSignature.dlog


// TODO: make implementation corresponding to RFC-8032 standard for EdDSA signatures

object SchnorrSignature {
  implicit val soundness = 256

  implicit val hf = Blake2b256

  implicit val dlog: DlogGroup = new BcDlogECFp()
}

case class SchnorrSigner(override val publicInput: ProveDlog, privateInputOpt: Option[DLogProverInput])
  extends NonInteractiveProver[DLogSigmaProtocol, DLogProverInput, ProveDlog, SchnorrNode] {

  def prove(challenge: Array[Byte]): SchnorrNode = {
    val prover = new DLogInteractiveProver(publicInput, privateInputOpt)

    val (fm, sm) = privateInputOpt.isDefined match {
      //real proving
      case true =>
        val firstMsg = prover.firstMessage
        val e = Blake2b256(firstMsg.ecData.getX.toByteArray ++ firstMsg.ecData.getY.toByteArray ++ challenge)
        firstMsg -> prover.secondMessage(Challenge(e))
      //simulation
      case false => prover.simulate(Challenge(challenge))
    }

    //val sb = SchnorrSigner.serialize(fm, sm)

    SchnorrNode(publicInput, Some(fm), challenge, sm)
  }
}

object SchnorrSigner {

  def serialize(fm: FirstDLogProverMessage, sm: SecondDLogProverMessage): Array[Byte] = {
    val grec = fm.ecData
    val z = sm.z

    val grxb = grec.getX.toByteArray
    val gryb = grec.getY.toByteArray
    val zb = z.toByteArray
    Array(grxb.length.toByte, gryb.length.toByte, zb.length.toByte) ++ grxb ++ gryb ++ zb
  }

  def generate(privateInput: DLogProverInput): SchnorrSigner = {
    val publicInput: ProveDlog = {
      val g = dlog.getGenerator
      val gw = dlog.exponentiate(g, privateInput.w)

      ProveDlog(gw)
    }

    SchnorrSigner(publicInput, Some(privateInput))
  }
}