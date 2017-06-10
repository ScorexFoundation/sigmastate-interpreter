package sigmastate

import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import edu.biu.scapi.primitives.dlog.DlogGroup
import scapi.sigma.rework.{Challenge, NonInteractiveProver}
import scapi.sigma.rework.DLogProtocol._
import scorex.crypto.hash.Blake2b256
import sigmastate.SchnorrSignature.dlog


// TODO: make implementation corresponding to RFC-8032 standard for EdDSA signatures

object SchnorrSignature {
  implicit val soundness = 256

  implicit val hf = Blake2b256

  implicit val dlog: DlogGroup = new BcDlogECFp()
}

case class SchnorrSignatureSigner(override val publicInput: DLogNode, privateInputOpt: Option[DLogProverInput])
  extends NonInteractiveProver[DLogSigmaProtocol, DLogProverInput, DLogNode, SchnorrNode] {

  def prove(message: Array[Byte]): SchnorrNode = {
    val prover = new DLogInteractiveProver(publicInput, privateInputOpt)

    val (fm, sm) = privateInputOpt.isDefined match {
      //real proving
      case true =>
        val firstMsg = prover.firstMessage
        val e = Blake2b256(firstMsg.ecData.getX.toByteArray ++ firstMsg.ecData.getY.toByteArray ++ message)
        firstMsg -> prover.secondMessage(Challenge(e))
      //simulation
      case false => prover.simulate(Challenge(message))
    }

    val grec = fm.ecData
    val z = sm.z

    val grxb = grec.getX.toByteArray
    val gryb = grec.getY.toByteArray
    val zb = z.toByteArray
    val sb = Array(grxb.length.toByte, gryb.length.toByte, zb.length.toByte) ++ grxb ++ gryb ++ zb
    SchnorrNode(publicInput, message, sb)
  }
}

object SchnorrSignatureSigner {
  def generate(privateInput: DLogProverInput): SchnorrSignatureSigner = {

    val publicInput: DLogNode = {
      val g = dlog.getGenerator
      val gw = dlog.exponentiate(g, privateInput.w)

      DLogNode(gw)
    }

    SchnorrSignatureSigner(publicInput, Some(privateInput))
  }
}