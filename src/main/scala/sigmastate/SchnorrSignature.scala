package sigmastate

import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import edu.biu.scapi.primitives.dlog.DlogGroup
import scapi.sigma.rework.{Challenge, NonInteractiveProver}
import scapi.sigma.rework.DLogProtocol._
import scorex.crypto.hash.Blake2b256


// TODO: make implementation corresponding to RFC-8032 standard for EdDSA signatures


object SchnorrSignature {
  implicit val soundness = 256

  implicit val hf = Blake2b256

  implicit val dlog: DlogGroup = new BcDlogECFp()
}

case class SchnorrSignatureSigner(privateInput: DLogProverInput)
  extends NonInteractiveProver[DLogSigmaProtocol, DLogProverInput, DLogNode, SchnorrNode] {

  import SchnorrSignature._

  lazy val proposition: DLogNode = {
    val g = dlog.getGenerator
    val gw = dlog.exponentiate(g, privateInput.w)

    DLogNode(gw)
  }

  def prove(challenge: Array[Byte]): SchnorrNode = {
    val g = dlog.getGenerator
    val gw = dlog.exponentiate(g, privateInput.w)
    val commonInput = DLogNode(gw)

    val prover = new DLogInteractiveProver(commonInput, privateInput)

    val fm = prover.firstMessage

    val sm = prover.secondMessage(Challenge(challenge))

    val grec = fm.ecData
    val z = sm.z

    val grxb = grec.getX.toByteArray
    val gryb = grec.getY.toByteArray
    val zb = z.toByteArray

    val sb = Array(grxb.length.toByte, gryb.length.toByte, zb.length.toByte) ++ grxb ++ gryb ++ zb
    SchnorrNode(commonInput, challenge, sb)
  }

  override val publicInput: DLogNode = proposition
}