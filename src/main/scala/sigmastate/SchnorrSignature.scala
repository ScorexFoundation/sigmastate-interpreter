package sigmastate

import scorex.core.serialization.Serializer
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import edu.biu.scapi.primitives.dlog.{DlogGroup, ECElementSendableData}
import scapi.sigma.rework.{Challenge, NonInteractiveProver}
import scapi.sigma.rework.DLogProtocol._
import scorex.crypto.hash.Blake2b256


// TODO: make implementation corresponding to RFC-8032 standard for EdDSA signatures

/*
case class SchnorrSignature(signature: Array[Byte])(implicit val dlogGroup: DlogGroup)
  extends ProofOfKnowledge[DLogSigmaProtocol, DLogCommonInput] {

  import SchnorrSignature._

  override def verify(proposition: DLogCommonInput, message: ProofOfKnowledge.Challenge): Boolean = {
    //signature is g^r as a pair of points, and z
    val (grx, gry, zb) = EcPointFunctions.decodeBigIntTriple(signature).get
    val gr = new ECElementSendableData(grx, gry)

    //h = g^w is a pubkey
    val x: DLogCommonInput = proposition

    val a: FirstDLogProverMessage = FirstDLogProverMessage(gr)

    val z: SecondDLogProverMessage = SecondDLogProverMessage(zb)

    val sigmaTranscript = DLogTranscript(x, a, Challenge(hf(message)), z)
    sigmaTranscript.accepted
  }

  override val propCode: SigmaProposition.PropositionCode = 0: Byte
  override type M = this.type

  override def serializer: Serializer[SchnorrSignature.this.type] = ???
}*/

object SchnorrSignature {
  implicit val soundness = 256

  implicit val hf = Blake2b256

  implicit val dlog: DlogGroup = new BcDlogECFp()
}

case class SchnorrSignatureSigner(privateInput: DLogProverInput)
  extends NonInteractiveProver[DLogSigmaProtocol, DLogProverInput, DLogCommonInput, SchnorrNode] {

  import SchnorrSignature._

  lazy val proposition: DLogCommonInput = {
    val g = dlog.getGenerator
    val gw = dlog.exponentiate(g, privateInput.w)

    DLogCommonInput(dlog, gw, soundness)
  }

  def sign(message: Array[Byte]): SchnorrNode = {

    val g = dlog.getGenerator
    val gw = dlog.exponentiate(g, privateInput.w)
    val commonInput = DLogCommonInput(dlog, gw, soundness)

    val prover = new DLogInteractiveProver(commonInput, privateInput)

    val fm = prover.firstMessage

    val sm = prover.secondMessage(Challenge(hf(message)))

    val grec = fm.ecData
    val z = sm.z

    val grxb = grec.getX.toByteArray
    val gryb = grec.getY.toByteArray
    val zb = z.toByteArray

    val sb = Array(grxb.length.toByte, gryb.length.toByte, zb.length.toByte) ++ grxb ++ gryb ++ zb
    SchnorrNode(commonInput, hf(message), sb)
  }

  override val publicInput: DLogCommonInput = proposition
}


