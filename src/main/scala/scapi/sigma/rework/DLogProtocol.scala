package scapi.sigma.rework

import java.math.BigInteger
import java.security.SecureRandom

import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import edu.biu.scapi.primitives.dlog.{DlogGroup, ECElementSendableData, GroupElement}
import org.bouncycastle.util.BigIntegers
import scorex.core.serialization.Serializer
import scorex.core.transaction.state.SecretCompanion
import sigmastate.{EcPointFunctions, SigmaProofOfKnowledgeTree}
import sigmastate.SigmaProposition.PropositionCode

import scala.concurrent.Future
import scala.util.Try


package object DLogProtocol {

  trait DLogSigmaProtocol extends SigmaProtocol[DLogSigmaProtocol] {
    override type A = FirstDLogProverMessage
    override type Z = SecondDLogProverMessage
  }

  case class DLogNode(h: GroupElement)
    extends SigmaProtocolCommonInput[DLogSigmaProtocol]
      with SigmaProofOfKnowledgeTree[DLogSigmaProtocol, DLogProverInput] {

    override type M = this.type
    override val code: PropositionCode = DLogNode.Code

    override def serializer: Serializer[DLogNode.this.type] = ???

    override lazy val dlogGroup: DlogGroup = DLogNode.dlogGroup
    override val soundness: Int = 256

    //lazy val toCommonInput: DLogCommonInput = DLogCommonInput(dlogGroup, h, soundness)

    override lazy val bytes = {
      val gw = h.generateSendableData().asInstanceOf[ECElementSendableData]
      val gwx = gw.getX.toByteArray
      val gwy = gw.getY.toByteArray
      gwx ++ gwy
    }
  }

  object DLogNode {
    val Code: PropositionCode = 102: Byte

    lazy val dlogGroup: DlogGroup = new BcDlogECFp()

    def fromBytes(bytes: Array[Byte]): DLogNode = {
      val (x, y) = EcPointFunctions.decodeBigIntPair(bytes).get
      val xy = new ECElementSendableData(x, y)
      val h = dlogGroup.reconstructElement(true, xy)
      DLogNode(h)
    }
  }


  case class DLogProverInput(w: BigInteger)(implicit val dlogGroup: DlogGroup, soundness: Int)
    extends SigmaProtocolPrivateInput[DLogSigmaProtocol] {

    override type S = DLogProverInput
    override type PK = DLogNode

    override def companion: SecretCompanion[DLogProverInput] = ???

    override lazy val publicImage: DLogNode = {
      val g = dlogGroup.getGenerator
      DLogNode(dlogGroup.exponentiate(g, w))
    }

    override type M = DLogProverInput

    override def serializer: Serializer[DLogProverInput] = ???
  }

  object DLogProverInput {
    def random()(implicit dlog:DlogGroup, soundness: Int): (DLogProverInput, DLogNode) = {
      val g = dlog.getGenerator
      val qMinusOne = dlog.getOrder.subtract(BigInteger.ONE)
      val w = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)
      val h = dlog.exponentiate(g, w)

      DLogProverInput(w) -> DLogNode(h)
    }
  }

  case class FirstDLogProverMessage(ecData: ECElementSendableData) extends FirstProverMessage[DLogSigmaProtocol] {
    override def bytes: Array[Byte] = {
      val x = ecData.getX.toByteArray
      val y = ecData.getY.toByteArray

      Array(x.size.toByte, y.size.toByte) ++ x ++ y
    }
  }

  case class SecondDLogProverMessage(z: BigInt) extends SecondProverMessage[DLogSigmaProtocol] {
    override def bytes: Array[Byte] = z.toByteArray
  }

  class DLogInteractiveProver(override val publicInput: DLogNode, override val privateInput: DLogProverInput)
    extends InteractiveProver[DLogSigmaProtocol, DLogNode, DLogProverInput] {

    lazy val group = publicInput.dlogGroup

    var rOpt: Option[BigInteger] = None

    override def firstMessage: FirstDLogProverMessage = {
      val qMinusOne = group.getOrder.subtract(BigInteger.ONE)
      val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)
      rOpt = Some(r)
      val a = group.exponentiate(group.getGenerator, r)
      FirstDLogProverMessage(a.generateSendableData().asInstanceOf[ECElementSendableData])
    }

    override def secondMessage(challenge: Challenge): SecondDLogProverMessage = {
      val q: BigInteger = group.getOrder
      val e: BigInteger = new BigInteger(1, challenge.bytes)
      val ew: BigInteger = e.multiply(privateInput.w).mod(q)
      val z: BigInteger = rOpt.get.add(ew).mod(q)
      rOpt = None
      SecondDLogProverMessage(z)
    }
  }

  case class DLogActorProver(override val publicInput: DLogNode, override val privateInput: DLogProverInput)
    extends DLogInteractiveProver(publicInput, privateInput) with ActorProver[DLogSigmaProtocol, DLogNode, DLogProverInput]

  case class DLogTranscript(override val x: DLogNode,
                            override val a: FirstDLogProverMessage,
                            override val e: Challenge,
                            override val z: SecondDLogProverMessage)
    extends SigmaProtocolTranscript[DLogSigmaProtocol, DLogNode] {


    override lazy val accepted: Boolean = Try {
      assert(x.dlogGroup.isMember(x.h))
      val aElem = x.dlogGroup.reconstructElement(true, a.ecData)
      val left = x.dlogGroup.exponentiate(x.dlogGroup.getGenerator, z.z.bigInteger)
      val hToe = x.dlogGroup.exponentiate(x.h, BigInt(1, e.bytes).bigInteger)
      val right = x.dlogGroup.multiplyGroupElements(aElem, hToe)

      left == right
    }.getOrElse(false)
  }

  abstract class DLogVerifier[DP <: DLogInteractiveProver](override val publicInput: DLogNode, override val prover: DP)
    extends Verifier[DLogSigmaProtocol, DLogNode] {

    override type P = DP
    override type ST = DLogTranscript
  }

  case class DLogActorVerifier(override val publicInput: DLogNode, override val prover: DLogActorProver)
    extends DLogVerifier[DLogActorProver](publicInput, prover) {

    override def transcript: Future[Option[DLogTranscript]] = ???
  }
}
