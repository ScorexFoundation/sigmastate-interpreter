package scapi.sigma

import java.math.BigInteger
import java.security.SecureRandom

import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import edu.biu.scapi.primitives.dlog.{DlogGroup, ECElementSendableData, GroupElement}
import org.bouncycastle.util.BigIntegers
import scapi.sigma.rework._
import scorex.core.serialization.Serializer
import scorex.core.transaction.state.SecretCompanion
import sigmastate.SigmaProposition.PropositionCode
import sigmastate.utxo.CostTable.Cost
import sigmastate.{EcPointFunctions, NotReadyValueBoolean, SigmaProofOfKnowledgeTree}

import scala.concurrent.Future
import scala.util.Try

object DLogProtocol {


  trait DLogSigmaProtocol extends SigmaProtocol[DLogSigmaProtocol]{
    override type A = FirstDLogProverMessage
    override type Z = SecondDLogProverMessage
  }

  case class DLogNode(h: GroupElement)
    extends SigmaProtocolCommonInput[DLogSigmaProtocol]
      with SigmaProofOfKnowledgeTree[DLogSigmaProtocol, DLogProverInput] {

    override val cost: Int = Cost.Dlog

    override type M = this.type
    override val code: PropositionCode = DLogNode.Code

    override lazy val dlogGroup: DlogGroup = DLogNode.dlogGroup
    override val soundness: Int = 256

    override lazy val bytes: Array[Byte] = {
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
    def random()(implicit dlog: DlogGroup, soundness: Int): (DLogProverInput, DLogNode) = {
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

  object FirstDLogProverMessage {
    def apply(a: GroupElement): FirstDLogProverMessage =
      FirstDLogProverMessage(a.generateSendableData().asInstanceOf[ECElementSendableData])
  }

  case class SecondDLogProverMessage(z: BigInt) extends SecondProverMessage[DLogSigmaProtocol] {
    override def bytes: Array[Byte] = z.toByteArray
  }

  class DLogInteractiveProver(override val publicInput: DLogNode, override val privateInputOpt: Option[DLogProverInput])
    extends InteractiveProver[DLogSigmaProtocol, DLogNode, DLogProverInput] {

    lazy val group: DlogGroup = publicInput.dlogGroup

    var rOpt: Option[BigInteger] = None

    override def firstMessage: FirstDLogProverMessage = {
      assert(privateInputOpt.isDefined, "Secret is not known")
      assert(rOpt.isEmpty, "Already generated r")

      val (r, fm) = DLogInteractiveProver.firstMessage(publicInput)
      rOpt = Some(r)
      fm
    }

    override def secondMessage(challenge: Challenge): SecondDLogProverMessage = {
      assert(privateInputOpt.isDefined, "Secret is not known")
      assert(rOpt.isDefined)

      val rnd = rOpt.get

      val privateInput = privateInputOpt.get

      val sm = DLogInteractiveProver.secondMessage(privateInput, rnd, challenge)
      rOpt = None
      sm
    }

    override def simulate(challenge: Challenge): (FirstDLogProverMessage, SecondDLogProverMessage) = {
      assert(privateInputOpt.isEmpty, "Secret is known, simulation is probably wrong action")
      val qMinusOne = group.getOrder.subtract(BigInteger.ONE)

      //SAMPLE a random z <- Zq
      val z = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)

      //COMPUTE a = g^z*h^(-e)  (where -e here means -e mod q)
      val e: BigInteger = new BigInteger(1, challenge.bytes)
      val minusE = group.getOrder.subtract(e)
      val hToE = group.exponentiate(publicInput.h, minusE)
      val gToZ = group.exponentiate(group.getGenerator, z)
      val a = group.multiplyGroupElements(gToZ, hToE)
      FirstDLogProverMessage(a.generateSendableData().asInstanceOf[ECElementSendableData]) -> SecondDLogProverMessage(z)
    }
  }

  object DLogInteractiveProver {
    def firstMessage(publicInput: DLogNode): (BigInteger, FirstDLogProverMessage) = {
      val group = publicInput.dlogGroup
      val qMinusOne = group.getOrder.subtract(BigInteger.ONE)
      val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)
      val a = group.exponentiate(group.getGenerator, r)
      r -> FirstDLogProverMessage(a.generateSendableData().asInstanceOf[ECElementSendableData])
    }

    def secondMessage(privateInput: DLogProverInput, rnd: BigInteger, challenge: Challenge): SecondDLogProverMessage = {
      val q: BigInteger = privateInput.dlogGroup.getOrder
      val e: BigInteger = new BigInteger(1, challenge.bytes)
      val ew: BigInteger = e.multiply(privateInput.w).mod(q)
      val z: BigInteger = rnd.add(ew).mod(q)
      SecondDLogProverMessage(z)
    }
  }

  case class DLogActorProver(override val publicInput: DLogNode, override val privateInputOpt: Option[DLogProverInput])
    extends DLogInteractiveProver(publicInput, privateInputOpt) with ActorProver[DLogSigmaProtocol, DLogNode, DLogProverInput]

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