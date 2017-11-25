package scapi.sigma

import java.math.BigInteger
import java.security.SecureRandom

import edu.biu.scapi.primitives.dlog.{DlogGroup, ECElementSendableData, GroupElement}
import org.bouncycastle.util.BigIntegers
import scapi.sigma.DLogProtocol._
import scapi.sigma.rework._
import scorex.core.serialization.Serializer
import scorex.core.transaction.state.SecretCompanion
import sigmastate.SigmaProofOfKnowledgeTree
import sigmastate.SigmaProposition.PropositionCode
import sigmastate.utxo.CostTable.Cost

trait DiffieHellmanTupleProtocol extends SigmaProtocol[DiffieHellmanTupleProtocol] {
  override type A = FirstDiffieHellmanTupleProverMessage
  override type Z = SecondDiffieHellmanTupleProverMessage
}

case class DiffieHellmanTupleProverInput(w: BigInteger, commonInput: DiffieHellmanTupleNode)
                                        (implicit val dlogGroup: DlogGroup, soundness: Int)
  extends SigmaProtocolPrivateInput[DiffieHellmanTupleProtocol] {

  override type S = DiffieHellmanTupleProverInput
  override type PK = DiffieHellmanTupleNode

  override def companion: SecretCompanion[DiffieHellmanTupleProverInput] = ???

  override lazy val publicImage: DiffieHellmanTupleNode = commonInput

  override type M = DiffieHellmanTupleProverInput

  override def serializer: Serializer[DiffieHellmanTupleProverInput] = ???
}

object DiffieHellmanTupleProverInput {
  def random()(implicit dlog: DlogGroup, soundness: Int): DiffieHellmanTupleProverInput = {
    val g = dlog.getGenerator
    val h = dlog.createRandomGenerator()
    val qMinusOne = dlog.getOrder.subtract(BigInteger.ONE)
    val w = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)
    val u = dlog.exponentiate(g, w)
    val v = dlog.exponentiate(h, w)
    val ci = DiffieHellmanTupleNode(g, h, u, v)
    DiffieHellmanTupleProverInput(w, ci)
  }
}

//a = g^r, b = h^r
case class FirstDiffieHellmanTupleProverMessage(a: ECElementSendableData, b: ECElementSendableData)
  extends FirstProverMessage[DiffieHellmanTupleProtocol] {
  override def bytes: Array[Byte] = {
    val xa = a.getX.toByteArray
    val ya = a.getY.toByteArray

    val xb = b.getX.toByteArray
    val yb = b.getY.toByteArray

    Array(xa.size.toByte, ya.size.toByte, xb.size.toByte, yb.size.toByte) ++ xa ++ ya ++ xb ++ yb
  }
}

//z = r + ew mod q
case class SecondDiffieHellmanTupleProverMessage(z: BigInteger)
  extends SecondProverMessage[DiffieHellmanTupleProtocol] {
  override def bytes: Array[PropositionCode] = ???
}

// Common input: (g,h,u,v)
case class DiffieHellmanTupleNode(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement)
  extends SigmaProtocolCommonInput[DiffieHellmanTupleProtocol]
    with SigmaProofOfKnowledgeTree[DiffieHellmanTupleProtocol, DiffieHellmanTupleProverInput] {

  override val cost: Int = Cost.Dlog * 2

  override type M = DiffieHellmanTupleNode
  override val code: PropositionCode = DiffieHellmanTupleNode.Code

  override def serializer: Serializer[DiffieHellmanTupleNode] = ???

  override lazy val dlogGroup: DlogGroup = ProveDlog.dlogGroup
  override val soundness: Int = 256
}

object DiffieHellmanTupleNode {
  val Code: PropositionCode = 103: Byte
}


class DiffieHellmanTupleInteractiveProver(override val publicInput: DiffieHellmanTupleNode,
                                          override val privateInputOpt: Option[DiffieHellmanTupleProverInput])
  extends InteractiveProver[DiffieHellmanTupleProtocol, DiffieHellmanTupleNode, DiffieHellmanTupleProverInput] {

  lazy val group: DlogGroup = publicInput.dlogGroup

  var rOpt: Option[BigInteger] = None

  override def firstMessage: FirstDiffieHellmanTupleProverMessage = {
    assert(privateInputOpt.isDefined, "Secret is not known")
    assert(rOpt.isEmpty, "Already generated r")

    val (r, fm) = DiffieHellmanTupleInteractiveProver.firstMessage(publicInput)
    rOpt = Some(r)
    fm
  }

  override def secondMessage(challenge: Challenge): SecondDiffieHellmanTupleProverMessage = {
    assert(privateInputOpt.isDefined, "Secret is not known")
    assert(rOpt.isDefined)

    val rnd = rOpt.get

    val privateInput = privateInputOpt.get

    val sm = DiffieHellmanTupleInteractiveProver.secondMessage(privateInput, rnd, challenge)
    rOpt = None
    sm
  }

  override def simulate(challenge: Challenge):
  (FirstDiffieHellmanTupleProverMessage, SecondDiffieHellmanTupleProverMessage) = {
    assert(privateInputOpt.isEmpty, "Secret is known, simulation is probably wrong action")
    val qMinusOne = group.getOrder.subtract(BigInteger.ONE)

    //SAMPLE a random z <- Zq
    val z = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)

    //COMPUTE a = g^z*h^(-e)  (where -e here means -e mod q)
    val e: BigInteger = new BigInteger(1, challenge.bytes)
    val minusE = group.getOrder.subtract(e)
    val hToZ = group.exponentiate(publicInput.h, z)
    val gToZ = group.exponentiate(publicInput.g, z)
    val uToMinusE = group.exponentiate(publicInput.u, minusE)
    val vToMinusE = group.exponentiate(publicInput.v, minusE)
    val a = group.multiplyGroupElements(gToZ, uToMinusE)
    val b = group.multiplyGroupElements(hToZ, vToMinusE)
    FirstDiffieHellmanTupleProverMessage(a.generateSendableData().asInstanceOf[ECElementSendableData],
      b.generateSendableData().asInstanceOf[ECElementSendableData]) -> SecondDiffieHellmanTupleProverMessage(z)
  }
}

object DiffieHellmanTupleInteractiveProver {
  def firstMessage(publicInput: DiffieHellmanTupleNode): (BigInteger, FirstDiffieHellmanTupleProverMessage) = {
    val group = publicInput.dlogGroup
    val qMinusOne = group.getOrder.subtract(BigInteger.ONE)
    val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)
    val a = group.exponentiate(publicInput.g, r)
    val b = group.exponentiate(publicInput.h, r)
    r -> FirstDiffieHellmanTupleProverMessage(
      a.generateSendableData().asInstanceOf[ECElementSendableData],
      b.generateSendableData().asInstanceOf[ECElementSendableData])
  }

  def secondMessage(privateInput: DiffieHellmanTupleProverInput,
                    rnd: BigInteger,
                    challenge: Challenge): SecondDiffieHellmanTupleProverMessage = {
    val q: BigInteger = privateInput.dlogGroup.getOrder
    val e: BigInteger = new BigInteger(1, challenge.bytes)
    val ew: BigInteger = e.multiply(privateInput.w).mod(q)
    val z: BigInteger = rnd.add(ew).mod(q)
    SecondDiffieHellmanTupleProverMessage(z)
  }
}