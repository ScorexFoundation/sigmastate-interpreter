package scapi.sigma

import java.math.BigInteger
import java.security.SecureRandom

import org.bouncycastle.util.BigIntegers
import sigmastate.Values._
import Value.PropositionCode
import sigmastate.utxo.CostTable.Cost
import sigmastate._
import sigmastate.interpreter.GroupSettings
import sigmastate.interpreter.GroupSettings.EcPointType

import scala.concurrent.Future
import scala.util.Try



object DLogProtocol {

  trait DLogSigmaProtocol extends SigmaProtocol[DLogSigmaProtocol]{
    override type A = FirstDLogProverMessage
    override type Z = SecondDLogProverMessage
  }

  case class ProveDlog(value: Value[SGroupElement.type])
    extends SigmaProofOfKnowledgeTree[DLogSigmaProtocol, DLogProverInput] {

    import GroupSettings.dlogGroup

    override val cost: Int = Cost.Dlog
    override val soundness: Int = 256

//    override def fields = ProveDlog.fields

    //todo: fix, we should consider that class parameter could be not evaluated
    lazy val h: EcPointType = value.asInstanceOf[GroupElementConstant].value

    lazy val bytes: Array[Byte] = dlogGroup.mapAnyGroupElementToByteArray(h)
  }

  object ProveDlog {
    import GroupSettings.dlogGroup

    def apply(h: GroupSettings.EcPointType): ProveDlog = ProveDlog(GroupElementConstant(h))

    val Code: PropositionCode = 102: Byte

    def fromBytes(bytes: Array[Byte]): ProveDlog = {
      val (x, y) = EcPointFunctions.decodeBigIntPair(bytes).get
      val xy = GroupAgnosticEcElement(x, y)
      val h = dlogGroup.reconstructElement(bCheckMembership = true, xy)
      ProveDlog(h)
    }
//    val fields = Seq(
//      "propBytes" -> SByteArray,
//    )
  }


  case class DLogProverInput(w: BigInteger)(implicit soundness: Int)
    extends SigmaProtocolPrivateInput[DLogSigmaProtocol, ProveDlog] {

    import GroupSettings.dlogGroup

    override lazy val publicImage: ProveDlog = {
      val g = dlogGroup.generator
      ProveDlog(dlogGroup.exponentiate(g, w))
    }
  }

  object DLogProverInput {

    import GroupSettings.dlogGroup

    def random()(implicit soundness: Int): DLogProverInput = {
      val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)
      val w = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)
      DLogProverInput(w)
    }
  }

  case class FirstDLogProverMessage(ecData: GroupSettings.EcPointType) extends FirstProverMessage[DLogSigmaProtocol] {
    override def bytes: Array[Byte] = {
      val bytes = ecData.getEncoded(true)

      //todo: is byte enough for any group?
      Array(bytes.length.toByte) ++ bytes
    }
  }

  case class SecondDLogProverMessage(z: BigInt) extends SecondProverMessage[DLogSigmaProtocol] {
    override def bytes: Array[Byte] = z.toByteArray
  }

  class DLogInteractiveProver(override val publicInput: ProveDlog, override val privateInputOpt: Option[DLogProverInput])
    extends InteractiveProver[DLogSigmaProtocol, ProveDlog, DLogProverInput] {

    import GroupSettings.dlogGroup

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
      val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)

      //SAMPLE a random z <- Zq
      val z = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)

      //COMPUTE a = g^z*h^(-e)  (where -e here means -e mod q)
      val e: BigInteger = new BigInteger(1, challenge.bytes)
      val minusE = dlogGroup.order.subtract(e)
      val hToE = dlogGroup.exponentiate(publicInput.h, minusE)
      val gToZ = dlogGroup.exponentiate(dlogGroup.generator, z)
      val a = dlogGroup.multiplyGroupElements(gToZ, hToE)
      FirstDLogProverMessage(a) -> SecondDLogProverMessage(z)
    }
  }

  object DLogInteractiveProver {
    def firstMessage(publicInput: ProveDlog): (BigInteger, FirstDLogProverMessage) = {
      import GroupSettings.dlogGroup

      val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)
      val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)
      val a = dlogGroup.exponentiate(dlogGroup.generator, r)
      r -> FirstDLogProverMessage(a)
    }

    def secondMessage(privateInput: DLogProverInput, rnd: BigInteger, challenge: Challenge): SecondDLogProverMessage = {
      import GroupSettings.dlogGroup

      val q: BigInteger = dlogGroup.order
      val e: BigInteger = new BigInteger(1, challenge.bytes)
      val ew: BigInteger = e.multiply(privateInput.w).mod(q)
      val z: BigInteger = rnd.add(ew).mod(q)
      SecondDLogProverMessage(z)
    }
  }

  case class DLogActorProver(override val publicInput: ProveDlog, override val privateInputOpt: Option[DLogProverInput])
    extends DLogInteractiveProver(publicInput, privateInputOpt) with ActorProver[DLogSigmaProtocol, ProveDlog, DLogProverInput]

  case class DLogTranscript(override val x: ProveDlog,
                            override val a: FirstDLogProverMessage,
                            override val e: Challenge,
                            override val z: SecondDLogProverMessage)
    extends SigmaProtocolTranscript[DLogSigmaProtocol, ProveDlog] {

    import GroupSettings.dlogGroup


    override lazy val accepted: Boolean = Try {
      assert(dlogGroup.isMember(x.h))
      val left = dlogGroup.exponentiate(dlogGroup.generator, z.z.bigInteger)
      val hToe = dlogGroup.exponentiate(x.h, BigInt(1, e.bytes).bigInteger)
      val right = dlogGroup.multiplyGroupElements(a.ecData, hToe)

      left == right
    }.getOrElse(false)
  }

  abstract class DLogVerifier[DP <: DLogInteractiveProver](override val publicInput: ProveDlog, override val prover: DP)
    extends Verifier[DLogSigmaProtocol, ProveDlog] {

    override type P = DP
    override type ST = DLogTranscript
  }

  case class DLogActorVerifier(override val publicInput: ProveDlog, override val prover: DLogActorProver)
    extends DLogVerifier[DLogActorProver](publicInput, prover) {

    override def transcript: Future[Option[DLogTranscript]] = ???
  }

}