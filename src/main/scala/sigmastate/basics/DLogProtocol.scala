package sigmastate.basics

import java.math.BigInteger

import org.bouncycastle.util.BigIntegers
import sigmastate.Values._
import Value.PropositionCode
import scalan.Nullable
import sigmastate._
import sigmastate.eval._
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.interpreter.CryptoConstants.{EcPointType, dlogGroup}
import sigmastate.interpreter.CryptoConstants
import sigmastate.serialization.{OpCodes, GroupElementSerializer}
import sigmastate.serialization.OpCodes.OpCode
import special.sigma.SigmaProp

object DLogProtocol {

  trait DLogSigmaProtocol extends SigmaProtocol[DLogSigmaProtocol] {
    override type A = FirstDLogProverMessage
    override type Z = SecondDLogProverMessage
  }

  /** Construct a new SigmaBoolean value representing public key of discrete logarithm signature protocol. */
  case class ProveDlog(value: EcPointType)
    extends SigmaProofOfKnowledgeTree[DLogSigmaProtocol, DLogProverInput] {

    override val opCode: OpCode = OpCodes.ProveDlogCode
    //todo: fix, we should consider that class parameter could be not evaluated
    lazy val h: EcPointType = value
    lazy val pkBytes: Array[Byte] = GroupElementSerializer.toBytes(h)
  }

  object ProveDlog {
    val Code: PropositionCode = 102: Byte
  }
  object ProveDlogProp {
    def unapply(p: SigmaProp): Nullable[ProveDlog] = SigmaDsl.toSigmaBoolean(p) match {
      case d: ProveDlog => Nullable(d)
      case _ => Nullable.None
    }
  }
  case class DLogProverInput(w: BigInteger)
    extends SigmaProtocolPrivateInput[DLogSigmaProtocol, ProveDlog] {

    import CryptoConstants.dlogGroup

    override lazy val publicImage: ProveDlog = {
      val g = dlogGroup.generator
      ProveDlog(dlogGroup.exponentiate(g, w))
    }
  }

  object DLogProverInput {

    import CryptoConstants.dlogGroup

    /** Create random secret in a range 0..q-1, where q - an order of DLog group. */
    def random(): DLogProverInput = {
      val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)
      val w = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, dlogGroup.secureRandom)
      DLogProverInput(w)
    }
  }

  case class FirstDLogProverMessage(ecData: EcPointType) extends FirstProverMessage[DLogSigmaProtocol] {
    override def bytes: Array[Byte] = {
      GroupElementSerializer.toBytes(ecData)
    }
  }

  case class SecondDLogProverMessage(z: BigInt) extends SecondProverMessage[DLogSigmaProtocol] {
    override def bytes: Array[Byte] = z.toByteArray
  }

  class DLogInteractiveProver(override val publicInput: ProveDlog, override val privateInputOpt: Option[DLogProverInput])
    extends InteractiveProver[DLogSigmaProtocol, ProveDlog, DLogProverInput] {

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
      DLogInteractiveProver.simulate(publicInput, challenge)
    }
  }

  object DLogInteractiveProver {
    import CryptoConstants.secureRandom

    def firstMessage(publicInput: ProveDlog): (BigInteger, FirstDLogProverMessage) = {
      import CryptoConstants.dlogGroup

      val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)
      val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, secureRandom)
      val a = dlogGroup.exponentiate(dlogGroup.generator, r)
      r -> FirstDLogProverMessage(a)
    }

    def secondMessage(privateInput: DLogProverInput, rnd: BigInteger, challenge: Challenge): SecondDLogProverMessage = {
      import CryptoConstants.dlogGroup

      val q: BigInteger = dlogGroup.order
      val e: BigInteger = new BigInteger(1, challenge)
      val ew: BigInteger = e.multiply(privateInput.w).mod(q)
      val z: BigInteger = rnd.add(ew).mod(q)
      SecondDLogProverMessage(z)
    }

    def simulate(publicInput: ProveDlog, challenge: Challenge): (FirstDLogProverMessage, SecondDLogProverMessage) = {
      val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)

      //SAMPLE a random z <- Zq
      val z = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, secureRandom)

      //COMPUTE a = g^z*h^(-e)  (where -e here means -e mod q)
      val e: BigInteger = new BigInteger(1, challenge)
      val minusE = dlogGroup.order.subtract(e)
      val hToE = dlogGroup.exponentiate(publicInput.h, minusE)
      val gToZ = dlogGroup.exponentiate(dlogGroup.generator, z)
      val a = dlogGroup.multiplyGroupElements(gToZ, hToE)
      FirstDLogProverMessage(a) -> SecondDLogProverMessage(z)
    }

    /**
      * The function computes initial prover's commitment to randomness
      * ("a" message of the sigma-protocol) based on the verifier's challenge ("e")
      * and prover's response ("z")
      *
      * g^z = a*h^e => a = g^z/h^e
      *
      * @param proposition
      * @param challenge
      * @param secondMessage
      * @return
      */
    def computeCommitment(proposition: ProveDlog,
                          challenge: Challenge,
                          secondMessage: SecondDLogProverMessage): EcPointType = {
      val g = dlogGroup.generator
      val h = proposition.h

      dlogGroup.multiplyGroupElements(
        dlogGroup.exponentiate(g, secondMessage.z.underlying()),
        dlogGroup.getInverse(dlogGroup.exponentiate(h, new BigInteger(1, challenge))))
    }
  }

}
