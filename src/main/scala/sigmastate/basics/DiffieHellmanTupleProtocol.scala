package sigmastate.basics

import java.math.BigInteger

import org.bouncycastle.util.BigIntegers
import sigmastate.Values.Value.PropositionCode
import sigmastate._
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.eval.SigmaDsl
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.CryptoConstants
import sigmastate.serialization.{OpCodes, GroupElementSerializer}
import sigmastate.serialization.OpCodes.OpCode
import special.sigma.SigmaProp


trait DiffieHellmanTupleProtocol extends SigmaProtocol[DiffieHellmanTupleProtocol] {
  override type A = FirstDiffieHellmanTupleProverMessage
  override type Z = SecondDiffieHellmanTupleProverMessage
}

case class DiffieHellmanTupleProverInput(w: BigInteger, commonInput: ProveDHTuple)
  extends SigmaProtocolPrivateInput[DiffieHellmanTupleProtocol, ProveDHTuple] {

  override lazy val publicImage: ProveDHTuple = commonInput
}

object DiffieHellmanTupleProverInput {
  import sigmastate.interpreter.CryptoConstants.dlogGroup

  def random(): DiffieHellmanTupleProverInput = {
    val g = dlogGroup.generator
    val h = dlogGroup.createRandomGenerator()

    val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)
    val w = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, dlogGroup.secureRandom)
    val u = dlogGroup.exponentiate(g, w)
    val v = dlogGroup.exponentiate(h, w)
    val ci = ProveDHTuple(g, h, u, v)
    DiffieHellmanTupleProverInput(w, ci)
  }
}

//a = g^r, b = h^r
case class FirstDiffieHellmanTupleProverMessage(a: CryptoConstants.EcPointType, b: CryptoConstants.EcPointType)
  extends FirstProverMessage[DiffieHellmanTupleProtocol] {
  override def bytes: Array[Byte] = {
    GroupElementSerializer.toBytes(a) ++ GroupElementSerializer.toBytes(b)
  }
}

//z = r + ew mod q
case class SecondDiffieHellmanTupleProverMessage(z: BigInteger)
  extends SecondProverMessage[DiffieHellmanTupleProtocol] {
  override def bytes: Array[PropositionCode] = ???
}

/** Construct a new SigmaProp value representing public key of Diffie Hellman signature protocol.
  * Common input: (g,h,u,v)*/
case class ProveDHTuple(gv: EcPointType, hv: EcPointType, uv: EcPointType, vv: EcPointType)
  extends SigmaProtocolCommonInput[DiffieHellmanTupleProtocol]
    with SigmaProofOfKnowledgeTree[DiffieHellmanTupleProtocol, DiffieHellmanTupleProverInput] {
  override val opCode: OpCode = OpCodes.ProveDiffieHellmanTupleCode
  lazy val g = gv
  lazy val h = hv
  lazy val u = uv
  lazy val v = vv
}

object ProveDHTuple {
  val Code: PropositionCode = 103: Byte
}

/** Helper extractor to match SigmaProp values and extract ProveDHTuple out of it. */
object ProveDHTupleProp {
  def unapply(p: SigmaProp): Option[ProveDHTuple] = SigmaDsl.toSigmaBoolean(p) match {
    case d: ProveDHTuple => Some(d)
    case _ => None
  }
}

class DiffieHellmanTupleInteractiveProver(override val publicInput: ProveDHTuple,
                                          override val privateInputOpt: Option[DiffieHellmanTupleProverInput])
  extends InteractiveProver[DiffieHellmanTupleProtocol, ProveDHTuple, DiffieHellmanTupleProverInput] {

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
    DiffieHellmanTupleInteractiveProver.simulate(publicInput, challenge)
  }
}

object DiffieHellmanTupleInteractiveProver {
  import sigmastate.interpreter.CryptoConstants.dlogGroup

  def firstMessage(publicInput: ProveDHTuple): (BigInteger, FirstDiffieHellmanTupleProverMessage) = {
    val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)
    val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, dlogGroup.secureRandom)
    val a = dlogGroup.exponentiate(publicInput.g, r)
    val b = dlogGroup.exponentiate(publicInput.h, r)
    r -> FirstDiffieHellmanTupleProverMessage(a, b)
  }

  def secondMessage(privateInput: DiffieHellmanTupleProverInput,
                    rnd: BigInteger,
                    challenge: Challenge): SecondDiffieHellmanTupleProverMessage = {
    val q: BigInteger = dlogGroup.order
    val e: BigInteger = new BigInteger(1, challenge)
    val ew: BigInteger = e.multiply(privateInput.w).mod(q)
    val z: BigInteger = rnd.add(ew).mod(q)
    SecondDiffieHellmanTupleProverMessage(z)
  }

  def simulate(publicInput: ProveDHTuple, challenge: Challenge):
    (FirstDiffieHellmanTupleProverMessage, SecondDiffieHellmanTupleProverMessage) = {

    val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)

    //SAMPLE a random z <- Zq
    val z = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, dlogGroup.secureRandom)

    // COMPUTE a = g^z*u^(-e) and b = h^z*v^{-e}  (where -e here means -e mod q)
    val e: BigInteger = new BigInteger(1, challenge)
    val minusE = dlogGroup.order.subtract(e)
    val hToZ = dlogGroup.exponentiate(publicInput.h, z)
    val gToZ = dlogGroup.exponentiate(publicInput.g, z)
    val uToMinusE = dlogGroup.exponentiate(publicInput.u, minusE)
    val vToMinusE = dlogGroup.exponentiate(publicInput.v, minusE)
    val a = dlogGroup.multiplyGroupElements(gToZ, uToMinusE)
    val b = dlogGroup.multiplyGroupElements(hToZ, vToMinusE)
    FirstDiffieHellmanTupleProverMessage(a, b) -> SecondDiffieHellmanTupleProverMessage(z)
  }

  /**
    * The function computes initial prover's commitment to randomness
    * ("a" message of the sigma-protocol, which in this case has two parts "a" and "b")
    * based on the verifier's challenge ("e")
    * and prover's response ("z")
    *
    * g^z = a*u^e, h^z = b*v^e  => a = g^z/u^e, b = h^z/v^e
    *
    * @param proposition
    * @param challenge
    * @param secondMessage
    * @return
    */
  def computeCommitment(proposition: ProveDHTuple,
                        challenge: Challenge,
                        secondMessage: SecondDiffieHellmanTupleProverMessage): (EcPointType, EcPointType) = {

    val g = proposition.g
    val h = proposition.h
    val u = proposition.u
    val v = proposition.v

    val z = secondMessage.z

    val e = new BigInteger(1, challenge)

    val gToZ = dlogGroup.exponentiate(g, z)
    val hToZ = dlogGroup.exponentiate(h, z)

    val uToE = dlogGroup.exponentiate(u, e)
    val vToE = dlogGroup.exponentiate(v, e)

    val a = dlogGroup.multiplyGroupElements(gToZ, dlogGroup.getInverse(uToE))
    val b = dlogGroup.multiplyGroupElements(hToZ, dlogGroup.getInverse(vToE))
    a -> b
  }
}