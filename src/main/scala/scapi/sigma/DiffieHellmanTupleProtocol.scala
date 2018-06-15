package scapi.sigma

import java.math.BigInteger
import java.security.SecureRandom

import org.bouncycastle.util.BigIntegers
import sigmastate._
import sigmastate.interpreter.{Context, CryptoConstants}
import sigmastate.Values._
import Value.PropositionCode
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utxo.CostTable.Cost

trait DiffieHellmanTupleProtocol extends SigmaProtocol[DiffieHellmanTupleProtocol] {
  override type A = FirstDiffieHellmanTupleProverMessage
  override type Z = SecondDiffieHellmanTupleProverMessage
}

case class DiffieHellmanTupleProverInput(w: BigInteger, commonInput: ProveDiffieHellmanTuple)
  extends SigmaProtocolPrivateInput[DiffieHellmanTupleProtocol, ProveDiffieHellmanTuple] {

  override lazy val publicImage: ProveDiffieHellmanTuple = commonInput
}

object DiffieHellmanTupleProverInput {
  import sigmastate.interpreter.CryptoConstants.dlogGroup

  def random(): DiffieHellmanTupleProverInput = {
    val g = dlogGroup.generator
    val h = dlogGroup.createRandomGenerator()

    val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)
    val w = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)
    val u = dlogGroup.exponentiate(g, w)
    val v = dlogGroup.exponentiate(h, w)
    val ci = ProveDiffieHellmanTuple(
      GroupElementConstant(g), GroupElementConstant(h),
      GroupElementConstant(u), GroupElementConstant(v))
    DiffieHellmanTupleProverInput(w, ci)
  }
}

//a = g^r, b = h^r
case class FirstDiffieHellmanTupleProverMessage(a: CryptoConstants.EcPointType, b: CryptoConstants.EcPointType)
  extends FirstProverMessage[DiffieHellmanTupleProtocol] {
  override def bytes: Array[Byte] = {
    val ba = a.getEncoded(true)

    val bb = b.getEncoded(true)

    Array(ba.length.toByte, bb.length.toByte) ++ ba ++ bb
  }
}

//z = r + ew mod q
case class SecondDiffieHellmanTupleProverMessage(z: BigInteger)
  extends SecondProverMessage[DiffieHellmanTupleProtocol] {
  override def bytes: Array[PropositionCode] = ???
}

// Common input: (g,h,u,v)
case class ProveDiffieHellmanTuple(gv: Value[SGroupElement.type],
                                   hv: Value[SGroupElement.type],
                                   uv: Value[SGroupElement.type],
                                   vv: Value[SGroupElement.type])
  extends SigmaProtocolCommonInput[DiffieHellmanTupleProtocol]
    with SigmaProofOfKnowledgeTree[DiffieHellmanTupleProtocol, DiffieHellmanTupleProverInput] {

  override val opCode: OpCode = OpCodes.ProveDiffieHellmanTupleCode

  override def cost[C <: Context[C]](context: C): Long = Cost.Dlog * 2

  //todo: fix code below , we should consider that class parameters could be not evaluated
  lazy val g = gv.asInstanceOf[GroupElementConstant].value
  lazy val h = hv.asInstanceOf[GroupElementConstant].value
  lazy val u = uv.asInstanceOf[GroupElementConstant].value
  lazy val v = vv.asInstanceOf[GroupElementConstant].value
}

object ProveDiffieHellmanTuple {
  val Code: PropositionCode = 103: Byte
}


class DiffieHellmanTupleInteractiveProver(override val publicInput: ProveDiffieHellmanTuple,
                                          override val privateInputOpt: Option[DiffieHellmanTupleProverInput])
  extends InteractiveProver[DiffieHellmanTupleProtocol, ProveDiffieHellmanTuple, DiffieHellmanTupleProverInput] {

  import sigmastate.interpreter.CryptoConstants.dlogGroup

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
    val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)

    //SAMPLE a random z <- Zq
    val z = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)

    //COMPUTE a = g^z*h^(-e)  (where -e here means -e mod q)
    val e: BigInteger = new BigInteger(1, challenge.bytes)
    val minusE = dlogGroup.order.subtract(e)
    val hToZ = dlogGroup.exponentiate(publicInput.h, z)
    val gToZ = dlogGroup.exponentiate(publicInput.g, z)
    val uToMinusE = dlogGroup.exponentiate(publicInput.u, minusE)
    val vToMinusE = dlogGroup.exponentiate(publicInput.v, minusE)
    val a = dlogGroup.multiplyGroupElements(gToZ, uToMinusE)
    val b = dlogGroup.multiplyGroupElements(hToZ, vToMinusE)
    FirstDiffieHellmanTupleProverMessage(a, b) -> SecondDiffieHellmanTupleProverMessage(z)
  }
}

object DiffieHellmanTupleInteractiveProver {
  import sigmastate.interpreter.CryptoConstants.dlogGroup

  def firstMessage(publicInput: ProveDiffieHellmanTuple): (BigInteger, FirstDiffieHellmanTupleProverMessage) = {
    val qMinusOne = dlogGroup.order.subtract(BigInteger.ONE)
    val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, new SecureRandom)
    val a = dlogGroup.exponentiate(publicInput.g, r)
    val b = dlogGroup.exponentiate(publicInput.h, r)
    r -> FirstDiffieHellmanTupleProverMessage(a, b)
  }

  def secondMessage(privateInput: DiffieHellmanTupleProverInput,
                    rnd: BigInteger,
                    challenge: Challenge): SecondDiffieHellmanTupleProverMessage = {
    val q: BigInteger = dlogGroup.order
    val e: BigInteger = new BigInteger(1, challenge.bytes)
    val ew: BigInteger = e.multiply(privateInput.w).mod(q)
    val z: BigInteger = rnd.add(ew).mod(q)
    SecondDiffieHellmanTupleProverMessage(z)
  }
}