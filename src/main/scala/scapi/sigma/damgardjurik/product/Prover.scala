package scapi.sigma.damgardjurik.product

import java.math.BigInteger
import java.security.SecureRandom

import akka.actor.{Actor, ActorRef}
import org.bouncycastle.util.BigIntegers
import scapi.sigma.rework.SigmaProtocolFunctions.{RandomChallenge, StartInteraction}


/**
  * Prover could be used only once
  */
class Prover(protocolParams: ProtocolParams,
             commonInput: CommonInput,
             proverInput: ProverInput,
             verifier: ActorRef) extends Actor {

  val modulus = commonInput.publicKey.getModulus
  val third = modulus.bitLength / 3
  private val random = new SecureRandom()
  require(protocolParams.soundness < third,
    s"soundness must be less than a third of the length of the public key n(${modulus.bitLength})")
  private val n: BigInteger = modulus

  //Calculate N = n^s and N' = n^(s+1)
  private val N: BigInteger = n.pow(protocolParams.lengthParameter)
  private val NTag: BigInteger = n.pow(protocolParams.lengthParameter + 1)

  /**
    * Computes the following line from the protocol:
    * "SAMPLE random values d <- ZN, rd <- Z*n, rdb <- Z*n"
    */
  private val d: BigInteger = BigIntegers.createRandomInRange(BigInteger.ZERO, N.subtract(BigInteger.ONE), random)
  private val rd: BigInteger = BigIntegers.createRandomInRange(BigInteger.ONE, n.subtract(BigInteger.ONE), random)
  private val rdb: BigInteger = BigIntegers.createRandomInRange(BigInteger.ONE, n.subtract(BigInteger.ONE), random)

  override def receive: Receive = {
    case StartInteraction =>
      self ! Prover.SendFirstMessage

    case Prover.SendFirstMessage =>
      //Calculate 1+n
      val nPlusOne: BigInteger = n.add(BigInteger.ONE)
      //Calculate (1+n)^d
      val nPlusOneToD: BigInteger = nPlusOne.modPow(d, NTag)
      //Calculate rd^N
      val rdToN: BigInteger = rd.modPow(N, NTag)
      //Calculate a1=(1+n)^d*rd^N mod N'
      val a1: BigInteger = nPlusOneToD.multiply(rdToN).mod(NTag)

      //Calculate (1+n)^(d*x2)
      val exponent: BigInteger = d.multiply(proverInput.x2.getX)
      val nPlusOnePow: BigInteger = nPlusOne.modPow(exponent, NTag)
      //Calculate rdb^N
      val rdbToN: BigInteger = rdb.modPow(N, NTag)
      //Calculate a2 = ((1+n)^(d*x2))*(rdb^N) mod N'
      val a2: BigInteger = nPlusOnePow.multiply(rdbToN).mod(NTag)

      verifier ! SigmaDJProductFirstMsg(a1, a2)

    case RandomChallenge(challenge) =>
      //todo: check challenge length
      self ! Prover.SendSecondMessage(challenge.bytes)

    case Prover.SendSecondMessage(challenge) =>
      //Compute z1 = e*x1+d mod N
      val e: BigInteger = new BigInteger(1, challenge)
      val ex1: BigInteger = e.multiply(proverInput.x1.getX)
      val z1: BigInteger = ex1.add(d).mod(N)

      //Compute z2 = r1^e*rd mod n
      val r1Toe: BigInteger = proverInput.r1(commonInput).modPow(e, n)
      val z2: BigInteger = r1Toe.multiply(rd).mod(n)

      //Compute z3=(r2^z1)/(rdb*r3^e) mod n
      val numerator: BigInteger = proverInput.r2(commonInput).modPow(z1, n)
      val r3ToE: BigInteger = proverInput.r3(commonInput).modPow(e, n)
      val denominator: BigInteger = rdb.multiply(r3ToE)
      val denominatorInv: BigInteger = denominator.modInverse(n)
      val z3: BigInteger = numerator.multiply(denominatorInv).mod(n)

      //todo: Delete the random values

      verifier ! SigmaDJProductSecondMsg(z1, z2, z3)
  }
}

object Prover {

  case class SendSecondMessage(challenge: Array[Byte])

  case object SendFirstMessage

}

