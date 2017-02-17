package scapi.sigma.damgardjurik.product

import java.math.BigInteger
import java.security.SecureRandom

import akka.actor.Actor
import scapi.sigma.rework.Challenge
import scapi.sigma.rework.SigmaProtocolFunctions.RandomChallenge


class Verifier(protocolParams: ProtocolParams, commonInput: CommonInput) extends Actor {


  val challenge = {
    val c = new Array[Byte](protocolParams.soundness / 8)
    new SecureRandom().nextBytes(c)
    c
  }
  var firstMessage: Option[SigmaDJProductFirstMsg] = None

  override def receive: Receive = {
    case f: SigmaDJProductFirstMsg =>
      firstMessage = Some(f)
      sender() ! RandomChallenge(Challenge(challenge))

    case s: SigmaDJProductSecondMsg =>
      val isProduct = verify(commonInput, firstMessage.get, s, challenge)
      println(s"Is product: $isProduct")
  }

  /**
    * * Computes the verification of the protocol.<p>
    * "ACC IFF c1,c2,c3,a1,a2,z1,z2,z3 are relatively prime to n <p>
				AND c1^e*a1 = (1+n)^z1*z2^N mod N'<p>
				AND (c2^z1)/(a2*c3^e) = z3^N mod N'".

    * @param commonInput
    * @param a - first message
    * @param z - second message
    */
  private def verify(commonInput: CommonInput,
                     a: SigmaDJProductFirstMsg,
                     z: SigmaDJProductSecondMsg,
                     challenge: Array[Byte]): Boolean = {

    /* Checks the validity of the given soundness parameter.<p>
       t must be less than a third of the length of the public key n. */
    val modulus = commonInput.publicKey.getModulus
    val third: Int = modulus.bitLength / 3
    require(protocolParams.soundness < third, "t must be less than a third of the length of the public key n")


    var verified: Boolean = true

    val firstMsg: SigmaDJProductFirstMsg = a.asInstanceOf[SigmaDJProductFirstMsg]
    val secondMsg: SigmaDJProductSecondMsg = z.asInstanceOf[SigmaDJProductSecondMsg]
    val n: BigInteger = commonInput.publicKey.getModulus
    val c1: BigInteger = commonInput.c1.getCipher
    val c2: BigInteger = commonInput.c2.getCipher
    val c3: BigInteger = commonInput.c3.getCipher
    val a1: BigInteger = firstMsg.a1
    val a2: BigInteger = firstMsg.a2
    val z1: BigInteger = secondMsg.z1
    val z2: BigInteger = secondMsg.z2
    val z3: BigInteger = secondMsg.z3
    verified = verified && areRelativelyPrime(n, c1, c2, a1, a2, z1, z2, z3)

    val N: BigInteger = n.pow(protocolParams.lengthParameter)
    val NTag: BigInteger = n.pow(protocolParams.lengthParameter + 1)
    val eBI: BigInteger = new BigInteger(1, challenge)
    val c1ToE: BigInteger = c1.modPow(eBI, NTag)
    var left: BigInteger = c1ToE.multiply(a1).mod(NTag)
    val nPlusOneToZ1: BigInteger = n.add(BigInteger.ONE).modPow(z1, NTag)
    val z2ToN: BigInteger = z2.modPow(N, NTag)
    var right: BigInteger = nPlusOneToZ1.multiply(z2ToN).mod(NTag)
    verified = verified && (left == right)

    val numerator: BigInteger = c2.modPow(z1, NTag)
    val c3ToE: BigInteger = c3.modPow(eBI, NTag)
    val denominator: BigInteger = a2.multiply(c3ToE).mod(NTag)
    val denominatorInv: BigInteger = denominator.modInverse(NTag)
    left = numerator.multiply(denominatorInv).mod(NTag)
    right = z3.modPow(N, NTag)
    verified = verified && (left == right)

    verified
  }

  private def areRelativelyPrime(n: BigInteger, c1: BigInteger, c2: BigInteger,
                                 a1: BigInteger, a2: BigInteger,
                                 z1: BigInteger, z2: BigInteger, z3: BigInteger): Boolean =
    (c1.gcd(n) == BigInteger.ONE) &&
      (c2.gcd(n) == BigInteger.ONE) &&
      (c2.gcd(n) == BigInteger.ONE) &&
      (a1.gcd(n) == BigInteger.ONE) &&
      (a2.gcd(n) == BigInteger.ONE) &&
      (z1.gcd(n) == BigInteger.ONE) &&
      (z2.gcd(n) == BigInteger.ONE) &&
      (z3.gcd(n) == BigInteger.ONE)
}
