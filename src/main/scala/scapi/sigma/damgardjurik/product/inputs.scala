package scapi.sigma.damgardjurik.product


import java.math.BigInteger

import edu.biu.scapi.interactiveMidProtocols.sigmaProtocol.utility.SigmaProtocolMsg
import edu.biu.scapi.midLayer.asymmetricCrypto.keys.{DamgardJurikPrivateKey, DamgardJurikPublicKey}
import edu.biu.scapi.midLayer.ciphertext.BigIntegerCiphertext
import edu.biu.scapi.midLayer.plaintext.BigIntegerPlainText


//soundness = 40 in ScAPI by default
case class ProtocolParams(soundness: Int, lengthParameter: Int)

case class SigmaDJProductFirstMsg(a1: BigInteger, a2: BigInteger) extends SigmaProtocolMsg

case class SigmaDJProductSecondMsg(z1: BigInteger, z2: BigInteger, z3: BigInteger) extends SigmaProtocolMsg

case class CommonInput(publicKey: DamgardJurikPublicKey,
                       c1: BigIntegerCiphertext,
                       c2: BigIntegerCiphertext,
                       c3: BigIntegerCiphertext)

case class ProverInput(privateKey: DamgardJurikPrivateKey,
                       x1: BigIntegerPlainText,
                       x2: BigIntegerPlainText) {

  //Calculate r from the given private key.
  private lazy val p: BigInteger = privateKey.getP
  private lazy val q: BigInteger = privateKey.getQ
  private lazy val pMinusOne: BigInteger = p.subtract(BigInteger.ONE)
  private lazy val qMinusOne: BigInteger = q.subtract(BigInteger.ONE)
  private lazy val n: BigInteger = p.multiply(q)

  //(p-1)*(q-1)
  private lazy val phiN: BigInteger = pMinusOne.multiply(qMinusOne)

  //m = n^(-1) mod (p-1)(q-1).
  private lazy val m: BigInteger = n.modInverse(phiN)

  //ri = ci^m mod n
  def r1(commonInput: CommonInput) = commonInput.c1.getCipher.modPow(m, n)

  def r2(commonInput: CommonInput) = commonInput.c2.getCipher.modPow(m, n)

  def r3(commonInput: CommonInput) = commonInput.c3.getCipher.modPow(m, n)
}