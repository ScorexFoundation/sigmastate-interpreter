package scapi.ahe

import java.math.BigInteger

import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.{DJKeyGenParameterSpec, ScDamgardJurikEnc}
import edu.biu.scapi.midLayer.asymmetricCrypto.keys.{DamgardJurikPrivateKey, DamgardJurikPublicKey}
import edu.biu.scapi.midLayer.ciphertext.BigIntegerCiphertext
import edu.biu.scapi.midLayer.plaintext.BigIntegerPlainText


object DamgardJurikExperiment extends App {

  val b1 = new BigInteger("1000")
  val b2 = new BigInteger("2000")
  val x1 = new BigIntegerPlainText(b1)
  val x2 = new BigIntegerPlainText(b2)

  val djEncScheme = new ScDamgardJurikEnc()
  val keyPair = djEncScheme.generateKey(new DJKeyGenParameterSpec())
  djEncScheme.setKey(keyPair.getPublic, keyPair.getPrivate)
  val pubKey = keyPair.getPublic.asInstanceOf[DamgardJurikPublicKey]
  val privKey = keyPair.getPrivate.asInstanceOf[DamgardJurikPrivateKey]

  val c1 = djEncScheme.encrypt(x1).asInstanceOf[BigIntegerCiphertext]
  val c2 = djEncScheme.encrypt(x2).asInstanceOf[BigIntegerCiphertext]

  val c3 = djEncScheme.add(c1, c2)

  println(djEncScheme.decrypt(c3))
}
