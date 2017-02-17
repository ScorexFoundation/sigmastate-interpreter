package scapi.sigma.damgardjurik.product

import java.math.BigInteger

import akka.actor.{Actor, ActorSystem, Props}
import edu.biu.scapi.midLayer.asymmetricCrypto.encryption.{DJKeyGenParameterSpec, ScDamgardJurikEnc}
import edu.biu.scapi.midLayer.asymmetricCrypto.keys.{DamgardJurikPrivateKey, DamgardJurikPublicKey}
import edu.biu.scapi.midLayer.ciphertext.BigIntegerCiphertext
import edu.biu.scapi.midLayer.plaintext.BigIntegerPlainText
import scapi.sigma.rework.SigmaProtocolFunctions.StartInteraction
import scapi.sigma.damgardjurik.product.Prover.SendFirstMessage


class Dealer extends Actor {

  lazy val protocolParams = ProtocolParams(soundness = 40, lengthParameter = 1)
  lazy val commonInput = CommonInput(pubKey, c1, c2, c3)
  lazy val proverInput = ProverInput(privKey, x1, x2)
  val b1 = new BigInteger("1000")
  val b2 = new BigInteger("2000")
  val b3 = new BigInteger("2000000")
  val x1 = new BigIntegerPlainText(b1)
  val x2 = new BigIntegerPlainText(b2)
  val x3 = new BigIntegerPlainText(b3)
  val djEncScheme = new ScDamgardJurikEnc()
  val keyPair = djEncScheme.generateKey(new DJKeyGenParameterSpec())
  djEncScheme.setKey(keyPair.getPublic, keyPair.getPrivate)
  val pubKey = keyPair.getPublic.asInstanceOf[DamgardJurikPublicKey]
  val privKey = keyPair.getPrivate.asInstanceOf[DamgardJurikPrivateKey]
  val c1 = djEncScheme.encrypt(x1).asInstanceOf[BigIntegerCiphertext]
  val c2 = djEncScheme.encrypt(x2).asInstanceOf[BigIntegerCiphertext]
  val c3 = djEncScheme.encrypt(x3).asInstanceOf[BigIntegerCiphertext]
  val verifier = context.actorOf(Props(classOf[Verifier], protocolParams, commonInput))
  val prover = context.actorOf(Props(classOf[Prover], protocolParams, commonInput, proverInput, verifier))

  override def receive: Receive = {
    case StartInteraction =>
      prover ! SendFirstMessage
  }
}


object Launcher extends App {
  val actorSystem = ActorSystem()
  val dealer = actorSystem.actorOf(Props[Dealer])
  dealer ! StartInteraction
}