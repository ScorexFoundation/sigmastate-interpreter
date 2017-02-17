package scapi.sigma.dlog

import java.math.BigInteger
import java.security.SecureRandom

import akka.actor.{ActorSystem, Props}
import edu.biu.scapi.primitives.dlog.miracl.MiraclDlogECF2m
import org.bouncycastle.util.BigIntegers
import scapi.sigma.rework.SigmaProtocolFunctions.StartInteraction

/**
  * Sigma Protocols are a basic building block for Zero-knowledge proofs,
  * Zero-Knowledge Proofs Of Knowledge and more. A sigma protocol is a 3-round proof,
  * comprised of:

    1. A first message from the prover to the verifier
    2. A random challenge from the verifier
    3. A second message from the prover.
  */

object Dealer extends App {
  //adding Miracl to libraries being loaded
  System.setProperty("java.library.path", System.getProperty("java.library.path") + ":/usr/lib/scapi")
  val sysPathsField = classOf[ClassLoader].getDeclaredField("sys_paths")
  sysPathsField.setAccessible(true)
  sysPathsField.set(null, null)
  //println(System.getProperty("java.library.path"))
  System.loadLibrary("MiraclJavaInterface")

  val sys = ActorSystem("SigmaProtocolExample")

  val soundness = 40
  val protocolParams = ProtocolParams(soundness)

  val random = new SecureRandom()

  val dlog = new MiraclDlogECF2m("K-233")
  val qMinusOne = dlog.getOrder.subtract(BigInteger.ONE)
  val w = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, random)
  val h = dlog.exponentiate(dlog.getGenerator, w)

  val commonInput = CommonInput(protocolParams, dlog, h)
  val proverInput = ProverInput(w)

  val verifier = sys.actorOf(Props(classOf[Verifier], commonInput))
  val prover = sys.actorOf(Props(classOf[Prover], commonInput, proverInput, verifier))

  prover ! StartInteraction
}
