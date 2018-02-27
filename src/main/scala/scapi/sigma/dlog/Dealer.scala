package scapi.sigma.dlog

import java.math.BigInteger
import java.security.SecureRandom

import akka.actor.{ActorSystem, Props}
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.bouncycastle.util.BigIntegers
import scapi.sigma.rework.SigmaProtocolFunctions.StartInteraction

/**
  * Sigma Protocols are a basic building block for Zero-knowledge proofs,
  * Zero-Knowledge Proofs Of Knowledge and more. A sigma protocol is a 3-round proof,
  * comprised of:

    1. A first message from the prover to the verifier
    2. A random challenge from the verifier
    3. A second message from the prover.

    See Efficient Secure Two-Party Protocols: Techniques and Constructions, p.148
  */

object Dealer extends App {
  val sys = ActorSystem("SigmaProtocolExample")

  val soundness = 40
  val protocolParams = ProtocolParams(soundness)

  val random = new SecureRandom()

  val dlog = new BcDlogECFp()
  val qMinusOne = dlog.getOrder.subtract(BigInteger.ONE)
  val w = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, random)
  val h = dlog.exponentiate(dlog.getGenerator, w)

  val commonInput = CommonInput(protocolParams, dlog, h)
  val proverInput = ProverInput(w)

  val verifier = sys.actorOf(Props(classOf[Verifier], commonInput))
  val prover = sys.actorOf(Props(classOf[Prover], commonInput, proverInput, verifier))

  prover ! StartInteraction
}
