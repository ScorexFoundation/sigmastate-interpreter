package scapi.sigma.dlog

import java.math.BigInteger
import java.security.SecureRandom

import akka.actor.{Actor, ActorLogging, ActorRef}
import edu.biu.scapi.interactiveMidProtocols.sigmaProtocol.utility.{SigmaBIMsg, SigmaGroupElementMsg}
import edu.biu.scapi.primitives.dlog.GroupElement
import org.bouncycastle.util.BigIntegers
import scapi.sigma.rework.SigmaProtocolFunctions.{FirstMessage, RandomChallenge, SecondMessage, StartInteraction}


class Prover(commonInput: CommonInput,
             proverInput: ProverInput,
             verifierActor: ActorRef) extends Actor with ActorLogging {

  val dlog = commonInput.dlogGroup
  val w = proverInput.w
  val random = new SecureRandom()

  override def receive = beforeFirstMessage

  private def beforeFirstMessage: Receive = {
    case StartInteraction =>
      val qMinusOne = dlog.getOrder.subtract(BigInteger.ONE)

      val r = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, random)

      //Compute a = g^r.
      val a: GroupElement = dlog.exponentiate(dlog.getGenerator, r)

      val sigmaProtocolMsg = new SigmaGroupElementMsg(a.generateSendableData)
      verifierActor ! FirstMessage(sigmaProtocolMsg)
      context become beforeSecondMessage(r)
  }

  private def beforeSecondMessage(r: BigInteger): Receive = {
    case RandomChallenge(challenge) =>
      require(challenge.bytes.length * 8 == commonInput.protocolParams.soundness, "wrong challenge length")

      //Compute z = (r+ew) mod q
      val q: BigInteger = dlog.getOrder
      val e: BigInteger = new BigInteger(1, challenge.bytes)
      val ew: BigInteger = e.multiply(w).mod(q)
      val z: BigInteger = r.add(ew).mod(q)
      verifierActor ! SecondMessage(new SigmaBIMsg(z))
      context become finished
  }

  private def finished: Receive = {
    case a: Any => log.warning(s"Got a message after protocol being finished: $a")
  }
}
