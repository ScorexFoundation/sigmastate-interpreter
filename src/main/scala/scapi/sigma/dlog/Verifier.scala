package scapi.sigma.dlog

import java.math.BigInteger
import java.security.SecureRandom

import akka.actor.Actor
import edu.biu.scapi.interactiveMidProtocols.sigmaProtocol.utility.{SigmaBIMsg, SigmaGroupElementMsg, SigmaProtocolMsg}
import edu.biu.scapi.primitives.dlog.GroupElement
import scapi.sigma.rework.Challenge
import scapi.sigma.rework.SigmaProtocolFunctions.{FirstMessage, RandomChallenge, SecondMessage, Transcript}


class Verifier(commonInput: CommonInput) extends Actor {
  val dlog = commonInput.dlogGroup

  require(commonInput.protocolParams.soundness % 8 == 0, "soundness must be fit in bytes")

  override def receive: Receive = onfirstMessage

  def onfirstMessage: Receive = {
    case FirstMessage(a) =>

      val challenge = new Array[Byte](commonInput.protocolParams.soundness / 8)
      new SecureRandom().nextBytes(challenge) //modifies challenge
      sender() ! RandomChallenge(Challenge(challenge))
      context become onSecondMessage(a, challenge)
  }

  def onSecondMessage(a: SigmaProtocolMsg, challenge: Array[Byte]): Receive = {
    case SecondMessage(z) =>

      //Get the element of the first message from the prover.
      val firstMsg: SigmaGroupElementMsg = a.asInstanceOf[SigmaGroupElementMsg]
      val aElement: GroupElement = dlog.reconstructElement(true, firstMsg.getElement)

      //Get the exponent in the second message from the prover.
      val exponent: SigmaBIMsg = z.asInstanceOf[SigmaBIMsg]

      //Get the h from the input and verify that it is in the Dlog Group.
      val h: GroupElement = commonInput.h

      //Compute g^z (left size of the verify equation).
      lazy val left = dlog.exponentiate(dlog.getGenerator, exponent.getMsg)
      //Compute a*h^e (right side of the verify equation).
      //Convert e to BigInteger.
      lazy val eBI: BigInteger = new BigInteger(1, challenge)
      //Calculate h^e.
      lazy val hToe: GroupElement = dlog.exponentiate(h, eBI)
      //Calculate a*h^e.
      lazy val right: GroupElement = dlog.multiplyGroupElements(aElement, hToe)

      val accepted = dlog.isMember(h) && (left == right)

      val transcript = Transcript(a, Challenge(challenge), z, accepted)
      println("Protocol transcript: " + transcript)
  }
}
