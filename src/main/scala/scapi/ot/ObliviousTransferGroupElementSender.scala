package scapi.ot

import java.math.BigInteger
import java.security.SecureRandom

import akka.actor.Actor
import edu.biu.scapi.interactiveMidProtocols.ot._
import edu.biu.scapi.interactiveMidProtocols.ot.semiHonest.{OTSemiHonestDDHOnGroupElementSender, OTSemiHonestDDHOnGroupElementSenderMsg}
import edu.biu.scapi.primitives.dlog.GroupElement
import edu.biu.scapi.primitives.dlog.openSSL.OpenSSLDlogECF2m
import org.bouncycastle.util.BigIntegers
import scapi.ot.ObliviousTransferProtocolMessages.{ComputedTuple, GroupElements}

/**
  * In Oblivious Transfer, a party called the sender has n messages,
  * and a party called the receiver has an index i.
  * The receiver wishes to receive the i-th message of the sender,
  * without the sender learning i, while the sender wants to ensure that the
  * receiver receives only one of the n messages.
  */

class ObliviousTransferGroupElementSender extends Actor {
  val sender0 = new OTSemiHonestDDHOnGroupElementSender()

  val dlog = new OpenSSLDlogECF2m("K-233")
  val x0 = dlog.createRandomElement()
  val x1 = dlog.createRandomElement()

  println("x0: " + x0)
  println("x1: " + x1)

  val input = new OTOnGroupElementSInput(x0, x1)
  val qMinusOne = dlog.getOrder.subtract(BigInteger.ONE)
  val random = new SecureRandom()

  override def receive = {
    case ComputedTuple(tuple) =>
      val r = BigIntegers.createRandomInRange(BigInteger.ZERO, this.qMinusOne, this.random)
      val u = this.computeU(r)
      val k0 = this.computeK0(r, tuple)
      val k1 = this.computeK1(r, tuple)
      val messageToSend = computeTuple(input, u, k0, k1)
      sender ! GroupElements(messageToSend)
  }

  def computeU(r: BigInteger): GroupElement = {
    val g = dlog.getGenerator
    dlog.exponentiate(g, r)
  }

  def computeK0(r: BigInteger, message: OTRGroupElementPairMsg): GroupElement = {
    val h0 = dlog.reconstructElement(true, message.getFirstGE)
    dlog.exponentiate(h0, r)
  }

  def computeK1(r: BigInteger, message: OTRGroupElementPairMsg): GroupElement = {
    val h1 = dlog.reconstructElement(true, message.getSecondGE)
    dlog.exponentiate(h1, r)
  }

  def computeTuple(input: OTSInput, u: GroupElement, k0: GroupElement, k1: GroupElement): OTSMsg =
    input match {
      case geInput: OTOnGroupElementSInput =>
        val x0 = geInput.getX0
        val x1 = geInput.getX1
        val v0 = this.dlog.multiplyGroupElements(x0, k0)
        val v1 = this.dlog.multiplyGroupElements(x1, k1)
        new OTSemiHonestDDHOnGroupElementSenderMsg(u.generateSendableData(), v0.generateSendableData(), v1.generateSendableData())

      case _ =>
        throw new IllegalArgumentException("x0 and x1 should be DlogGroup elements.")
    }
}







