package scapi.ot

import java.math.BigInteger
import java.security.SecureRandom

import akka.actor.{Actor, ActorRef}
import edu.biu.scapi.generals.ScapiDefaultConfiguration
import edu.biu.scapi.interactiveMidProtocols.ot._
import edu.biu.scapi.interactiveMidProtocols.ot.semiHonest.OTSemiHonestDDHOnGroupElementSenderMsg
import edu.biu.scapi.primitives.dlog.{DlogGroup, GroupElement}
import edu.biu.scapi.tools.Factories.DlogGroupFactory
import org.bouncycastle.util.BigIntegers


class ObliviousTransferReceiver(obliviousSender: ActorRef) extends Actor {

  import ObliviousTransferProtocolMessages._

  var state: (DlogGroup, Byte, BigInteger) = _

  override def receive = {
    case Start(sigma) =>
      //transfer code
      val dlogName = ScapiDefaultConfiguration.getInstance().getProperty("DDHDlogGroup")
      val dlog = DlogGroupFactory.getInstance().getObject(dlogName)
      val qMinusOne = dlog.getOrder.subtract(BigInteger.ONE)
      val random = new SecureRandom()

      val alpha = BigIntegers.createRandomInRange(BigInteger.ZERO, qMinusOne, random)
      val tuple = computeTuple(dlog, alpha, sigma)

      state = (dlog, sigma, alpha)

      obliviousSender ! ComputedTuple(tuple)

    case GroupElements(msg) =>
      val (dlog, sigma, alpha) = state
      val output = computeFinalXSigma(dlog, sigma, alpha, msg).asInstanceOf[OTOnGroupElementROutput]
      println(s"output: ${output.getXSigma}")
  }

  def computeTuple(dlog: DlogGroup, alpha: BigInteger, sigma: Byte): OTRGroupElementPairMsg = {
    val h = dlog.createRandomElement()
    val g = dlog.getGenerator
    val gAlpha = dlog.exponentiate(g, alpha)

    val (h0, h1) = if (sigma == 0) (gAlpha, h)
    else if (sigma == 1) (h, gAlpha)
    else throw new IllegalStateException()

    new OTRGroupElementPairMsg(h0.generateSendableData(), h1.generateSendableData())
  }

  def computeFinalXSigma(dlog: DlogGroup, sigma: Byte, alpha: BigInteger, message: OTSMsg): OTROutput = message match {
    case msg: OTSemiHonestDDHOnGroupElementSenderMsg =>
      val u = dlog.reconstructElement(true, msg.getU)
      val beta = dlog.getOrder.subtract(alpha)
      val kSigma = dlog.exponentiate(u, beta)
      val vSigma: GroupElement = if (sigma == 0)
        dlog.reconstructElement(true, msg.getV0)
      else if (sigma == 1)
        dlog.reconstructElement(true, msg.getV1)
      else
        throw new IllegalArgumentException("sigma")

      val xSigma = dlog.multiplyGroupElements(vSigma, kSigma)
      new OTOnGroupElementROutput(xSigma)


    case _ => throw new IllegalArgumentException("message should be instance of OTSOnGroupElementSemiHonestMessage")
  }
}
