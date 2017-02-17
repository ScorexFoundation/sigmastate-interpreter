package scapi.ot

import akka.actor.{ActorSystem, Props}
import scapi.ot.ObliviousTransferProtocolMessages.Start


object ObliviousTransferExample extends App {
  val sigma = 0: Byte

  val system = ActorSystem("OT")

  val otSender = system.actorOf(Props[ObliviousTransferGroupElementSender])
  val otReceiver = system.actorOf(Props(classOf[ObliviousTransferReceiver], otSender))

  otReceiver ! Start(sigma)
}
