package scapi.ot

import edu.biu.scapi.interactiveMidProtocols.ot.{OTRGroupElementPairMsg, OTSMsg}


object ObliviousTransferProtocolMessages {

  case class Start(sigma: Byte)

  case class ComputedTuple(msg: OTRGroupElementPairMsg)

  case class GroupElements(msg: OTSMsg)

}
