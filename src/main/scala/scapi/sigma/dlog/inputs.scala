package scapi.sigma.dlog

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.{DlogGroup, GroupElement}


case class ProtocolParams(soundness: Int)

case class CommonInput(protocolParams: ProtocolParams, dlogGroup: DlogGroup, h: GroupElement)

case class ProverInput(w: BigInteger)

