package org.ergoplatform.sdk.wallet.protocol.context

import scorex.crypto.authds.ADDigest
import special.collection.Coll

import java.util

/**
  * Blockchain context used in transaction validation.
  */
trait ErgoLikeStateContext {

  /**
    * @return fixed number (10 in Ergo) of last block headers
    */
  def sigmaLastHeaders: Coll[special.sigma.Header]

  // todo remove from ErgoLikeContext and from ErgoStateContext
  /**
    * @return UTXO set digest from a last header (of sigmaLastHeaders)
    */
  def previousStateDigest: ADDigest

  /**
    * @return returns pre-header (header without certain fields) of the current block
    */
  def sigmaPreHeader: special.sigma.PreHeader
}

/** Representis the Ergo-like state context for tx signing.
  *
  * @param sigmaLastHeaders    the last headers of the Sigma blockchain
  * @param previousStateDigest the bytes representing the previous state digest
  * @param sigmaPreHeader      the pre-header object
  */
case class CErgoLikeStateContext(
  sigmaLastHeaders: Coll[special.sigma.Header],
  previousStateDigest: ADDigest,
  sigmaPreHeader: special.sigma.PreHeader
) extends ErgoLikeStateContext {
  override def hashCode(): Int =
    (sigmaLastHeaders.hashCode() * 41 + util.Arrays.hashCode(previousStateDigest)) * 41 + sigmaPreHeader.hashCode()

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case c: CErgoLikeStateContext =>
      sigmaLastHeaders == c.sigmaLastHeaders &&
        util.Arrays.equals(previousStateDigest, c.previousStateDigest) &&
        sigmaPreHeader == c.sigmaPreHeader
    case _ => false
  })
}
