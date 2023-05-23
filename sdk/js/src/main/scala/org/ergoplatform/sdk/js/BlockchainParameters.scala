package org.ergoplatform.sdk.js

import org.ergoplatform.sdk.wallet.protocol.context.ErgoLikeParameters

import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("BlockchainParameters")
class BlockchainParameters(
    val storageFeeFactor: Int,
    val minValuePerByte: Int,
    val maxBlockSize: Int,
    val tokenAccessCost: Int,
    val inputCost: Int,
    val dataInputCost: Int,
    val outputCost: Int,
    val maxBlockCost: Int,
    val _softForkStartingHeight: UndefOr[Int],
    val _softForkVotesCollected: UndefOr[Int],
    val blockVersion: Byte
) extends ErgoLikeParameters {
  import org.ergoplatform.sdk.Iso._
  /**
    * @return height when voting for a soft-fork had been started
    */
  override def softForkStartingHeight: Option[Int] =
    Isos.isoUndefOr[Int, Int](identityIso).to(_softForkStartingHeight)

  /**
    * @return votes for soft-fork collected in previous epochs
    */
  override def softForkVotesCollected: Option[Int] =
    Isos.isoUndefOr[Int, Int](identityIso).to(_softForkVotesCollected)
}
