package org.ergoplatform.sdk.js

import org.ergoplatform.sdk

import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExportTopLevel
import org.ergoplatform.sdk.Iso._

/** JS exported version of the [[sdk.BlockchainParameters]] class with the same fields.
  * @see sdk.BlockchainParameters
  */
@JSExportTopLevel("BlockchainParameters")
class BlockchainParameters(
    storageFeeFactor: Int,
    minValuePerByte: Int,
    maxBlockSize: Int,
    tokenAccessCost: Int,
    inputCost: Int,
    dataInputCost: Int,
    outputCost: Int,
    maxBlockCost: Int,
    _softForkStartingHeight: UndefOr[Int],
    _softForkVotesCollected: UndefOr[Int],
    blockVersion: Byte
) extends sdk.BlockchainParameters(
  storageFeeFactor, minValuePerByte, maxBlockSize, tokenAccessCost, inputCost, dataInputCost,
  outputCost, maxBlockCost,
  Isos.isoUndefOr[Int, Int](identityIso).to(_softForkStartingHeight),
  Isos.isoUndefOr[Int, Int](identityIso).to(_softForkVotesCollected), blockVersion
)
