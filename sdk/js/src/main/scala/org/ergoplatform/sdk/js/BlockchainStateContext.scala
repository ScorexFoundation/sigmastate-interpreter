package org.ergoplatform.sdk.js

import org.ergoplatform.sdk.wallet.protocol.context.ErgoLikeStateContext

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[ErgoLikeStateContext]] available from JS. */
@JSExportTopLevel("BlockchainStateContext")
class BlockchainStateContext(
    val sigmaLastHeaders: js.Array[Header],
    val previousStateDigest: String,
    val sigmaPreHeader: PreHeader
)
