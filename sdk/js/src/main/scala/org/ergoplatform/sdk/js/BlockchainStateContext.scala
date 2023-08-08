package org.ergoplatform.sdk.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/** Equivalent of [[org.ergoplatform.sdk.wallet.protocol.context.BlockchainStateContext]] available from JS. */
@JSExportTopLevel("BlockchainStateContext")
class BlockchainStateContext(
    val sigmaLastHeaders: js.Array[Header],
    val previousStateDigest: String,
    val sigmaPreHeader: PreHeader
)
