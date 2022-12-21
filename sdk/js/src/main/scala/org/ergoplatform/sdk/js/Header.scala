package org.ergoplatform.sdk.js

import scorex.crypto.authds.ADDigest
import sigmastate.AvlTreeFlags
import special.sigma

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExportTopLevel


@JSExportTopLevel("BlockchainStateContext")
class AvlTree(
    val digest: String,
    val insertAllowed: Boolean,
    val updateAllowed: Boolean,
    val removeAllowed: Boolean,
    val keyLength: Int,
    val valueLengthOpt: UndefOr[Int]
)

@JSExportTopLevel("BlockchainStateContext")
class Header(
    val id: String,
    val version: Byte,
    val parentId: String,
    val ADProofsRoot: String,
    val stateRoot: AvlTree,
    val transactionsRoot: String,
    val timestamp: js.BigInt,
    val nBits: js.BigInt,
    val height: Int,
    val extensionRoot: String,
    val minerPk: String,
    val powOnetimePk: String,
    val powNonce: String,
    val powDistance: js.BigInt,
    val votes: String
)
