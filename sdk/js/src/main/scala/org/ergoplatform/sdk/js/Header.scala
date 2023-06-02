package org.ergoplatform.sdk.js

import scorex.crypto.authds.ADDigest
import sigmastate.AvlTreeFlags
import special.sigma

import scala.scalajs.js
import scala.scalajs.js.UndefOr
import scala.scalajs.js.annotation.JSExportTopLevel


/** Equivalent of [[special.sigma.AvlTree]] available from JS. */
@JSExportTopLevel("AvlTree")
class AvlTree(
    val digest: String,
    val insertAllowed: Boolean,
    val updateAllowed: Boolean,
    val removeAllowed: Boolean,
    val keyLength: Int,
    val valueLengthOpt: UndefOr[Int]
) extends js.Object

/** Equivalent of [[special.sigma.Header]] available from JS. */
@JSExportTopLevel("Header")
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
) extends js.Object
