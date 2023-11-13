package org.ergoplatform.js

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress, Pay2SAddress}
import scorex.util.encode.Base58
import sigma.js.SigmaProp

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.{UndefOr, undefined}
import scala.util.{Failure, Success}

/** An exported JavaScript class wrapping the Scala `ErgoAddress` type. */
@JSExportTopLevel("Address")
class Address(
    ergoAddress: org.ergoplatform.ErgoAddress,
    _addrBytes: Array[Byte]
) extends js.Object {
  def this(ergoAddress: org.ergoplatform.ErgoAddress) = {
    this(ergoAddress, ErgoAddressEncoder(ergoAddress.networkPrefix).toBytes(ergoAddress))
  }
  private lazy val _base58String: String = Base58.encode(_addrBytes)

  /** First byte is used to encode network type and address type.
    *
    * @see ErgoAddressEncoder
    */
  private def headByte: Byte = _addrBytes(0)

  /** @return true if this address from Ergo mainnet. */
  def isMainnet(): Boolean = Address.isMainnet(headByte)

  /** @return true if this address has Pay-To-Public-Key type. */
  def isP2PK(): Boolean = ergoAddress.isInstanceOf[P2PKAddress]

  /** @return true if this address has Pay-To-Script type. */
  def isP2S(): Boolean = ergoAddress.isInstanceOf[Pay2SAddress]

  def toSigmaPropOpt(): UndefOr[sigma.js.SigmaProp] = {
    ergoAddress.script.toSigmaBooleanOpt match {
      case Some(sb) => new SigmaProp(sb)
      case _ => undefined
    }
  }

  /** Converts the given [[Address]] to Base58 string. */
  override def toString(): String = ergoAddress.toString
}

/** An exported JavaScript object providing utility methods for working with Address instances. */
@JSExportTopLevel("AddressObj")
object Address extends js.Object {

  /** @return true if this address from Ergo mainnet. */
  private def isMainnet(headByte: Byte): Boolean = headByte < ErgoAddressEncoder.TestnetNetworkPrefix

  private def getNetworkType(headByte: Byte): NetworkPrefix = {
    if (isMainnet(headByte)) ErgoAddressEncoder.MainnetNetworkPrefix else ErgoAddressEncoder.TestnetNetworkPrefix
  }

  /** Deserializes an ErgoTree instance from a hexadecimal string.
    *
    * @param hex a hexadecimal string representing the serialized ErgoTree
    */
  def fromString(base58String: String): Address = {
    Base58.decode(base58String) match {
      case Success(bytes) =>
        val headByte = bytes(0)
        val encoder = ErgoAddressEncoder(getNetworkType(headByte))
        encoder.fromBytes(bytes, base58String) match {
          case Success(ergoAddress) =>
            new Address(ergoAddress, bytes)
          case Failure(t) =>
            throw new RuntimeException(
              "Invalid address encoding, expected base58 string: " + base58String, t)
        }
      case Failure(t) =>
        throw new RuntimeException(
          "Invalid address encoding, expected base58 string: " + base58String, t)
    }
  }
}