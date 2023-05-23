package org.ergoplatform.sdk

import scorex.utils.Ints

import java.util

object ErgoId {
  /** Creates a new ErgoId decoding it from the given hex string. */
  def create(base16Str: String) = new ErgoId(JavaHelpers.decodeStringToBytes(base16Str))
}

/**
  * Identifier of Ergo object which wraps byte array (usually 256 bit hash).
  * ErgoId supports equality.
  */
class ErgoId(val _idBytes: Array[Byte]) {
  /** Extracts underlying byte array with id bytes. */
  def getBytes = _idBytes

  override def hashCode =
    if (_idBytes != null && _idBytes.length >= 4) Ints.fromByteArray(_idBytes)
    else util.Arrays.hashCode(_idBytes)

  override def equals(obj: Any): Boolean = {
    if (obj == null) return false
    if (this eq obj.asInstanceOf[AnyRef]) return true
    obj match {
      case that: ErgoId =>
        util.Arrays.equals(this._idBytes, that._idBytes)
      case _ => false
    }
  }

  /** String representation of id using Base16 encoding. */
  override def toString = JavaHelpers.Algos.encode(_idBytes)
}