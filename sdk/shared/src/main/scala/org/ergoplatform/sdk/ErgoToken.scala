package org.ergoplatform.sdk

/**
  * Represents ergo token (aka asset) paired with its value.
  * Implements equality and can be used as keys for maps and sets.
  */
case class ErgoToken(id: ErgoId, value: Long) {
  def this(idBytes: Array[Byte], value: Long) {
    this(new ErgoId(idBytes), value)
  }

  def this(id: String, value: Long) {
    this(JavaHelpers.decodeStringToBytes(id), value)
  }

  /** Java friendly id accessor method. */
  def getId: ErgoId = id

  /** Java friendly value accessor method. */
  def getValue: Long = value
}