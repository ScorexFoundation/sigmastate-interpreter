package sigmastate.crypto

import java.math.BigInteger

/** A context for cryptographic operations. */
abstract class CryptoContext {
  /** The order of the underlying finite field. */
  def getModulus: BigInteger

  /** The order of the underlying group. */
  def getOrder: BigInteger

  /** The generator of the underlying group. */
  def validatePoint(x: BigInteger, y: BigInteger): Ecp

  /** The point at infinity. */
  def getInfinity(): Ecp

  /** Decodes a point from its byte representation. */
  def decodePoint(encoded: Array[Byte]): Ecp

  /** The generator of the underlying group. */
  def getGenerator: Ecp
}

