package sigmastate.crypto

import java.math.BigInteger

abstract class CryptoContext {
  def getModulus: BigInteger

  def getOrder: BigInteger

  def validatePoint(x: BigInteger,
                    y: BigInteger): Ecp

  def getInfinity(): Ecp

  def decodePoint(encoded: Array[Byte]): Ecp

  def getGenerator: Ecp
}

