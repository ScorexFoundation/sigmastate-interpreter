package sigmastate

import java.math.BigInteger

import scala.util.Try


object EcPointFunctions {
  def decodeBigIntPair(bytes: Array[Byte]): Try[(BigInteger, BigInteger)] = Try {
    val xSize = bytes.head
    val ySize = bytes.tail.head

    assert(bytes.length == 2 + xSize + ySize)

    val xBytes = bytes.slice(2, xSize + 2)
    val yBytes = bytes.slice(xSize + 2, xSize + ySize + 2)

    val x = new BigInteger(1, xBytes)
    val y = new BigInteger(1, yBytes)

    x -> y
  }

  def decodeBigIntTriple(bytes: Array[Byte]): Try[(BigInteger, BigInteger, BigInteger)] = Try {
    val xSize = bytes.head
    val ySize = bytes.tail.head
    val zSize = bytes.tail.tail.head

    assert(bytes.length == 3 + xSize + ySize + zSize)

    val xBytes = bytes.slice(3, xSize + 3)
    val yBytes = bytes.slice(xSize + 3, xSize + ySize + 3)
    val zBytes = bytes.slice(xSize + ySize + 3, xSize + ySize + zSize + 3)

    val x = new BigInteger(1, xBytes)
    val y = new BigInteger(1, yBytes)
    val z = new BigInteger(1, zBytes)

    (x, y, z)
  }
}
