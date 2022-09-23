package sigmastate.crypto

import java.math.BigInteger
import java.security.SecureRandom

object BigIntegers {

  val ZERO: BigInteger = BigInteger.valueOf(0)

  private val MAX_ITERATIONS = 1000

  @throws[IllegalArgumentException]
  def createRandom(bitLength: Int, random: SecureRandom) = {
    if (bitLength < 1) throw new IllegalArgumentException("bitLength must be at least 1")
    val nBytes = (bitLength + 7) / 8
    val rv = new Array[Byte](nBytes)
    random.nextBytes(rv)

    // strip off any excess bits in the MSB
    val xBits = 8 * nBytes - bitLength
    rv(0) = (rv(0) & 255 >>> xBits).toByte
    rv
  }

  /**
    * Return a positive BigInteger in the range of 0 to 2**bitLength - 1.
    *
    * @param bitLength maximum bit length for the generated BigInteger.
    * @param random    a source of randomness.
    * @return a positive BigInteger
    */
  def createRandomBigInteger(
      bitLength: Int,
      random: SecureRandom): BigInteger = {
    new BigInteger(1, createRandom(bitLength, random))
  }

  /**
    * Return a random BigInteger not less than 'min' and not greater than 'max'
    *
    * @param min    the least value that may be generated
    * @param max    the greatest value that may be generated
    * @param random the source of randomness
    * @return a random BigInteger value in the range [min,max]
    */
  def createRandomInRange(
      min: BigInteger,
      max: BigInteger,
      random: SecureRandom): BigInteger = {
    val cmp = min.compareTo(max)
    if (cmp >= 0) {
      if (cmp > 0) throw new IllegalArgumentException("'min' may not be greater than 'max'")
      return min
    }

    if (min.bitLength > max.bitLength / 2)
      return createRandomInRange(ZERO, max.subtract(min), random).add(min)

    for ( i <- 0 until MAX_ITERATIONS ) {
      val x = createRandomBigInteger(max.bitLength, random)
      if (x.compareTo(min) >= 0 && x.compareTo(max) <= 0) return x
    }
    // fall back to a faster (restricted) method
    createRandomBigInteger(max.subtract(min).bitLength - 1, random).add(min)
  }
}
