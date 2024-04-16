package sigma.pow


import org.bouncycastle.util.BigIntegers
import scorex.crypto.hash.Blake2b256
import scorex.utils.{Bytes, Ints, Longs}

object Autolykos2PowValidation {

  /**
    * Blake2b256 hash function invocation
    * @param in - input bit-string
    * @return - 256 bits (32 bytes) array
    */
  def hash(in: Array[Byte]): Array[Byte] = Blake2b256.hash(in)

  /**
    * Convert byte array to unsigned integer
    * @param in - byte array
    * @return - unsigned integer
    */
  def toBigInt(in: Array[Byte]): BigInt = BigInt(BigIntegers.fromUnsignedByteArray(in))

  /**
    * Constant data to be added to hash function to increase its calculation time
    */
  val M: Array[Byte] = (0 until 1024).toArray.flatMap(i => Longs.toByteArray(i.toLong))

  /**
    * Hash function that takes `m` and `nonceBytes` and returns a list of size `k` with numbers in
    * [0,`N`)
    */
  private def genIndexes(k: Int, seed: Array[Byte], N: Int): Seq[Int] = {
    val hash = Blake2b256(seed)
    val extendedHash = Bytes.concat(hash, hash.take(3))
    (0 until k).map { i =>
      BigInt(1, extendedHash.slice(i, i + 4)).mod(N).toInt
    }
  }.ensuring(_.length == k)

  /**
    * Generate element of Autolykos equation.
    */
  private def genElementV2(m: Array[Byte],
                         pk: Array[Byte], // not used in v2
                         w: Array[Byte], // not used in v2
                         indexBytes: Array[Byte],
                         heightBytes: => Array[Byte] // not used in v1
                        ): BigInt = {
    // Autolykos v. 2: H(j|h|M) (line 5 from the Algo 2 of the spec)
    toBigInt(hash(Bytes.concat(indexBytes, heightBytes, M)).drop(1))
  }

  def hitForVersion2ForMessage(k: Int, msg: Array[Byte], nonce: Array[Byte], h: Array[Byte], N: Int): BigInt = {

    val prei8 = BigIntegers.fromUnsignedByteArray(hash(Bytes.concat(msg, nonce)).takeRight(8))
    val i = BigIntegers.asUnsignedByteArray(4, prei8.mod(BigInt(N).underlying()))
    val f = Blake2b256(Bytes.concat(i, h, M)).drop(1) // .drop(1) is the same as takeRight(31)
    val seed = Bytes.concat(f, msg, nonce) // Autolykos v1, Alg. 2, line4:

    val indexes = genIndexes(k, seed, N)
    //pk and w not used in v2
    val elems = indexes.map(idx => genElementV2(msg, null, null, Ints.toByteArray(idx), h))
    val f2 = elems.sum

    // sum as byte array is always about 32 bytes
    val array: Array[Byte] = BigIntegers.asUnsignedByteArray(32, f2.underlying())
    val ha = hash(array)
    toBigInt(ha)
  }

}
