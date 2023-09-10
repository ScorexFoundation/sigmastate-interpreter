package sigma

package object crypto {
  /** Length of hash function used in the signature scheme. Blake2b hash function is used. */
  val hashLengthBits = 256

  /** Length of hash in bytes. */
  val hashLength: Int = hashLengthBits / 8
}
