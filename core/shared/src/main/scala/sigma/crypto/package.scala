package sigma

package object crypto {
  /** Length of hash function used in the signature scheme. Blake2b hash function is used. */
  val hashLengthBits = 256

  /** Length of hash in bytes. */
  val hashLength: Int = hashLengthBits / 8

  /** Instance of Elliptic Curve descriptor. */
  type Curve = Platform.Curve

  /** Instance of Elliptic Curve point. */
  type Ecp = Platform.Ecp

  /** Instance of Elliptic Curve field element. */
  type ECFieldElem = Platform.ECFieldElem

  /** A cryptographically strong random number generator. */
  type SecureRandom = Platform.SecureRandom
}
