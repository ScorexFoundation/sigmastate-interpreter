package sigmastate.interpreter

import java.math.BigInteger
import java.security.SecureRandom

import org.bouncycastle.math.ec.custom.sec.SecP256K1Point
import sigmastate.basics.{BcDlogGroup, SecP256K1}

object CryptoConstants {
  type EcPointType = SecP256K1Point

  val EncodedGroupElementLength: Byte = 33

  val dlogGroup: BcDlogGroup[EcPointType] = SecP256K1

  val secureRandom: SecureRandom = dlogGroup.secureRandom

  /** Size of the binary representation of any group element (2 ^ groupSizeBits == <number of elements in a group>) */
  val groupSizeBits: Int = 256

  /** Number of bytes to represent any group element as byte array */
  val groupSize: Int = 256 / 8 //32 bytes

  /** Group order, i.e. number of elements in the group */
  val groupOrder: BigInteger = dlogGroup.order

  //size of challenge in Sigma protocols, in bits
  //if this anything but 192, threshold won't work, because we have polynomials over GF(2^192) and no others
  //so DO NOT change the value without implementing polynomials over GF(2^soundnessBits) first
  //and changing code that calls on GF2_192 and GF2_192_Poly classes!!!
  implicit val soundnessBits: Int = 192.ensuring(_ < groupSizeBits, "2^t < q condition is broken!")

  def secureRandomBytes(howMany: Int): Array[Byte] = {
    val bytes = new Array[Byte](howMany)
    secureRandom.nextBytes(bytes)
    bytes
  }

}
