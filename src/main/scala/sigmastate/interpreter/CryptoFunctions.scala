package sigmastate.interpreter

import org.bouncycastle.math.ec.ECPoint
import scorex.crypto.hash.Blake2b256

object CryptoFunctions {
  lazy val soundnessBytes: Int = CryptoConstants.soundnessBits / 8

  def hashFn(input: Array[Byte]): Array[Byte] = {
    Blake2b256.hash(input).take(soundnessBytes)
  }

}
