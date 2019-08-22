package sigmastate.interpreter

import org.bouncycastle.math.ec.ECPoint
import scorex.crypto.hash.Blake2b256

object CryptoFunctions {
  lazy val soundnessBytes: Int = CryptoConstants.soundnessBits / 8

  def hashFn(input: Array[Byte]): Array[Byte] = {
    Blake2b256.hash(input).take(soundnessBytes)
  }

  def showECPoint(p: ECPoint): String = {
    val rawX = p.getRawXCoord.toString.substring(0, 6)
    val rawY = p.getRawYCoord.toString.substring(0, 6)
    s"ECPoint($rawX,$rawY,...)"
  }

}
