package sigmastate.interpreter

import scorex.crypto.hash.Blake2b256

object CryptoFunctions {
  lazy val soundnessBytes: Int = CryptoConstants.soundnessBits / 8

  /** @hotspot don't beautify this code. Used in Interpreter.verify. */
  def hashFn(input: Array[Byte]): Array[Byte] = {
    val h = Blake2b256.hash(input)
    val res = new Array[Byte](soundnessBytes)
    Array.copy(h, 0, res, 0, soundnessBytes)
    res
  }

}
