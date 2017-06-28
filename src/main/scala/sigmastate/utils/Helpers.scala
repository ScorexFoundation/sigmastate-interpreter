package sigmastate.utils

import com.google.common.primitives.Ints
import scorex.crypto.hash.Blake2b256


object Helpers {
  def tagInt(ba: Array[Byte]): Int = Ints.fromByteArray(Blake2b256(ba).take(4))

  def xor(ba1: Array[Byte], ba2: Array[Byte]): Array[Byte] = ba1.zip(ba2).map(t => (t._1 ^ t._2).toByte)

  def xor(bas: Array[Byte]*): Array[Byte] =
    bas.reduce({case (ba, ba1) => xor(ba, ba1)}: ((Array[Byte], Array[Byte]) => Array[Byte]))
}
