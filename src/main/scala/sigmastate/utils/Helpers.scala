package sigmastate.utils

import com.google.common.primitives.Ints
import scorex.crypto.hash.Blake2b256


object Helpers {
  def tagInt(ba: Array[Byte]): Int = Ints.fromByteArray(Blake2b256(ba).take(4))
}
