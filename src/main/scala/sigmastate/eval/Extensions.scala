package sigmastate.eval

import java.math.BigInteger
import special.sigma._

object Extensions {
  implicit class ByteExt(val b: Byte) extends AnyVal {
    def toBigInt: BigInt = CostingSigmaDslBuilder.BigInt(BigInteger.valueOf(b.toLong))
  }

  implicit class IntExt(val x: Int) extends AnyVal {
    /** Convert this value to BigInt. */
    def toBigInt: BigInt = CostingSigmaDslBuilder.BigInt(BigInteger.valueOf(x.toLong))
  }
}
