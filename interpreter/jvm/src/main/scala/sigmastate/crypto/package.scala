package sigmastate

import sigma.crypto.CryptoContextJvm
import sigma.crypto.Platform.Curve

package object crypto {
  /** This JVM specific methods are used in Ergo node which won't be JS cross-compiled. */
  implicit class BcDlogGroupOps(val group: BcDlogGroup) extends AnyVal {
    def curve: Curve = group.ctx.asInstanceOf[CryptoContextJvm].curve
  }
}
