package special.sigma.wrappers

import java.math.BigInteger

import scalan.WrapSpec
import special.wrappers.{WrapSpecBase}
import org.bouncycastle.math.ec.ECPoint
import special.sigma.SigmaPredef

import scala.reflect.ClassTag

trait SigmaPredefWrapSpec extends WrapSpecBase {
  def dataSize(v: Any): Long = SigmaPredef.dataSize(v)
}
