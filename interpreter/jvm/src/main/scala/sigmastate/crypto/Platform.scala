package sigmastate.crypto

import org.bouncycastle.crypto.digests.SHA512Digest
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator
import org.bouncycastle.crypto.params.KeyParameter
import org.bouncycastle.math.ec.{ECCurve, ECFieldElement, ECPoint}
import scalan.RType

import java.math.BigInteger
import java.security.SecureRandom
import scala.util.Random
import sigmastate._
import sigmastate.basics.BcDlogGroup
import special.collection.Coll
import special.sigma._

import java.text.Normalizer.Form.NFKD
import java.text.Normalizer

/** JVM specific implementation of crypto methods*/
object Platform {
  def getXCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getXCoord)
  def getYCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getYCoord)
  def getAffineXCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getAffineXCoord)
  def getAffineYCoord(p: Ecp): ECFieldElem = ECFieldElem(p.value.getAffineYCoord)

  def getEncodedOfFieldElem(p: ECFieldElem): Array[Byte] = p.value.getEncoded

  def getEncodedPoint(p: Ecp, compressed: Boolean): Array[Byte] = p.value.getEncoded(compressed)

  def testBitZeroOfFieldElem(p: ECFieldElem): Boolean = p.value.testBitZero()

  def normalizePoint(p: Ecp): Ecp = Ecp(p.value.normalize())

  def showPoint(p: Ecp): String = {
    val rawX = p.value.getRawXCoord.toString.substring(0, 6)
    val rawY = p.value.getRawYCoord.toString.substring(0, 6)
    s"ECPoint($rawX,$rawY,...)"
  }

  def multiplyPoints(p1: Ecp, p2: Ecp): Ecp = {
    /*
     * BC treats EC as additive group while we treat that as multiplicative group.
     */
    Ecp(p1.value.add(p2.value))
  }

  def exponentiatePoint(p: Ecp, n: BigInteger): Ecp = {
    /*
     * BC treats EC as additive group while we treat that as multiplicative group.
     * Therefore, exponentiate point is multiply.
     */
    Ecp(p.value.multiply(n))
  }

  def isInfinityPoint(p: Ecp): Boolean = p.value.isInfinity

  def negatePoint(p: Ecp): Ecp = Ecp(p.value.negate())

  /** Opaque point type. */
  case class Ecp(private[crypto] val value: ECPoint)

  case class ECFieldElem(value: ECFieldElement)

  def createContext(): CryptoContext = new CryptoContextJvm(CustomNamedCurves.getByName("secp256k1"))

  def createSecureRandom(): Random = new SecureRandom()

  def hashHmacSHA512(key: Array[Byte], data: Array[Byte]): Array[Byte] = HmacSHA512.hash(key, data)

  /** Seed generation using on bouncycastle implementation.
    * See https://github.com/ergoplatform/ergo-appkit/issues/82
    */
  def generatePbkdf2Key(normalizedMnemonic: String, normalizedPass: String): Array[Byte] = {
    val gen = new PKCS5S2ParametersGenerator(new SHA512Digest)
    gen.init(
      normalizedMnemonic.getBytes(CryptoFacade.Encoding),
      normalizedPass.getBytes(CryptoFacade.Encoding),
      CryptoFacade.Pbkdf2Iterations)
    val dk = gen.generateDerivedParameters(CryptoFacade.Pbkdf2KeyLength).asInstanceOf[KeyParameter].getKey
    dk
  }

  /** Implementation that uses [[java.text.Normalizer]].
    * See https://www.unicode.org/reports/tr15/  */
  def normalizeChars(chars: Array[Char]): String = {
    Normalizer.normalize(ArrayCharSequence(chars), NFKD)
  }

  /** Checks that the type of the value corresponds to the descriptor `tpe`.
    * If the value has complex structure only root type constructor is checked.
    * NOTE, this is surface check with possible false positives, but it is ok
    * when used in assertions, like `assert(isCorrestType(...))`, see `ConstantNode`.
    */
  def isCorrectType[T <: SType](value: Any, tpe: T): Boolean = value match {
    case c: Coll[_] => tpe match {
      case STuple(items) => c.tItem == RType.AnyType && c.length == items.length
      case tpeColl: SCollection[_] => true
      case _ => sys.error(s"Collection value $c has unexpected type $tpe")
    }
    case _: Option[_] => tpe.isOption
    case _: Tuple2[_, _] => tpe.isTuple && tpe.asTuple.items.length == 2
    case _: Boolean => tpe == SBoolean
    case _: Byte => tpe == SByte
    case _: Short => tpe == SShort
    case _: Int => tpe == SInt
    case _: Long => tpe == SLong
    case _: BigInt => tpe == SBigInt
    case _: String => tpe == SString
    case _: GroupElement => tpe.isGroupElement
    case _: SigmaProp => tpe.isSigmaProp
    case _: AvlTree => tpe.isAvlTree
    case _: Box => tpe.isBox
    case _: PreHeader => tpe == SPreHeader
    case _: Header => tpe == SHeader
    case _: Context => tpe == SContext
    case _: Function1[_, _] => tpe.isFunc
    case _: Unit => tpe == SUnit
    case _ => false
  }

  /** This JVM specific methods are used in Ergo node which won't be JS cross-compiled. */
  implicit class EcpOps(val p: Ecp) extends AnyVal {
    def getCurve: ECCurve = p.value.getCurve
    def isInfinity: Boolean = CryptoFacade.isInfinityPoint(p)
    def add(p2: Ecp): Ecp = CryptoFacade.multiplyPoints(p, p2)
    def multiply(n: BigInteger): Ecp = CryptoFacade.exponentiatePoint(p, n)
  }

  /** This JVM specific methods are used in Ergo node which won't be JS cross-compiled. */
  implicit class BcDlogGroupOps(val group: BcDlogGroup) extends AnyVal {
    def curve: ECCurve = group.ctx.asInstanceOf[CryptoContextJvm].curve
  }
}
