package sigmastate.crypto

import scalan.RType
import scorex.util.encode.Base16
import sigmastate._
import special.collection.Coll
import special.sigma._

import java.math.BigInteger
import scala.scalajs.js
import scala.scalajs.js.UnicodeNormalizationForm
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.Random

/** JVM specific implementation of crypto methods (NOT yet implemented). */
object Platform {
  /** Description of elliptic curve of point `p` which belongs to the curve.
    *
    * @param p the elliptic curve point
    */
  def getCurve(p: Ecp): Curve = ???

  def getXCoord(p: Ecp): ECFieldElem = new ECFieldElem(CryptoFacadeJs.getXCoord(p.point))
  def getYCoord(p: Ecp): ECFieldElem = new ECFieldElem(CryptoFacadeJs.getYCoord(p.point))
  def getAffineXCoord(p: Ecp): ECFieldElem = new ECFieldElem(CryptoFacadeJs.getAffineXCoord(p.point))
  def getAffineYCoord(p: Ecp): ECFieldElem = new ECFieldElem(CryptoFacadeJs.getAffineYCoord(p.point))

  def Uint8ArrayToBytes(jsShorts: Uint8Array): Array[Byte] = {
    jsShorts.toArray[Short].map(x => x.toByte)
  }

  def bytesToJsShorts(bytes: Array[Byte]): js.Array[Short] = {
    js.Array(bytes.map(x => (x & 0xFF).toShort): _*)
  }

  def jsShortsToBytes(jsShorts: js.Array[Short]): Array[Byte] = {
    jsShorts.toArray[Short].map(x => x.toByte)
  }

  def encodeFieldElem(p: ECFieldElem): Array[Byte] = {
    Uint8ArrayToBytes(CryptoFacadeJs.getEncodedOfFieldElem(p.elem))
  }

  /** Byte representation of the given point.
    *
    * @param p point to encode
    * @param compressed if true, generates a compressed point encoding
    */
  def encodePoint(p: Ecp, compressed: Boolean): Array[Byte] = ???

  def signOf(p: ECFieldElem): Boolean = CryptoFacadeJs.testBitZeroOfFieldElem(p.elem)

  def getEncodedPoint(p: Ecp, compressed: Boolean): Array[Byte] = ???

  def testBitZeroOfFieldElem(p: ECFieldElem): Boolean = CryptoFacadeJs.testBitZeroOfFieldElem(p.elem)

  def normalizePoint(p: Ecp): Ecp = new Ecp(CryptoFacadeJs.normalizePoint(p.point))

  def showPoint(p: Ecp): String = CryptoFacadeJs.showPoint(p.point)

  def multiplyPoints(p1: Ecp, p2: Ecp): Ecp = new Ecp(CryptoFacadeJs.addPoint(p1.point, p2.point))

  def exponentiatePoint(p: Ecp, n: BigInteger): Ecp = {
    val scalar = Convert.bigIntegerToBigInt(n)
    new Ecp(CryptoFacadeJs.multiplyPoint(p.point, scalar))
  }

  def isInfinityPoint(p: Ecp): Boolean = CryptoFacadeJs.isInfinityPoint(p.point)

  def negatePoint(p: Ecp): Ecp = new Ecp(CryptoFacadeJs.negatePoint(p.point))

  class Curve
//  class ECPoint
//  class ECFieldElement

  // TODO JS: Use JS library for secure source of randomness
  type SecureRandom = Random

  /** Opaque point type. */
  @js.native
  trait Point extends js.Object {
    def x: js.BigInt = js.native
    def y: js.BigInt = js.native
    def toHex(b: Boolean): String = js.native
  }

  class Ecp(val point: Point) {
    lazy val hex = point.toHex(true)
    override def hashCode(): Int = hex.hashCode

    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
      case that: Ecp => this.hex == that.hex
      case _ => false
    })
  }

  class ECFieldElem(val elem: js.BigInt) {
    lazy val digits: String = elem.toString(10)

    override def hashCode(): Int = digits.hashCode

    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
      case that: ECFieldElem => this.digits == that.digits
      case _ => false
    })
  }

  object Convert {
    def bigIntToBigInteger(jsValue: js.BigInt): BigInteger = {
      new BigInteger(jsValue.toString(10), 10)
    }

    def bigIntegerToBigInt(value: BigInteger): js.BigInt = {
      js.BigInt(value.toString(10))
    }
  }

  def createContext(): CryptoContext = new CryptoContext {
    val ctx = new CryptoContextJs

    /** The underlying elliptic curve descriptor. */
    override def curve: crypto.Curve = ???

    override def fieldCharacteristic: BigInteger = Convert.bigIntToBigInteger(ctx.getModulus())

    override def order: BigInteger = Convert.bigIntToBigInteger(ctx.getOrder())

    override def validatePoint(x: BigInteger, y: BigInteger): crypto.Ecp = {
      val point = ctx.validatePoint(Convert.bigIntegerToBigInt(x), Convert.bigIntegerToBigInt(y))
      new Ecp(point)
    }

    override def infinity(): crypto.Ecp =
      new Ecp(ctx.getInfinity())

    override def decodePoint(encoded: Array[Byte]): crypto.Ecp =
      new Ecp(ctx.decodePoint(Base16.encode(encoded)))

    override def generator: crypto.Ecp =
      new Ecp(ctx.getGenerator())
  }

  /** Create JS specific source of secure randomness. */
  def createSecureRandom(): Random = new Random()

  def hashHmacSHA512(key: Array[Byte], data: Array[Byte]): Array[Byte] = {
    val keyArg = Uint8Array.from(bytesToJsShorts(key))
    val dataArg = Uint8Array.from(bytesToJsShorts(data))
    val hash = CryptoFacadeJs.hashHmacSHA512(keyArg, dataArg)
    Uint8ArrayToBytes(hash)
  }

  def generatePbkdf2Key(normalizedMnemonic: String, normalizedPass: String): Array[Byte] = {
    val res = CryptoFacadeJs.generatePbkdf2Key(normalizedMnemonic, normalizedPass)
    Uint8ArrayToBytes(res)
  }

  def normalizeChars(chars: Array[Char]): String = {
    import js.JSStringOps._
    String.valueOf(chars).normalize(UnicodeNormalizationForm.NFKD)
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
    case _: Byte | _: Short | _: Int | _: Long => tpe.isInstanceOf[SNumericType]
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
}
