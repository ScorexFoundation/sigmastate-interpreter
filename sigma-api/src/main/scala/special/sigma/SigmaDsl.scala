package special.sigma

import java.math.BigInteger
import org.bouncycastle.math.ec.ECPoint
import scala.reflect.ClassTag
import special.collection._
import scalan.{Internal, RType, _}

@scalan.Liftable
trait CostModel {
  def AccessBox: Int // costOf("AccessBox: Context => Box")
  def AccessAvlTree: Int // costOf("AccessAvlTree: Context => AvlTree")

  def GetVar: Int // costOf("ContextVar: (Context, Byte) => Option[T]")
  def DeserializeVar: Int // costOf("DeserializeVar: (Context, Byte) => Option[T]")

  def GetRegister: Int // costOf("AccessRegister: (Box,Byte) => Option[T]")
  def DeserializeRegister: Int // costOf("DeserializeRegister: (Box,Byte) => Option[T]")

  def SelectField: Int // costOf("SelectField")
  def CollectionConst: Int // costOf("Const: () => Array[IV]")
  def AccessKiloByteOfData: Int // costOf("AccessKiloByteOfData")
  @Reified("T") def dataSize[T](x: T)(implicit cT: ClassTag[T]): Long
  /** Size of public key in bytes */
  def PubKeySize: Long = 32
}

/**
  * All `modQ` operations assume that Q is a global constant (an order of the only one cryptographically strong group
  * which is used for all cryptographic operations).
  * So it is globally and implicitly used in all methods.
  * */
@scalan.Liftable
trait BigInt {
  /** Convert this BigInt value to Byte.
    * @throws ArithmeticException if overflow happens.
    */
  def toByte: Byte

  /** Convert this BigInt value to Short.
    * @throws ArithmeticException if overflow happens.
    */
  def toShort: Short

  /** Convert this BigInt value to Int.
    * @throws ArithmeticException if overflow happens.
    */
  def toInt: Int

  /** Convert this BigInt value to Int.
    * @throws ArithmeticException if overflow happens.
    */
  def toLong: Long

  /** Returns a big-endian representation of this BigInt in a collection of bytes.
    * For example, the value {@code 0x1213141516171819} would yield the
    * byte array {@code {0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19}}.
    * @since 2.0
    */
  def toBytes: Coll[Byte]

  /** Returns a big-endian representation of this BigInt in a collection of Booleans.
    * Each boolean corresponds to one bit of the representation.
    * @since 2.0
    */
  def toBits: Coll[Boolean]

  /** Absolute value of this numeric value.
    * @since 2.0
    */
  def toAbs: BigInt

  /** Compares this numeric with that numeric for order.  Returns a negative integer, zero, or a positive integer as the
    * `this` is less than, equal to, or greater than `that`.
    */
  def compareTo(that: BigInt): Int

  /** Returns this `mod` Q, i.e. remainder of division by Q, where Q is an order of the cryprographic group.
    * @since 2.0
    */
  def modQ: BigInt

  /** Adds this number with `other` by module Q.
    * @since 2.0
    */
  def plusModQ(other: BigInt): BigInt

  /** Subracts this number with `other` by module Q.
    * @since 2.0
    */
  def minusModQ(other: BigInt): BigInt

  /** Multiply this number with `other` by module Q.
    * @since 2.0
    */
  def multModQ(other: BigInt): BigInt

  /** Multiply this number with `other` by module Q.
    * @since Mainnet
    */
  def inverseModQ: BigInt // ??? @kushti do we need it
}

/** Base class for points on elliptic curves.
  */
@scalan.Liftable
trait GroupElement {

  def isIdentity: Boolean

  /** this should replace the currently used ^
    * @since 2.0
    */
  def exp(n: BigInt): GroupElement
}

@scalan.Liftable
trait SigmaProp {
  def isValid: Boolean
  def propBytes: Coll[Byte]
  @OverloadId("and_sigma") def &&(other: SigmaProp): SigmaProp
  @OverloadId("and_bool")  def &&(other: Boolean): SigmaProp
  @OverloadId("or_sigma") def ||(other: SigmaProp): SigmaProp
  @OverloadId("or_bool")  def ||(other: Boolean): SigmaProp
  def lazyAnd(other: => SigmaProp): SigmaProp
  def lazyOr(other: => SigmaProp): SigmaProp
}

@scalan.Liftable
trait AnyValue {
  def dataSize: Long
}

@scalan.Liftable
trait Box {
  /** Blake2b256 hash of this box's content, basically equals to `blake2b256(bytes)` */
  def id: Coll[Byte]

  /** Mandatory: Monetary value, in Ergo tokens */
  def value: Long

  /** Serialized bytes of guarding script, which should be evaluated to true in order to
    * open this box. (aka spend it in a transaction)*/
  def propositionBytes: Coll[Byte]

  /** Serialized bytes of this box's content, including proposition bytes. */
  def bytes: Coll[Byte]

  /** Serialized bytes of this box's content, excluding transactionId and index of output. */
  def bytesWithoutRef: Coll[Byte]
  def cost: Int
  def dataSize: Long
  def registers: Coll[AnyValue]

  /** Extracts register by id and type.
    * @param regId zero-based identifier of the register.
    * @tparam T expected type of the register.
    * @return Some(value) if the register is defined and has given type.
    *         None otherwise
    * @since 2.0
    */
  def getReg[@Reified T](i: Int)(implicit cT: RType[T]): Option[T]


  /** Secondary tokens */
  def tokens: Coll[(Coll[Byte], Long)]

  /** If `tx` is a transaction which generated this box, then `creationInfo._1` is a height of the tx's block.
    *  The `creationInfo._2` is a serialized transaction identifier followed by box index in the transaction outputs.
    */
  def creationInfo: (Int, Coll[Byte])

  /** Extracts register as Coll[Byte], deserializes it to script and then executes this script in the current context.
    * The original Coll[Byte] of the script is available as getReg[Coll[Byte]](id)
    * @param regId identifier of the register
    * @tparam T result type of the deserialized script.
    * @throws IllegalArgumentException if the actual script type doesn't conform to `T`
    * @return result of the script execution in the current context
    * @since Mainnet
    */
  def executeFromRegister[T](regId: Byte): T

  @Internal
  override def toString = s"Box(id=$id; value=$value; cost=$cost; size=$dataSize; regs=$registers)"
}

@scalan.Liftable
trait AvlTree {
  def startingDigest: Coll[Byte]
  def keyLength: Int
  def valueLengthOpt: Option[Int]
  def maxNumOperations: Option[Int]
  def maxDeletes: Option[Int]
  def cost: Int
  def dataSize: Long
  /** Returns digest of the state represent by this tree.
    * @since 2.0
    */
  def digest: Coll[Byte]
}

/** Represents data of the block headers available in scripts.
  * @since 2.0
  */
trait Header {
  def version: Byte

  /** Bytes representation of ModifierId of the previous block in the blockchain */
  def parentId: Coll[Byte] //

  def ADProofsRoot: Coll[Byte] // Digest32. Can we build AvlTree out of it?
  def stateRoot: Coll[Byte]  // ADDigest  //33 bytes! extra byte with tree height here!
  def transactionsRoot: Coll[Byte]  // Digest32
  def timestamp: Long
  def nBits: Long  // actually it is unsigned Int
  def height: Int
  def extensionRoot: Coll[Byte] // Digest32
  def minerPk: GroupElement    // pk
  def powOnetimePk: GroupElement  // w
  def powNonce: Coll[Byte]        // n
  def powDistance: BigInt        // d
}

/** Only header fields that can be predicted by a miner
  * @since 2.0
  */
trait Preheader { // Testnet2
  def version: Byte
  def parentId: Coll[Byte] // ModifierId
  def timestamp: Long
  def nBits: Long  // actually it is unsigned Int
  def height: Int
  def minerPk: GroupElement
}

@scalan.Liftable
trait Context {
  def builder: SigmaDslBuilder

  /** A collection of outputs of the current transaction. */
  def OUTPUTS: Coll[Box]

  /** A collection of inputs of the current transaction, the transaction where selfBox is one of the inputs. */
  def INPUTS: Coll[Box]

  /** Height (block number) of the block which is currently being validated. */
  def HEIGHT: Int

  /** Box whose proposition is being currently executing */
  def SELF: Box

  /** Zero based index in `inputs` of `selfBox`. */
  def selfBoxIndex: Int

  /** Authenticated dynamic dictionary digest representing Utxo state before current state. */
  def lastBlockUtxoRoot: AvlTree

  /**
    * @since 2.0
    */
  def headers: Coll[Header]

  /**
    * @since 2.0
    */
  def preheader: Preheader

  def MinerPubKey: Coll[Byte]
  def getVar[T](id: Byte)(implicit cT: RType[T]): Option[T]
  def getConstant[T](id: Byte)(implicit cT: RType[T]): T
  def cost: Int
  def dataSize: Long
}

@scalan.Liftable
trait SigmaContract {
  def builder: SigmaDslBuilder

  @NeverInline
  @Reified("T")
  def Collection[T](items: T*)(implicit cT: RType[T]): Coll[T] = this.builder.Colls.fromItems[T](items:_*)

  /** !!! all methods should delegate to builder */

  def verifyZK(cond: => SigmaProp): Boolean = this.builder.verifyZK(cond)
  def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp = this.builder.atLeast(bound, props)

  def allOf(conditions: Coll[Boolean]): Boolean = this.builder.allOf(conditions)
  def allZK(conditions: Coll[SigmaProp]): SigmaProp = this.builder.allZK(conditions)

  def anyOf(conditions: Coll[Boolean]): Boolean = this.builder.anyOf(conditions)
  def anyZK(conditions: Coll[SigmaProp]): SigmaProp = this.builder.anyZK(conditions)

  def PubKey(base64String: String): SigmaProp = this.builder.PubKey(base64String)

  def sigmaProp(b: Boolean): SigmaProp = this.builder.sigmaProp(b)

  def blake2b256(bytes: Coll[Byte]): Coll[Byte] = this.builder.blake2b256(bytes)
  def sha256(bytes: Coll[Byte]): Coll[Byte] = this.builder.sha256(bytes)

  def byteArrayToBigInt(bytes: Coll[Byte]): BigInteger = this.builder.byteArrayToBigInt(bytes)
  def longToByteArray(l: Long): Coll[Byte] = this.builder.longToByteArray(l)

  def proveDlog(g: ECPoint): SigmaProp = this.builder.proveDlog(g)
  def proveDHTuple(g: ECPoint, h: ECPoint, u: ECPoint, v: ECPoint): SigmaProp = this.builder.proveDHTuple(g, h, u, v)

  def isMember(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Boolean = this.builder.isMember(tree, key, proof)
  def treeLookup(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = this.builder.treeLookup(tree, key, proof)
  def treeModifications(tree: AvlTree, operations: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = this.builder.treeModifications(tree, operations, proof)

  def groupGenerator: ECPoint = this.builder.groupGenerator
  def exponentiate(base: ECPoint, exponent: BigInteger): ECPoint = this.builder.exponentiate(base, exponent)

  @clause def canOpen(ctx: Context): Boolean

  def asFunction: Context => Boolean = (ctx: Context) => this.canOpen(ctx)
}

@scalan.Liftable
trait SigmaDslBuilder {
  def Colls: CollBuilder
  def Monoids: MonoidBuilder
  def Costing: CostedBuilder
  def CostModel: CostModel

  def costBoxes(bs: Coll[Box]): CostedColl[Box]

  /** Cost of collection with static size elements. */
  def costColWithConstSizedItem[T](xs: Coll[T], len: Int, itemSize: Long): CostedColl[T]

  def costOption[T](opt: Option[T], opCost: Int)(implicit cT: RType[T]): CostedOption[T]

  def verifyZK(cond: => SigmaProp): Boolean

  def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp

  def allOf(conditions: Coll[Boolean]): Boolean
  def allZK(conditions: Coll[SigmaProp]): SigmaProp

  def anyOf(conditions: Coll[Boolean]): Boolean
  def anyZK(conditions: Coll[SigmaProp]): SigmaProp

  def PubKey(base64String: String): SigmaProp

  def sigmaProp(b: Boolean): SigmaProp

  def blake2b256(bytes: Coll[Byte]): Coll[Byte]
  def sha256(bytes: Coll[Byte]): Coll[Byte]

  def byteArrayToBigInt(bytes: Coll[Byte]): BigInteger
  def longToByteArray(l: Long): Coll[Byte]

  def proveDlog(g: ECPoint): SigmaProp
  def proveDHTuple(g: ECPoint, h: ECPoint, u: ECPoint, v: ECPoint): SigmaProp

  def isMember(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Boolean
  def treeLookup(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]]
  def treeModifications(tree: AvlTree, operations: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]]

  def groupGenerator: ECPoint
  def exponentiate(base: ECPoint, exponent: BigInteger): ECPoint
  @Reified("T")
  def substConstants[T](scriptBytes: Coll[Byte], positions: Coll[Int], newValues: Coll[T])(implicit cT: RType[T]): Coll[Byte]
  def decodePoint(encoded: Coll[Byte]): ECPoint
}

