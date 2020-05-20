package special.sigma

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint

import special.collection._
import scalan._

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
  /** Size of public key in bytes */
  def PubKeySize: Long
}

/**
  * All `modQ` operations assume that Q is a global constant (an order of the only one cryptographically strong group
  * which is used for all cryptographic operations).
  * So it is globally and implicitly used in all methods.
  * */
@scalan.Liftable
@WithMethodCallRecognizers
trait BigInt {
  @Internal
  private[sigma] def value: BigInteger
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

  /** Subtracts this number with `other` by module Q.
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

  /** Returns the signum function of this BigInt.
    *
    * @return -1, 0 or 1 as the value of this BigInt is negative, zero or
    *         positive.
    */
  def signum: Int

  /** Returns a BigInt whose value is {@code (this + that)}.
    *
    * @param  that value to be added to this BigInt.
    * @return { @code this + that}
    */
  def add(that: BigInt): BigInt
  @Internal def +(that: BigInt): BigInt = add(that)

  /** Returns a BigInt whose value is {@code (this - that)}.
    *
    * @param  that value to be subtracted from this BigInt.
    * @return { @code this - that}
    */
  def subtract(that: BigInt): BigInt
  @Internal def -(that: BigInt): BigInt = subtract(that)

  /** Returns a BigInt whose value is {@code (this * that)}.
    *
    * @implNote An implementation may offer better algorithmic
    *           performance when { @code that == this}.
    * @param  that value to be multiplied by this BigInt.
    * @return { @code this * that}
    */
  def multiply(that: BigInt): BigInt
  @Internal def *(that: BigInt): BigInt = multiply(that)

  /** Returns a BigInt whose value is {@code (this / that)}.
    *
    * @param  that value by which this BigInt is to be divided.
    * @return { @code this / that}
    * @throws ArithmeticException if { @code that} is zero.
    */
  def divide(that: BigInt): BigInt
  @Internal def /(that: BigInt): BigInt = divide(that)

  /**
    * Returns a BigInt whose value is {@code (this mod m}).  This method
    * differs from {@code remainder} in that it always returns a
    * <i>non-negative</i> BigInteger.
    *
    * @param  m the modulus.
    * @return { @code this mod m}
    * @throws ArithmeticException { @code m} &le; 0
    * @see #remainder
    */
  def mod(m: BigInt): BigInt
  @Internal def %(m: BigInt): BigInt = mod(m)

  /**
    * Returns a BigInt whose value is {@code (this % that)}.
    *
    * @param  that value by which this BigInt is to be divided, and the
    *             remainder computed.
    * @return { @code this % that}
    * @throws ArithmeticException if { @code that} is zero.
    */
  def remainder(that: BigInt): BigInt

  /**
    * Returns the minimum of this BigInteger and {@code val}.
    *
    * @param  val value with which the minimum is to be computed.
    * @return the BigInteger whose value is the lesser of this BigInteger and
    *         { @code val}.  If they are equal, either may be returned.
    */
  def min(that: BigInt): BigInt

  /**
    * Returns the maximum of this BigInteger and {@code val}.
    *
    * @param  val value with which the maximum is to be computed.
    * @return the BigInteger whose value is the greater of this and
    *         { @code val}.  If they are equal, either may be returned.
    */
  def max(that: BigInt): BigInt

  /** Returns a BigInt whose value is {@code (-this)}.
    * @return { @code -this}
    */
  def negate(): BigInt
}

/** Base class for points on elliptic curves.
  */
@scalan.Liftable
@WithMethodCallRecognizers
trait GroupElement {
  @Internal
  private[sigma] def value: ECPoint

  def isInfinity: Boolean

  /** Exponentiate this <code>GroupElement</code> to the given number.
    * @param k The power.
    * @return <code>this to the power of k</code>.
    * @since 2.0
    */
  def exp(k: BigInt): GroupElement

  /** Group operation. */
  def multiply(that: GroupElement): GroupElement

  /** Inverse element in the group. */
  def negate: GroupElement

  /**
    * Get an encoding of the point value.
    *
    * @return the point encoding
    */
  def getEncoded: Coll[Byte]
}

/** Proposition which can be proven and verified by sigma protocol. */
@scalan.Liftable
@WithMethodCallRecognizers
trait SigmaProp {
  def isValid: Boolean
  /** Serialized bytes of this sigma proposition taken as ErgoTree and then serialized. */
  def propBytes: Coll[Byte]

  /** Logical AND between this SigmaProp and other SigmaProp.
    * This constructs a new CAND node of sigma tree with two children. */
  @OverloadId("and_sigma") def &&(other: SigmaProp): SigmaProp

  /** Logical AND between this `SigmaProp` and `Boolean` value on the right.
    * The boolean value will be wrapped into `SigmaProp` using `sigmaProp` function.
    * This constructs a new CAND node of sigma tree with two children. */
  @OverloadId("and_bool")  def &&(other: Boolean): SigmaProp

  /** Logical OR between this SigmaProp and other SigmaProp.
    * This constructs a new COR node of sigma tree with two children. */
  @OverloadId("or_sigma") def ||(other: SigmaProp): SigmaProp

  /** Logical OR between this `SigmaProp` and `Boolean` value on the right.
    * The boolean value will be wrapped into `SigmaProp` using `sigmaProp` function.
    * This constructs a new COR node of sigma tree with two children. */
  @OverloadId("or_bool")  def ||(other: Boolean): SigmaProp
}

@scalan.Liftable
@WithMethodCallRecognizers
trait AnyValue {
  def value: Any
  def tVal: RType[Any]
}

@scalan.Liftable
@WithMethodCallRecognizers
trait Box {
  /** Blake2b256 hash of this box's content, basically equals to `blake2b256(bytes)` */
  def id: Coll[Byte]

  /** Mandatory: Monetary value, in Ergo tokens (NanoErg unit of measure)*/
  def value: Long

  /** Serialized bytes of guarding script, which should be evaluated to true in order to
    * open this box. (aka spend it in a transaction)*/
  def propositionBytes: Coll[Byte]

  /** Serialized bytes of this box's content, including proposition bytes. */
  def bytes: Coll[Byte]

  /** Serialized bytes of this box's content, excluding transactionId and index of output. */
  def bytesWithoutRef: Coll[Byte]

  def registers: Coll[AnyValue]

  /** Extracts register by id and type.
    * ErgoScript is typed, so accessing a register is an operation which involves some
    * expected type given in brackets. Thus `SELF.R4[Int]` expression should evaluate to a
    * valid value of the `Option[Int]` type.
    *
    * For example `val x = SELF.R4[Int]` expects the
    * register, if it is present, to have type `Int`. At runtime the corresponding type
    * descriptor is passed as `cT` parameter.
    *
    * There are three cases:
    * 1) If the register doesn't exist.
    *   Then `val x = SELF.R4[Int]` succeeds and returns the None value, which conforms to
    *   any value of type `Option[T]` for any T. (In the example above T is equal to
    *   `Int`). Calling `x.get` fails when x is equal to None, but `x.isDefined`
    *   succeeds and returns `false`.
    * 2) If the register contains a value `v` of type `Int`.
    *   Then `val x = SELF.R4[Int]` succeeds and returns `Some(v)`, which is a valid value
    *   of type `Option[Int]`. In this case, calling `x.get` succeeds and returns the
    *   value `v` of type `Int`. Calling `x.isDefined` returns `true`.
    * 3) If the register contains a value `v` of type T other then `Int`.
    *   Then `val x = SELF.R4[Int]` fails, because there is no way to return a valid value
    *   of type `Option[Int]`. The value of register is present, so returning it as None
    *   would break the typed semantics of registers collection.
    *
    * In some use cases one register may have values of different types. To access such
    * register an additional register can be used as a tag.
    *
    * <pre class="stHighlight">
    *   val tagOpt = SELF.R5[Int]
    *   val res = if (tagOpt.isDefined) {
    *     val tag = tagOpt.get
    *     if (tag == 1) {
    *       val x = SELF.R4[Int].get
    *       // compute res using value x is of type Int
    *     } else if (tag == 2) {
    *       val x = SELF.R4[GroupElement].get
    *       // compute res using value x is of type GroupElement
    *     } else if (tag == 3) {
    *       val x = SELF.R4[ Array[Byte] ].get
    *       // compute res using value x of type Array[Byte]
    *     } else {
    *       // compute `res` when `tag` is not 1, 2 or 3
    *     }
    *   }
    *   else {
    *     // compute value of res when register is not present
    *   }
    * </pre>
    *
    * @param i zero-based identifier of the register.
    * @tparam T expected type of the register.
    * @return Some(value) if the register is defined AND has the given type.
    *         None otherwise
    * @throws special.sigma.InvalidType exception when the type of the register value is
    *                                   different from cT.
    * @since 2.0
    */
  def getReg[@Reified T](i: Int)(implicit cT: RType[T]): Option[T]

  /** Mandatory: Monetary value, in Ergo tokens */
  def R0[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](0)

  /** Mandatory: Guarding script */
  def R1[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](1)

  /** Mandatory: Secondary tokens */
  def R2[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](2)

  /** Mandatory: Reference to transaction and output id where the box was created */
  def R3[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](3)

  /** Non-mandatory register */
  def R4[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](4)
  /** Non-mandatory register */
  def R5[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](5)
  /** Non-mandatory register */
  def R6[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](6)
  /** Non-mandatory register */
  def R7[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](7)
  /** Non-mandatory register */
  def R8[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](8)
  /** Non-mandatory register */
  def R9[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](9)

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
  def executeFromRegister[@Reified T](regId: Byte)(implicit cT:RType[T]): T

  @Internal
  override def toString = s"Box(id=$id; value=$value; regs=$registers)"
}

/** Type of data which efficiently authenticates potentially huge dataset having key-value dictionary interface.
  * Only root hash of dynamic AVL+ tree, tree height, key length, optional value length, and access flags are stored
  * in an instance of the datatype.
  *
  * Please note that standard hash function from `scorex.crypto.hash` is used, and height is stored along with root hash of
  * the tree, thus `digest` size is always CryptoConstants.hashLength + 1 bytes.
  */
@scalan.Liftable
trait AvlTree {
  /** Returns digest of the state represented by this tree.
    * Authenticated tree digest = root hash bytes ++ tree height
    * @since 2.0
    */
  def digest: Coll[Byte]

  /** Flags of enabled operations packed in single byte.
    * isInsertAllowed == (enabledOperations & 0x01) != 0
    * isUpdateAllowed == (enabledOperations & 0x02) != 0
    * isRemoveAllowed == (enabledOperations & 0x04) != 0
    */
  def enabledOperations: Byte

  /** All the elements under the tree have the same length of the keys */
  def keyLength: Int
  
  /** If non-empty, all the values under the tree are of the same length. */
  def valueLengthOpt: Option[Int]

  /** Checks if Insert operation is allowed for this tree instance. */
  def isInsertAllowed: Boolean

  /** Checks if Update operation is allowed for this tree instance. */
  def isUpdateAllowed: Boolean

  /** Checks if Remove operation is allowed for this tree instance. */
  def isRemoveAllowed: Boolean

  /** Replace digest of this tree producing a new tree.
    * Since AvlTree is immutable, this tree instance remains unchanged.
    * @param newDigest   a new digest
    * @return a copy of this AvlTree instance where `this.digest` replaced by `newDigest`
    */
  def updateDigest(newDigest: Coll[Byte]): AvlTree

  /** Enable/disable operations of this tree producing a new tree.
    * Since AvlTree is immutable, `this` tree instance remains unchanged.
    * @param newOperations  a new flags which specify available operations on a new tree.
    * @return               a copy of this AvlTree instance where `this.enabledOperations`
    *                       replaced by `newOperations`
    */
  def updateOperations(newOperations: Byte): AvlTree

  /** Checks if an entry with key `key` exists in this tree using proof `proof`.
    * Throws exception if proof is incorrect
    *
    * @note CAUTION! Does not support multiple keys check, use [[getMany]] instead.
    * Return `true` if a leaf with the key `key` exists
    * Return `false` if leaf with provided key does not exist.
    * @param key    a key of an element of this authenticated dictionary.
    * @param proof
    */
  def contains(key: Coll[Byte], proof: Coll[Byte]): Boolean

  /** Perform a lookup of key `key` in this tree using proof `proof`.
    * Throws exception if proof is incorrect
    *
    * @note CAUTION! Does not support multiple keys check, use [[getMany]] instead.
    * Return Some(bytes) of leaf with key `key` if it exists
    * Return None if leaf with provided key does not exist.
    * @param key    a key of an element of this authenticated dictionary.
    * @param proof
    */
  def get(key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]]

  /** Perform a lookup of many keys `keys` in this tree using proof `proof`.
    *
    * @note CAUTION! Keys must be ordered the same way they were in lookup before proof was generated.
    * For each key return Some(bytes) of leaf if it exists and None if is doesn't.
    * @param keys    keys of elements of this authenticated dictionary.
    * @param proof
    */
  def getMany(keys: Coll[Coll[Byte]], proof: Coll[Byte]): Coll[Option[Coll[Byte]]]

  /** Perform insertions of key-value entries into this tree using proof `proof`.
    * Throws exception if proof is incorrect
    *
    * @note CAUTION! Pairs must be ordered the same way they were in insert ops before proof was generated.
    * Return Some(newTree) if successful
    * Return None if operations were not performed.
    * @param operations   collection of key-value pairs to insert in this authenticated dictionary.
    * @param proof
    */
  def insert(operations: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree]

  /** Perform updates of key-value entries into this tree using proof `proof`.
    * Throws exception if proof is incorrect
    *
    * @note CAUTION! Pairs must be ordered the same way they were in update ops before proof was generated.
    * Return Some(newTree) if successful
    * Return None if operations were not performed.
    * @param operations   collection of key-value pairs to update in this authenticated dictionary.
    * @param proof
    */
  def update(operations: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree]

  /** Perform removal of entries into this tree using proof `proof`.
    * Throws exception if proof is incorrect
    * Return Some(newTree) if successful
    * Return None if operations were not performed.
    *
    * @note CAUTION! Keys must be ordered the same way they were in remove ops before proof was generated.
    * @param operations   collection of keys to remove from this authenticated dictionary.
    * @param proof
    */
  def remove(operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree]
}

/** Only header fields that can be predicted by a miner.
  * @since 2.0
  */
@scalan.Liftable
trait PreHeader { // Testnet2
  /** Block version, to be increased on every soft and hardfork. */
  def version: Byte

  /** Id of parent block */
  def parentId: Coll[Byte] // ModifierId

  /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
  def timestamp: Long

  /** Current difficulty in a compressed view.
    * NOTE: actually it is unsigned Int*/
  def nBits: Long  // actually it is unsigned Int

  /** Block height */
  def height: Int

  /** Miner public key. Should be used to collect block rewards. */
  def minerPk: GroupElement

  def votes: Coll[Byte]
}

/** Represents data of the block header available in Sigma propositions.
  * @since 2.0
  */
@scalan.Liftable
trait Header {
  /** Bytes representation of ModifierId of this Header */
  def id: Coll[Byte]

  /** Block version, to be increased on every soft and hardfork. */
  def version: Byte

  /** Bytes representation of ModifierId of the parent block */
  def parentId: Coll[Byte] //

  /** Hash of ADProofs for transactions in a block */
  def ADProofsRoot: Coll[Byte] // Digest32. Can we build AvlTree out of it?

  /** AvlTree) of a state after block application */
  def stateRoot: AvlTree

  /** Root hash (for a Merkle tree) of transactions in a block. */
  def transactionsRoot: Coll[Byte]  // Digest32

  /** Block timestamp (in milliseconds since beginning of Unix Epoch) */
  def timestamp: Long

  /** Current difficulty in a compressed view.
    * NOTE: actually it is unsigned Int*/
  def nBits: Long

  /** Block height */
  def height: Int

  /** Root hash of extension section */
  def extensionRoot: Coll[Byte] // Digest32

  /** Miner public key. Should be used to collect block rewards.
    * Part of Autolykos solution. */
  def minerPk: GroupElement

  /** One-time public key. Prevents revealing of miners secret. */
  def powOnetimePk: GroupElement

  /** nonce */
  def powNonce: Coll[Byte]

  /** Distance between pseudo-random number, corresponding to nonce `powNonce` and a secret,
    * corresponding to `minerPk`. The lower `powDistance` is, the harder it was to find this solution. */
  def powDistance: BigInt

  def votes: Coll[Byte] //3 bytes
}

/** Represents data available in Sigma language using `CONTEXT` global variable*/
@scalan.Liftable
@WithMethodCallRecognizers
trait Context {
  def builder: SigmaDslBuilder

  /** A collection of outputs of the current transaction. */
  def OUTPUTS: Coll[Box]

  /** A collection of inputs of the current transaction, the transaction where selfBox is one of the inputs. */
  def INPUTS: Coll[Box]

  /** A collection of inputs of the current transaction that will not be spent. */
  def dataInputs: Coll[Box]

  /** Height (block number) of the block which is currently being validated. */
  def HEIGHT: Int

  /** Box whose proposition is being currently executing */
  def SELF: Box

  /** Zero based index in `inputs` of `selfBox`. -1 if self box is not in the INPUTS collection. */
  def selfBoxIndex: Int

  /** Authenticated dynamic dictionary digest representing Utxo state before current state. */
  def LastBlockUtxoRootHash: AvlTree

  /** A fixed number of last block headers in descending order (first header is the newest one)
    * @since 2.0
    */
  def headers: Coll[Header]

  /**
    * @since 2.0
    */
  def preHeader: PreHeader

  def minerPubKey: Coll[Byte]
  def getVar[T](id: Byte)(implicit cT: RType[T]): Option[T]
  def vars: Coll[AnyValue]
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

  def xorOf(conditions: Coll[Boolean]): Boolean = this.builder.xorOf(conditions)

  def PubKey(base64String: String): SigmaProp = this.builder.PubKey(base64String)

  def sigmaProp(b: Boolean): SigmaProp = this.builder.sigmaProp(b)

  def blake2b256(bytes: Coll[Byte]): Coll[Byte] = this.builder.blake2b256(bytes)
  def sha256(bytes: Coll[Byte]): Coll[Byte] = this.builder.sha256(bytes)

  def byteArrayToBigInt(bytes: Coll[Byte]): BigInt = this.builder.byteArrayToBigInt(bytes)
  def longToByteArray(l: Long): Coll[Byte] = this.builder.longToByteArray(l)
  def byteArrayToLong(bytes: Coll[Byte]): Long = this.builder.byteArrayToLong(bytes)

  def proveDlog(g: GroupElement): SigmaProp = this.builder.proveDlog(g)
  def proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp =
    this.builder.proveDHTuple(g, h, u, v)

  def groupGenerator: GroupElement = this.builder.groupGenerator

  def decodePoint(encoded: Coll[Byte]): GroupElement = this.builder.decodePoint(encoded)

  @Reified("T")
  def substConstants[T](scriptBytes: Coll[Byte],
      positions: Coll[Int],
      newValues: Coll[T])
      (implicit cT: RType[T]): Coll[Byte] = this.builder.substConstants(scriptBytes, positions, newValues)
}

@scalan.Liftable
@WithMethodCallRecognizers
trait SigmaDslBuilder {
  def Colls: CollBuilder
  def Monoids: MonoidBuilder
  def Costing: CostedBuilder
  def CostModel: CostModel

  def verifyZK(cond: => SigmaProp): Boolean

  def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp

  def allOf(conditions: Coll[Boolean]): Boolean
  def allZK(conditions: Coll[SigmaProp]): SigmaProp

  def anyOf(conditions: Coll[Boolean]): Boolean
  def anyZK(conditions: Coll[SigmaProp]): SigmaProp

  def xorOf(conditions: Coll[Boolean]): Boolean

  def PubKey(base64String: String): SigmaProp

  def sigmaProp(b: Boolean): SigmaProp

  def blake2b256(bytes: Coll[Byte]): Coll[Byte]
  def sha256(bytes: Coll[Byte]): Coll[Byte]

  def byteArrayToBigInt(bytes: Coll[Byte]): BigInt
  def longToByteArray(l: Long): Coll[Byte]
  def byteArrayToLong(bytes: Coll[Byte]): Long

  def proveDlog(g: GroupElement): SigmaProp
  def proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp

  /**
    * The generator g of the group is an element of the group such that, when written multiplicatively, every element
    * of the group is a power of g.
    * @return the generator of this Dlog group
    */
  def groupGenerator: GroupElement

  @Reified("T")
  def substConstants[T](scriptBytes: Coll[Byte], positions: Coll[Int], newValues: Coll[T])(implicit cT: RType[T]): Coll[Byte]
  def decodePoint(encoded: Coll[Byte]): GroupElement

  /** Create DSL big integer from existing `java.math.BigInteger`*/
  @Internal def BigInt(n: BigInteger): BigInt

  /** Extract `java.math.BigInteger` from DSL's `BigInt` type*/
  @Internal def toBigInteger(n: BigInt): BigInteger

  /** Construct a new authenticated dictionary with given parameters and tree root digest. */
  def avlTree(operationFlags: Byte, digest: Coll[Byte], keyLength: Int, valueLengthOpt: Option[Int]): AvlTree

  def xor(l: Coll[Byte], r: Coll[Byte]): Coll[Byte]
}

