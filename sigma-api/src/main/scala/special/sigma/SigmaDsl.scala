package special.sigma

import java.math.BigInteger

import org.bouncycastle.math.ec.custom.sec.SecP256K1Point

import scala.reflect.ClassTag
import special.collection._
import scalan._
import scalan.Internal
import scalan.RType

@scalan.Liftable
trait CostModel {
  def AccessBox: Int //= "AccessBox: Context => Box"
  def AccessAvlTree: Int //= "AccessAvlTree: Context => AvlTree"

  def GetVar: Int // = "ContextVar: (Context, Byte) => Option[T]"
  def DeserializeVar: Int // = "DeserializeVar: (Context, Byte) => Option[T]"

  def GetRegister: Int // = "AccessRegister: (Box,Byte) => Option[T]"
  def DeserializeRegister: Int // = "DeserializeRegister: (Box,Byte) => Option[T]"

  def SelectField: Int // = "SelectField"
  def CollectionConst: Int // = "Const: () => Array[IV]"
  def AccessKiloByteOfData: Int // = "AccessKiloByteOfData"
  @Reified("T") def dataSize[T](x: T)(implicit cT: ClassTag[T]): Long
  /** Size of public key in bytes */
  def PubKeySize: Long = 32
}

trait DslBuilder {}
trait DslObject {
  def builder: SigmaDslBuilder
}

@scalan.Liftable
trait SigmaProp extends DslObject {
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
trait Box extends DslObject {
  def id: Coll[Byte]
  def value: Long
  def bytes: Coll[Byte]
  def bytesWithoutRef: Coll[Byte]
  def propositionBytes: Coll[Byte]
  def cost: Int
  def dataSize: Long
  def registers: Coll[AnyValue]

  def getReg[@Reified T](i: Int)(implicit cT: RType[T]): Option[T]

  /** Mandatory: Monetary value, in Ergo tokens */
  def R0[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](0)

  /** Mandatory: Guarding script */
  def R1[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](1)

  /** Mandatory: Secondary tokens */
  def R2[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](2)

  /** Mandatory: Reference to transaction and output id where the box was created */
  def R3[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](3)

  // Non-mandatory registers
  def R4[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](4)
  def R5[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](5)
  def R6[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](6)
  def R7[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](7)
  def R8[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](8)
  def R9[@Reified T](implicit cT:RType[T]): Option[T] = this.getReg[T](9)

  def tokens: Coll[(Coll[Byte], Long)]
  def creationInfo: (Int, Coll[Byte])
  @Internal
  override def toString = s"Box(id=$id; value=$value; cost=$cost; size=$dataSize; regs=$registers)"
}

@scalan.Liftable
trait TreeFlags extends DslObject {
  def insertAllowed: Boolean
  def updateAllowed: Boolean
  def removeAllowed: Boolean
}

@scalan.Liftable
trait AvlTree extends DslObject {
  def startingDigest: Coll[Byte]
  def treeFlags: TreeFlags
  def keyLength: Int
  def valueLengthOpt: Option[Int]
  def cost: Int
  def dataSize: Long

  def updateDigest(newDigest: Coll[Byte]): AvlTree
}

@scalan.Liftable
trait Context {
  def builder: SigmaDslBuilder
  def OUTPUTS: Coll[Box]
  def INPUTS: Coll[Box]
  def HEIGHT: Int
  def SELF: Box
  def LastBlockUtxoRootHash: AvlTree
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

  def proveDlog(g: SecP256K1Point): SigmaProp = this.builder.proveDlog(g)
  def proveDHTuple(g: SecP256K1Point, h: SecP256K1Point, u: SecP256K1Point, v: SecP256K1Point): SigmaProp = this.builder.proveDHTuple(g, h, u, v)

  def isMember(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Boolean = this.builder.isMember(tree, key, proof)
  def treeLookup(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = this.builder.treeLookup(tree, key, proof)
  def treeModifications(tree: AvlTree, operations: Coll[Byte], proof: Coll[Byte]): Option[AvlTree] = this.builder.treeModifications(tree, operations, proof)

  def groupGenerator: SecP256K1Point = this.builder.groupGenerator
  def exponentiate(base: SecP256K1Point, exponent: BigInteger): SecP256K1Point = this.builder.exponentiate(base, exponent)

  @clause def canOpen(ctx: Context): Boolean

  def asFunction: Context => Boolean = (ctx: Context) => this.canOpen(ctx)
}

@scalan.Liftable
trait SigmaDslBuilder extends DslBuilder {
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

  def proveDlog(g: SecP256K1Point): SigmaProp
  def proveDHTuple(g: SecP256K1Point, h: SecP256K1Point, u: SecP256K1Point, v: SecP256K1Point): SigmaProp

  def isMember(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Boolean
  def treeLookup(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]]
  def treeModifications(tree: AvlTree, operations: Coll[Byte], proof: Coll[Byte]): Option[AvlTree]
  def treeInserts(tree: AvlTree, operations: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree]
  def treeRemovals(tree: AvlTree, operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree]


  def groupGenerator: SecP256K1Point
  def exponentiate(base: SecP256K1Point, exponent: BigInteger): SecP256K1Point
  @Reified("T")
  def substConstants[T](scriptBytes: Coll[Byte], positions: Coll[Int], newValues: Coll[T])(implicit cT: RType[T]): Coll[Byte]
  def decodePoint(encoded: Coll[Byte]): SecP256K1Point
}

