package special.sigma

import java.math.BigInteger

import com.google.common.primitives.Longs
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.custom.sec.SecP256K1Point
import scalan.RType
import scalan.RType._
import scalan.{Internal, NeverInline, OverloadId, Reified}
import scorex.crypto.hash.{Blake2b256, Sha256}
import special.SpecialPredef
import special.collection._

import scala.reflect.ClassTag

class TestBox(
  val id: Coll[Byte],
  val value: Long,
  val bytes: Coll[Byte],
  val bytesWithoutRef: Coll[Byte],
  val propositionBytes: Coll[Byte],
  val registers: Coll[AnyValue]) extends Box
{
  def builder = new TestSigmaDslBuilder
  @NeverInline
  def getReg[T](id: Int)(implicit cT: RType[T]): Option[T] = {
    implicit val tag: ClassTag[T] = cT.classTag
    if (id < 0 || id >= registers.length) return None
    val value = registers(id)
    if (value != null ) {
      // once the value is not null it should be of the right type
      value match {
        case value: TestValue[_] if value.value != null =>
          Some(value.value.asInstanceOf[T])
        case _ =>
          throw new InvalidType(s"Cannot getVar($id): invalid type of value $value at id=$id")
      }
    } else None
  }
  @NeverInline
  def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt
  @NeverInline
  def dataSize = bytes.length

  def creationInfo: (Int, Coll[Byte]) = this.R3[(Int, Coll[Byte])].get

  def tokens: Coll[(Coll[Byte], Long)] = {
    this.R2[Coll[(Coll[Byte], Long)]].get
  }
}

case class TestAvlTree(
    startingDigest: Coll[Byte],
    treeFlags: TreeFlags,
    keyLength: Int,
    valueLengthOpt: Option[Int] = None) extends AvlTree {
  def builder = new TestSigmaDslBuilder
  @NeverInline
  def dataSize = startingDigest.length + 4 + valueLengthOpt.fold(0L)(_ => 4)
  @NeverInline
  def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt
}

class TestValue[T](val value: T) extends AnyValue {
  @NeverInline
  def dataSize = SigmaPredef.dataSize(value)
  @Internal
  override def toString = s"Value($value)"
}

class TestContext(
    val inputs: Array[Box],
    val outputs: Array[Box],
    val height: Int,
    val selfBox: Box,
    val lastBlockUtxoRootHash: AvlTree,
    val minerPubKey: Array[Byte],
    val vars: Array[AnyValue]
) extends Context {
  def builder = new TestSigmaDslBuilder

  @NeverInline
  def HEIGHT = height
  @NeverInline
  def SELF   = selfBox
  @NeverInline
  def INPUTS = builder.Colls.fromArray(inputs)

  @NeverInline
  def OUTPUTS = builder.Colls.fromArray(outputs)

  @NeverInline
  def LastBlockUtxoRootHash = lastBlockUtxoRootHash

  @NeverInline
  def MinerPubKey = builder.Colls.fromArray(minerPubKey)

  @NeverInline
  def getVar[T](id: Byte)(implicit cT: RType[T]): Option[T] = {
    implicit val tag: ClassTag[T] = cT.classTag
    if (id < 0 || id >= vars.length) return None
    val value = vars(id)
    if (value != null ) {
      // once the value is not null it should be of the right type
      value match {
        case value: TestValue[_] if value.value != null =>
          Some(value.value.asInstanceOf[T])
        case _ =>
          throw new InvalidType(s"Cannot getVar($id): invalid type of value $value at id=$id")
      }
    } else None
  }

  @NeverInline
  def getConstant[T](id: Byte)(implicit cT: RType[T]): T =
    sys.error(s"Method getConstant is not defined in TestContext. Should be overriden in real context.")

  @NeverInline
  def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt

  @NeverInline
  def dataSize = {
    val inputsSize = INPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
    val outputsSize = OUTPUTS.map(_.dataSize).sum(builder.Monoids.longPlusMonoid)
    8L + (if (SELF == null) 0 else SELF.dataSize) + inputsSize + outputsSize + LastBlockUtxoRootHash.dataSize
  }
}

class TestSigmaDslBuilder extends SigmaDslBuilder {
  // manual fix
  def Colls: CollBuilder = new CollOverArrayBuilder
  def Monoids: MonoidBuilder = new MonoidBuilderInst
  def Costing: CostedBuilder = new CCostedBuilder
  @NeverInline
  def CostModel: CostModel = new TestCostModel

  def costBoxes(bs: Coll[Box]): CostedColl[Box] = {
    val len = bs.length
    val perItemCost = this.CostModel.AccessBox
    val costs = this.Colls.replicate(len, perItemCost)
    val sizes = bs.map(b => b.dataSize)
    val valuesCost = this.CostModel.CollectionConst
    this.Costing.mkCostedColl(bs, costs, sizes, valuesCost)
  }

  /** Cost of collection with static size elements. */
  def costColWithConstSizedItem[T](xs: Coll[T], len: Int, itemSize: Long): CostedColl[T] = {
    val perItemCost = (len.toLong * itemSize / 1024L + 1L) * this.CostModel.AccessKiloByteOfData.toLong
    val costs = this.Colls.replicate(len, perItemCost.toInt)
    val sizes = this.Colls.replicate(len, itemSize)
    val valueCost = this.CostModel.CollectionConst
    this.Costing.mkCostedColl(xs, costs, sizes, valueCost)
  }

  def costOption[T](opt: Option[T], opCost: Int)(implicit cT: RType[T]): CostedOption[T] = {
    val none = this.Costing.mkCostedNone[T](opCost)
    opt.fold[CostedOption[T]](none)(x => this.Costing.mkCostedSome(this.Costing.costedValue(x, SpecialPredef.some(opCost))))
  }

  @NeverInline
  def verifyZK(proof: => SigmaProp): Boolean = proof.isValid

  @NeverInline
  def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp = {
    if (bound <= 0) return TrivialSigma(true)
    if (bound > props.length) return TrivialSigma(false)
    var nValids = 0
    for (p <- props) {
      if (p.isValid)  nValids += 1
      if (nValids == bound) return TrivialSigma(true)
    }
    TrivialSigma(false)
  }

  @NeverInline
  def allOf(conditions: Coll[Boolean]): Boolean = conditions.forall(c => c)
  @NeverInline
  def anyOf(conditions: Coll[Boolean]): Boolean = conditions.exists(c => c)

  @NeverInline
  def allZK(proofs: Coll[SigmaProp]): SigmaProp = new TrivialSigma(proofs.forall(p => p.isValid))
  @NeverInline
  def anyZK(proofs: Coll[SigmaProp]): SigmaProp = new TrivialSigma(proofs.exists(p => p.isValid))

  @NeverInline
  def sigmaProp(b: Boolean): SigmaProp = TrivialSigma(b)

  @NeverInline
  def blake2b256(bytes: Coll[Byte]): Coll[Byte] = Colls.fromArray(Blake2b256.hash(bytes.toArray))

  @NeverInline
  def sha256(bytes: Coll[Byte]): Coll[Byte] = Colls.fromArray(Sha256.hash(bytes.toArray))

  @NeverInline
  def PubKey(base64String: String): SigmaProp = ???

  @NeverInline
  def byteArrayToBigInt(bytes: Coll[Byte]): BigInteger = {
    val dlogGroupOrder = __curve__.getN
    val bi = new BigInteger(1, bytes.toArray)
    if (bi.compareTo(dlogGroupOrder) == 1) {
      throw new RuntimeException(s"BigInt value exceeds the order of the dlog group (${__curve__}). Expected to be less than: $dlogGroupOrder, actual: $bi")
    }
    bi
  }

  @NeverInline
  def longToByteArray(l: Long): Coll[Byte] = Colls.fromArray(Longs.toByteArray(l))

  @NeverInline
  def proveDlog(g: SecP256K1Point): SigmaProp = new ProveDlogEvidence(g)

  @NeverInline
  def proveDHTuple(g: SecP256K1Point, h: SecP256K1Point, u: SecP256K1Point, v: SecP256K1Point): SigmaProp = ???

  @NeverInline
  def isMember(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Boolean = treeLookup(tree, key, proof).isDefined

  @NeverInline
  def treeLookup(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = ???

  @NeverInline
  def treeModifications(tree: AvlTree, operations: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = ???

  @Internal val __curve__ = CustomNamedCurves.getByName("secp256k1")
  @Internal val __g__ = __curve__.getG.asInstanceOf[SecP256K1Point]

  @NeverInline
  def groupGenerator: SecP256K1Point = __g__

  @NeverInline
  def exponentiate(base: SecP256K1Point, exponent: BigInteger): SecP256K1Point = ???

  @Reified("T")
  @NeverInline
  override def substConstants[T](scriptBytes: Coll[Byte],
      positions: Coll[Int],
      newValues: Coll[T])
      (implicit cT: RType[T]): Coll[Byte] = ???

  @NeverInline
  override def decodePoint(encoded: Coll[Byte]): SecP256K1Point =
    __curve__.getCurve.decodePoint(encoded.toArray).asInstanceOf[SecP256K1Point]
}

trait DefaultSigma extends SigmaProp {
  def builder = new TestSigmaDslBuilder
  @NeverInline
  @OverloadId("and_sigma")
  def &&(other: SigmaProp): SigmaProp = new TrivialSigma(isValid && other.isValid)

  @NeverInline
  @OverloadId("and_bool")
  def &&(other: Boolean): SigmaProp = new TrivialSigma(isValid && other)

  @NeverInline
  @OverloadId("or_sigma")
  def ||(other: SigmaProp): SigmaProp = new TrivialSigma(isValid || other.isValid)

  @NeverInline
  @OverloadId("or_bool")
  def ||(other: Boolean): SigmaProp = new TrivialSigma(isValid || other)

  @NeverInline
  def lazyAnd(other: => SigmaProp): SigmaProp = new TrivialSigma(isValid && other.isValid)
  @NeverInline
  def lazyOr(other: => SigmaProp): SigmaProp = new TrivialSigma(isValid || other.isValid)
}

/**NOTE: this should extend SigmaProp because semantically it subclass of SigmaProp
  * and DefaultSigma is used just to mixin implementations. */
case class TrivialSigma(val _isValid: Boolean) extends SigmaProp with DefaultSigma {
  @NeverInline
  def propBytes: Coll[Byte] = builder.Colls.fromItems(if(isValid) 1 else 0)
  @NeverInline
  def isValid: Boolean = _isValid
  @NeverInline
  @OverloadId("and_sigma")
  override def &&(other: SigmaProp) = super.&&(other)
  @NeverInline
  @OverloadId("and_bool")
  override def &&(other: Boolean) = super.&&(other)
  @NeverInline
  @OverloadId("or_sigma")
  override def ||(other: SigmaProp) = super.||(other)
  @NeverInline
  @OverloadId("or_bool")
  override def ||(other: Boolean) = super.||(other)
  @NeverInline
  override def lazyAnd(other: => SigmaProp) = super.lazyAnd(other)
  @NeverInline
  override def lazyOr(other: => SigmaProp) = super.lazyOr(other)
}

case class ProveDlogEvidence(val value: SecP256K1Point) extends SigmaProp with DefaultSigma {
  @NeverInline
  def propBytes: Coll[Byte] = new CollOverArray(value.getEncoded(true))
  @NeverInline
  def isValid: Boolean = true
  @NeverInline
  @OverloadId("and_sigma")
  override def &&(other: SigmaProp) = super.&&(other)
  @NeverInline
  @OverloadId("and_bool")
  override def &&(other: Boolean) = super.&&(other)
  @NeverInline
  @OverloadId("or_sigma")
  override def ||(other: SigmaProp) = super.||(other)
  @NeverInline
  @OverloadId("or_bool")
  override def ||(other: Boolean) = super.||(other)
  @NeverInline
  override def lazyAnd(other: => SigmaProp) = super.lazyAnd(other)
  @NeverInline
  override def lazyOr(other: => SigmaProp) = super.lazyOr(other)
}

case class ProveDHTEvidence(val gv: SecP256K1Point, val hv: SecP256K1Point, val uv: SecP256K1Point, val vv: SecP256K1Point) extends SigmaProp with DefaultSigma {
  @NeverInline
  def propBytes: Coll[Byte] = new CollOverArray(gv.getEncoded(true))
  @NeverInline
  def isValid: Boolean = true
  @NeverInline
  @OverloadId("and_sigma")
  override def &&(other: SigmaProp) = super.&&(other)
  @NeverInline
  @OverloadId("and_bool")
  override def &&(other: Boolean) = super.&&(other)
  @NeverInline
  @OverloadId("or_sigma")
  override def ||(other: SigmaProp) = super.||(other)
  @NeverInline
  @OverloadId("or_bool")
  override def ||(other: Boolean) = super.||(other)
  @NeverInline
  override def lazyAnd(other: => SigmaProp) = super.lazyAnd(other)
  @NeverInline
  override def lazyOr(other: => SigmaProp) = super.lazyOr(other)
}

trait DefaultContract extends SigmaContract {
  def builder: SigmaDslBuilder = new TestSigmaDslBuilder
}


