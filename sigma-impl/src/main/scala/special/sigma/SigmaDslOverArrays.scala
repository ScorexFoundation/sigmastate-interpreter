package special.sigma

import java.math.BigInteger

import com.google.common.primitives.Longs
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECPoint
import scalan.RType
import scalan.RType._
import scalan.{Internal, NeverInline, Reified, OverloadId}
import scorex.crypto.hash.{Sha256, Blake2b256}
import special.SpecialPredef
import special.collection._

case class TestAvlTree(
    startingDigest: Coll[Byte],
    keyLength: Int,
    valueLengthOpt: Option[Int] = None,
    maxNumOperations: Option[Int] = None,
    maxDeletes: Option[Int] = None ) extends AvlTree {
  def builder = new TestSigmaDslBuilder
  @NeverInline
  def dataSize = startingDigest.length + 4 + valueLengthOpt.fold(0L)(_ => 4)
  @NeverInline
  def cost = (dataSize / builder.CostModel.AccessKiloByteOfData.toLong).toInt
  @NeverInline
  def digest: Coll[Byte] = ???
}

class TestValue[T](val value: T) extends AnyValue {
  @NeverInline
  def dataSize = SigmaPredef.dataSize(value)
  @Internal
  override def toString = s"Value($value)"
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
  def atLeast(bound: Int, props: Coll[SigmaProp]): SigmaProp = ???

  @NeverInline
  def allOf(conditions: Coll[Boolean]): Boolean = conditions.forall(c => c)
  @NeverInline
  def anyOf(conditions: Coll[Boolean]): Boolean = conditions.exists(c => c)

  @NeverInline
  def allZK(props: Coll[SigmaProp]): SigmaProp = MockSigma(props.forall(p => p.isValid))
  @NeverInline
  def anyZK(props: Coll[SigmaProp]): SigmaProp = MockSigma(props.exists(p => p.isValid))

  @NeverInline
  def sigmaProp(b: Boolean): SigmaProp = MockSigma(b)

  @NeverInline
  def blake2b256(bytes: Coll[Byte]): Coll[Byte] = Colls.fromArray(Blake2b256.hash(bytes.toArray))

  @NeverInline
  def sha256(bytes: Coll[Byte]): Coll[Byte] = Colls.fromArray(Sha256.hash(bytes.toArray))

  @NeverInline
  def PubKey(base64String: String): SigmaProp = ???

  @NeverInline
  def byteArrayToBigInt(bytes: Coll[Byte]): BigInt = {
    val dlogGroupOrder = __curve__.getN
    val bi = new BigInteger(1, bytes.toArray)
    if (bi.compareTo(dlogGroupOrder) == 1) {
      throw new RuntimeException(s"BigInt value exceeds the order of the dlog group (${__curve__}). Expected to be less than: $dlogGroupOrder, actual: $bi")
    }
    this.BigInt(bi)
  }

  @NeverInline
  def longToByteArray(l: Long): Coll[Byte] = Colls.fromArray(Longs.toByteArray(l))

  @NeverInline
  def proveDlog(g: GroupElement): SigmaProp = MockProveDlog(true, Colls.emptyColl[Byte])

  @NeverInline
  def proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp = ???

  @NeverInline
  def isMember(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Boolean = treeLookup(tree, key, proof).isDefined

  @NeverInline
  def treeLookup(tree: AvlTree, key: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = ???

  @NeverInline
  def treeModifications(tree: AvlTree, operations: Coll[Byte], proof: Coll[Byte]): Option[Coll[Byte]] = ???

  @Internal val __curve__ = CustomNamedCurves.getByName("curve25519")
  @Internal val __g__ = __curve__.getG

  @NeverInline
  def groupGenerator: GroupElement = this.GroupElement(__g__)

  @Reified("T")
  @NeverInline
  override def substConstants[T](scriptBytes: Coll[Byte],
      positions: Coll[Int],
      newValues: Coll[T])
      (implicit cT: RType[T]): Coll[Byte] = ???

  @NeverInline
  override def decodePoint(encoded: Coll[Byte]): GroupElement =
    this.GroupElement(__curve__.getCurve.decodePoint(encoded.toArray))

  @NeverInline
  override def BigInt(n: BigInteger): BigInt = SpecialPredef.rewritableMethod

  @NeverInline
  override def toBigInteger(n: BigInt): BigInteger = n.asInstanceOf[TestBigInt].value

  /** Create DSL's group element from existing `org.bouncycastle.math.ec.ECPoint`. */
  @NeverInline
  def GroupElement(p: ECPoint): GroupElement = SpecialPredef.rewritableMethod

  /** Extract `org.bouncycastle.math.ec.ECPoint` from DSL's `GroupElement` type. */
  @NeverInline
  def toECPoint(ge: GroupElement): ECPoint = ge.value
}

