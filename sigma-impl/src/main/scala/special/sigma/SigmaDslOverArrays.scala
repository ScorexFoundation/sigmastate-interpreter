package special.sigma

import java.math.BigInteger
import com.google.common.primitives.Longs
import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.math.ec.custom.sec.SecP256K1Point
import scalan.RType._
import scalan.{RType, Internal, NeverInline, Reified}
import scorex.crypto.hash.{Sha256, Blake2b256}
import special.SpecialPredef
import special.collection._
import scalan.util.Extensions.BigIntegerOps
import sigmastate.VersionContext
import sigmastate.VersionContext.JitActivationVersion
import spire.syntax.all.cfor

// TODO refactor: this class is not necessary and can be removed
class TestSigmaDslBuilder extends SigmaDslBuilder {
  // manual fix
  def Colls: CollBuilder = new CollOverArrayBuilder
  def Monoids: MonoidBuilder = new MonoidBuilderInst

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
  override def xorOf(conditions: Coll[Boolean]): Boolean = {
    if (VersionContext.current.isJitActivated) {
      val len = conditions.length
      if (len == 0) false
      else if (len == 1) conditions(0)
      else {
        var res = conditions(0)
        cfor(1)(_ < len, _ + 1) { i =>
          res ^= conditions(i)
        }
        res
      }
    } else {
      // This is buggy version used in v4.x interpreter (for ErgoTrees v0, v1)
      conditions.toArray.distinct.length == 2
    }
  }

  @NeverInline
  def sigmaProp(b: Boolean): SigmaProp = MockSigma(b)

  @NeverInline
  def blake2b256(bytes: Coll[Byte]): Coll[Byte] = Colls.fromArray(Blake2b256.hash(bytes.toArray))

  @NeverInline
  def sha256(bytes: Coll[Byte]): Coll[Byte] = Colls.fromArray(Sha256.hash(bytes.toArray))

  @NeverInline
  def byteArrayToBigInt(bytes: Coll[Byte]): BigInt = {
    val bi = new BigInteger(bytes.toArray).to256BitValueExact
    this.BigInt(bi)
  }

  @NeverInline
  def longToByteArray(l: Long): Coll[Byte] = Colls.fromArray(Longs.toByteArray(l))
  @NeverInline
  def byteArrayToLong(bytes: Coll[Byte]): Long = Longs.fromByteArray(bytes.toArray)

  @NeverInline
  def proveDlog(g: GroupElement): SigmaProp = MockProveDlog(true, Colls.emptyColl[Byte])

  @NeverInline
  def proveDHTuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp = ???

  @Internal val __curve__ = CustomNamedCurves.getByName("secp256k1")
  @Internal val __g__ = __curve__.getG.asInstanceOf[SecP256K1Point]

  @NeverInline
  def groupGenerator: GroupElement = this.GroupElement(__g__)

  @Reified("T")
  @NeverInline
  override def substConstants[T](scriptBytes: Coll[Byte],
      positions: Coll[Int],
      newValues: Coll[T]): Coll[Byte] = ???

  @NeverInline
  override def decodePoint(encoded: Coll[Byte]): GroupElement =
    this.GroupElement(__curve__.getCurve.decodePoint(encoded.toArray))

  @NeverInline
  @Internal
  override def BigInt(n: BigInteger): BigInt = SpecialPredef.rewritableMethod

  @NeverInline
  @Internal
  override def toBigInteger(n: BigInt): BigInteger = n.asInstanceOf[TestBigInt].value

  /** Create DSL's group element from existing `org.bouncycastle.math.ec.ECPoint`. */
  @NeverInline
  @Internal
  def GroupElement(p: ECPoint): GroupElement = SpecialPredef.rewritableMethod

  /** Extract `org.bouncycastle.math.ec.ECPoint` from DSL's `GroupElement` type. */
  @NeverInline
  @Internal
  def toECPoint(ge: GroupElement): ECPoint = ge.value

  @NeverInline
  override def avlTree(operationFlags: Byte, digest: Coll[Byte], keyLength: Int, valueLengthOpt: Option[Int]): AvlTree = SpecialPredef.rewritableMethod

  @NeverInline
  override def xor(l: Coll[Byte], r: Coll[Byte]): Coll[Byte] = Colls.xor(l, r)
}

