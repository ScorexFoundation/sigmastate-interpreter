package sigmastate

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.ergoplatform.ErgoScriptPredef.TrueProp
import org.ergoplatform.settings.ErgoAlgos
import scalan.RType
import scorex.crypto.hash.Digest32
import sigmastate.basics.DLogProtocol
import sigmastate.interpreter.CryptoConstants
import special.sigma.SigmaTestingData
import sigmastate.SType.AnyOps
import sigmastate.Values.{ShortConstant, LongConstant, BigIntConstant, AvlTreeConstant, IntConstant, ByteConstant}
import sigmastate.eval.SigmaDsl
import special.collection.Coll
import special.sigma.BigInt

class TypesSpecification extends SigmaTestingData {

  def Coll[T](items: T*)(implicit cT: RType[T]) = SigmaDsl.Colls.fromItems(items:_*)

  def check(tpe: SType, v: Any, exp: Long) =
    tpe.dataSize(v.asWrappedType) shouldBe exp

  def checkColl[T](elemTpe: SType, arr: Coll[T], exp: Long) =
    check(sigmastate.SCollection(elemTpe), arr, exp)

  property("SType.dataSize") {
    check(SUnit, (), 1)
    check(SBoolean, true, 1)
    check(SByte, 1.toByte, 1)
    check(SShort, 1.toShort, 2)
    check(SInt, 1, 4)
    check(SLong, 1, 8)
    check(SString, "abc", 3)
    check(SBigInt, BigInteger.ZERO, 32L)
    check(SBigInt, BigInteger.ONE, 32L)
    check(SBigInt, BigInteger.valueOf(Long.MaxValue), 32L)
    check(SBigInt, { val i = BigInteger.valueOf(Long.MaxValue); i.multiply(i) }, 32L)
    forAll { n: BigInt =>
      check(SBigInt, n, 32L)

    }
    val g = CryptoConstants.dlogGroup.generator
    check(SGroupElement, g, 33L)
    check(SSigmaProp, DLogProtocol.ProveDlog(g), 1024L)
    check(SAvlTree, null, 44L)
    check(sigmastate.SOption(SInt), Some(10), 1L + 4)
    checkColl(SInt, Coll[Int](10,20), 2L * 4)
    checkColl(SInt, Coll[Int](), 0L)
    checkColl(SBigInt, Coll[BigInt](SigmaDsl.BigInt(BigInteger.ZERO), SigmaDsl.BigInt(BigInteger.valueOf(Long.MaxValue))), 2 * 32L)
    check(STuple(SInt, STuple(SInt, SInt)), Array(10, Array[Any](20, 30)), 2 + 4 + (2 + 4 + 4))

    val (tree, _) = createAvlTreeAndProver()

    val box = SigmaDsl.Box(ErgoBox.create(20, TrueProp, 0,
      Seq(
        (Digest32 @@ (ErgoAlgos.decodeUnsafe("6e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f0001")),
            10000000L)
      ),
      Map(
        ErgoBox.R4 -> ByteConstant(1.toByte),
        ErgoBox.R5 -> ShortConstant(1024.toShort),
        ErgoBox.R6 -> IntConstant(1024 * 1024),
        ErgoBox.R7 -> LongConstant(1024.toLong),
        ErgoBox.R8 -> BigIntConstant(222L),
        ErgoBox.R9 -> AvlTreeConstant(tree)
      )))

    check(SBox, box, 4096L)
  }

  property("SType.dataSize (not defined)") {
    val assertError = (t: Throwable) => t.getMessage.contains("is not defined")
    assertExceptionThrown(check(NoType, null, 0), assertError)
    assertExceptionThrown(check(SAny, null, 0), assertError)
    assertExceptionThrown(check(STypeApply("T"), null, 0), assertError)
    assertExceptionThrown(check(SType.tT, null, 0), assertError)
    assertExceptionThrown(check(SGlobal, null, 0), assertError)
    assertExceptionThrown(check(SContext, null, 0), assertError)
  }

}
