package sigmastate

import java.math.BigInteger

import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.ErgoAlgos
import scalan.RType
import scorex.crypto.hash.Digest32
import sigmastate.basics.DLogProtocol
import sigmastate.interpreter.CryptoConstants
import special.sigma.SigmaTestingData
import sigmastate.SType.{isValueOfType, AnyOps}
import sigmastate.Values.{ShortConstant, LongConstant, BigIntConstant, AvlTreeConstant, IntConstant, ByteConstant}
import sigmastate.eval.{CSigmaProp, SigmaDsl, CostingSigmaDslBuilder}
import special.collection.Coll
import special.sigma.BigInt
import sigmastate.helpers.TestingHelpers._

class TypesSpecification extends SigmaTestingData {

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

    val box = SigmaDsl.Box(testBox(20, TrueTree, 0,
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

  property("SType.isValueOfType") {
    forAll { t: SPredefType =>
      implicit val tWrapped = wrappedTypeGen(t)
      forAll { x: SPredefType#WrappedType =>
        isValueOfType(x, t) shouldBe true
        // since forall t. SHeader != t
        isValueOfType(x, SHeader) shouldBe false
      }
    }
  }

  import TestData._

  property("SType.isValueOfType test vectors") {
    def assertTrue(x: Any, t: SType) = {
      isValueOfType(x, t) shouldBe true
    }
    def assertFalse(x: Any, t: SType) = {
      isValueOfType(x, t) shouldBe false
    }
    
    assertTrue(true, SBoolean)
    assertTrue(false, SBoolean)
    assertFalse(true, SByte)
    
    assertTrue(0.toByte, SByte)
    assertFalse(0.toByte, SShort)
    
    assertTrue(0.toShort, SShort)
    assertFalse(0.toShort, SInt)
    
    assertTrue(0, SInt)
    assertFalse(0, SShort)
    
    assertTrue(0L, SLong)
    assertFalse(0L, SShort)

    assertTrue(BigIntZero, SBigInt)
    assertFalse(BigIntZero, SShort)

    assertTrue(ge1, SGroupElement)
    assertFalse(ge1, SShort)

    assertTrue(CSigmaProp(create_dlog()), SSigmaProp)
    assertFalse(CSigmaProp(create_dlog()), SShort)

    assertTrue(b1, SBox)
    assertFalse(b1, SShort)

    val coll = Coll[Int](1, 2)
    assertTrue(coll, SCollection(SInt))
    assertFalse(coll, SShort)
    assertTrue(Coll[Long](1L), SCollection(SInt)) // because type check is shallow

    assertTrue(None, SOption(SInt))
    assertTrue(Some(10), SOption(SInt))
    assertTrue(Some(10), SOption(SLong)) // because type check is shallow
    assertFalse(None, SShort)
    assertFalse(Some(10), SShort)

    val ctx = fakeContext.toSigmaContext(false)
    assertTrue(ctx, SContext)
    assertFalse(ctx, SShort)

    assertTrue(t1, SAvlTree)
    assertFalse(t1, SShort)

    assertTrue(CostingSigmaDslBuilder, SGlobal)
    assertFalse(CostingSigmaDslBuilder, SShort)

    assertTrue(h1, SHeader)
    assertFalse(h1, SShort)

    assertTrue(preH1, SPreHeader)
    assertFalse(preH1, SShort)

    assertTrue((1, 1L), STuple(SInt, SLong))
    assertTrue((1, 1L), STuple(SInt, SInt)) // because type check is shallow
    assertTrue((1, Some(1)), STuple(SInt, SOption(SLong))) // because type check is shallow
    assertExceptionThrown(
      assertTrue((1, 1L, Some(1)), STuple(SInt, SLong, SOption(SInt))),
      exceptionLike[RuntimeException](s"Unsupported tuple type")
    )

    assertTrue((x: Any) => x, SFunc(SInt, SLong))  // note, arg and result types not checked
    assertFalse((x: Any) => x, STuple(SInt, SLong))
    assertFalse(1, SFunc(SInt, SLong))
    assertExceptionThrown(
      assertTrue((x: Any) => x, SFunc(Array(SInt, SLong), SOption(SInt))),
      exceptionLike[RuntimeException](s"Unsupported function type")
    )

    assertExceptionThrown(
      assertTrue("", SString),
      exceptionLike[RuntimeException](s"Unknown type")
    )
  }
}
