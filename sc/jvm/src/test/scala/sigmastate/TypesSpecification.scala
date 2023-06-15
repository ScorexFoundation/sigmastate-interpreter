package sigmastate

import sigmastate.SType.isValueOfType
import sigmastate.eval.{CSigmaProp, CostingSigmaDslBuilder}
import special.sigma.SigmaTestingData

class TypesSpecification extends SigmaTestingData {

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
      withClue(s"x = $x, t = $t: ") {
        isValueOfType(x, t) shouldBe true
      }
    }
    def assertFalse(x: Any, t: SType) = {
      withClue(s"x = $x, t = $t: ") {
        isValueOfType(x, t) shouldBe false
      }
    }
    
    assertTrue(true, SBoolean)
    assertTrue(false, SBoolean)
    assertFalse(true, SByte)
    
    assertTrue(0.toByte, SByte)
    assertFalse(0.toByte, SShort)  // TODO JS: this and the other similar cases below fail on JS
    
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

    val ctx = fakeContext.toSigmaContext()
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
