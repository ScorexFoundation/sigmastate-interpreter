package sigmastate

import special.sigma.SigmaTestingData

/** Regression tests with ErgoTree related test vectors.
  * This test vectors verify various constants which are consensus critical and should not change.
  */
class ErgoTreeSpecification extends SigmaTestingData {

  val typeCodes = Table(
    ("constant", "expectedValue"),
    (SPrimType.LastPrimTypeCode, 8),
    (SPrimType.MaxPrimTypeCode, 11),
  )
  
  property("Expected values of constants") {
    forAll(typeCodes) { (const, expValue) =>
      const shouldBe expValue
    }
  }

  // Expected meta-parameters of predefined types (see Predefined Types section in docs/spec/spec.pdf)
  val types = Table(
    ("type", "Code", "IsConst", "IsPrim", "IsEmbed", "IsNum"),
    (SBoolean, 1, true, true, true, false),
    (SByte,    2, true, true, true, true),
    (SShort,   3, true, true, true, true),
    (SInt,     4, true, true, true, true),
    (SLong,    5, true, true, true, true),
    (SBigInt,  6, true, true, true, true),
    (SGroupElement, 7, true, true, true, false),
    (SSigmaProp,    8, true, true, true, false),
    (SBox,       99,  false, false, false, false),
    (SAvlTree,   100, true, false, false, false),
    (SContext,   101, false, false, false, false),
    (SHeader,    104, true, false, false, false),
    (SPreHeader, 105, true, false, false, false),
    (SGlobal,    106, true, false, false, false)
  )

  property("Predefined Types") {
    forAll(types) { (t, code, isConst, isPrim, isEmbed, isNum) =>
      t.typeCode shouldBe code
      t.isConstantSize shouldBe isConst
      t.isInstanceOf[SPrimType] shouldBe isPrim
      t.isEmbeddable shouldBe isEmbed
      t.isNumType shouldBe isNum
      whenever(isPrim) {
        t.typeCode should be <= SPrimType.LastPrimTypeCode
      }
    }
  }

}
