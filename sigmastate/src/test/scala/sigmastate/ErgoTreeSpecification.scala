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

  case class MInfo(methodId: Byte, method: SMethod, isSupported: Boolean = true)

  // NOTE, the type code constants are checked above
  // The methodId codes as checked here.
  // The following table should be made dependent on HF activation
  val methods = Table(
    ("typeId",        "methods",               "CanHaveMethods"),
    (SBoolean.typeId, Seq.empty[MInfo], true),
    (SByte.typeId,    Seq.empty[MInfo], false),
    (SShort.typeId,   Seq.empty[MInfo], false),
    (SInt.typeId,     Seq.empty[MInfo], false),
    (SLong.typeId,    Seq.empty[MInfo], false),
    { import SNumericType._
      (SBigInt.typeId,  Seq(
        MInfo(methodId = 1, ToByteMethod, isSupported = false),
        MInfo(2, ToShortMethod, isSupported = false),
        MInfo(3, ToIntMethod, isSupported = false),
        MInfo(4, ToLongMethod, isSupported = false),
        MInfo(5, ToBigIntMethod, isSupported = false),
        MInfo(6, ToBytesMethod, isSupported = false),
        MInfo(7, ToBitsMethod, isSupported = false)
      ), true)
    },
    { import SContext._
      (SContext.typeId, Seq(
        MInfo(1, dataInputsMethod), MInfo(2, headersMethod), MInfo(3, preHeaderMethod),
        MInfo(4, inputsMethod), MInfo(5, outputsMethod), MInfo(6, heightMethod),
        MInfo(7, selfMethod), MInfo(8, selfBoxIndexMethod), MInfo(9, lastBlockUtxoRootHashMethod),
        MInfo(10, minerPubKeyMethod), MInfo(11, getVarMethod, isSupported = false)
      ), true)
    },
    { import SGlobal._
      (SGlobal.typeId, Seq(
        MInfo(1, groupGeneratorMethod), MInfo(2, xorMethod)
        ), true)
    }
//    (SNumericType.typeId, Seq.empty[(Int, SMethod)], true)
  )

  property("MethodCall Codes") {
    forAll(methods) { (typeId, methods, shouldHaveMethods) =>
      SType.types.get(typeId) match {
        case Some(tyDesc) =>
          assert(shouldHaveMethods, s"Type $tyDesc should NOT have methods")

          tyDesc.methods.length shouldBe methods.length
          for (expectedMethod <- methods) {
            if (expectedMethod.isSupported) {

              // the following line is used in MethodCall deserializer to resolve SMethod
              val resolvedMethod = SMethod.fromIds(typeId, expectedMethod.methodId)

              resolvedMethod.objType.typeId shouldBe typeId
              resolvedMethod.name shouldBe expectedMethod.method.name
              assert(resolvedMethod.irInfo.irBuilder.isDefined, s"irBuilder is not defined for $resolvedMethod")
              resolvedMethod.irInfo shouldBe expectedMethod.method.irInfo
            } else {
              // declared, but not supported
            }
          }
        case None =>
          assert(!shouldHaveMethods, s"Type with code $typeId should have methods")
      }
    }

  }
}
