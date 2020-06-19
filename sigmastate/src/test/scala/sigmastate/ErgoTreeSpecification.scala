package sigmastate

import org.ergoplatform.validation.ValidationException
import special.sigma.SigmaTestingData

/** Regression tests with ErgoTree related test vectors.
  * This test vectors verify various constants which are consensus critical and should not change.
  */
class ErgoTreeSpecification extends SigmaTestingData {

  val typeCodes = Table(
    ("constant", "expectedValue"),
    (SPrimType.LastPrimTypeCode, 8),
    (SPrimType.MaxPrimTypeCode, 11)
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
      t.typeId shouldBe code
      t.isConstantSize shouldBe isConst
      t.isInstanceOf[SPrimType] shouldBe isPrim
      t.isEmbeddable shouldBe isEmbed
      t.isNumType shouldBe isNum
      whenever(isPrim) {
        t.typeCode should be <= SPrimType.LastPrimTypeCode
      }
    }
  }

  case class MInfo(methodId: Byte, method: SMethod, isResolvableFromIds: Boolean = true)

  // NOTE, the type code constants are checked above
  // The methodId codes as checked here, they MUST be PRESERVED.
  // The following table should be made dependent on HF activation
  val methods = Table(
    ("typeId",        "methods",               "CanHaveMethods"),
    (SBoolean.typeId, Seq.empty[MInfo], true),
    (SByte.typeId,    Seq.empty[MInfo], false),
    (SShort.typeId,   Seq.empty[MInfo], false),
    (SInt.typeId,     Seq.empty[MInfo], false),
    (SLong.typeId,    Seq.empty[MInfo], false),

    { // SNumericType.typeId is erroneously shadowed by SGlobal.typeId
      // this should be preserved in 3.x and fixed in 4.0
      (SNumericType.typeId,  Seq(
        MInfo(methodId = 1, SGlobal.groupGeneratorMethod),
        MInfo(2, SGlobal.xorMethod)
      ), true)
    },

    { // SBigInt inherit methods from SNumericType.methods
      // however they are not resolvable via SBigInt.typeId
      import SNumericType._
      (SBigInt.typeId,  Seq(
        MInfo(methodId = 1, ToByteMethod, isResolvableFromIds = false),
        MInfo(2, ToShortMethod, isResolvableFromIds = false),
        MInfo(3, ToIntMethod, isResolvableFromIds = false),
        MInfo(4, ToLongMethod, isResolvableFromIds = false),
        MInfo(5, ToBigIntMethod, isResolvableFromIds = false),
        MInfo(6, ToBytesMethod, isResolvableFromIds = false),
        MInfo(7, ToBitsMethod, isResolvableFromIds = false)
      ), true)
    },
    { import SGroupElement._
      (SGroupElement.typeId,  Seq(
        MInfo(2, GetEncodedMethod),
        MInfo(3, ExponentiateMethod),
        MInfo(4, MultiplyMethod),
        MInfo(5, NegateMethod)
      ), true)
    },
    { import SSigmaProp._
      (SSigmaProp.typeId,  Seq(
        MInfo(1, PropBytesMethod),
        MInfo(2, IsProvenMethod)  // TODO HF: this method must be removed
      ), true)
    },
    { import SBox._
      (SBox.typeId,  Seq(
        MInfo(1, ValueMethod),
        MInfo(2, PropositionBytesMethod),
        MInfo(3, BytesMethod),
        MInfo(4, BytesWithoutRefMethod),
        MInfo(5, IdMethod),
        MInfo(6, creationInfoMethod),
        MInfo(7, getRegMethod),
        MInfo(8, tokensMethod)
      ) ++ registers(idOfs = 8)
        .zipWithIndex
        .map { case (m,i) => MInfo((8 + i + 1).toByte, m) }, true)
    },
    { import SAvlTree._
      (SAvlTree.typeId,  Seq(
        MInfo(1, digestMethod),
        MInfo(2, enabledOperationsMethod),
        MInfo(3, keyLengthMethod),
        MInfo(4, valueLengthOptMethod),
        MInfo(5, isInsertAllowedMethod),
        MInfo(6, isUpdateAllowedMethod),
        MInfo(7, isRemoveAllowedMethod),
        MInfo(8, updateOperationsMethod),
        MInfo(9, containsMethod),
        MInfo(10, getMethod),
        MInfo(11, getManyMethod),
        MInfo(12, insertMethod),
        MInfo(13, updateMethod),
        MInfo(14, removeMethod),
        MInfo(15, updateDigestMethod)
      ), true)
    },
    { import SHeader._
      (SHeader.typeId,  Seq(
        MInfo(1, idMethod), MInfo(2, versionMethod), MInfo(3, parentIdMethod),
        MInfo(4, ADProofsRootMethod), MInfo(5, stateRootMethod), MInfo(6, transactionsRootMethod),
        MInfo(7, timestampMethod), MInfo(8, nBitsMethod), MInfo(9, heightMethod),
        MInfo(10, extensionRootMethod), MInfo(11, minerPkMethod), MInfo(12, powOnetimePkMethod),
        MInfo(13, powNonceMethod), MInfo(14, powDistanceMethod), MInfo(15, votesMethod)
      ), true)
    },
    { import SPreHeader._
      (SPreHeader.typeId,  Seq(
        MInfo(1, versionMethod), MInfo(2, parentIdMethod), MInfo(3, timestampMethod),
        MInfo(4, nBitsMethod), MInfo(5, heightMethod), MInfo(6, minerPkMethod),
        MInfo(7, votesMethod)
      ), true)
    },
    { import SContext._
      (SContext.typeId, Seq(
        MInfo(1, dataInputsMethod), MInfo(2, headersMethod), MInfo(3, preHeaderMethod),
        MInfo(4, inputsMethod), MInfo(5, outputsMethod), MInfo(6, heightMethod),
        MInfo(7, selfMethod), MInfo(8, selfBoxIndexMethod), MInfo(9, lastBlockUtxoRootHashMethod),
        MInfo(10, minerPubKeyMethod), MInfo(11, getVarMethod)
      ), true)
    },
    { import SGlobal._
      (SGlobal.typeId, Seq(
        MInfo(1, groupGeneratorMethod), MInfo(2, xorMethod)
        ), true)
    },
    { import SCollection._
      (SCollection.typeId, Seq(
        MInfo(1, SizeMethod),
        MInfo(2, GetOrElseMethod),
        MInfo(3, MapMethod),
        MInfo(4, ExistsMethod),
        MInfo(5, FoldMethod),
        MInfo(6, ForallMethod),
        MInfo(7, SliceMethod),
        MInfo(8, FilterMethod),
        MInfo(9, AppendMethod),
        MInfo(10, ApplyMethod),
        /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        BitShiftLeftMethod,
        BitShiftRightMethod,
        BitShiftRightZeroedMethod,
        */
        MInfo(14, IndicesMethod),
        MInfo(15, FlatMapMethod),
        MInfo(19, PatchMethod),
        MInfo(20, UpdatedMethod),
        MInfo(21, UpdateManyMethod),
        /*TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        UnionSetsMethod,
        DiffMethod,
        IntersectMethod,
        PrefixLengthMethod,
        */
        MInfo(26, IndexOfMethod),
        /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        LastIndexOfMethod,
        FindMethod,
        */
        MInfo(29, ZipMethod)
        /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        DistinctMethod,
        StartsWithMethod,
        EndsWithMethod,
        MapReduceMethod,
        */
      ), true)
    },
    { import SOption._
      (SOption.typeId, Seq(
        MInfo(2, IsDefinedMethod),
        MInfo(3, GetMethod),
        MInfo(4, GetOrElseMethod),
        /* TODO soft-fork: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
        FoldMethod,
        */
        MInfo(7, MapMethod),
        MInfo(8, FilterMethod)
      ), true)
    }
  //    (SNumericType.typeId, Seq.empty[(Int, SMethod)], true)
  )

  property("MethodCall Codes") {
    forAll(methods) { (typeId, methods, canHaveMethods) =>
      SType.types.get(typeId) match {
        case Some(tyDesc) =>
          assert(canHaveMethods, s"Type $tyDesc should NOT have methods")

          tyDesc.methods.length shouldBe methods.length
          for (expectedMethod <- methods) {
            if (expectedMethod.isResolvableFromIds) {

              // the following line is used in MethodCall deserializer to resolve SMethod
              val resolvedMethod = SMethod.fromIds(typeId, expectedMethod.methodId)

              resolvedMethod.objType.typeId shouldBe typeId
              resolvedMethod.name shouldBe expectedMethod.method.name
//              assert(resolvedMethod.irInfo.irBuilder.isDefined, s"irBuilder is not defined for $resolvedMethod")
              resolvedMethod.irInfo shouldBe expectedMethod.method.irInfo
            } else {
              // declared, but not supported
              assertExceptionThrown(
                SMethod.fromIds(typeId, expectedMethod.methodId),
                { case ve: ValidationException => true
                  case _ => false },
                s"MethodCall shouldn't resolve for typeId = $typeId and $expectedMethod"
              )
            }
          }
        case None =>
          assert(!canHaveMethods, s"Type with code $typeId can have methods")
      }
    }

  }
}
