package sigmastate

import org.ergoplatform.ErgoBox
import org.ergoplatform.settings.ErgoAlgos
import org.ergoplatform.validation.{ValidationException, ValidationRules}
import scalan.RType.asType
import scalan.{Nullable, RType}
import sigmastate.SCollection.{SByteArray, SByteArray2, checkValidFlatmap}
import sigmastate.Values._
import sigmastate.VersionContext._
import sigmastate.eval.{Evaluation, Profiler}
import sigmastate.interpreter.ErgoTreeEvaluator
import sigmastate.lang.SourceContext
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.{CostLimitException, CosterException, InterpreterException}
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import sigmastate.utxo._
import special.collection._
import special.sigma.SigmaDslTesting


/** Regression tests with ErgoTree related test vectors.
  * This test vectors verify various constants which are consensus critical and should not change.
  */
class ErgoTreeSpecification extends SigmaDslTesting {

  property("Value.sourceContext") {
    val srcCtx = SourceContext.fromParserIndex(0, "")
    val value = IntConstant(10)
    value.sourceContext shouldBe Nullable.None

    value.withSrcCtx(Nullable(srcCtx))
    value.sourceContext shouldBe Nullable(srcCtx)

    assertExceptionThrown(
      value.withSrcCtx(Nullable(srcCtx)),
      t => t.isInstanceOf[RuntimeException] && t.getMessage.contains("can be set only once"))
  }

  property("Value.opType") {
    Seq(
      Block(Seq(), null),
      ValNode("x", SInt, null),
      ApplyTypes(null, Seq()),
      TaggedVariableNode(1, SByte),
      ValDef(1, null),
      ValUse(1, SInt),
      BlockValue(IndexedSeq(), null)
      ).foreach { node =>
      assertExceptionThrown(node.opType, t => t.getMessage.contains("is not supported for node"))
    }
    FuncValue(Vector((1, SInt)), ValUse(1, SInt)).opType shouldBe
      SFunc(Vector(), SFunc(SInt, SInt))
  }

  property("ErgoTree.toProposition") {
    val t1 = new ErgoTree(
      16.toByte,
      Array(IntConstant(1)),
      Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
    )

    val t = new ErgoTree(
      16.toByte,
      Array(IntConstant(1)),
      Left(UnparsedErgoTree(t1.bytes, ValidationException("", ValidationRules.CheckTypeCode, Seq())))
    )
    assertExceptionThrown(
      t.toProposition(true),
      t => t.isInstanceOf[ValidationException]
    )
  }

  property("ErgoTree.template") {
    val t = new ErgoTree(
      16.toByte,
      Array(IntConstant(1)),
      Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
    )
    t.template shouldBe ErgoAlgos.decodeUnsafe("d19373000402")
  }

  property("ErgoTree.bytes") {
    val t = new ErgoTree(
      16.toByte,
      Array(IntConstant(1)),
      Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
    )
    val expectedBytes = DefaultSerializer.serializeErgoTree(t)
    t._bytes shouldBe expectedBytes
  }

  property("Value.hasDeserialize") {
    val const = IntConstant(10)
    Value.hasDeserialize(const) shouldBe false
    val dc = DeserializeContext(1.toByte, SInt)
    Value.hasDeserialize(dc) shouldBe true
    val dr = DeserializeRegister(ErgoBox.R4, SInt)
    Value.hasDeserialize(dr) shouldBe true
    Value.hasDeserialize(EQ(const, dc)) shouldBe true
    Value.hasDeserialize(Plus(Plus(const, dc), dr)) shouldBe true
    val t = new ErgoTree(
      16.toByte,
      Array(IntConstant(1)),
      Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
    )
    Value.hasDeserialize(t.toProposition(replaceConstants = true)) shouldBe false
  }

  property("ErgoTree.hasDeserialize") {
    {
      val t = new ErgoTree(
        0.toByte,
        Array[Constant[SType]](),
        Right(TrueSigmaProp))
      t._hasDeserialize shouldBe None
      t.hasDeserialize shouldBe false
    }

    {
      val t = new ErgoTree(
        16.toByte,
        Array(IntConstant(1)),
        Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), DeserializeContext(1.toByte, SInt))))
      )
      t._hasDeserialize shouldBe None
      t.hasDeserialize shouldBe true
    }
  }

  property("ErgoTree equality") {
    val t1 = new ErgoTree(
      16.toByte,
      Array(IntConstant(1)),
      Right(BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(1))))
    )
    val t2 = new ErgoTree(16.toByte, Array(IntConstant(1)), Right(TrueSigmaProp))
    val t3 = new ErgoTree(16.toByte, Array(IntConstant(1)), Right(TrueSigmaProp))
    val t4 = new ErgoTree(16.toByte, Vector(), Right(TrueSigmaProp))
    val t5 = new ErgoTree(ErgoTree.DefaultHeader, Vector(), Right(TrueSigmaProp))
    assert(t1 != t2)
    assert(t2 == t3)
    assert(t3 != t4)
    assert(t4 != t5)
    assert(t5 != t1)
  }

  property("ConstantNode equality") {
    assert(IntConstant(10) == IntConstant(10))
    assert(ShortConstant(10) == ShortConstant(10))
    assert(IntConstant(10) != IntConstant(11))
    assert(IntConstant(10) != ShortConstant(10))
  }

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
    forAll(Table(("type", "isConstantSize"),
      (NoType, true),
      (SString, false),
      (SAny, false),
      (SUnit, true),
      (SFunc(SInt, SAny), false),
      (STypeApply("T"), false),
      (SType.tT, false)
    )) { (t, isConst) =>
      t.isConstantSize shouldBe isConst
    }
  }

  property("Tuple Types") {
    val constSizeTuple = STuple(SByte, SInt, SBigInt)
    val dynSizeTuple = STuple(SByte, SInt, SBox, SBigInt)
    forAll(Table(("type", "isConstantSize"),
      (STuple(SByte), true),
      (STuple(SByte, SInt), true),
      (STuple(SByte, SInt, SAvlTree), true),
      (STuple(SBox), false),
      (STuple(SByte, SBox), false),
      (STuple(SByte, SInt, SBox), false),
      (STuple(SBox, SByte, SInt), false),
      (constSizeTuple, true),
      (constSizeTuple, true), // should avoid re-computation
      (dynSizeTuple, false),
      (dynSizeTuple, false)   // should avoid re-computation
    )) { (t, isConst) =>
      t.isConstantSize shouldBe isConst
    }
  }

  /** Expected parameters of resolved method (see `methods` table below).
    *
    * @param isResolvableFromIds if true, them SMethod.fromIds must resolve, otherwise
    *                            ValidationException must be thrown
    */
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
      // this should be preserved in v3.x and fixed in v4.0
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
        MInfo(2, IsProvenMethod)  // TODO v5.x (3h): this method must be removed
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
              resolvedMethod.irInfo shouldBe expectedMethod.method.irInfo
            } else {
              // declared, but not supported
              assertExceptionThrown(
                SMethod.fromIds(typeId, expectedMethod.methodId),
                { case _: ValidationException => true
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

  property("MethodCall on numerics") {
    forAll(Table[STypeCompanion]("type", SByte, SShort, SInt, SLong, SBigInt)) { t =>
      // this methods are expected to fail resolution in v3.x (but may change in future)
      (1 to 7).foreach { methodId =>
        assertExceptionThrown(
          SMethod.fromIds(t.typeId, methodId.toByte),
          {
            case _: ValidationException => true
            case _ => false
          },
          s"SMethod mustn't resolve for typeId = ${t.typeId} and methodId = $methodId"
        )
      }
    }
  }

  implicit def IR = new TestingIRContext
  implicit def cs = compilerSettingsInTests
  implicit def es = evalSettings

  property("Apply with 0 arguments") {
    val expr = Apply(FuncValue(Vector(), IntConstant(1)), IndexedSeq())

    forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
        // old v4.x interpreter
        assertExceptionThrown(
          {
            val oldF = funcFromExpr[Int, Int]("({ (x: Int) => 1 })()", expr)
          },
          exceptionLike[CosterException]("Don't know how to evalNode")
        )

        // new v5.0 interpreter
        val newF = funcJitFromExpr[Int, Int]("({ (x: Int) => 1 })()", expr)
        assertExceptionThrown(
          {
            val x = 100 // any value which is not used anyway
            val (y, _) = newF.apply(x)
          },
          exceptionLike[InterpreterException]("Function application must have 1 argument, but was:")
        )
      }
    }
    
  }

  property("Apply with one argument") {
    val expr = Apply(
      FuncValue(Vector((1, SInt)), Negation(ValUse(1, SInt))),
      IndexedSeq(IntConstant(1)))
    val script = "({ (x: Int) => -x })(1)"

    val x = 1

    forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {

        { // old v4.x interpreter
          val oldF = funcFromExpr[Int, Int](script, expr)
          val (y, _) = oldF.apply(x)
          y shouldBe -1
        }

        { // new v5.0 interpreter
          val newF = funcJitFromExpr[Int, Int](script, expr)
          val (y, _) = newF.apply(x)
          y shouldBe -1
        }

      }
    }
  }

  property("Apply with 2 and more arguments") {
    val expr = Apply(
      FuncValue(Vector((1, SInt), (2, SInt)), Plus(ValUse(1, SInt), ValUse(2, SInt))),
      IndexedSeq(IntConstant(1), IntConstant(1))
    )
    val script = "{ (x: Int, y: Int) => x + y }"

    forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {

        // old v4.x interpreter
        assertExceptionThrown(
          {
            val oldF = funcFromExpr[(Int, Int), Int](script, expr)
          },
          exceptionLike[CosterException]("Don't know how to evalNode")
        )

        // ndw v5.0 interpreter
        val newF = funcJitFromExpr[(Int, Int), Int](script, expr)
          assertExceptionThrown(
          {
            val (y, _) = newF.apply((1, 1))
          },
          exceptionLike[InterpreterException]("Function application must have 1 argument, but was:")
        )
      }
    }
  }

  /** Deeply nested maps which creates deeply nested collections.
    * @return lambda like `(xs: Coll[Byte]) => xs.map(_ => xs.map(... xs.map(_ => xs)...))`
    */
  def mkFuncValue(nDepth: Int, level: Int): SValue = {
    def mkCollection(nDepth: Int, level: Int): SValue = {
      if (level < nDepth)
        MapCollection(
          ValUse(1, SByteArray),
          FuncValue(
            Array((level + 1, SByte)),
            mkCollection(nDepth, level + 1)
          )
        )
      else
        ValUse(1, SByteArray)
    }

    FuncValue(
      Array((1, SByteArray)),
      mkCollection(nDepth, level))
  }

  property("Building deeply nested expression") {

    mkFuncValue(1, 1) shouldBe
      FuncValue(
        Array((1, SByteArray)),
        ValUse(1, SByteArray))

    mkFuncValue(2, 1) shouldBe
      FuncValue(
        Array((1, SByteArray)),
        MapCollection(
          ValUse(1, SByteArray),
          FuncValue(
            Array((2, SByte)),
            ValUse(1, SByteArray)
          )
        ))

    mkFuncValue(3, 1) shouldBe
      FuncValue(
        Array((1, SByteArray)),
        MapCollection(
          ValUse(1, SByteArray),
          FuncValue(
            Array((2, SByte)),
            MapCollection(
              ValUse(1, SByteArray),
              FuncValue(
                Array((3, SByte)),
                ValUse(1, SByteArray)
              )
            )
          )
        ))
  }

  def exprCostForSize(size: Int, oldF: CompiledFunc[Coll[Byte], AnyRef], expected: Option[Coll[Byte] => AnyRef]) = {
    val xs = Coll(Array.tabulate[Byte](size)(i => i.toByte):_*)

    val (y, details) = oldF(xs)
    assert(details.actualTimeNano.get < 1000000000 /* 1 sec */)

    if (expected.isDefined) {
      val e = expected.get(xs)
      y shouldBe e
    }
    details.cost
  }

  def mkCompiledFunc(depth: Int): CompiledFunc[Coll[Byte], AnyRef] = {
    val expr = Apply(
      mkFuncValue(depth, 1),
      Array(OptionGet(GetVar(1.toByte, SOption(SByteArray))))
    )
    val script = "{ just any string }"
    implicit val tRes: RType[AnyRef] = asType[AnyRef](Evaluation.stypeToRType(expr.tpe))
    val oldF = funcJitFromExpr[Coll[Byte], AnyRef](script, expr)
    oldF
  }

  property("Spam: Building large nested collection") {

    forEachScriptAndErgoTreeVersion(
       activatedVers = Array(JitActivationVersion),
       ergoTreeVers = ergoTreeVersions) {
      VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {

        { // depth 3
          val cf = mkCompiledFunc(3)
          val expected = (xs: Coll[Byte]) => xs.map(_ => xs.map(_ => xs))
          exprCostForSize(10, cf, Some(expected)) shouldBe JitCost(1456)
          exprCostForSize(100, cf, Some(expected)) shouldBe JitCost(104605)

          assertExceptionThrown(
            exprCostForSize(1000, cf, Some(expected)),
            exceptionLike[CostLimitException]("Estimated execution cost JitCost(10000005) exceeds the limit JitCost(10000000)")
          )
        }

        { // depth 60
          val cf = mkCompiledFunc(60)
          val expected = (xs: Coll[Byte]) => xs.map(_ => xs.map(_ => xs))
          exprCostForSize(1, cf, None) shouldBe JitCost(2194)

          assertExceptionThrown(
            exprCostForSize(2, cf, None),
            exceptionLike[CostLimitException]("Estimated execution cost JitCost(10000001) exceeds the limit JitCost(10000000)")
          )
        }

      }
    }
  }

  property("checkValidFlatmap") {
    implicit val E = ErgoTreeEvaluator.forProfiling(new Profiler, evalSettings)
    def mkLambda(t: SType, mkBody: SValue => SValue) = {
      MethodCall(
        ValUse(1, SCollectionType(t)),
        SCollection.getMethodByName("flatMap").withConcreteTypes(
          Map(STypeVar("IV") -> t, STypeVar("OV") -> SByte)
        ),
        Vector(FuncValue(Vector((3, t)), mkBody(ValUse(3, t)))),
        Map()
      )
    }
    val validLambdas = Seq[(SType, SValue => SValue)](
      (SBox, x => ExtractScriptBytes(x.asBox)),
      (SBox, x => ExtractId(x.asBox)),
      (SBox, x => ExtractBytes(x.asBox)),
      (SBox, x => ExtractBytesWithNoRef(x.asBox)),
      (SSigmaProp, x => SigmaPropBytes(x.asSigmaProp)),
      (SBox, x => MethodCall(x, SBox.getMethodByName("id"), Vector(), Map()))
    ).map { case (t, f) => mkLambda(t, f) }

    validLambdas.foreach { l =>
      checkValidFlatmap(l)
    }

    val invalidLambdas = Seq[(SType, SValue => SValue)](
      // identity lambda `xss.flatMap(xs => xs)`
      (SByteArray, x => x),

      // identity lambda `xss.flatMap(xs => xs ++ xs)`
      (SByteArray, x => Append(x.asCollection[SByte.type], x.asCollection[SByte.type]))
    ).map { case (t, f) => mkLambda(t, f) } ++
      Seq(
        // invalid MC like `boxes.flatMap(b => b.id, 10)`
        MethodCall(
          ValUse(1, SBox),
          SCollection.getMethodByName("flatMap").withConcreteTypes(
            Map(STypeVar("IV") -> SBox, STypeVar("OV") -> SByte)
          ),
          Vector(
            FuncValue(Vector((3, SBox)), ExtractId(ValUse(3, SBox))),
            IntConstant(10) // too much arguments
          ),
          Map()
        ),
        // invalid MC like `boxes.flatMap((b,_) => b.id)`
        MethodCall(
          ValUse(1, SBox),
          SCollection.getMethodByName("flatMap").withConcreteTypes(
            Map(STypeVar("IV") -> SBox, STypeVar("OV") -> SByte)
          ),
          Vector(
            FuncValue(Vector((3, SBox), (4, SInt)/*too much arguments*/), ExtractId(ValUse(3, SBox)))
          ),
          Map()
        )
      )

    invalidLambdas.foreach { l =>
      assertExceptionThrown(
        checkValidFlatmap(l),
        exceptionLike[RuntimeException](
          s"Unsupported lambda in flatMap: allowed usage `xs.flatMap(x => x.property)`")
      )
    }

  }
}
