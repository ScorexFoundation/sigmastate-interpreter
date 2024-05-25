package sigma

import org.ergoplatform.sdk.utils.ErgoTreeUtils
import sigma.ast.ErgoTree.ZeroHeader
import sigma.ast.SCollection.SByteArray
import sigma.ast.syntax.TrueSigmaProp
import sigma.ast.{BoolToSigmaProp, CompanionDesc, ConcreteCollection, Constant, ConstantPlaceholder, Downcast, ErgoTree, FalseLeaf, FixedCostItem, FuncValue, Global, JitCost, MethodCall, PerItemCost, SBigInt, SByte, SCollection, SGlobalMethods, SInt, SLong, SPair, SShort, SSigmaProp, STypeVar, SelectField, SubstConstants, ValUse, Value}
import sigma.data.{CBigInt, ExactNumeric, RType}
import sigma.eval.{CostDetails, SigmaDsl, TracedCost}
import sigma.serialization.ErgoTreeSerializer
import sigma.util.Extensions.{BooleanOps, ByteOps, IntOps, LongOps}
import sigmastate.exceptions.MethodNotFound
import sigmastate.utils.Extensions.ByteOpsForSigma
import sigmastate.utils.Helpers

import java.math.BigInteger
import scala.util.Success

/** This suite tests all operations for v6.0 version of the language.
  * The base classes establish the infrastructure for the tests.
  *
  * @see SigmaDslSpecificationBase
  */
class LanguageSpecificationV6 extends LanguageSpecificationBase { suite =>
  override def languageVersion: Byte = VersionContext.V6SoftForkVersion

  def expectedSuccessForAllTreeVersions[A](value: A, cost: Int, costDetails: CostDetails) = {
    val res = ExpectedResult(Success(value), Some(cost)) -> Some(costDetails)
    Seq(0, 1, 2, 3).map(version => version -> res)
  }

  def mkSerializeFeature[A: RType]: Feature[A, Coll[Byte]] = {
    val tA = RType[A]
    val tpe = Evaluation.rtypeToSType(tA)
    newFeature(
      (x: A) => SigmaDsl.serialize(x),
      s"{ (x: ${tA.name}) => serialize(x) }",
      expectedExpr = FuncValue(
        Array((1, tpe)),
        MethodCall(
          Global,
          SGlobalMethods.serializeMethod.withConcreteTypes(Map(STypeVar("T") -> tpe)),
          Array(ValUse(1, tpe)),
          Map()
        )
      ),
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
  }

  property("Global.serialize[Byte]") {
    lazy val serializeByte = mkSerializeFeature[Byte]
    val cases = Seq(
      (-128.toByte, Success(Coll(-128.toByte))),
      (-1.toByte, Success(Coll(-1.toByte))),
      (0.toByte, Success(Coll(0.toByte))),
      (1.toByte, Success(Coll(1.toByte))),
      (127.toByte, Success(Coll(127.toByte)))
    )
    testCases(cases, serializeByte)
  }

  property("Global.serialize[Short]") {
    lazy val serializeShort = mkSerializeFeature[Short]
    val cases = Seq(
      (Short.MinValue, Success(Coll[Byte](0xFF.toByte, 0xFF.toByte, 0x03.toByte))),
      (-1.toShort, Success(Coll(1.toByte))),
      (0.toShort, Success(Coll(0.toByte))),
      (1.toShort, Success(Coll(2.toByte))),
      (Short.MaxValue, Success(Coll(-2.toByte, -1.toByte, 3.toByte)))
    )
    testCases(cases, serializeShort)
  }

  // TODO v6.0: implement serialization roundtrip tests after merge with deserializeTo


  property("Boolean.toByte") {
    val toByte = newFeature((x: Boolean) => x.toByte, "{ (x: Boolean) => x.toByte }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0)
    )

    val cases = Seq(
      (true, Success(1.toByte)),
      (false, Success(0.toByte))
    )

    if (toByte.isSupportedIn(VersionContext.current)) {
      // TODO v6.0: implement as part of https://github.com/ScorexFoundation/sigmastate-interpreter/pull/932
      assertExceptionThrown(
        testCases(cases, toByte),
        rootCauseLike[MethodNotFound]("Cannot find method")
      )
    }
    else
      testCases(cases, toByte)
  }

  property("Byte methods equivalence (new features)") {
    // TODO v6.0: implement as part of https://github.com/ScorexFoundation/sigmastate-interpreter/issues/474
    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions the new features are not supported
      // which is checked below

      lazy val toAbs = newFeature((x: Byte) => x.toAbs, "{ (x: Byte) => x.toAbs }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      lazy val compareTo = newFeature(
        (x: (Byte, Byte)) => x._1.compareTo(x._2),
        "{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      lazy val bitOr = newFeature(
        { (x: (Byte, Byte)) => (x._1 | x._2).toByteExact },
        "{ (x: (Byte, Byte)) => (x._1 | x._2) }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      lazy val bitAnd = newFeature(
        { (x: (Byte, Byte)) => (x._1 & x._2).toByteExact },
        "{ (x: (Byte, Byte)) => (x._1 & x._2) }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      forAll { x: Byte =>
        Seq(toAbs).foreach(f => f.checkEquality(x))
      }

      forAll { x: (Byte, Byte) =>
        Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
      }
    }
  }

  // TODO v6.0: enable as part of https://github.com/ScorexFoundation/sigmastate-interpreter/issues/474
  property("Short methods equivalence (new features)") {
    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions the new features are not supported
      // which is checked below

      lazy val toAbs = newFeature((x: Short) => x.toAbs, "{ (x: Short) => x.toAbs }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      lazy val compareTo = newFeature((x: (Short, Short)) => x._1.compareTo(x._2),
        "{ (x: (Short, Short)) => x._1.compareTo(x._2) }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      lazy val bitOr = newFeature(
      { (x: (Short, Short)) => (x._1 | x._2).toShortExact },
      "{ (x: (Short, Short)) => x._1 | x._2 }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      lazy val bitAnd = newFeature(
      { (x: (Short, Short)) => (x._1 & x._2).toShortExact },
      "{ (x: (Short, Short)) => x._1 & x._2 }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      forAll { x: Short =>
        Seq(toAbs).foreach(_.checkEquality(x))
      }
      forAll { x: (Short, Short) =>
        Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
      }
    }
  }

  property("Int methods equivalence (new features)") {
    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions the new features are not supported
      // which is checked below
      lazy val toAbs     = newFeature((x: Int) => x.toAbs, "{ (x: Int) => x.toAbs }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      lazy val compareTo = newFeature((x: (Int, Int)) => x._1.compareTo(x._2),
        "{ (x: (Int, Int)) => x._1.compareTo(x._2) }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      lazy val bitOr = newFeature(
      { (x: (Int, Int)) => x._1 | x._2 },
      "{ (x: (Int, Int)) => x._1 | x._2 }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      lazy val bitAnd = newFeature(
      { (x: (Int, Int)) => x._1 & x._2 },
      "{ (x: (Int, Int)) => x._1 & x._2 }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      forAll { x: Int =>
        Seq(toAbs).foreach(_.checkEquality(x))
      }
      forAll { x: (Int, Int) =>
        Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
      }
    }
  }

  property("Long methods equivalence (new features)") {
    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions the new features are not supported
      // which is checked below
      lazy val toAbs = newFeature((x: Long) => x.toAbs, "{ (x: Long) => x.toAbs }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      lazy val compareTo = newFeature((x: (Long, Long)) => x._1.compareTo(x._2),
        "{ (x: (Long, Long)) => x._1.compareTo(x._2) }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      lazy val bitOr = newFeature(
        { (x: (Long, Long)) => x._1 | x._2 },
        "{ (x: (Long, Long)) => x._1 | x._2 }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      lazy val bitAnd = newFeature(
        { (x: (Long, Long)) => x._1 & x._2 },
        "{ (x: (Long, Long)) => x._1 & x._2 }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      forAll { x: Long =>
        Seq(toAbs).foreach(_.checkEquality(x))
      }
      forAll { x: (Long, Long) =>
        Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
      }
    }

  }

  property("BigInt methods equivalence (new features)") {
    // TODO v6.0: the behavior of `upcast` for BigInt is different from all other Numeric types (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/877)
    // The `Upcast(bigInt, SBigInt)` node is never produced by ErgoScript compiler, but is still valid ErgoTree.
    // It makes sense to fix this inconsistency as part of upcoming forks
    assertExceptionThrown(
      SBigInt.upcast(CBigInt(new BigInteger("0", 16)).asInstanceOf[AnyVal]),
      _.getMessage.contains("Cannot upcast value")
    )

    // TODO v6.0: the behavior of `downcast` for BigInt is different from all other Numeric types (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/877)
    // The `Downcast(bigInt, SBigInt)` node is never produced by ErgoScript compiler, but is still valid ErgoTree.
    // It makes sense to fix this inconsistency as part of HF
    assertExceptionThrown(
      SBigInt.downcast(CBigInt(new BigInteger("0", 16)).asInstanceOf[AnyVal]),
      _.getMessage.contains("Cannot downcast value")
    )

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions the new features are not supported
      // which is checked below
      val toByte = newFeature((x: BigInt) => x.toByte,
        "{ (x: BigInt) => x.toByte }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SByte)),
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      val toShort = newFeature((x: BigInt) => x.toShort,
        "{ (x: BigInt) => x.toShort }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SShort)),
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      val toInt = newFeature((x: BigInt) => x.toInt,
        "{ (x: BigInt) => x.toInt }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SInt)),
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      val toLong = newFeature((x: BigInt) => x.toLong,
        "{ (x: BigInt) => x.toLong }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SLong)),
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      lazy val toAbs   = newFeature((x: BigInt) => x.toAbs, "{ (x: BigInt) => x.toAbs }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      lazy val compareTo = newFeature((x: (BigInt, BigInt)) => x._1.compareTo(x._2),
        "{ (x: (BigInt, BigInt)) => x._1.compareTo(x._2) }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      lazy val bitOr = newFeature({ (x: (BigInt, BigInt)) => x._1 | x._2 },
        "{ (x: (BigInt, BigInt)) => x._1 | x._2 }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
      lazy val bitAnd = newFeature({ (x: (BigInt, BigInt)) => x._1 & x._2 },
        "{ (x: (BigInt, BigInt)) => x._1 & x._2 }",
        sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

      forAll { x: BigInt =>
        Seq(toByte, toShort, toInt, toLong, toAbs).foreach(_.checkEquality(x))
      }
      forAll { x: (BigInt, BigInt) =>
        Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
      }
    }
  }

  property("Box properties equivalence (new features)") {
    // TODO v6.0: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
    val getReg = newFeature((x: Box) => x.getReg[Int](1).get,
      "{ (x: Box) => x.getReg[Int](1).get }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { box: Box =>
        Seq(getReg).foreach(_.checkEquality(box))
      }
    }
  }

  // TODO v6.0 (3h): https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  property("Coll find method equivalence") {
    val find = newFeature((x: Coll[Int]) => x.find({ (v: Int) => v > 0 }),
      "{ (x: Coll[Int]) => x.find({ (v: Int) => v > 0} ) }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Coll[Int] =>
        find.checkEquality(x)
      }
    }
  }

  // TODO v6.0 (3h): https://github.com/ScorexFoundation/sigmastate-interpreter/issues/418
  property("Coll bitwise methods equivalence") {
    val shiftRight = newFeature(
      { (x: Coll[Boolean]) =>
        if (x.size > 2) x.slice(0, x.size - 2) else Colls.emptyColl[Boolean]
      },
      "{ (x: Coll[Boolean]) => x >> 2 }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Array[Boolean] =>
        shiftRight.checkEquality(Colls.fromArray(x))
      }
    }
  }

  // TODO v6.0 (3h): https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  property("Coll diff methods equivalence") {
    val diff = newFeature((x: (Coll[Int], Coll[Int])) => x._1.diff(x._2),
      "{ (x: (Coll[Int], Coll[Int])) => x._1.diff(x._2) }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { (x: Coll[Int], y: Coll[Int]) =>
        diff.checkEquality((x, y))
      }
    }
  }

  // TODO v6.0: implement Option.fold (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479)
  property("Option new methods") {
    val n = ExactNumeric.LongIsExactNumeric
    val fold = newFeature({ (x: Option[Long]) => x.fold(5.toLong)( (v: Long) => n.plus(v, 1) ) },
      "{ (x: Option[Long]) => x.fold(5, { (v: Long) => v + 1 }) }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Option[Long] =>
        Seq(fold).map(_.checkEquality(x))
      }
    }
  }

  // TODO v6.0 (3h): implement allZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  property("allZK equivalence") {
    lazy val allZK = newFeature((x: Coll[SigmaProp]) => SigmaDsl.allZK(x),
      "{ (x: Coll[SigmaProp]) => allZK(x) }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Coll[SigmaProp] =>
        allZK.checkEquality(x)
      }
    }
  }

  // TODO v6.0 (3h): implement anyZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  property("anyZK equivalence") {
    lazy val anyZK = newFeature((x: Coll[SigmaProp]) => SigmaDsl.anyZK(x),
      "{ (x: Coll[SigmaProp]) => anyZK(x) }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Coll[SigmaProp] =>
        anyZK.checkEquality(x)
      }
    }
  }

  property("Fix substConstants in v6.0 for ErgoTree version > 0") {
    // tree with one segregated constant and v0
    val t1 = ErgoTree(
      header = ZeroHeader.withConstantSegregation,
      constants = Vector(TrueSigmaProp),
      ConstantPlaceholder(0, SSigmaProp))

    // tree with one segregated constant and max supported version
    val t2 = ErgoTree(
      header = ZeroHeader
        .withVersion(VersionContext.MaxSupportedScriptVersion)
        .withConstantSegregation,
      Vector(TrueSigmaProp),
      ConstantPlaceholder(0, SSigmaProp))

    def costDetails(nItems: Int) = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ConcreteCollection),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ConcreteCollection),
        FixedCostItem(Constant),
        FixedCostItem(BoolToSigmaProp),
        ast.SeqCostItem(CompanionDesc(SubstConstants), PerItemCost(JitCost(100), JitCost(100), 1), nItems)
      )
    )

    val expectedTreeBytes_beforeV6 = Helpers.decodeBytes("1b0108d27300")
    val expectedTreeBytes_V6 = Helpers.decodeBytes("1b050108d27300")

    verifyCases(
      Seq(
        // for tree v0, the result is the same for all versions
        (Coll(t1.bytes: _*), 0) -> Expected(
          Success(Helpers.decodeBytes("100108d27300")),
          cost = 1793,
          expectedDetails = CostDetails.ZeroCost,
          newCost = 1793,
          newVersionedResults = expectedSuccessForAllTreeVersions(Helpers.decodeBytes("100108d27300"), 1793, costDetails(1))
         ),
        // for tree version > 0, the result depend on activated version
        {
          (Coll(t2.bytes: _*), 0) -> Expected(
            Success(expectedTreeBytes_beforeV6),
            cost = 1793,
            expectedDetails = CostDetails.ZeroCost,
            newCost = 1793,
            newVersionedResults = expectedSuccessForAllTreeVersions(expectedTreeBytes_V6, 1793, costDetails(1)))
        }
      ),
      changedFeature(
        changedInVersion = VersionContext.sinceV6AndTreeVersion(0),
        { (x: (Coll[Byte], Int)) =>
          SigmaDsl.substConstants(x._1, Coll[Int](x._2), Coll[Any](SigmaDsl.sigmaProp(false))(sigma.AnyType))
        },
        { (x: (Coll[Byte], Int)) =>
          SigmaDsl.substConstants(x._1, Coll[Int](x._2), Coll[Any](SigmaDsl.sigmaProp(false))(sigma.AnyType))
        },
        "{ (x: (Coll[Byte], Int)) => substConstants[Any](x._1, Coll[Int](x._2), Coll[Any](sigmaProp(false))) }",
        FuncValue(
          Vector((1, SPair(SByteArray, SInt))),
          SubstConstants(
            SelectField.typed[Value[SCollection[SByte.type]]](ValUse(1, SPair(SByteArray, SInt)), 1.toByte),
            ConcreteCollection(
              Array(SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SByteArray, SInt)), 2.toByte)),
              SInt
            ),
            ConcreteCollection(Array(BoolToSigmaProp(FalseLeaf)), SSigmaProp)
          )
        )
      )
    )

    // before v6.0 the expected tree is not parsable
    ErgoTree.fromBytes(expectedTreeBytes_beforeV6.toArray).isRightParsed shouldBe false

    // in v6.0 the expected tree should be parsable and similar to the original tree
    val tree = ErgoTree.fromBytes(expectedTreeBytes_V6.toArray)
    tree.isRightParsed shouldBe true
    tree.header shouldBe t2.header
    tree.constants.length shouldBe t2.constants.length
    tree.root shouldBe t2.root
  }

  property("Numeric.toBytes methods equivalence") {
    lazy val toBytes = newFeature(
      { (x: Byte) => x.toBigEndianBytes },
      "{ (x: Byte) => x.toBytes }",
      sinceVersion = VersionContext.sinceV6AndTreeVersion(0))
    val cases = Seq(
      (0.toByte, Success(Coll(0.toByte))),
      (1.toByte, Success(Coll(1.toByte)))
    )

    testCases(cases, toBytes)
  }


}
