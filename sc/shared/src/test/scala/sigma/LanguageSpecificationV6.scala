package sigma

import sigma.VersionContext.V6SoftForkVersion
import sigma.ast.ErgoTree.ZeroHeader
import sigma.ast.SCollection.SByteArray
import sigma.ast.syntax.TrueSigmaProp
import sigma.ast._
import sigma.data.{CBigInt, ExactNumeric}
import sigma.eval.{CostDetails, SigmaDsl, TracedCost}
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

  property("Boolean.toByte") {
    val toByte = newFeature((x: Boolean) => x.toByte, "{ (x: Boolean) => x.toByte }",
      sinceVersion = V6SoftForkVersion
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
        sinceVersion = V6SoftForkVersion)

      lazy val compareTo = newFeature(
        (x: (Byte, Byte)) => x._1.compareTo(x._2),
        "{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }",
        sinceVersion = V6SoftForkVersion)

      lazy val bitOr = newFeature(
        { (x: (Byte, Byte)) => (x._1 | x._2).toByteExact },
        "{ (x: (Byte, Byte)) => (x._1 | x._2) }",
        sinceVersion = V6SoftForkVersion)

      lazy val bitAnd = newFeature(
        { (x: (Byte, Byte)) => (x._1 & x._2).toByteExact },
        "{ (x: (Byte, Byte)) => (x._1 & x._2) }",
        sinceVersion = V6SoftForkVersion)

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
        sinceVersion = V6SoftForkVersion)

      lazy val compareTo = newFeature((x: (Short, Short)) => x._1.compareTo(x._2),
        "{ (x: (Short, Short)) => x._1.compareTo(x._2) }",
        sinceVersion = V6SoftForkVersion)

      lazy val bitOr = newFeature(
      { (x: (Short, Short)) => (x._1 | x._2).toShortExact },
      "{ (x: (Short, Short)) => x._1 | x._2 }",
      sinceVersion = V6SoftForkVersion)

      lazy val bitAnd = newFeature(
      { (x: (Short, Short)) => (x._1 & x._2).toShortExact },
      "{ (x: (Short, Short)) => x._1 & x._2 }",
      sinceVersion = V6SoftForkVersion)

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
        sinceVersion = V6SoftForkVersion)
      lazy val compareTo = newFeature((x: (Int, Int)) => x._1.compareTo(x._2),
        "{ (x: (Int, Int)) => x._1.compareTo(x._2) }",
        sinceVersion = V6SoftForkVersion)
      lazy val bitOr = newFeature(
      { (x: (Int, Int)) => x._1 | x._2 },
      "{ (x: (Int, Int)) => x._1 | x._2 }",
      sinceVersion = V6SoftForkVersion)
      lazy val bitAnd = newFeature(
      { (x: (Int, Int)) => x._1 & x._2 },
      "{ (x: (Int, Int)) => x._1 & x._2 }",
      sinceVersion = V6SoftForkVersion)
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
        sinceVersion = V6SoftForkVersion)
      lazy val compareTo = newFeature((x: (Long, Long)) => x._1.compareTo(x._2),
        "{ (x: (Long, Long)) => x._1.compareTo(x._2) }",
        sinceVersion = V6SoftForkVersion)

      lazy val bitOr = newFeature(
        { (x: (Long, Long)) => x._1 | x._2 },
        "{ (x: (Long, Long)) => x._1 | x._2 }",
        sinceVersion = V6SoftForkVersion)

      lazy val bitAnd = newFeature(
        { (x: (Long, Long)) => x._1 & x._2 },
        "{ (x: (Long, Long)) => x._1 & x._2 }",
        sinceVersion = V6SoftForkVersion)

      forAll { x: Long =>
        Seq(toAbs).foreach(_.checkEquality(x))
      }
      forAll { x: (Long, Long) =>
        Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
      }
    }

  }

  property("BigInt methods equivalence (new features)") {
    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // The `Upcast(bigInt, SBigInt)` node is never produced by ErgoScript compiler, but is still valid ErgoTree.
      // Fixed in 6.0
      assertExceptionThrown(
        SBigInt.upcast(CBigInt(new BigInteger("0", 16)).asInstanceOf[AnyVal]),
        _.getMessage.contains("Cannot upcast value")
      )

      // The `Downcast(bigInt, SBigInt)` node is never produced by ErgoScript compiler, but is still valid ErgoTree.
      // Fixed in 6.0
      assertExceptionThrown(
        SBigInt.downcast(CBigInt(new BigInteger("0", 16)).asInstanceOf[AnyVal]),
        _.getMessage.contains("Cannot downcast value")
      )
    } else {
      forAll { x: BigInteger =>
        SBigInt.upcast(CBigInt(x).asInstanceOf[AnyVal]) shouldBe CBigInt(x)
        SBigInt.downcast(CBigInt(x).asInstanceOf[AnyVal]) shouldBe CBigInt(x)
      }
    }

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions the new features are not supported
      // which is checked below
      val toByte = newFeature((x: BigInt) => x.toByte,
        "{ (x: BigInt) => x.toByte }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SByte)),
        sinceVersion = V6SoftForkVersion)
      val toShort = newFeature((x: BigInt) => x.toShort,
        "{ (x: BigInt) => x.toShort }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SShort)),
        sinceVersion = V6SoftForkVersion)
      val toInt = newFeature((x: BigInt) => x.toInt,
        "{ (x: BigInt) => x.toInt }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SInt)),
        sinceVersion = V6SoftForkVersion)
      val toLong = newFeature((x: BigInt) => x.toLong,
        "{ (x: BigInt) => x.toLong }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SLong)),
        sinceVersion = V6SoftForkVersion)
      lazy val toAbs   = newFeature((x: BigInt) => x.toAbs, "{ (x: BigInt) => x.toAbs }",
        sinceVersion = V6SoftForkVersion)
      lazy val compareTo = newFeature((x: (BigInt, BigInt)) => x._1.compareTo(x._2),
        "{ (x: (BigInt, BigInt)) => x._1.compareTo(x._2) }",
        sinceVersion = V6SoftForkVersion)
      lazy val bitOr = newFeature({ (x: (BigInt, BigInt)) => x._1 | x._2 },
        "{ (x: (BigInt, BigInt)) => x._1 | x._2 }",
        sinceVersion = V6SoftForkVersion)
      lazy val bitAnd = newFeature({ (x: (BigInt, BigInt)) => x._1 & x._2 },
        "{ (x: (BigInt, BigInt)) => x._1 & x._2 }",
        sinceVersion = V6SoftForkVersion)

      forAll { x: BigInt =>
        Seq(toByte, toShort, toInt, toLong, toAbs).foreach(_.checkEquality(x))
      }
      forAll { x: (BigInt, BigInt) =>
        Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
      }

      forAll { x: Long =>
        assertExceptionThrown(
          SLong.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]),
          _.getMessage.contains("Cannot downcast value")
        )
      }
      forAll { x: Int =>
        assertExceptionThrown(
          SInt.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]),
          _.getMessage.contains("Cannot downcast value")
        )
      }
      forAll { x: Byte =>
        assertExceptionThrown(
          SByte.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]),
          _.getMessage.contains("Cannot downcast value")
        )
      }
      forAll { x: Short =>
        assertExceptionThrown(
          SShort.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]),
          _.getMessage.contains("Cannot downcast value")
        )
      }
    } else {
      forAll { x: Long =>
          SLong.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]) shouldBe x
      }
      forAll { x: Int =>
          SInt.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]) shouldBe x
      }
      forAll { x: Byte =>
        SByte.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]) shouldBe x
      }
      forAll { x: Short =>
        SShort.downcast(CBigInt(new BigInteger(x.toString)).asInstanceOf[AnyVal]) shouldBe x
      }
    }
  }

  property("Box properties equivalence (new features)") {
    // TODO v6.0: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
    val getReg = newFeature((x: Box) => x.getReg[Int](1).get,
      "{ (x: Box) => x.getReg[Int](1).get }",
      sinceVersion = V6SoftForkVersion)

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
      sinceVersion = V6SoftForkVersion)

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
      sinceVersion = V6SoftForkVersion)

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
      sinceVersion = V6SoftForkVersion)

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
      sinceVersion = V6SoftForkVersion)

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
      sinceVersion = V6SoftForkVersion)

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
      sinceVersion = V6SoftForkVersion)

    if (activatedVersionInTests < VersionContext.V6SoftForkVersion) {
      // NOTE, for such versions getReg is not supported
      // which is checked below

      forAll { x: Coll[SigmaProp] =>
        anyZK.checkEquality(x)
      }
    }
  }

  property("Numeric.toBytes methods equivalence") {
    lazy val toBytes = newFeature(
      { (x: Byte) => x.toBigEndianBytes },
      "{ (x: Byte) => x.toBytes }",
      FuncValue(
        Array((1, SByte)),
        MethodCall.typed[Value[SCollection[SByte.type]]](
          ValUse(1, SByte),
          SByteMethods.getMethodByName("toBytes"),
          Vector(),
          Map()
        )
      ),
      sinceVersion = V6SoftForkVersion)
    val cases = Seq(
      (0.toByte, Success(Coll(0.toByte))),
      (1.toByte, Success(Coll(1.toByte)))
    )

    testCases(cases, toBytes)
  }

  property("Fix substConstants in v6.0 for ErgoTree version > 0") {
    // tree with one segregated constant and v0
    val t1 = ErgoTree(
      header = ErgoTree.setConstantSegregation(ZeroHeader),
      constants = Vector(TrueSigmaProp),
      ConstantPlaceholder(0, SSigmaProp))

    // tree with one segregated constant and max supported version
    val t2 = ErgoTree(
      header = ErgoTree.setConstantSegregation(
        ErgoTree.headerWithVersion(ZeroHeader, VersionContext.MaxSupportedScriptVersion)
      ),
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
          newCost = 2065,
          newVersionedResults = expectedSuccessForAllTreeVersions(Helpers.decodeBytes("100108d27300"), 2065, costDetails(1))
        ),
        // for tree version > 0, the result depend on activated version
        (Coll(t2.bytes: _*), 0) -> Expected(
          Success(expectedTreeBytes_beforeV6),
          cost = 1793,
          expectedDetails = CostDetails.ZeroCost,
          newCost = 2065,
          newVersionedResults = expectedSuccessForAllTreeVersions(expectedTreeBytes_V6, 2065, costDetails(1)))
      ),
      changedFeature(
        changedInVersion = VersionContext.V6SoftForkVersion,
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

}
