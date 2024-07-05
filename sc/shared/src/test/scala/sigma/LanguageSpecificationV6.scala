package sigma

import org.ergoplatform.sdk.utils.ErgoTreeUtils
import sigma.VersionContext.V6SoftForkVersion
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

  /** Returns the VersionContext with V6 activation and the given ErgoTree version. */
  def sinceV6AndTreeVersion(treeVersion: Byte): VersionContext =
    VersionContext(V6SoftForkVersion, ergoTreeVersion = treeVersion)

  def expectedSuccessForAllTreeVersions[A](value: A, cost: Int, costDetails: CostDetails) = {
    val res = ExpectedResult(Success(value), Some(cost)) -> Some(costDetails)
    Seq(0, 1, 2, 3).map(version => version -> res)
  }

  property("Boolean.toByte") {
    val toByte = newFeature((x: Boolean) => x.toByte, "{ (x: Boolean) => x.toByte }",
      sinceVersion = sinceV6AndTreeVersion(0)
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
        sinceVersion = sinceV6AndTreeVersion(0))

      lazy val compareTo = newFeature(
        (x: (Byte, Byte)) => x._1.compareTo(x._2),
        "{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }",
        sinceVersion = sinceV6AndTreeVersion(0))

      lazy val bitOr = newFeature(
        { (x: (Byte, Byte)) => (x._1 | x._2).toByteExact },
        "{ (x: (Byte, Byte)) => (x._1 | x._2) }",
        sinceVersion = sinceV6AndTreeVersion(0))

      lazy val bitAnd = newFeature(
        { (x: (Byte, Byte)) => (x._1 & x._2).toByteExact },
        "{ (x: (Byte, Byte)) => (x._1 & x._2) }",
        sinceVersion = sinceV6AndTreeVersion(0))

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
        sinceVersion = sinceV6AndTreeVersion(0))

      lazy val compareTo = newFeature((x: (Short, Short)) => x._1.compareTo(x._2),
        "{ (x: (Short, Short)) => x._1.compareTo(x._2) }",
        sinceVersion = sinceV6AndTreeVersion(0))

      lazy val bitOr = newFeature(
      { (x: (Short, Short)) => (x._1 | x._2).toShortExact },
      "{ (x: (Short, Short)) => x._1 | x._2 }",
      sinceVersion = sinceV6AndTreeVersion(0))

      lazy val bitAnd = newFeature(
      { (x: (Short, Short)) => (x._1 & x._2).toShortExact },
      "{ (x: (Short, Short)) => x._1 & x._2 }",
      sinceVersion = sinceV6AndTreeVersion(0))

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
        sinceVersion = sinceV6AndTreeVersion(0))
      lazy val compareTo = newFeature((x: (Int, Int)) => x._1.compareTo(x._2),
        "{ (x: (Int, Int)) => x._1.compareTo(x._2) }",
        sinceVersion = sinceV6AndTreeVersion(0))
      lazy val bitOr = newFeature(
      { (x: (Int, Int)) => x._1 | x._2 },
      "{ (x: (Int, Int)) => x._1 | x._2 }",
      sinceVersion = sinceV6AndTreeVersion(0))
      lazy val bitAnd = newFeature(
      { (x: (Int, Int)) => x._1 & x._2 },
      "{ (x: (Int, Int)) => x._1 & x._2 }",
      sinceVersion = sinceV6AndTreeVersion(0))
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
        sinceVersion = sinceV6AndTreeVersion(0))
      lazy val compareTo = newFeature((x: (Long, Long)) => x._1.compareTo(x._2),
        "{ (x: (Long, Long)) => x._1.compareTo(x._2) }",
        sinceVersion = sinceV6AndTreeVersion(0))

      lazy val bitOr = newFeature(
        { (x: (Long, Long)) => x._1 | x._2 },
        "{ (x: (Long, Long)) => x._1 | x._2 }",
        sinceVersion = sinceV6AndTreeVersion(0))

      lazy val bitAnd = newFeature(
        { (x: (Long, Long)) => x._1 & x._2 },
        "{ (x: (Long, Long)) => x._1 & x._2 }",
        sinceVersion = sinceV6AndTreeVersion(0))

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
        sinceVersion = sinceV6AndTreeVersion(0))
      val toShort = newFeature((x: BigInt) => x.toShort,
        "{ (x: BigInt) => x.toShort }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SShort)),
        sinceVersion = sinceV6AndTreeVersion(0))
      val toInt = newFeature((x: BigInt) => x.toInt,
        "{ (x: BigInt) => x.toInt }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SInt)),
        sinceVersion = sinceV6AndTreeVersion(0))
      val toLong = newFeature((x: BigInt) => x.toLong,
        "{ (x: BigInt) => x.toLong }",
        FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SLong)),
        sinceVersion = sinceV6AndTreeVersion(0))
      lazy val toAbs   = newFeature((x: BigInt) => x.toAbs, "{ (x: BigInt) => x.toAbs }",
        sinceVersion = sinceV6AndTreeVersion(0))
      lazy val compareTo = newFeature((x: (BigInt, BigInt)) => x._1.compareTo(x._2),
        "{ (x: (BigInt, BigInt)) => x._1.compareTo(x._2) }",
        sinceVersion = sinceV6AndTreeVersion(0))
      lazy val bitOr = newFeature({ (x: (BigInt, BigInt)) => x._1 | x._2 },
        "{ (x: (BigInt, BigInt)) => x._1 | x._2 }",
        sinceVersion = sinceV6AndTreeVersion(0))
      lazy val bitAnd = newFeature({ (x: (BigInt, BigInt)) => x._1 & x._2 },
        "{ (x: (BigInt, BigInt)) => x._1 & x._2 }",
        sinceVersion = sinceV6AndTreeVersion(0))

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
      sinceVersion = sinceV6AndTreeVersion(0))

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
      sinceVersion = sinceV6AndTreeVersion(0))

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
      sinceVersion = sinceV6AndTreeVersion(0))

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
      sinceVersion = sinceV6AndTreeVersion(0))

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
      sinceVersion = sinceV6AndTreeVersion(0))

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
      sinceVersion = sinceV6AndTreeVersion(0))

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
      sinceVersion = sinceV6AndTreeVersion(0))

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
      sinceVersion = sinceV6AndTreeVersion(0))
    val cases = Seq(
      (0.toByte, Success(Coll(0.toByte))),
      (1.toByte, Success(Coll(1.toByte)))
    )

    testCases(cases, toBytes)
  }


}
