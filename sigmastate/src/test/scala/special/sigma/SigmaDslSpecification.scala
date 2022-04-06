package special.sigma

import java.lang.reflect.InvocationTargetException
import java.math.BigInteger
import org.ergoplatform._
import org.ergoplatform.settings.ErgoAlgos
import org.scalacheck.{Arbitrary, Gen}
import scalan.{ExactIntegral, ExactNumeric, ExactOrdering, RType}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scalan.util.Extensions._
import sigma.util.Extensions._
import sigmastate.SCollection._
import sigmastate.Values.IntConstant
import sigmastate._
import sigmastate.basics.DLogProtocol._
import sigmastate.Values._
import sigmastate.lang.Terms.Apply
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.lang.Terms.{MethodCall, PropertyCall}
import sigmastate.utxo._
import special.collection._
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utils.Helpers
import sigmastate.utils.Helpers._
import sigmastate.helpers.TestingHelpers._

import scala.util.{Failure, Success, Try}
import OrderingOps._
import org.ergoplatform.ErgoBox.AdditionalRegisters
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen.frequency
import org.scalatest.{BeforeAndAfterAll, Tag}
import scalan.RType._
import scorex.util.ModifierId
import sigmastate.basics.ProveDHTuple
import sigmastate.interpreter._
import org.scalactic.source.Position
import sigmastate.helpers.SigmaPPrint
import sigmastate.lang.exceptions.CosterException

import scala.collection.mutable

/** This suite tests every method of every SigmaDsl type to be equivalent to
  * the evaluation of the corresponding ErgoScript operation
  *
  * This suite can be used for Cost profiling, i.e. measurements of operations times and
  * comparing them with cost parameteres of the operations.
  *
  * The following settings should be specified for profiling:
  * isMeasureOperationTime = true
  * isMeasureScriptTime = true
  * isLogEnabled = false
  * printTestVectors = false
  * costTracingEnabled = false
  * isTestRun = true
  * perTestWarmUpIters = 1
  * nBenchmarkIters = 1
  */
class SigmaDslSpecification extends SigmaDslTesting
  with CrossVersionProps
  with BeforeAndAfterAll { suite =>

  /** Use VersionContext so that each property in this suite runs under correct
    * parameters.
    */
  protected override def testFun_Run(testName: String, testFun: => Any): Unit = {
    VersionContext.withVersions(activatedVersionInTests, ergoTreeVersionInTests) {
      super.testFun_Run(testName, testFun)
    }
  }

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 30)

  val evalSettingsInTests = ErgoTreeEvaluator.DefaultEvalSettings.copy(
    isMeasureOperationTime = true,
    isMeasureScriptTime = true,
    isLogEnabled = false, // don't commit the `true` value (travis log is too high)
    printTestVectors = false, // don't commit the `true` value (travis log is too high)

    /** Should always be enabled in tests (and false by default)
      * Should be disabled for cost profiling, which case the new costs are not checked.
      */
    costTracingEnabled = true,

    profilerOpt = Some(ErgoTreeEvaluator.DefaultProfiler),
    isTestRun = true
  )

  def warmupSettings(p: Profiler) = evalSettingsInTests.copy(
    isLogEnabled = false,
    printTestVectors = false,
    profilerOpt = Some(p)
  )

  implicit override def evalSettings: EvalSettings = {
    warmupProfiler match {
      case Some(p) => warmupSettings(p)
      case _ => evalSettingsInTests
    }
  }

  override val perTestWarmUpIters = 0

  override val nBenchmarkIters = 0

  override val okRunTestsWithoutMCLowering: Boolean = true

  implicit def IR = createIR()

  def testCases[A, B](cases: Seq[(A, Expected[B])], f: Feature[A, B]) = {
    val table = Table(("x", "y"), cases:_*)
    forAll(table) { (x, expectedRes) =>
      val res = f.checkEquality(x)
      val resValue = res.map(_._1)
      val expected = expectedRes.newResults(ergoTreeVersionInTests)._1
      checkResult(resValue, expected.value, failOnTestVectors = true)
      if (res.isSuccess) {
        res.get._2.cost shouldBe JitCost(expected.verificationCost.get)
      }
    }
  }

  import TestData._

  override protected def beforeAll(): Unit = {
    prepareSamples[BigInt]
    prepareSamples[GroupElement]
    prepareSamples[AvlTree]
    prepareSamples[Box]
    prepareSamples[PreHeader]
    prepareSamples[Header]
    prepareSamples[(BigInt, BigInt)]
    prepareSamples[(GroupElement, GroupElement)]
    prepareSamples[(AvlTree, AvlTree)]
    prepareSamples[(Box, Box)]
    prepareSamples[(PreHeader, PreHeader)]
    prepareSamples[(Header, Header)]
  }

  ///=====================================================
  ///         CostDetails shared among test cases
  ///-----------------------------------------------------
  val traceBase = Array(
    FixedCostItem(Apply),
    FixedCostItem(FuncValue),
    FixedCostItem(GetVar),
    FixedCostItem(OptionGet),
    FixedCostItem(FuncValue.AddToEnvironmentDesc, FuncValue.AddToEnvironmentDesc_CostKind),
    FixedCostItem(ValUse)
  )
  def upcastCostDetails(tpe: SType) = TracedCost(traceBase :+ TypeBasedCostItem(Upcast, tpe))
  def downcastCostDetails(tpe: SType) = TracedCost(traceBase :+ TypeBasedCostItem(Downcast, tpe))
  def arithOpsCostDetails(tpe: SType) = CostDetails(
    Array(
      FixedCostItem(Apply),
      FixedCostItem(FuncValue),
      FixedCostItem(GetVar),
      FixedCostItem(OptionGet),
      FixedCostItem(FuncValue.AddToEnvironmentDesc, FuncValue.AddToEnvironmentDesc_CostKind),
      SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10),  2),
      FixedCostItem(ValUse),
      FixedCostItem(SelectField),
      FixedCostItem(FuncValue.AddToEnvironmentDesc, FuncValue.AddToEnvironmentDesc_CostKind),
      FixedCostItem(ValUse),
      FixedCostItem(SelectField),
      FixedCostItem(FuncValue.AddToEnvironmentDesc, FuncValue.AddToEnvironmentDesc_CostKind),
      FixedCostItem(ValUse),
      FixedCostItem(ValUse),
      TypeBasedCostItem(ArithOp.Plus, tpe),
      FixedCostItem(ValUse),
      FixedCostItem(ValUse),
      TypeBasedCostItem(ArithOp.Minus, tpe),
      FixedCostItem(ValUse),
      FixedCostItem(ValUse),
      TypeBasedCostItem(ArithOp.Multiply, tpe),
      FixedCostItem(ValUse),
      FixedCostItem(ValUse),
      TypeBasedCostItem(ArithOp.Division, tpe),
      FixedCostItem(ValUse),
      FixedCostItem(ValUse),
      TypeBasedCostItem(ArithOp.Modulo, tpe),
      FixedCostItem(Tuple),
      FixedCostItem(Tuple),
      FixedCostItem(Tuple),
      FixedCostItem(Tuple)
    )
  )

  def binaryRelationCostDetails(rel: RelationCompanion, tpe: SType) = CostDetails(
    traceBase ++ Array(
      FixedCostItem(SelectField),
      FixedCostItem(ValUse),
      FixedCostItem(SelectField),
      TypeBasedCostItem(rel, tpe)
    )
  )
  def costNEQ(neqCost: Seq[CostItem]) = CostDetails(
    traceBase ++
    Array(
      FixedCostItem(SelectField),
      FixedCostItem(ValUse),
      FixedCostItem(SelectField)
    ) ++
    neqCost
  )

  def methodCostDetails(sMethod: SMethod, methodCost: Int) = TracedCost(
    traceBase ++ Array(
      FixedCostItem(PropertyCall),
      FixedCostItem(sMethod, FixedCost(JitCost(methodCost)))
    )
  )

  ///=====================================================
  ///              Boolean type operations
  ///-----------------------------------------------------

  property("Boolean methods equivalence") {
    val toByte = newFeature((x: Boolean) => x.toByte, "{ (x: Boolean) => x.toByte }")

    val cases = Seq(
      (true, Success(1.toByte)),
      (false, Success(0.toByte))
    )

    testCases(cases, toByte)
  }

  property("BinXor(logical XOR) equivalence") {
    val binXor = existingFeature((x: (Boolean, Boolean)) => x._1 ^ x._2,
      "{ (x: (Boolean, Boolean)) => x._1 ^ x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SBoolean, SBoolean)))),
        BinXor(
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 1.toByte),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 2.toByte)
        )
      ))
    val newDetails = CostDetails(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(BinXor)
      )
    )
    val newCost = 1788
    def success(b: Boolean) = Expected(Success(b), 36518, newDetails, newCost)
    val cases = Seq(
      (true, true) -> success(false),
      (true, false) -> success(true),
      (false, false) -> success(false),
      (false, true) -> success(true)
    )
    verifyCases(cases, binXor)
  }

  property("verify should respect Context.initCost") {
    val feature = existingFeature((x: (Boolean, Boolean)) => x._1 ^ x._2,
      "{ (x: (Boolean, Boolean)) => x._1 ^ x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SBoolean, SBoolean)))),
        BinXor(
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 1.toByte),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 2.toByte))))
    val newDetails = CostDetails(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(BinXor)
      )
    )
    val expectedCost = 36518
    val newCost = 1788
    val cases = Seq(
      (true, true) -> Expected(Success(false), expectedCost, newDetails, newCost)
    )
    verifyCases(cases, feature)

    val initCost = 100
    initialCostInTests.withValue(initCost) {
      val cases = Seq(
        (true, true) -> Expected(Success(false), expectedCost + initCost, newDetails, newCost + initCost)
      )
      verifyCases(cases, feature)
    }
  }

  property("BinXor(logical XOR) test") {
    val xor = existingFeature((x: (Int, Boolean)) => (x._1 == 0) ^ x._2,
      "{ (x: (Int, Boolean)) => (x._1 == 0) ^ x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SInt, SBoolean)))),
        BinXor(
          EQ(
            SelectField.typed[IntValue](ValUse(1, STuple(Vector(SInt, SBoolean))), 1.toByte),
            IntConstant(0)
          ),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SInt, SBoolean))), 2.toByte)
        )
      ))
    val newDetails = CostDetails(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(BinXor)
      )
    )
    def success(b: Boolean) = Expected(Success(b), 36865, newDetails, 1789)
    val cases = Seq(
      (1095564593, true) -> success(true),
      (-901834021, true) -> success(true),
      (595045530, false) -> success(false),
      (-1157998227, false) -> success(false),
      (0, true) -> success(false),
      (0, false) -> success(true)
    )
    verifyCases(cases, xor)
  }

  property("&& boolean equivalence") {
    lazy val eq = existingFeature((x:(Boolean, Boolean)) => x._1 && x._2,
      "{ (x:(Boolean, Boolean)) => x._1 && x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SBoolean, SBoolean)))),
        BinAnd(
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 1.toByte),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 2.toByte)
        )
      ))
    val newDetails1 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(BinAnd)
      )
    )
    val newDetails2 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField)
      )
    )
    val cases = Seq(
      (false, true) -> Expected(Success(false), 38241, newDetails1, 1786),
      (false, false) -> Expected(Success(false), 38241, newDetails1, 1786),
      (true, true) -> Expected(Success(true), 38241, newDetails2, 1788),
      (true, false) -> Expected(Success(false), 38241, newDetails2, 1788)
    )
    verifyCases(cases, eq)
  }

  property("|| boolean equivalence") {
    lazy val eq = existingFeature((x:(Boolean, Boolean)) => x._1 || x._2,
      "{ (x:(Boolean, Boolean)) => x._1 || x._2 }",
      FuncValue(
        Vector((1, STuple(Vector(SBoolean, SBoolean)))),
        BinOr(
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 1.toByte),
          SelectField.typed[BoolValue](ValUse(1, STuple(Vector(SBoolean, SBoolean))), 2.toByte)
        )
      ))
    val newDetails1 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(BinOr)
      )
    )
    val newDetails2 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(BinOr),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField)
      )
    )
    val cost = 38241
    val cases = Seq(
      (true, false) -> Expected(Success(true), cost, newDetails1, 1786),
      (true, true) -> Expected(Success(true), cost, newDetails1, 1786),
      (false, false) -> Expected(Success(false), cost, newDetails2, 1788),
      (false, true) -> Expected(Success(true), cost, newDetails2, 1788)
    )
    verifyCases(cases, eq)
  }

  def runLazy_And_Or_BooleanEquivalence(implicit evalSettings: EvalSettings) = {
    val newDetails1 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(BinOr)
      )
    )
    val newDetails2 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(BinAnd)
      )
    )
    val newDetails3 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinOr)
      )
    )
    val newDetails4 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinOr)
      )
    )
    val newDetails5 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinOr)
      )
    )
    verifyCases(
      Seq(
        (true, Expected(Success(true), 38467, newDetails1, 1785)),
        (false, Expected(new ArithmeticException("/ by zero")))
      ),
      existingFeature((x: Boolean) => x || (1 / 0 == 1),
        "{ (x: Boolean) => x || (1 / 0 == 1) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinOr(
            ValUse(1, SBoolean),
            EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
          )
        )))

    verifyCases(
      Seq(
        (true, Expected(new ArithmeticException("/ by zero"))),
        (false, Expected(Success(false), 38467, newDetails2, 1785))
      ),
      existingFeature((x: Boolean) => x && (1 / 0 == 1),
        "{ (x: Boolean) => x && (1 / 0 == 1) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            ValUse(1, SBoolean),
            EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
          )
        )))

    verifyCases(
      Seq(
        (false, Expected(Success(false), 40480, newDetails2, 1785)),
        (true, Expected(Success(true), 40480, newDetails3, 1788))
      ),
      existingFeature((x: Boolean) => x && (x || (1 / 0 == 1)),
        "{ (x: Boolean) => x && (x || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            ValUse(1, SBoolean),
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            )
          )
        )))

    verifyCases(
      Seq(
        (false, Expected(Success(false), 42493, newDetails2, 1785)),
        (true, Expected(Success(true), 42493, newDetails4, 1790))
      ),
      existingFeature((x: Boolean) => x && (x && (x || (1 / 0 == 1))),
        "{ (x: Boolean) => x && (x && (x || (1 / 0 == 1))) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            ValUse(1, SBoolean),
            BinAnd(
              ValUse(1, SBoolean),
              BinOr(
                ValUse(1, SBoolean),
                EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
              )
            )
          )
        )))

    verifyCases(
      Seq(
        (false, Expected(Success(false), 44506, newDetails2, 1785)),
        (true, Expected(Success(true), 44506, newDetails5, 1793))
      ),
      existingFeature((x: Boolean) => x && (x && (x && (x || (1 / 0 == 1)))),
        "{ (x: Boolean) => x && (x && (x && (x || (1 / 0 == 1)))) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            ValUse(1, SBoolean),
            BinAnd(
              ValUse(1, SBoolean),
              BinAnd(
                ValUse(1, SBoolean),
                BinOr(
                  ValUse(1, SBoolean),
                  EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
                )
              )
            )
          )
        )))

    val newDetails6 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(LogicalNot),
        FixedCostItem(BinAnd),
        FixedCostItem(LogicalNot),
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinOr)
      )
    )
    verifyCases(
      Seq(
        (false, Expected(new ArithmeticException("/ by zero"))),
        (true, Expected(Success(true), 43281, newDetails6, 1793))
      ),
      existingFeature((x: Boolean) => !(!x && (1 / 0 == 1)) && (x || (1 / 0 == 1)),
        "{ (x: Boolean) => !(!x && (1 / 0 == 1)) && (x || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            LogicalNot(
              BinAnd(
                LogicalNot(ValUse(1, SBoolean)),
                EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
              )
            ),
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            )
          )
        )))

    val newDetails7 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(BinOr),
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse)
      )
    )
    verifyCases(
      Seq(
        (true, Expected(Success(true), 40480, newDetails7, 1788)),
        (false, Expected(new ArithmeticException("/ by zero")))
      ),
      existingFeature((x: Boolean) => (x || (1 / 0 == 1)) && x,
        "{ (x: Boolean) => (x || (1 / 0 == 1)) && x }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            ),
            ValUse(1, SBoolean)
          )
        )))

    val newDetails8 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(BinOr),
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinOr)
      )
    )
    verifyCases(
      Seq(
        (true, Expected(Success(true), 43149, newDetails8, 1790)),
        (false, Expected(new ArithmeticException("/ by zero")))
      ),
      existingFeature((x: Boolean) => (x || (1 / 0 == 1)) && (x || (1 / 0 == 1)),
        "{ (x: Boolean) => (x || (1 / 0 == 1)) && (x || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            ),
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            )
          )
        )))

    val newDetails9 = CostDetails(
      traceBase ++ Array(
        FixedCostItem(LogicalNot),
        FixedCostItem(BinAnd),
        FixedCostItem(LogicalNot),
        FixedCostItem(BinOr),
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinOr)
      )
    )
    verifyCases(
      Seq(
        (true, Expected(Success(true), 45950, newDetails9, 1795)),
        (false, Expected(new ArithmeticException("/ by zero")))
      ),
      existingFeature(
        (x: Boolean) => (!(!x && (1 / 0 == 1)) || (1 / 0 == 0)) && (x || (1 / 0 == 1)),
        "{ (x: Boolean) => (!(!x && (1 / 0 == 1)) || (1 / 0 == 0)) && (x || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BinAnd(
            BinOr(
              LogicalNot(
                BinAnd(
                  LogicalNot(ValUse(1, SBoolean)),
                  EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
                )
              ),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(0))
            ),
            BinOr(
              ValUse(1, SBoolean),
              EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
            )
          )
        )))

    val newDetails10 = CostDetails(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(LogicalNot),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(BinAnd),
        FixedCostItem(LogicalNot),
        FixedCostItem(BinOr),
        FixedCostItem(BinAnd),
        FixedCostItem(ValUse),
        FixedCostItem(BinAnd),
        FixedCostItem(LogicalNot),
        FixedCostItem(BinOr)
      )
    )
    verifyCases(
      Seq(
        (false, Expected(new ArithmeticException("/ by zero"))),
        (true, Expected(Success(true), 48862, newDetails10, 1800))
      ),
      existingFeature(
        (x: Boolean) => (!(!x && (1 / 0 == 1)) || (1 / 0 == 0)) && (!(!x && (1 / 0 == 1)) || (1 / 0 == 1)),
        "{ (x: Boolean) => (!(!x && (1 / 0 == 1)) || (1 / 0 == 0)) && (!(!x && (1 / 0 == 1)) || (1 / 0 == 1)) }",
        FuncValue(
          Vector((1, SBoolean)),
          BlockValue(
            Vector(ValDef(3, List(), LogicalNot(ValUse(1, SBoolean)))),
            BinAnd(
              BinOr(
                LogicalNot(
                  BinAnd(
                    ValUse(3, SBoolean),
                    EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
                  )
                ),
                EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(0))
              ),
              BinOr(
                LogicalNot(
                  BinAnd(
                    ValUse(3, SBoolean),
                    EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
                  )
                ),
                EQ(ArithOp(IntConstant(1), IntConstant(0), OpCode @@ (-99.toByte)), IntConstant(1))
              )
            )
          )
        )))
  }

  property("lazy || and && boolean equivalence") {
    runLazy_And_Or_BooleanEquivalence(evalSettings)
  }  

  property("Byte methods equivalence") {
    SByte.upcast(0.toByte) shouldBe 0.toByte  // boundary test case
    SByte.downcast(0.toByte) shouldBe 0.toByte  // boundary test case

    verifyCases(
      {
        def expect(v: Byte) = Expected(Success(v), 35798, TracedCost(traceBase), 1783)
        Seq(
          (0.toByte, expect(0.toByte)),
          (1.toByte, expect(1.toByte)),
          (55.toByte, expect(55.toByte)),
          (Byte.MaxValue, expect(Byte.MaxValue)),
          (-1.toByte, expect(-1.toByte)),
          (-65.toByte, expect(-65.toByte)),
          (Byte.MinValue, expect(Byte.MinValue))
        )
      },
      existingFeature(
        (x: Byte) => x.toByte, "{ (x: Byte) => x.toByte }",
        FuncValue(Vector((1, SByte)), ValUse(1, SByte))))

    verifyCases(
      {
        def expected(v: Short) = Expected(Success(v), 35902, upcastCostDetails(SShort), 1784)
        Seq(
          (0.toByte, expected(0.toShort)),
          (1.toByte, expected(1.toShort)),
          (55.toByte, expected(55.toShort)),
          (Byte.MaxValue, expected(Byte.MaxValue.toShort)),
          (-1.toByte, expected(-1.toShort)),
          (-65.toByte, expected(-65.toShort)),
          (Byte.MinValue, expected(Byte.MinValue.toShort))
        )
      },
      existingFeature(
        (x: Byte) => x.toShort, "{ (x: Byte) => x.toShort }",
        FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SShort))))

    verifyCases(
      {
        def expected(v: Int) = Expected(Success(v), 35902, upcastCostDetails(SInt), 1784)
        Seq(
          (0.toByte, expected(0)),
          (1.toByte, expected(1)),
          (55.toByte, expected(55)),
          (Byte.MaxValue, expected(Byte.MaxValue.toInt)),
          (-1.toByte, expected(-1)),
          (-65.toByte, expected(-65)),
          (Byte.MinValue, expected(Byte.MinValue.toInt))
        )
      },
      existingFeature(
        (x: Byte) => x.toInt, "{ (x: Byte) => x.toInt }",
        FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SInt))))

    verifyCases(
      {
        def expected(v: Long) = Expected(Success(v), 35902, upcastCostDetails(SLong), 1784)
        Seq(
          (0.toByte, expected(0L)),
          (1.toByte, expected(1L)),
          (55.toByte, expected(55L)),
          (Byte.MaxValue, expected(Byte.MaxValue.toLong)),
          (-1.toByte, expected(-1L)),
          (-65.toByte, expected(-65L)),
          (Byte.MinValue, expected(Byte.MinValue.toLong))
        )
      },
      existingFeature(
        (x: Byte) => x.toLong, "{ (x: Byte) => x.toLong }",
        FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SLong))))

    verifyCases(
      {
        def expected(v: BigInt) = Expected(Success(v), 35932, upcastCostDetails(SBigInt), 1787)
        Seq(
          (0.toByte, expected(CBigInt(new BigInteger("0", 16)))),
          (1.toByte, expected(CBigInt(new BigInteger("1", 16)))),
          (-1.toByte, expected(CBigInt(new BigInteger("-1", 16)))),
          (127.toByte, expected(CBigInt(new BigInteger("7f", 16)))),
          (-128.toByte, expected(CBigInt(new BigInteger("-80", 16)))),
          (90.toByte, expected(CBigInt(new BigInteger("5a", 16)))),
          (-53.toByte, expected(CBigInt(new BigInteger("-35", 16))))
        )
      },
      existingFeature(
        (x: Byte) => x.toBigInt, "{ (x: Byte) => x.toBigInt }",
        FuncValue(Vector((1, SByte)), Upcast(ValUse(1, SByte), SBigInt))))

    val n = ExactIntegral.ByteIsExactIntegral
    verifyCases(
      {
        def success[T](v: (T, (T, (T, (T, T))))) = Expected(Success(v), 39654, arithOpsCostDetails(SByte), 1808)
        Seq(
          ((-128.toByte, -128.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((-128.toByte, 0.toByte), Expected(new ArithmeticException("/ by zero"))),
          ((-128.toByte, 17.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((-128.toByte, 127.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((-120.toByte, 82.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((-103.toByte, 1.toByte), success((-102.toByte, (-104.toByte, (-103.toByte, (-103.toByte, 0.toByte)))))),
          ((-90.toByte, 37.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((-78.toByte, -111.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((-71.toByte, -44.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((-53.toByte, 0.toByte), Expected(new ArithmeticException("/ by zero"))),
          ((-34.toByte, 8.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((-24.toByte, 127.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((-1.toByte, -1.toByte), success((-2.toByte, (0.toByte, (1.toByte, (1.toByte, 0.toByte)))))),
          ((-1.toByte, 23.toByte), success((22.toByte, (-24.toByte, (-23.toByte, (0.toByte, -1.toByte)))))),
          ((0.toByte, -128.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((0.toByte, -23.toByte), success((-23.toByte, (23.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
          ((0.toByte, -1.toByte), success((-1.toByte, (1.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
          ((0.toByte, 0.toByte), Expected(new ArithmeticException("/ by zero"))),
          ((0.toByte, 1.toByte), success((1.toByte, (-1.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
          ((0.toByte, 60.toByte), success((60.toByte, (-60.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
          ((0.toByte, 127.toByte), success((127.toByte, (-127.toByte, (0.toByte, (0.toByte, 0.toByte)))))),
          ((1.toByte, -1.toByte), success((0.toByte, (2.toByte, (-1.toByte, (-1.toByte, 0.toByte)))))),
          ((1.toByte, 0.toByte), Expected(new ArithmeticException("/ by zero"))),
          ((1.toByte, 26.toByte), success((27.toByte, (-25.toByte, (26.toByte, (0.toByte, 1.toByte)))))),
          ((7.toByte, -32.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((33.toByte, 1.toByte), success((34.toByte, (32.toByte, (33.toByte, (33.toByte, 0.toByte)))))),
          ((90.toByte, 0.toByte), Expected(new ArithmeticException("/ by zero"))),
          ((127.toByte, -128.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((127.toByte, -47.toByte), Expected(new ArithmeticException("Byte overflow"))),
          ((127.toByte, 127.toByte), Expected(new ArithmeticException("Byte overflow")))
        )
      },
      existingFeature(
        { (x: (Byte, Byte)) =>
          val a = x._1; val b = x._2
          val plus = n.plus(a, b)
          val minus = n.minus(a, b)
          val mul = n.times(a, b)
          val div = (a / b).toByteExact
          val mod = (a % b).toByteExact
          (plus, (minus, (mul, (div, mod))))
        },
        """{ (x: (Byte, Byte)) =>
         |  val a = x._1; val b = x._2
         |  val plus = a + b
         |  val minus = a - b
         |  val mul = a * b
         |  val div = a / b
         |  val mod = a % b
         |  (plus, (minus, (mul, (div, mod))))
         |}""".stripMargin,
        FuncValue(
          Vector((1, STuple(Vector(SByte, SByte)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[ByteValue](ValUse(1, STuple(Vector(SByte, SByte))), 1.toByte)
              ),
              ValDef(
                4,
                List(),
                SelectField.typed[ByteValue](ValUse(1, STuple(Vector(SByte, SByte))), 2.toByte)
              )
            ),
            Tuple(
              Vector(
                ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-102.toByte)),
                Tuple(
                  Vector(
                    ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-103.toByte)),
                    Tuple(
                      Vector(
                        ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-100.toByte)),
                        Tuple(
                          Vector(
                            ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-99.toByte)),
                            ArithOp(ValUse(3, SByte), ValUse(4, SByte), OpCode @@ (-98.toByte))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ))
  }

  def swapArgs[A](cases: Seq[((A, A), Expected[Boolean])], cost: Int, newCostDetails: CostDetails) =
    cases.map { case ((x, y), res) =>
      ((y, x), Expected(res.value, cost, newCostDetails, 1788))
    }

  def newCasesFrom[A, R](
    cases: Seq[(A, A)]
  )(
    getExpectedRes: (A, A) => R,
    cost: Int,
    newDetails: CostDetails,
    newCost: Int
  ) =
    cases.map { case (x, y) =>
      ((x, y), Expected(Success(getExpectedRes(x, y)), cost = cost, newDetails, newCost))
    }    

  def newCasesFrom2[A, R](cases: Seq[(A, A)])
                        (getExpectedRes: (A, A) => R, cost: Int, newCostDetails: CostDetails) =
    cases.map { case (x, y) =>
      ((x, y), Expected(Success(getExpectedRes(x, y)), cost = cost, expectedDetails = newCostDetails, expectedNewCost = 1786))
    }

  def verifyOp[A: Ordering: Arbitrary]
              (cases: Seq[((A, A), Expected[Boolean])],
               opName: String,
               op: (SValue, SValue) => SValue)
              (expectedFunc: (A, A) => Boolean, generateCases: Boolean = true)
              (implicit tA: RType[A], sampled: Sampled[(A, A)], evalSettings: EvalSettings) = {
    val nameA = RType[A].name
    val tpeA = Evaluation.rtypeToSType(tA)
    verifyCases(cases,
      existingFeature(
        { (x: (A, A)) => expectedFunc(x._1, x._2) },
        s"""{ (x: ($nameA, $nameA)) => x._1 $opName x._2 }""".stripMargin,
        {
          val tPair = SPair(tpeA, tpeA)
          FuncValue(
            Array((1, tPair)),
            op(
              SelectField.typed[Value[SType]](ValUse(1, tPair), 1.toByte),
              SelectField.typed[Value[SType]](ValUse(1, tPair), 2.toByte)
            )
          )
        }
      ),
      preGeneratedSamples = Some(sampled.samples))
  }

  val constNeqCost: Seq[CostItem] = Array[CostItem](FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))))  

  property("Byte LT, GT, NEQ") {
    val o = ExactOrdering.ByteIsExactOrdering
    def expect(v: Boolean) = Expected(Success(v), 36328, binaryRelationCostDetails(LT, SByte), 1788)
    val LT_cases: Seq[((Byte, Byte), Expected[Boolean])] = Seq(
      (-128.toByte, -128.toByte) -> expect(false),
      (-128.toByte, -127.toByte) -> expect(true),
      (-128.toByte, -1.toByte) -> expect(true),
      (-128.toByte, 0.toByte) -> expect(true),
      (-128.toByte, 1.toByte) -> expect(true),
      (-128.toByte, 127.toByte) -> expect(true),
      (-120.toByte, -128.toByte) -> expect(false),
      (-120.toByte, -121.toByte) -> expect(false),
      (-120.toByte, -120.toByte) -> expect(false),
      (-120.toByte, -82.toByte) -> expect(true),
      (-103.toByte, -1.toByte) -> expect(true),
      (-103.toByte, -0.toByte) -> expect(true),
      (-103.toByte, 1.toByte) -> expect(true),
      (-103.toByte, 127.toByte) -> expect(true),
      (-1.toByte, -2.toByte) -> expect(false),
      (-1.toByte, -1.toByte) -> expect(false),
      (-1.toByte, 0.toByte) -> expect(true),
      (-1.toByte, 1.toByte) -> expect(true),
      (0.toByte, -128.toByte) -> expect(false),
      (0.toByte, -1.toByte) -> expect(false),
      (0.toByte, 0.toByte) -> expect(false),
      (0.toByte, 1.toByte) -> expect(true),
      (0.toByte, 60.toByte) -> expect(true),
      (0.toByte, 127.toByte) -> expect(true),
      (1.toByte, -1.toByte) -> expect(false),
      (1.toByte, 0.toByte) -> expect(false),
      (1.toByte, 26.toByte) -> expect(true),
      (7.toByte, -32.toByte) -> expect(false),
      (7.toByte, 0.toByte) -> expect(false),
      (33.toByte, 1.toByte) -> expect(false),
      (126.toByte, 127.toByte) -> expect(true),
      (127.toByte, -128.toByte) -> expect(false),
      (127.toByte, -47.toByte) -> expect(false),
      (127.toByte, 127.toByte) -> expect(false)
    )

    verifyOp(LT_cases, "<", LT.apply)(_ < _)

    verifyOp(
      swapArgs(LT_cases, cost = 36342, newCostDetails = binaryRelationCostDetails(GT, SByte)),
      ">", GT.apply)(_ > _)

    val neqCases = newCasesFrom2(LT_cases.map(_._1))(_ != _, cost = 36337, newCostDetails = costNEQ(constNeqCost))
    verifyOp(neqCases, "!=", NEQ.apply)(_ != _)
  }

  property("Byte LE, GE") {
    val o = ExactOrdering.ByteIsExactOrdering
    def expect(v: Boolean) = Expected(Success(v), 36337, binaryRelationCostDetails(LE, SByte), 1788)
    val LE_cases: Seq[((Byte, Byte), Expected[Boolean])] = Seq(
      (-128.toByte, -128.toByte) -> expect(true),
      (-128.toByte, -127.toByte) -> expect(true),
      (-128.toByte, -1.toByte) -> expect(true),
      (-128.toByte, 0.toByte) -> expect(true),
      (-128.toByte, 1.toByte) -> expect(true),
      (-128.toByte, 127.toByte) -> expect(true),
      (-120.toByte, -128.toByte) -> expect(false),
      (-120.toByte, -121.toByte) -> expect(false),
      (-120.toByte, -120.toByte) -> expect(true),
      (-120.toByte, -82.toByte) -> expect(true),
      (-103.toByte, -1.toByte) -> expect(true),
      (-103.toByte, -0.toByte) -> expect(true),
      (-103.toByte, 1.toByte) -> expect(true),
      (-103.toByte, 127.toByte) -> expect(true),
      (-1.toByte, -2.toByte) -> expect(false),
      (-1.toByte, -1.toByte) -> expect(true),
      (-1.toByte, 0.toByte) -> expect(true),
      (-1.toByte, 1.toByte) -> expect(true),
      (0.toByte, -128.toByte) -> expect(false),
      (0.toByte, -1.toByte) -> expect(false),
      (0.toByte, 0.toByte) -> expect(true),
      (0.toByte, 1.toByte) -> expect(true),
      (0.toByte, 60.toByte) -> expect(true),
      (0.toByte, 127.toByte) -> expect(true),
      (1.toByte, -1.toByte) -> expect(false),
      (1.toByte, 0.toByte) -> expect(false),
      (1.toByte, 1.toByte) -> expect(true),
      (1.toByte, 26.toByte) -> expect(true),
      (7.toByte, -32.toByte) -> expect(false),
      (7.toByte, 0.toByte) -> expect(false),
      (33.toByte, 1.toByte) -> expect(false),
      (126.toByte, 127.toByte) -> expect(true),
      (127.toByte, -128.toByte) -> expect(false),
      (127.toByte, -47.toByte) -> expect(false),
      (127.toByte, 127.toByte) -> expect(true)
    )

    verifyOp(LE_cases, "<=", LE.apply)(_ <= _)

    verifyOp(
      swapArgs(LE_cases, cost = 36336, newCostDetails = binaryRelationCostDetails(GE, SByte)),
      ">=", GE.apply)(_ >= _)
  }

  property("Byte methods equivalence (new features)") {
    lazy val toBytes = newFeature((x: Byte) => x.toBytes, "{ (x: Byte) => x.toBytes }")
    lazy val toBits = newFeature((x: Byte) => x.toBits, "{ (x: Byte) => x.toBits }")
    lazy val toAbs = newFeature((x: Byte) => x.toAbs, "{ (x: Byte) => x.toAbs }")
    lazy val compareTo = newFeature(
      (x: (Byte, Byte)) => x._1.compareTo(x._2),
      "{ (x: (Byte, Byte)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature(
    { (x: (Byte, Byte)) => (x._1 | x._2).toByteExact },
    "{ (x: (Byte, Byte)) => (x._1 | x._2).toByteExact }")

    lazy val bitAnd = newFeature(
    { (x: (Byte, Byte)) => (x._1 & x._2).toByteExact },
    "{ (x: (Byte, Byte)) => (x._1 & x._2).toByteExact }")

    forAll { x: Byte =>
      Seq(toBytes, toBits, toAbs).foreach(f => f.checkEquality(x))
    }

    forAll { x: (Byte, Byte) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("Short methods equivalence") {
    SShort.upcast(0.toShort) shouldBe 0.toShort  // boundary test case
    SShort.downcast(0.toShort) shouldBe 0.toShort  // boundary test case

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35976, downcastCostDetails(SByte), 1784)
        Seq(
          (Short.MinValue, Expected(new ArithmeticException("Byte overflow"))),
          (-21626.toShort, Expected(new ArithmeticException("Byte overflow"))),
          (Byte.MinValue.toShort, success(Byte.MinValue)),
          (-1.toShort, success(-1.toByte)),
          (0.toShort, success(0.toByte)),
          (1.toShort, success(1.toByte)),
          (Byte.MaxValue.toShort, success(Byte.MaxValue)),
          (11768.toShort, Expected(new ArithmeticException("Byte overflow"))),
          (Short.MaxValue, Expected(new ArithmeticException("Byte overflow")))
        )
      },
      existingFeature((x: Short) => x.toByteExact,
        "{ (x: Short) => x.toByte }",
        FuncValue(Vector((1, SShort)), Downcast(ValUse(1, SShort), SByte))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35798, TracedCost(traceBase), 1783)
        Seq(
          (-32768.toShort, success(-32768.toShort)),
          (-27798.toShort, success(-27798.toShort)),
          (-1.toShort, success(-1.toShort)),
          (0.toShort, success(0.toShort)),
          (1.toShort, success(1.toShort)),
          (27929.toShort, success(27929.toShort)),
          (32767.toShort, success(32767.toShort))
        )
      },
      existingFeature((x: Short) => x.toShort,
        "{ (x: Short) => x.toShort }",
        FuncValue(Vector((1, SShort)), ValUse(1, SShort))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35902, upcastCostDetails(SInt), 1784)
        Seq(
          (-32768.toShort, success(-32768)),
          (-21064.toShort, success(-21064)),
          (-1.toShort, success(-1)),
          (0.toShort, success(0)),
          (1.toShort, success(1)),
          (18388.toShort, success(18388)),
          (32767.toShort, success(32767))
        )
      },
      existingFeature((x: Short) => x.toInt,
        "{ (x: Short) => x.toInt }",
        FuncValue(Vector((1, SShort)), Upcast(ValUse(1, SShort), SInt))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35902, upcastCostDetails(SLong), 1784)
        Seq(
          (-32768.toShort, success(-32768L)),
          (-23408.toShort, success(-23408L)),
          (-1.toShort, success(-1L)),
          (0.toShort, success(0L)),
          (1.toShort, success(1L)),
          (23318.toShort, success(23318L)),
          (32767.toShort, success(32767L))
        )
      },
      existingFeature((x: Short) => x.toLong,
        "{ (x: Short) => x.toLong }",
        FuncValue(Vector((1, SShort)), Upcast(ValUse(1, SShort), SLong))))

    verifyCases(
      {
        def success(v: BigInt) = Expected(Success(v), 35932, upcastCostDetails(SBigInt), 1787)
        Seq(
          (-32768.toShort, success(CBigInt(new BigInteger("-8000", 16)))),
          (-26248.toShort, success(CBigInt(new BigInteger("-6688", 16)))),
          (-1.toShort, success(CBigInt(new BigInteger("-1", 16)))),
          (0.toShort, success(CBigInt(new BigInteger("0", 16)))),
          (1.toShort, success(CBigInt(new BigInteger("1", 16)))),
          (22845.toShort, success(CBigInt(new BigInteger("593d", 16)))),
          (32767.toShort, success(CBigInt(new BigInteger("7fff", 16))))
        )
      },
      existingFeature((x: Short) => x.toBigInt,
        "{ (x: Short) => x.toBigInt }",
        FuncValue(Vector((1, SShort)), Upcast(ValUse(1, SShort), SBigInt))))

    val n = ExactIntegral.ShortIsExactIntegral
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 39654, arithOpsCostDetails(SShort), 1808)
        Seq(
          ((-32768.toShort, 1.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((-32768.toShort, 4006.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((-21384.toShort, 0.toShort), Expected(new ArithmeticException("/ by zero"))),
          ((-19027.toShort, 6073.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((-16800.toShort, 32767.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((-1.toShort, -30005.toShort), success((-30006.toShort, (30004.toShort, (30005.toShort, (0.toShort, -1.toShort)))))),
          ((-1.toShort, 0.toShort), Expected(new ArithmeticException("/ by zero"))),
          ((0.toShort, -1.toShort), success((-1.toShort, (1.toShort, (0.toShort, (0.toShort, 0.toShort)))))),
          ((0.toShort, 0.toShort), Expected(new ArithmeticException("/ by zero"))),
          ((0.toShort, 1.toShort), success((1.toShort, (-1.toShort, (0.toShort, (0.toShort, 0.toShort)))))),
          ((0.toShort, 25105.toShort), success((25105.toShort, (-25105.toShort, (0.toShort, (0.toShort, 0.toShort)))))),
          ((1.toShort, -32768.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((1.toShort, -1.toShort), success((0.toShort, (2.toShort, (-1.toShort, (-1.toShort, 0.toShort)))))),
          ((1.toShort, 0.toShort), Expected(new ArithmeticException("/ by zero"))),
          ((605.toShort, 7698.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((5094.toShort, -32768.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((5350.toShort, -1.toShort), success((5349.toShort, (5351.toShort, (-5350.toShort, (-5350.toShort, 0.toShort)))))),
          ((8115.toShort, -32768.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((14217.toShort, 32767.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((16223.toShort, -11686.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((16989.toShort, 1.toShort), success((16990.toShort, (16988.toShort, (16989.toShort, (16989.toShort, 0.toShort)))))),
          ((20397.toShort, -4450.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((20488.toShort, 1.toShort), success((20489.toShort, (20487.toShort, (20488.toShort, (20488.toShort, 0.toShort)))))),
          ((32767.toShort, -32768.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((32767.toShort, -13423.toShort), Expected(new ArithmeticException("Short overflow"))),
          ((32767.toShort, 32767.toShort), Expected(new ArithmeticException("Short overflow")))
        )
      },
      existingFeature(
        { (x: (Short, Short)) =>
          val a = x._1; val b = x._2
          val plus = n.plus(a, b)
          val minus = n.minus(a, b)
          val mul = n.times(a, b)
          val div = (a / b).toShortExact
          val mod = (a % b).toShortExact
          (plus, (minus, (mul, (div, mod))))
        },
        """{ (x: (Short, Short)) =>
         |  val a = x._1; val b = x._2
         |  val plus = a + b
         |  val minus = a - b
         |  val mul = a * b
         |  val div = a / b
         |  val mod = a % b
         |  (plus, (minus, (mul, (div, mod))))
         |}""".stripMargin,
        FuncValue(
          Vector((1, STuple(Vector(SShort, SShort)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[ShortValue](ValUse(1, STuple(Vector(SShort, SShort))), 1.toByte)
              ),
              ValDef(
                4,
                List(),
                SelectField.typed[ShortValue](ValUse(1, STuple(Vector(SShort, SShort))), 2.toByte)
              )
            ),
            Tuple(
              Vector(
                ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-102.toByte)),
                Tuple(
                  Vector(
                    ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-103.toByte)),
                    Tuple(
                      Vector(
                        ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-100.toByte)),
                        Tuple(
                          Vector(
                            ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-99.toByte)),
                            ArithOp(ValUse(3, SShort), ValUse(4, SShort), OpCode @@ (-98.toByte))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ))
  }

  property("Short LT, GT, NEQ") {
    val o = ExactOrdering.ShortIsExactOrdering
    def expect(v: Boolean) = Expected(Success(v), 36328, binaryRelationCostDetails(LT, SShort), 1788)
    val LT_cases: Seq[((Short, Short), Expected[Boolean])] = Seq(
      (Short.MinValue, Short.MinValue) -> expect(false),
      (Short.MinValue, (Short.MinValue + 1).toShort) -> expect(true),
      (Short.MinValue, -1.toShort) -> expect(true),
      (Short.MinValue, 0.toShort) -> expect(true),
      (Short.MinValue, 1.toShort) -> expect(true),
      (Short.MinValue, Short.MaxValue) -> expect(true),
      (-120.toShort, Short.MinValue) -> expect(false),
      (-120.toShort, -121.toShort) -> expect(false),
      (-120.toShort, -120.toShort) -> expect(false),
      (-120.toShort, -82.toShort) -> expect(true),
      (-103.toShort, -1.toShort) -> expect(true),
      (-103.toShort, -0.toShort) -> expect(true),
      (-103.toShort, 1.toShort) -> expect(true),
      (-103.toShort, Short.MaxValue) -> expect(true),
      (-1.toShort, -2.toShort) -> expect(false),
      (-1.toShort, -1.toShort) -> expect(false),
      (-1.toShort, 0.toShort) -> expect(true),
      (-1.toShort, 1.toShort) -> expect(true),
      (0.toShort, Short.MinValue) -> expect(false),
      (0.toShort, -1.toShort) -> expect(false),
      (0.toShort, 0.toShort) -> expect(false),
      (0.toShort, 1.toShort) -> expect(true),
      (0.toShort, 60.toShort) -> expect(true),
      (0.toShort, Short.MaxValue) -> expect(true),
      (1.toShort, -1.toShort) -> expect(false),
      (1.toShort, 0.toShort) -> expect(false),
      (1.toShort, 26.toShort) -> expect(true),
      (7.toShort, -32.toShort) -> expect(false),
      (7.toShort, 0.toShort) -> expect(false),
      (33.toShort, 1.toShort) -> expect(false),
      (126.toShort, Short.MaxValue) -> expect(true),
      (Short.MaxValue, Short.MinValue) -> expect(false),
      (Short.MaxValue, -47.toShort) -> expect(false),
      (Short.MaxValue, Short.MaxValue) -> expect(false)
    )

    verifyOp(LT_cases, "<", LT.apply)(_ < _)

    verifyOp(swapArgs(LT_cases, cost = 36342, newCostDetails = binaryRelationCostDetails(GT, SShort)), ">", GT.apply)(_ > _)

    val neqCases = newCasesFrom2(LT_cases.map(_._1))(_ != _, cost = 36337, newCostDetails = costNEQ(constNeqCost))
    verifyOp(neqCases, "!=", NEQ.apply)(_ != _)
  }

  property("Short LE, GE") {
    val o = ExactOrdering.ShortIsExactOrdering
    def expect(v: Boolean) = Expected(Success(v), 36337, binaryRelationCostDetails(LE, SShort), 1788)
    val LE_cases: Seq[((Short, Short), Expected[Boolean])] = Seq(
      (Short.MinValue, Short.MinValue) -> expect(true),
      (Short.MinValue, (Short.MinValue + 1).toShort) -> expect(true),
      (Short.MinValue, -1.toShort) -> expect(true),
      (Short.MinValue, 0.toShort) -> expect(true),
      (Short.MinValue, 1.toShort) -> expect(true),
      (Short.MinValue, Short.MaxValue) -> expect(true),
      (-120.toShort, Short.MinValue) -> expect(false),
      (-120.toShort, -121.toShort) -> expect(false),
      (-120.toShort, -120.toShort) -> expect(true),
      (-120.toShort, -82.toShort) -> expect(true),
      (-103.toShort, -1.toShort) -> expect(true),
      (-103.toShort, -0.toShort) -> expect(true),
      (-103.toShort, 1.toShort) -> expect(true),
      (-103.toShort, Short.MaxValue) -> expect(true),
      (-1.toShort, -2.toShort) -> expect(false),
      (-1.toShort, -1.toShort) -> expect(true),
      (-1.toShort, 0.toShort) -> expect(true),
      (-1.toShort, 1.toShort) -> expect(true),
      (0.toShort, Short.MinValue) -> expect(false),
      (0.toShort, -1.toShort) -> expect(false),
      (0.toShort, 0.toShort) -> expect(true),
      (0.toShort, 1.toShort) -> expect(true),
      (0.toShort, 60.toShort) -> expect(true),
      (0.toShort, Short.MaxValue) -> expect(true),
      (1.toShort, -1.toShort) -> expect(false),
      (1.toShort, 0.toShort) -> expect(false),
      (1.toShort, 1.toShort) -> expect(true),
      (1.toShort, 26.toShort) -> expect(true),
      (7.toShort, -32.toShort) -> expect(false),
      (7.toShort, 0.toShort) -> expect(false),
      (33.toShort, 1.toShort) -> expect(false),
      (126.toShort, Short.MaxValue) -> expect(true),
      (Short.MaxValue, Short.MinValue) -> expect(false),
      (Short.MaxValue, -47.toShort) -> expect(false),
      (Short.MaxValue, Short.MaxValue) -> expect(true)
    )

    verifyOp(LE_cases, "<=", LE.apply)(_ <= _)

    verifyOp(
      swapArgs(LE_cases, cost = 36336, newCostDetails = binaryRelationCostDetails(GE, SShort)),
      ">=", GE.apply)(_ >= _)
  }

  property("Short methods equivalence (new features)") {
    lazy val toBytes = newFeature((x: Short) => x.toBytes, "{ (x: Short) => x.toBytes }")
    lazy val toBits = newFeature((x: Short) => x.toBits, "{ (x: Short) => x.toBits }")
    lazy val toAbs = newFeature((x: Short) => x.toAbs, "{ (x: Short) => x.toAbs }")

    lazy val compareTo = newFeature((x: (Short, Short)) => x._1.compareTo(x._2),
      "{ (x: (Short, Short)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature(
    { (x: (Short, Short)) => (x._1 | x._2).toShortExact },
    "{ (x: (Short, Short)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (Short, Short)) => (x._1 & x._2).toShortExact },
    "{ (x: (Short, Short)) => x._1 & x._2 }")

    forAll { x: Short =>
      Seq(toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (Short, Short) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("Int methods equivalence") {
    SInt.upcast(0) shouldBe 0  // boundary test case
    SInt.downcast(0) shouldBe 0  // boundary test case

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35976, downcastCostDetails(SByte), 1784)
        Seq(
          (Int.MinValue, Expected(new ArithmeticException("Byte overflow"))),
          (-2014394379, Expected(new ArithmeticException("Byte overflow"))),
          (Byte.MinValue.toInt, success(Byte.MinValue)),
          (-1, success(-1.toByte)),
          (0, success(0.toByte)),
          (1, success(1.toByte)),
          (Byte.MaxValue.toInt, success(Byte.MaxValue)),
          (181686429, Expected(new ArithmeticException("Byte overflow"))),
          (Int.MaxValue, Expected(new ArithmeticException("Byte overflow")))
        )
      },
      existingFeature((x: Int) => x.toByteExact,
        "{ (x: Int) => x.toByte }",
        FuncValue(Vector((1, SInt)), Downcast(ValUse(1, SInt), SByte))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35976, downcastCostDetails(SShort), 1784)
        Seq(
          (Int.MinValue, Expected(new ArithmeticException("Short overflow"))),
          (Short.MinValue - 1, Expected(new ArithmeticException("Short overflow"))),
          (Short.MinValue.toInt, success(Short.MinValue)),
          (-1, success(-1.toShort)),
          (0, success(0.toShort)),
          (1, success(1.toShort)),
          (Short.MaxValue.toInt, success(Short.MaxValue)),
          (Short.MaxValue + 1, Expected(new ArithmeticException("Short overflow"))),
          (Int.MaxValue, Expected(new ArithmeticException("Short overflow")))
        )
      },
      existingFeature((x: Int) => x.toShortExact,
        "{ (x: Int) => x.toShort }",
        FuncValue(Vector((1, SInt)), Downcast(ValUse(1, SInt), SShort))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35798, TracedCost(traceBase), 1783)
        Seq(
          (Int.MinValue, success(Int.MinValue)),
          (-1, success(-1)),
          (0, success(0)),
          (1, success(1)),
          (Int.MaxValue, success(Int.MaxValue))
        )
      },
      existingFeature((x: Int) => x.toInt,
        "{ (x: Int) => x.toInt }",
        FuncValue(Vector((1, SInt)), ValUse(1, SInt))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35902, upcastCostDetails(SLong), 1784)
        Seq(
          (Int.MinValue, success(Int.MinValue.toLong)),
          (-1, success(-1L)),
          (0, success(0L)),
          (1, success(1L)),
          (Int.MaxValue, success(Int.MaxValue.toLong))
        )
      },
      existingFeature((x: Int) => x.toLong,
        "{ (x: Int) => x.toLong }",
        FuncValue(Vector((1, SInt)), Upcast(ValUse(1, SInt), SLong))))

    verifyCases(
      {
        def success(v: BigInt) = Expected(Success(v), 35932, upcastCostDetails(SBigInt), 1787)
        Seq(
          (Int.MinValue, success(CBigInt(new BigInteger("-80000000", 16)))),
          (-1937187314, success(CBigInt(new BigInteger("-737721f2", 16)))),
          (-1, success(CBigInt(new BigInteger("-1", 16)))),
          (0, success(CBigInt(new BigInteger("0", 16)))),
          (1, success(CBigInt(new BigInteger("1", 16)))),
          (1542171288, success(CBigInt(new BigInteger("5bebaa98", 16)))),
          (Int.MaxValue, success(CBigInt(new BigInteger("7fffffff", 16))))
        )
      },
      existingFeature((x: Int) => x.toBigInt,
        "{ (x: Int) => x.toBigInt }",
        FuncValue(Vector((1, SInt)), Upcast(ValUse(1, SInt), SBigInt))))

    val n = ExactNumeric.IntIsExactNumeric
    verifyCases(
    {
      def success[T](v: T) = Expected(Success(v), 39654, arithOpsCostDetails(SInt), 1808)
      Seq(
        ((Int.MinValue, 449583993), Expected(new ArithmeticException("integer overflow"))),
        ((-1589633733, 2147483647), Expected(new ArithmeticException("integer overflow"))),
        ((-1585471506, -1), success((-1585471507, (-1585471505, (1585471506, (1585471506, 0)))))),
        ((-1569005179, 1230236634), Expected(new ArithmeticException("integer overflow"))),
        ((-1493733356, -1319619597), Expected(new ArithmeticException("integer overflow"))),
        ((-1100263120, -880052091), Expected(new ArithmeticException("integer overflow"))),
        ((-1055955857, 309147303), Expected(new ArithmeticException("integer overflow"))),
        ((-569807371, 0), Expected(new ArithmeticException("/ by zero"))),
        ((-522264843, 2147483647), Expected(new ArithmeticException("integer overflow"))),
        ((-109552389, 0), Expected(new ArithmeticException("/ by zero"))),
        ((-1, -2147483648), Expected(new ArithmeticException("integer overflow"))),
        ((-1, -1), success((-2, (0, (1, (1, 0)))))),
        ((-1, 0), Expected(new ArithmeticException("/ by zero"))),
        ((0, -2147483648), Expected(new ArithmeticException("integer overflow"))),
        ((1, -1525049432), success((-1525049431, (1525049433, (-1525049432, (0, 1)))))),
        ((1, 0), Expected(new ArithmeticException("/ by zero"))),
        ((1, 805353746), success((805353747, (-805353745, (805353746, (0, 1)))))),
        ((1, 2147483647), Expected(new ArithmeticException("integer overflow"))),
        ((475797978, 0), Expected(new ArithmeticException("/ by zero"))),
        ((782343922, -1448560539), Expected(new ArithmeticException("integer overflow"))),
        ((928769361, 542647292), Expected(new ArithmeticException("integer overflow"))),
        ((1568062151, 0), Expected(new ArithmeticException("/ by zero"))),
        ((1698252401, -1), success((1698252400, (1698252402, (-1698252401, (-1698252401, 0)))))),
        ((1949795740, -1575667037), Expected(new ArithmeticException("integer overflow"))),
        ((Int.MaxValue, -1), Expected(new ArithmeticException("integer overflow"))),
        ((Int.MaxValue, 1), Expected(new ArithmeticException("integer overflow"))),
        ((Int.MaxValue, 1738276576), Expected(new ArithmeticException("integer overflow")))
      )
    },
    existingFeature(
      { (x: (Int, Int)) =>
        val a = x._1; val b = x._2
        val plus = n.plus(a, b)
        val minus = n.minus(a, b)
        val mul = n.times(a, b)
        val div = a / b
        val mod = a % b
        (plus, (minus, (mul, (div, mod))))
      },
      """{ (x: (Int, Int)) =>
        |  val a = x._1; val b = x._2
        |  val plus = a + b
        |  val minus = a - b
        |  val mul = a * b
        |  val div = a / b
        |  val mod = a % b
        |  (plus, (minus, (mul, (div, mod))))
        |}""".stripMargin,
        FuncValue(
          Vector((1, STuple(Vector(SInt, SInt)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[IntValue](ValUse(1, STuple(Vector(SInt, SInt))), 1.toByte)
              ),
              ValDef(
                4,
                List(),
                SelectField.typed[IntValue](ValUse(1, STuple(Vector(SInt, SInt))), 2.toByte)
              )
            ),
            Tuple(
              Vector(
                ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-102.toByte)),
                Tuple(
                  Vector(
                    ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-103.toByte)),
                    Tuple(
                      Vector(
                        ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-100.toByte)),
                        Tuple(
                          Vector(
                            ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-99.toByte)),
                            ArithOp(ValUse(3, SInt), ValUse(4, SInt), OpCode @@ (-98.toByte))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )))
  }

  property("Int LT, GT, NEQ") {
    val o = ExactOrdering.IntIsExactOrdering
    def expect(v: Boolean) = Expected(Success(v), 36328, binaryRelationCostDetails(LT, SInt), 1788)
    val LT_cases: Seq[((Int, Int), Expected[Boolean])] = Seq(
      (Int.MinValue, Int.MinValue) -> expect(false),
      (Int.MinValue, (Int.MinValue + 1).toInt) -> expect(true),
      (Int.MinValue, -1.toInt) -> expect(true),
      (Int.MinValue, 0.toInt) -> expect(true),
      (Int.MinValue, 1.toInt) -> expect(true),
      (Int.MinValue, Int.MaxValue) -> expect(true),
      (-120.toInt, Int.MinValue) -> expect(false),
      (-120.toInt, -121.toInt) -> expect(false),
      (-120.toInt, -120.toInt) -> expect(false),
      (-120.toInt, -82.toInt) -> expect(true),
      (-103.toInt, -1.toInt) -> expect(true),
      (-103.toInt, -0.toInt) -> expect(true),
      (-103.toInt, 1.toInt) -> expect(true),
      (-103.toInt, Int.MaxValue) -> expect(true),
      (-1.toInt, -2.toInt) -> expect(false),
      (-1.toInt, -1.toInt) -> expect(false),
      (-1.toInt, 0.toInt) -> expect(true),
      (-1.toInt, 1.toInt) -> expect(true),
      (0.toInt, Int.MinValue) -> expect(false),
      (0.toInt, -1.toInt) -> expect(false),
      (0.toInt, 0.toInt) -> expect(false),
      (0.toInt, 1.toInt) -> expect(true),
      (0.toInt, 60.toInt) -> expect(true),
      (0.toInt, Int.MaxValue) -> expect(true),
      (1.toInt, -1.toInt) -> expect(false),
      (1.toInt, 0.toInt) -> expect(false),
      (1.toInt, 26.toInt) -> expect(true),
      (7.toInt, -32.toInt) -> expect(false),
      (7.toInt, 0.toInt) -> expect(false),
      (33.toInt, 1.toInt) -> expect(false),
      (126.toInt, Int.MaxValue) -> expect(true),
      (Int.MaxValue, Int.MinValue) -> expect(false),
      (Int.MaxValue, -47.toInt) -> expect(false),
      (Int.MaxValue, Int.MaxValue) -> expect(false)
    )

    verifyOp(LT_cases, "<", LT.apply)(_ < _)

    verifyOp(
      swapArgs(LT_cases, cost = 36342, newCostDetails = binaryRelationCostDetails(GT, SInt)),
      ">", GT.apply)(_ > _)

    val neqCases = newCasesFrom2(LT_cases.map(_._1))(_ != _, cost = 36337, newCostDetails = costNEQ(constNeqCost))
    verifyOp(neqCases, "!=", NEQ.apply)(_ != _)
  }

  property("Int LE, GE") {
    val o = ExactOrdering.IntIsExactOrdering
    def expect(v: Boolean) = Expected(Success(v), 36337, binaryRelationCostDetails(LE, SInt), 1788)
    val LE_cases: Seq[((Int, Int), Expected[Boolean])] = Seq(
      (Int.MinValue, Int.MinValue) -> expect(true),
      (Int.MinValue, (Int.MinValue + 1).toInt) -> expect(true),
      (Int.MinValue, -1.toInt) -> expect(true),
      (Int.MinValue, 0.toInt) -> expect(true),
      (Int.MinValue, 1.toInt) -> expect(true),
      (Int.MinValue, Int.MaxValue) -> expect(true),
      (-120.toInt, Int.MinValue) -> expect(false),
      (-120.toInt, -121.toInt) -> expect(false),
      (-120.toInt, -120.toInt) -> expect(true),
      (-120.toInt, -82.toInt) -> expect(true),
      (-103.toInt, -1.toInt) -> expect(true),
      (-103.toInt, -0.toInt) -> expect(true),
      (-103.toInt, 1.toInt) -> expect(true),
      (-103.toInt, Int.MaxValue) -> expect(true),
      (-1.toInt, -2.toInt) -> expect(false),
      (-1.toInt, -1.toInt) -> expect(true),
      (-1.toInt, 0.toInt) -> expect(true),
      (-1.toInt, 1.toInt) -> expect(true),
      (0.toInt, Int.MinValue) -> expect(false),
      (0.toInt, -1.toInt) -> expect(false),
      (0.toInt, 0.toInt) -> expect(true),
      (0.toInt, 1.toInt) -> expect(true),
      (0.toInt, 60.toInt) -> expect(true),
      (0.toInt, Int.MaxValue) -> expect(true),
      (1.toInt, -1.toInt) -> expect(false),
      (1.toInt, 0.toInt) -> expect(false),
      (1.toInt, 1.toInt) -> expect(true),
      (1.toInt, 26.toInt) -> expect(true),
      (7.toInt, -32.toInt) -> expect(false),
      (7.toInt, 0.toInt) -> expect(false),
      (33.toInt, 1.toInt) -> expect(false),
      (126.toInt, Int.MaxValue) -> expect(true),
      (Int.MaxValue, Int.MinValue) -> expect(false),
      (Int.MaxValue, -47.toInt) -> expect(false),
      (Int.MaxValue, Int.MaxValue) -> expect(true)
    )

    verifyOp(LE_cases, "<=", LE.apply)(_ <= _)

    verifyOp(
      swapArgs(LE_cases, cost = 36336, newCostDetails = binaryRelationCostDetails(GE, SInt)),
      ">=", GE.apply)(_ >= _)
  }

  property("Int methods equivalence (new features)") {
    lazy val toBytes = newFeature((x: Int) => x.toBytes, "{ (x: Int) => x.toBytes }")
    lazy val toBits = newFeature((x: Int) => x.toBits, "{ (x: Int) => x.toBits }")
    lazy val toAbs = newFeature((x: Int) => x.toAbs, "{ (x: Int) => x.toAbs }")
    lazy val compareTo = newFeature((x: (Int, Int)) => x._1.compareTo(x._2),
      "{ (x: (Int, Int)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature(
    { (x: (Int, Int)) => x._1 | x._2 },
    "{ (x: (Int, Int)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (Int, Int)) => x._1 & x._2 },
    "{ (x: (Int, Int)) => x._1 & x._2 }")

    forAll { x: Int =>
      Seq(toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (Int, Int) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("Long downcast and upcast identity") {
    forAll { x: Long =>
      SLong.upcast(x) shouldBe x  // boundary test case
      SLong.downcast(x) shouldBe x  // boundary test case
    }
  }

  property("Long.toByte method") {
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35976, downcastCostDetails(SByte), 1784)
        Seq(
          (Long.MinValue, Expected(new ArithmeticException("Byte overflow"))),
          (Byte.MinValue.toLong - 1, Expected(new ArithmeticException("Byte overflow"))),
          (Byte.MinValue.toLong, success(Byte.MinValue)),
          (-1L, success(-1.toByte)),
          (0L, success(0.toByte)),
          (1L, success(1.toByte)),
          (Byte.MaxValue.toLong, success(Byte.MaxValue)),
          (Byte.MaxValue.toLong + 1, Expected(new ArithmeticException("Byte overflow"))),
          (Long.MinValue, Expected(new ArithmeticException("Byte overflow")))
        )
      },
      existingFeature((x: Long) => x.toByteExact,
        "{ (x: Long) => x.toByte }",
        FuncValue(Vector((1, SLong)), Downcast(ValUse(1, SLong), SByte))))
  }

  property("Long.toShort method") {
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35976, downcastCostDetails(SShort), 1784)
        Seq(
          (Long.MinValue, Expected(new ArithmeticException("Short overflow"))),
          (Short.MinValue.toLong - 1, Expected(new ArithmeticException("Short overflow"))),
          (Short.MinValue.toLong, success(Short.MinValue)),
          (-1L, success(-1.toShort)),
          (0L, success(0.toShort)),
          (1L, success(1.toShort)),
          (Short.MaxValue.toLong, success(Short.MaxValue)),
          (Short.MaxValue.toLong + 1, Expected(new ArithmeticException("Short overflow"))),
          (Long.MinValue, Expected(new ArithmeticException("Short overflow")))
        )
      },
      existingFeature((x: Long) => x.toShortExact,
        "{ (x: Long) => x.toShort }",
        FuncValue(Vector((1, SLong)), Downcast(ValUse(1, SLong), SShort))))
  }

  property("Long.toInt method") {
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35976, downcastCostDetails(SInt), 1784)
        Seq(
          (Long.MinValue, Expected(new ArithmeticException("Int overflow"))),
          (Int.MinValue.toLong - 1, Expected(new ArithmeticException("Int overflow"))),
          (Int.MinValue.toLong, success(Int.MinValue)),
          (-1L, success(-1.toInt)),
          (0L, success(0.toInt)),
          (1L, success(1.toInt)),
          (Int.MaxValue.toLong, success(Int.MaxValue)),
          (Int.MaxValue.toLong + 1, Expected(new ArithmeticException("Int overflow"))),
          (Long.MinValue, Expected(new ArithmeticException("Int overflow")))
        )
      },
      existingFeature((x: Long) => x.toIntExact,
        "{ (x: Long) => x.toInt }",
        FuncValue(Vector((1, SLong)), Downcast(ValUse(1, SLong), SInt))))
  }

  property("Long.toLong method") {
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35798, TracedCost(traceBase), 1783)
        Seq(
          (Long.MinValue, success(Long.MinValue)),
          (-1L, success(-1L)),
          (0L, success(0L)),
          (1L, success(1L)),
          (Long.MaxValue, success(Long.MaxValue))
        )
      },
      existingFeature((x: Long) => x.toLong,
        "{ (x: Long) => x.toLong }",
        FuncValue(Vector((1, SLong)), ValUse(1, SLong))))
  }

  property("Long.toBigInt method") {
    verifyCases(
      {
        def success(v: BigInt) = Expected(Success(v), 35932, upcastCostDetails(SBigInt), 1787)
        Seq(
          (Long.MinValue, success(CBigInt(new BigInteger("-8000000000000000", 16)))),
          (-1074651039980347209L, success(CBigInt(new BigInteger("-ee9ed6d57885f49", 16)))),
          (-1L, success(CBigInt(new BigInteger("-1", 16)))),
          (0L, success(CBigInt(new BigInteger("0", 16)))),
          (1L, success(CBigInt(new BigInteger("1", 16)))),
          (1542942726564696512L, success(CBigInt(new BigInteger("1569a23c25a951c0", 16)))),
          (Long.MaxValue, success(CBigInt(new BigInteger("7fffffffffffffff", 16))))
        )
      },
      existingFeature((x: Long) => x.toBigInt,
        "{ (x: Long) => x.toBigInt }",
        FuncValue(Vector((1, SLong)), Upcast(ValUse(1, SLong), SBigInt))))
  }

  property("Long methods equivalence") {

    val n = ExactNumeric.LongIsExactNumeric
    verifyCases(
    {
      def success[T](v: T) = Expected(Success(v), 39654, arithOpsCostDetails(SLong), 1808)
      Seq(
        ((Long.MinValue, -4677100190307931395L), Expected(new ArithmeticException("long overflow"))),
        ((Long.MinValue, -1L), Expected(new ArithmeticException("long overflow"))),
        ((Long.MinValue, 1L), Expected(new ArithmeticException("long overflow"))),
        ((-9223372036854775808L, 0L), Expected(new ArithmeticException("/ by zero"))),
        ((-5828066432064138816L, 9105034716270510411L), Expected(new ArithmeticException("long overflow"))),
        ((-4564956247298949325L, -1L), success(
          (-4564956247298949326L, (-4564956247298949324L, (4564956247298949325L, (4564956247298949325L, 0L))))
        )),
        ((-1499553565058783253L, -3237683216870282569L), Expected(new ArithmeticException("long overflow"))),
        ((-1368457031689886112L, 9223372036854775807L), Expected(new ArithmeticException("long overflow"))),
        ((-1L, -4354407074688367443L), success((-4354407074688367444L, (4354407074688367442L, (4354407074688367443L, (0L, -1L)))))),
        ((-1L, -1L), success((-2L, (0L, (1L, (1L, 0L)))))),
        ((-1L, 5665019549505434695L), success((5665019549505434694L, (-5665019549505434696L, (-5665019549505434695L, (0L, -1L)))))),
        ((0L, -1L), success((-1L, (1L, (0L, (0L, 0L)))))),
        ((0L, 0L), Expected(new ArithmeticException("/ by zero"))),
        ((0L, 2112386634269044172L), success((2112386634269044172L, (-2112386634269044172L, (0L, (0L, 0L)))))),
        ((2254604056782701370L, -5878231674026236574L), Expected(new ArithmeticException("long overflow"))),
        ((2903872550238813643L, 1L), success(
          (2903872550238813644L, (2903872550238813642L, (2903872550238813643L, (2903872550238813643L, 0L))))
        )),
        ((5091129735284641762L, -427673944382373638L), Expected(new ArithmeticException("long overflow"))),
        ((6029085020194630780L, 2261786144956037939L), Expected(new ArithmeticException("long overflow"))),
        ((8126382074515995418L, -4746652047588907829L), Expected(new ArithmeticException("long overflow"))),
        ((Long.MaxValue, 1L), Expected(new ArithmeticException("long overflow"))),
        ((Long.MaxValue, -1L), Expected(new ArithmeticException("long overflow")))
      )
    },
    existingFeature(
      { (x: (Long, Long)) =>
        val a = x._1; val b = x._2
        val plus = n.plus(a, b)
        val minus = n.minus(a, b)
        val mul = n.times(a, b)
        val div = a / b
        val mod = a % b
        (plus, (minus, (mul, (div, mod))))
      },
      """{ (x: (Long, Long)) =>
       |  val a = x._1; val b = x._2
       |  val plus = a + b
       |  val minus = a - b
       |  val mul = a * b
       |  val div = a / b
       |  val mod = a % b
       |  (plus, (minus, (mul, (div, mod))))
       |}""".stripMargin,
      FuncValue(
        Vector((1, STuple(Vector(SLong, SLong)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[LongValue](ValUse(1, STuple(Vector(SLong, SLong))), 1.toByte)
            ),
            ValDef(
              4,
              List(),
              SelectField.typed[LongValue](ValUse(1, STuple(Vector(SLong, SLong))), 2.toByte)
            )
          ),
          Tuple(
            Vector(
              ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-102.toByte)),
              Tuple(
                Vector(
                  ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-103.toByte)),
                  Tuple(
                    Vector(
                      ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-100.toByte)),
                      Tuple(
                        Vector(
                          ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-99.toByte)),
                          ArithOp(ValUse(3, SLong), ValUse(4, SLong), OpCode @@ (-98.toByte))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )))
  }

  property("Long LT, GT, NEQ") {
    val o = ExactOrdering.LongIsExactOrdering
    def expect(v: Boolean) = Expected(Success(v), 36328, binaryRelationCostDetails(LT, SLong), 1788)
    val LT_cases: Seq[((Long, Long), Expected[Boolean])] = Seq(
      (Long.MinValue, Long.MinValue) -> expect(false),
      (Long.MinValue, (Long.MinValue + 1).toLong) -> expect(true),
      (Long.MinValue, -1.toLong) -> expect(true),
      (Long.MinValue, 0.toLong) -> expect(true),
      (Long.MinValue, 1.toLong) -> expect(true),
      (Long.MinValue, Long.MaxValue) -> expect(true),
      (-120.toLong, Long.MinValue) -> expect(false),
      (-120.toLong, -121.toLong) -> expect(false),
      (-120.toLong, -120.toLong) -> expect(false),
      (-120.toLong, -82.toLong) -> expect(true),
      (-103.toLong, -1.toLong) -> expect(true),
      (-103.toLong, -0.toLong) -> expect(true),
      (-103.toLong, 1.toLong) -> expect(true),
      (-103.toLong, Long.MaxValue) -> expect(true),
      (-1.toLong, -2.toLong) -> expect(false),
      (-1.toLong, -1.toLong) -> expect(false),
      (-1.toLong, 0.toLong) -> expect(true),
      (-1.toLong, 1.toLong) -> expect(true),
      (0.toLong, Long.MinValue) -> expect(false),
      (0.toLong, -1.toLong) -> expect(false),
      (0.toLong, 0.toLong) -> expect(false),
      (0.toLong, 1.toLong) -> expect(true),
      (0.toLong, 60.toLong) -> expect(true),
      (0.toLong, Long.MaxValue) -> expect(true),
      (1.toLong, -1.toLong) -> expect(false),
      (1.toLong, 0.toLong) -> expect(false),
      (1.toLong, 26.toLong) -> expect(true),
      (7.toLong, -32.toLong) -> expect(false),
      (7.toLong, 0.toLong) -> expect(false),
      (33.toLong, 1.toLong) -> expect(false),
      (126.toLong, Long.MaxValue) -> expect(true),
      (Long.MaxValue, Long.MinValue) -> expect(false),
      (Long.MaxValue, -47.toLong) -> expect(false),
      (Long.MaxValue, Long.MaxValue) -> expect(false)
    )

    verifyOp(LT_cases, "<", LT.apply)(_ < _)

    verifyOp(
      swapArgs(LT_cases, cost = 36342, newCostDetails = binaryRelationCostDetails(GT, SLong)),
      ">", GT.apply)(_ > _)

    val neqCases = newCasesFrom2(LT_cases.map(_._1))(_ != _, cost = 36337, newCostDetails = costNEQ(constNeqCost))
    verifyOp(neqCases, "!=", NEQ.apply)(_ != _)
  }

  property("Long LE, GE") {
    val o = ExactOrdering.LongIsExactOrdering
    def expect(v: Boolean) = Expected(Success(v), 36337, binaryRelationCostDetails(LE, SLong), 1788)
    val LE_cases: Seq[((Long, Long), Expected[Boolean])] = Seq(
      (Long.MinValue, Long.MinValue) -> expect(true),
      (Long.MinValue, (Long.MinValue + 1).toLong) -> expect(true),
      (Long.MinValue, -1.toLong) -> expect(true),
      (Long.MinValue, 0.toLong) -> expect(true),
      (Long.MinValue, 1.toLong) -> expect(true),
      (Long.MinValue, Long.MaxValue) -> expect(true),
      (-120.toLong, Long.MinValue) -> expect(false),
      (-120.toLong, -121.toLong) -> expect(false),
      (-120.toLong, -120.toLong) -> expect(true),
      (-120.toLong, -82.toLong) -> expect(true),
      (-103.toLong, -1.toLong) -> expect(true),
      (-103.toLong, -0.toLong) -> expect(true),
      (-103.toLong, 1.toLong) -> expect(true),
      (-103.toLong, Long.MaxValue) -> expect(true),
      (-1.toLong, -2.toLong) -> expect(false),
      (-1.toLong, -1.toLong) -> expect(true),
      (-1.toLong, 0.toLong) -> expect(true),
      (-1.toLong, 1.toLong) -> expect(true),
      (0.toLong, Long.MinValue) -> expect(false),
      (0.toLong, -1.toLong) -> expect(false),
      (0.toLong, 0.toLong) -> expect(true),
      (0.toLong, 1.toLong) -> expect(true),
      (0.toLong, 60.toLong) -> expect(true),
      (0.toLong, Long.MaxValue) -> expect(true),
      (1.toLong, -1.toLong) -> expect(false),
      (1.toLong, 0.toLong) -> expect(false),
      (1.toLong, 1.toLong) -> expect(true),
      (1.toLong, 26.toLong) -> expect(true),
      (7.toLong, -32.toLong) -> expect(false),
      (7.toLong, 0.toLong) -> expect(false),
      (33.toLong, 1.toLong) -> expect(false),
      (126.toLong, Long.MaxValue) -> expect(true),
      (Long.MaxValue, Long.MinValue) -> expect(false),
      (Long.MaxValue, -47.toLong) -> expect(false),
      (Long.MaxValue, Long.MaxValue) -> expect(true)
    )

    verifyOp(LE_cases, "<=", LE.apply)(_ <= _)

    verifyOp(
      swapArgs(LE_cases, cost = 36336, newCostDetails = binaryRelationCostDetails(GE, SLong)),
      ">=", GE.apply)(_ >= _)
  }

  property("Long methods equivalence (new features)") {
    lazy val toBytes = newFeature((x: Long) => x.toBytes, "{ (x: Long) => x.toBytes }")
    lazy val toBits = newFeature((x: Long) => x.toBits, "{ (x: Long) => x.toBits }")
    lazy val toAbs = newFeature((x: Long) => x.toAbs, "{ (x: Long) => x.toAbs }")

    lazy val compareTo = newFeature((x: (Long, Long)) => x._1.compareTo(x._2),
      "{ (x: (Long, Long)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature(
    { (x: (Long, Long)) => x._1 | x._2 },
    "{ (x: (Long, Long)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature(
    { (x: (Long, Long)) => x._1 & x._2 },
    "{ (x: (Long, Long)) => x._1 & x._2 }")

    forAll { x: Long =>
      Seq(toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (Long, Long) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  property("BigInt methods equivalence") {
    verifyCases(
      {
        def success(v: BigInt) = Expected(Success(v), 35798, TracedCost(traceBase), 1784)
        Seq(
          (CBigInt(new BigInteger("-85102d7f884ca0e8f56193b46133acaf7e4681e1757d03f191ae4f445c8e0", 16)), success(
            CBigInt(new BigInteger("-85102d7f884ca0e8f56193b46133acaf7e4681e1757d03f191ae4f445c8e0", 16))
          )),
          (CBigInt(new BigInteger("-8000000000000000", 16)), success(CBigInt(new BigInteger("-8000000000000000", 16)))),
          (CBigInt(new BigInteger("-1", 16)), success(CBigInt(new BigInteger("-1", 16)))),
          (CBigInt(new BigInteger("0", 16)), success(CBigInt(new BigInteger("0", 16)))),
          (CBigInt(new BigInteger("1", 16)), success(CBigInt(new BigInteger("1", 16)))),
          (CBigInt(new BigInteger("7fffffffffffffff", 16)), success(CBigInt(new BigInteger("7fffffffffffffff", 16)))),
          (CBigInt(new BigInteger("bdd56c22eb3eace8bc4e1c38c65dfdb2e4ffdcf421ae78c36b93b9ff37dc0", 16)), success(
            CBigInt(new BigInteger("bdd56c22eb3eace8bc4e1c38c65dfdb2e4ffdcf421ae78c36b93b9ff37dc0", 16))
          ))
        )
      },
      existingFeature((x: BigInt) => x,
        "{ (x: BigInt) => x.toBigInt }",
        FuncValue(Vector((1, SBigInt)), ValUse(1, SBigInt))))

    val n = NumericOps.BigIntIsExactIntegral
    verifyCases(
    {
      def success(v: (BigInt, (BigInt, (BigInt, (BigInt, BigInt))))) =
        Expected(Success(v), 39774, arithOpsCostDetails(SBigInt), 1813)
      Seq(
        ((CBigInt(new BigInteger("-8683d1cd99d5fcf0e6eff6295c285c36526190e13dbde008c49e5ae6fddc1c", 16)),
            CBigInt(new BigInteger("-2ef55db3f245feddacf0182e299dd", 16))),
            Expected(new ArithmeticException("BigInteger out of 256 bit range"))),

        ((CBigInt(new BigInteger("-68e1136872f98fb0245ec5aa4bef46e16273e860746c892", 16)),
            CBigInt(new BigInteger("-352aaa769b41a327", 16))),
            Expected(new ArithmeticException("BigInteger: modulus not positive"))),

        ((CBigInt(new BigInteger("-39fc00ebf09080cbd8408dd38c4b7490bea533447047140", 16)),
            CBigInt(new BigInteger("31de9e96177dbd39", 16))),
            success((
                CBigInt(new BigInteger("-39fc00ebf09080cbd8408dd38c4b748da0bb49e2f86b407", 16)),
                (CBigInt(new BigInteger("-39fc00ebf09080cbd8408dd38c4b7493dc8f1ca5e822e79", 16)),
                    (CBigInt(new BigInteger("-b4ba8a17d328dac74ef014d7be35597a1259f8b16f0ff1c9820dea23d97740", 16)),
                        (CBigInt(new BigInteger("-129a8045376e104f0d3771b6c2c128fc", 16)),
                            CBigInt(new BigInteger("12fe89836fc97815", 16)))))) )),

        ((CBigInt(new BigInteger("-8000000000000000", 16)), CBigInt(new BigInteger("8000000000000000", 16))),
            success((
                CBigInt(new BigInteger("0", 16)),
                (CBigInt(new BigInteger("-10000000000000000", 16)),
                    (CBigInt(new BigInteger("-40000000000000000000000000000000", 16)),
                        (CBigInt(new BigInteger("-1", 16)), CBigInt(new BigInteger("0", 16)))))) )),

        ((CBigInt(new BigInteger("-47dede8d3e4804bb", 16)), CBigInt(new BigInteger("-388828eb6dfce683", 16))),
            Expected(new ArithmeticException("BigInteger: modulus not positive"))),

        ((CBigInt(new BigInteger("-4fde491150ea00d", 16)), CBigInt(new BigInteger("-80000001", 16))),
            Expected(new ArithmeticException("BigInteger: modulus not positive"))),

        ((CBigInt(new BigInteger("-80000001", 16)), CBigInt(new BigInteger("-80000001", 16))),
            Expected(new ArithmeticException("BigInteger: modulus not positive"))),

        ((CBigInt(new BigInteger("0", 16)), CBigInt(new BigInteger("-8000000000000000", 16))),
            Expected(new ArithmeticException("BigInteger: modulus not positive"))),

        ((CBigInt(new BigInteger("0", 16)), CBigInt(new BigInteger("0", 16))),
            Expected(new ArithmeticException("BigInteger divide by zero"))),

        ((CBigInt(new BigInteger("1", 16)),
            CBigInt(new BigInteger("-86063f66e06d6d535c95862cd506309a95d10102422fee", 16))),
            Expected(new ArithmeticException("BigInteger: modulus not positive"))),

        ((CBigInt(new BigInteger("80000000", 16)), CBigInt(new BigInteger("4e592ce5b544b8f7a91f97ec9ea2f2c3660111360297a4", 16))),
            success((
                CBigInt(new BigInteger("4e592ce5b544b8f7a91f97ec9ea2f2c3660111b60297a4", 16)),
                (CBigInt(new BigInteger("-4e592ce5b544b8f7a91f97ec9ea2f2c3660110b60297a4", 16)),
                    (CBigInt(new BigInteger("272c9672daa25c7bd48fcbf64f517961b300889b014bd200000000", 16)),
                        (CBigInt(new BigInteger("0", 16)), CBigInt(new BigInteger("80000000", 16)))))) )),

        ((CBigInt(new BigInteger("3d31398dc4783303", 16)),
            CBigInt(new BigInteger("-37b381db4e6e927e202a2a421d5a09ca", 16))),
            Expected(new ArithmeticException("BigInteger: modulus not positive"))),

        ((CBigInt(new BigInteger("5524814a26357cb71488b6fb26af2d3", 16)),
            CBigInt(new BigInteger("c413b7d975a9972427f46996299fe57cfe79479ac954a7", 16))),
            Expected(new ArithmeticException("BigInteger out of 256 bit range")))
      )
    },
    existingFeature(
      { (x: (BigInt, BigInt)) =>
        val a = x._1; val b = x._2
        val plus = n.plus(a, b)
        val minus = n.minus(a, b)
        val mul = n.times(a, b)
        val div = a / b
        val mod = a % b
        (plus, (minus, (mul, (div, mod))))
      },
      """{ (x: (BigInt, BigInt)) =>
       |  val a = x._1; val b = x._2
       |  val plus = a + b
       |  val minus = a - b
       |  val mul = a * b
       |  val div = a / b
       |  val mod = a % b
       |  (plus, (minus, (mul, (div, mod))))
       |}""".stripMargin,
      FuncValue(
        Vector((1, STuple(Vector(SBigInt, SBigInt)))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[BigIntValue](ValUse(1, STuple(Vector(SBigInt, SBigInt))), 1.toByte)
            ),
            ValDef(
              4,
              List(),
              SelectField.typed[BigIntValue](ValUse(1, STuple(Vector(SBigInt, SBigInt))), 2.toByte)
            )
          ),
          Tuple(
            Vector(
              ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-102.toByte)),
              Tuple(
                Vector(
                  ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-103.toByte)),
                  Tuple(
                    Vector(
                      ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-100.toByte)),
                      Tuple(
                        Vector(
                          ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-99.toByte)),
                          ArithOp(ValUse(3, SBigInt), ValUse(4, SBigInt), OpCode @@ (-98.toByte))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ))
  }

  property("BigInt LT, GT, NEQ") {
    val o = NumericOps.BigIntIsExactOrdering

    def expect(v: Boolean) = Expected(Success(v), 36328, binaryRelationCostDetails(LT, SBigInt), 1788)
    
    val LT_cases: Seq[((BigInt, BigInt), Expected[Boolean])] = Seq(
      (BigIntMinValue, BigIntMinValue) -> expect(false),
      (BigIntMinValue, BigIntMinValue.add(1.toBigInt)) -> expect(true),
      (BigIntMinValue, -1.toBigInt) -> expect(true),
      (BigIntMinValue, 0.toBigInt) -> expect(true),
      (BigIntMinValue, 1.toBigInt) -> expect(true),
      (BigIntMinValue, BigIntMaxValue) -> expect(true),
      (-120.toBigInt, BigIntMinValue) -> expect(false),
      (-120.toBigInt, -121.toBigInt) -> expect(false),
      (-120.toBigInt, -120.toBigInt) -> expect(false),
      (-120.toBigInt, -82.toBigInt) -> expect(true),
      (-103.toBigInt, -1.toBigInt) -> expect(true),
      (-103.toBigInt, -0.toBigInt) -> expect(true),
      (-103.toBigInt, 1.toBigInt) -> expect(true),
      (-103.toBigInt, BigIntMaxValue) -> expect(true),
      (-1.toBigInt, -2.toBigInt) -> expect(false),
      (-1.toBigInt, -1.toBigInt) -> expect(false),
      (-1.toBigInt, 0.toBigInt) -> expect(true),
      (-1.toBigInt, 1.toBigInt) -> expect(true),
      (0.toBigInt, BigIntMinValue) -> expect(false),
      (0.toBigInt, -1.toBigInt) -> expect(false),
      (0.toBigInt, 0.toBigInt) -> expect(false),
      (0.toBigInt, 1.toBigInt) -> expect(true),
      (0.toBigInt, 60.toBigInt) -> expect(true),
      (0.toBigInt, BigIntMaxValue) -> expect(true),
      (1.toBigInt, -1.toBigInt) -> expect(false),
      (1.toBigInt, 0.toBigInt) -> expect(false),
      (1.toBigInt, 26.toBigInt) -> expect(true),
      (7.toBigInt, -32.toBigInt) -> expect(false),
      (7.toBigInt, 0.toBigInt) -> expect(false),
      (33.toBigInt, 1.toBigInt) -> expect(false),
      (126.toBigInt, BigIntMaxValue) -> expect(true),
      (BigIntMaxValue, BigIntMinValue) -> expect(false),
      (BigIntMaxValue, -47.toBigInt) -> expect(false),
      (BigIntMaxValue, BigIntMaxValue) -> expect(false),
      (BigIntMaxValue, BigIntOverlimit) -> expect(true),  // TODO v6.0: reject this overlimit cases
      (BigIntOverlimit, BigIntOverlimit) -> expect(false)
    )

    verifyOp(LT_cases, "<", LT.apply)(o.lt(_, _))

    verifyOp(
      swapArgs(LT_cases, cost = 36342, newCostDetails = binaryRelationCostDetails(GT, SBigInt)),
      ">", GT.apply)(o.gt(_, _))

    val constBigIntCost = Array[CostItem](FixedCostItem(NamedDesc("EQ_BigInt"), FixedCost(JitCost(5))))
    val neqCases = newCasesFrom2(LT_cases.map(_._1))(_ != _, cost = 36337, newCostDetails = costNEQ(constBigIntCost))
    verifyOp(neqCases, "!=", NEQ.apply)(_ != _)
  }

  property("BigInt LE, GE") {
    val o = NumericOps.BigIntIsExactOrdering
    // TODO v6.0: this values have bitCount == 255 (see to256BitValueExact)
    val BigIntMinValue = CBigInt(new BigInteger("-7F" + "ff" * 31, 16))
    val BigIntMaxValue = CBigInt(new BigInteger("7F" + "ff" * 31, 16))
    val BigIntOverlimit = CBigInt(new BigInteger("7F" + "ff" * 33, 16))

    def expect(v: Boolean) = Expected(Success(v), 36337, binaryRelationCostDetails(LE, SBigInt), 1788)
    
    val LE_cases: Seq[((BigInt, BigInt), Expected[Boolean])] = Seq(
      (BigIntMinValue, BigIntMinValue) -> expect(true),
      (BigIntMinValue, BigIntMinValue.add(1.toBigInt)) -> expect(true),
      (BigIntMinValue, -1.toBigInt) -> expect(true),
      (BigIntMinValue, 0.toBigInt) -> expect(true),
      (BigIntMinValue, 1.toBigInt) -> expect(true),
      (BigIntMinValue, BigIntMaxValue) -> expect(true),
      (-120.toBigInt, BigIntMinValue) -> expect(false),
      (-120.toBigInt, -121.toBigInt) -> expect(false),
      (-120.toBigInt, -120.toBigInt) -> expect(true),
      (-120.toBigInt, -82.toBigInt) -> expect(true),
      (-103.toBigInt, -1.toBigInt) -> expect(true),
      (-103.toBigInt, -0.toBigInt) -> expect(true),
      (-103.toBigInt, 1.toBigInt) -> expect(true),
      (-103.toBigInt, BigIntMaxValue) -> expect(true),
      (-1.toBigInt, -2.toBigInt) -> expect(false),
      (-1.toBigInt, -1.toBigInt) -> expect(true),
      (-1.toBigInt, 0.toBigInt) -> expect(true),
      (-1.toBigInt, 1.toBigInt) -> expect(true),
      (0.toBigInt, BigIntMinValue) -> expect(false),
      (0.toBigInt, -1.toBigInt) -> expect(false),
      (0.toBigInt, 0.toBigInt) -> expect(true),
      (0.toBigInt, 1.toBigInt) -> expect(true),
      (0.toBigInt, 60.toBigInt) -> expect(true),
      (0.toBigInt, BigIntMaxValue) -> expect(true),
      (1.toBigInt, -1.toBigInt) -> expect(false),
      (1.toBigInt, 0.toBigInt) -> expect(false),
      (1.toBigInt, 1.toBigInt) -> expect(true),
      (1.toBigInt, 26.toBigInt) -> expect(true),
      (7.toBigInt, -32.toBigInt) -> expect(false),
      (7.toBigInt, 0.toBigInt) -> expect(false),
      (33.toBigInt, 1.toBigInt) -> expect(false),
      (126.toBigInt, BigIntMaxValue) -> expect(true),
      (BigIntMaxValue, BigIntMinValue) -> expect(false),
      (BigIntMaxValue, -47.toBigInt) -> expect(false),
      (BigIntMaxValue, BigIntMaxValue) -> expect(true),
      (BigIntMaxValue, BigIntOverlimit) -> expect(true), // TODO v6.0: reject this overlimit cases
      (BigIntOverlimit, BigIntOverlimit) -> expect(true)
    )

    verifyOp(LE_cases, "<=", LE.apply)(o.lteq(_, _))

    verifyOp(
      swapArgs(LE_cases, cost = 36336, newCostDetails = binaryRelationCostDetails(GE, SBigInt)),
      ">=", GE.apply)(o.gteq(_, _))
  }

  property("BigInt methods equivalence (new features)") {
    // TODO v6.0 (2h): the behavior of `upcast` for BigInt is different from all other Numeric types
    // The `Upcast(bigInt, SBigInt)` node is never produced by ErgoScript compiler, but is still valid ErgoTree.
    // It makes sense to fix this inconsistency as part of upcoming forks
    assertExceptionThrown(
      SBigInt.upcast(CBigInt(new BigInteger("0", 16)).asInstanceOf[AnyVal]),
      _.getMessage.contains("Cannot upcast value")
    )

    // TODO v6.0 (2h): the behavior of `downcast` for BigInt is different from all other Numeric types
    // The `Downcast(bigInt, SBigInt)` node is never produced by ErgoScript compiler, but is still valid ErgoTree.
    // It makes sense to fix this inconsistency as part of HF
    assertExceptionThrown(
      SBigInt.downcast(CBigInt(new BigInteger("0", 16)).asInstanceOf[AnyVal]),
      _.getMessage.contains("Cannot downcast value")
    )

    val toByte = newFeature((x: BigInt) => x.toByte,
      "{ (x: BigInt) => x.toByte }",
      FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SByte)))

    val toShort = newFeature((x: BigInt) => x.toShort,
      "{ (x: BigInt) => x.toShort }",
      FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SShort)))

    val toInt = newFeature((x: BigInt) => x.toInt,
      "{ (x: BigInt) => x.toInt }",
      FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SInt)))

    val toLong = newFeature((x: BigInt) => x.toLong,
      "{ (x: BigInt) => x.toLong }",
      FuncValue(Vector((1, SBigInt)), Downcast(ValUse(1, SBigInt), SLong)))

    lazy val toBytes = newFeature((x: BigInt) => x.toBytes, "{ (x: BigInt) => x.toBytes }")
    lazy val toBits = newFeature((x: BigInt) => x.toBits, "{ (x: BigInt) => x.toBits }")
    lazy val toAbs = newFeature((x: BigInt) => x.toAbs, "{ (x: BigInt) => x.toAbs }")

    lazy val compareTo = newFeature((x: (BigInt, BigInt)) => x._1.compareTo(x._2),
      "{ (x: (BigInt, BigInt)) => x._1.compareTo(x._2) }")

    lazy val bitOr = newFeature({ (x: (BigInt, BigInt)) => x._1 | x._2 },
    "{ (x: (BigInt, BigInt)) => x._1 | x._2 }")

    lazy val bitAnd = newFeature({ (x: (BigInt, BigInt)) => x._1 & x._2 },
    "{ (x: (BigInt, BigInt)) => x._1 & x._2 }")

    forAll { x: BigInt =>
      Seq(toByte, toShort, toInt, toLong, toBytes, toBits, toAbs).foreach(_.checkEquality(x))
    }
    forAll { x: (BigInt, BigInt) =>
      Seq(compareTo, bitOr, bitAnd).foreach(_.checkEquality(x))
    }
  }

  /** Executed a series of test cases of NEQ operation verify using two _different_
    * data instances `x` and `y`.
    * @param cost the expected cost of `verify` (the same for all cases)
    */
  def verifyNeq[A: Ordering: Arbitrary: RType]
      (x: A, y: A, cost: Int, neqCost: Seq[CostItem] = mutable.WrappedArray.empty, newCost: Int)
      (copy: A => A, generateCases: Boolean = true)
      (implicit sampled: Sampled[(A, A)], evalSettings: EvalSettings) = {
    val copied_x = copy(x)
    val newCostDetails = if (neqCost.isEmpty) CostDetails.ZeroCost else costNEQ(neqCost)
    def expected(v: Boolean) = Expected(Success(v), cost, newCostDetails, newCost)
    def expectedNoCost(v: Boolean) = Expected(Success(v), cost, CostDetails.ZeroCost)
    verifyOp(Seq(
        (x, y) -> expected(true), // check cost only for this test case, because the trace depends in x and y
        (x, x) -> expectedNoCost(false), // and don't check for others
        (x, copied_x) -> expectedNoCost(false),
        (copied_x, x) -> expectedNoCost(false),
        (y, x) -> expectedNoCost(true)
      ),
      "!=", NEQ.apply)(_ != _, generateCases)
  }

  property("NEQ of pre-defined types") {
    verifyNeq(ge1, ge2, 36337, Array[CostItem](FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172)))), 1803)(_.asInstanceOf[CGroupElement].copy())
    verifyNeq(t1, t2, 36337, Array[CostItem](FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6)))), 1787)(_.asInstanceOf[CAvlTree].copy())
    verifyNeq(b1, b2, 36417, Array[CostItem](), 1787)(_.asInstanceOf[CostingBox].copy())
    verifyNeq(preH1, preH2, 36337, Array[CostItem](FixedCostItem(NamedDesc("EQ_PreHeader"), FixedCost(JitCost(4)))), 1786)(_.asInstanceOf[CPreHeader].copy())
    verifyNeq(h1, h2, 36337, Array[CostItem](FixedCostItem(NamedDesc("EQ_Header"), FixedCost(JitCost(6)))), 1787)(_.asInstanceOf[CHeader].copy())
  }

  property("NEQ of tuples of numerics") {
    val tuplesNeqCost = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3)))
    )
    verifyNeq((0.toByte, 1.toByte), (1.toByte, 1.toByte), 36337, tuplesNeqCost, 1787)(_.copy())
    verifyNeq((0.toShort, 1.toByte), (1.toShort, 1.toByte), 36337, tuplesNeqCost, 1787)(_.copy())
    verifyNeq((0, 1.toByte), (1, 1.toByte), 36337, tuplesNeqCost, 1787)(_.copy())
    verifyNeq((0.toLong, 1.toByte), (1.toLong, 1.toByte), 36337, tuplesNeqCost, 1787)(_.copy())
    verifyNeq((0.toBigInt, 1.toByte), (1.toBigInt, 1.toByte), 36337, Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_BigInt"), FixedCost(JitCost(5)))
    ), 1787)(_.copy())
  }

  property("NEQ of tuples of pre-defined types") {
    val groupNeqCost = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172)))
    )
    verifyNeq((ge1, ge1), (ge1, ge2), 36337, groupNeqCost, 1821)(_.copy())

    val treeNeqCost = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6)))
    )
    verifyNeq((t1, t1), (t1, t2), 36337, treeNeqCost, 1788)(_.copy())

    verifyNeq((b1, b1), (b1, b2), 36497, Array[CostItem](), 1788)(_.copy())
    
    val preHeaderNeqCost = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_PreHeader"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_PreHeader"), FixedCost(JitCost(4)))
    )
    verifyNeq((preH1, preH1), (preH1, preH2), 36337, preHeaderNeqCost, 1787)(_.copy())

    val headerNeqCost = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Header"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Header"), FixedCost(JitCost(6)))
    )
    verifyNeq((h1, h1), (h1, h2), 36337, headerNeqCost, 1788)(_.copy())
  }
  
  property("NEQ of nested tuples") {
    val nestedTuplesNeqCost1 = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6)))
    )
    val nestedTuplesNeqCost2 = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Box"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Box"), FixedCost(JitCost(6)))
    )
    val nestedTuplesNeqCost3 = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Box"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_PreHeader"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_PreHeader"), FixedCost(JitCost(4)))
    )
    val nestedTuplesNeqCost4 = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Box"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_PreHeader"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Header"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Header"), FixedCost(JitCost(6)))
    )
    val nestedTuplesNeqCost5 = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6)))
    )
    val nestedTuplesNeqCost6 = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Box"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Box"), FixedCost(JitCost(6)))
    )
    val nestedTuplesNeqCost7 = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Box"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_PreHeader"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_PreHeader"), FixedCost(JitCost(4)))
    )
    val nestedTuplesNeqCost8 = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Box"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_PreHeader"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Header"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("EQ_Header"), FixedCost(JitCost(6)))
    )
    verifyNeq((ge1, (t1, t1)), (ge1, (t1, t2)), 36337, nestedTuplesNeqCost1, 1805)(_.copy())
    verifyNeq((ge1, (t1, (b1, b1))), (ge1, (t1, (b1, b2))), 36497, nestedTuplesNeqCost2, 1806)(_.copy())
    verifyNeq((ge1, (t1, (b1, (preH1, preH1)))), (ge1, (t1, (b1, (preH1, preH2)))), 36417, nestedTuplesNeqCost3, 1807)(_.copy())
    verifyNeq((ge1, (t1, (b1, (preH1, (h1, h1))))), (ge1, (t1, (b1, (preH1, (h1, h2))))), 36427, nestedTuplesNeqCost4, 1808)(_.copy())

    verifyNeq(((ge1, t1), t1), ((ge1, t1), t2), 36337, nestedTuplesNeqCost5, 1805)(_.copy())
    verifyNeq((((ge1, t1), b1), b1), (((ge1, t1), b1), b2), 36497, nestedTuplesNeqCost6, 1806)(_.copy())
    verifyNeq((((ge1, t1), b1), (preH1, preH1)), (((ge1, t1), b1), (preH1, preH2)), 36417, nestedTuplesNeqCost7, 1807)(_.copy())
    verifyNeq((((ge1, t1), b1), (preH1, (h1, h1))), (((ge1, t1), b1), (preH1, (h1, h2))), 36427, nestedTuplesNeqCost8, 1808)(_.copy())
  }

  property("NEQ of collections of pre-defined types") {
    val collNeqCost1 = Array(
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1)))
    )
    val collNeqCost2 = Array(
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      SeqCostItem(NamedDesc("EQ_COA_Box"), PerItemCost(JitCost(15), JitCost(5), 1), 0)
    )
    implicit val evalSettings = suite.evalSettings.copy(isMeasureOperationTime = false)
    verifyNeq(Coll[Byte](), Coll(1.toByte), 36337, collNeqCost1, 1786)(cloneColl(_))
    verifyNeq(Coll[Byte](0, 1), Coll(1.toByte, 1.toByte), 36337,
      Array(
        FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
        SeqCostItem(NamedDesc("EQ_COA_Byte"), PerItemCost(JitCost(15), JitCost(2), 128), 1)),
      1788
    )(cloneColl(_))

    verifyNeq(Coll[Short](), Coll(1.toShort), 36337, collNeqCost1, 1786)(cloneColl(_))
    verifyNeq(Coll[Short](0), Coll(1.toShort), 36337,
      Array(
        FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
        SeqCostItem(NamedDesc("EQ_COA_Short"), PerItemCost(JitCost(15), JitCost(2), 96), 1)),
      1788
    )(cloneColl(_))

    verifyNeq(Coll[Int](), Coll(1), 36337, collNeqCost1, 1786)(cloneColl(_))
    verifyNeq(Coll[Int](0), Coll(1), 36337,
      Array(
        FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
        SeqCostItem(NamedDesc("EQ_COA_Int"), PerItemCost(JitCost(15), JitCost(2), 64), 1)),
      1788
    )(cloneColl(_))

    verifyNeq(Coll[Long](), Coll(1.toLong), 36337, collNeqCost1, 1786)(cloneColl(_))
    verifyNeq(Coll[Long](0), Coll(1.toLong), 36337,
      Array(
        FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
        SeqCostItem(NamedDesc("EQ_COA_Long"), PerItemCost(JitCost(15), JitCost(2), 48), 1)),
      1788
    )(cloneColl(_))

    prepareSamples[Coll[BigInt]]
    verifyNeq(Coll[BigInt](), Coll(1.toBigInt), 36337, collNeqCost1, 1786)(cloneColl(_))
    verifyNeq(Coll[BigInt](0.toBigInt), Coll(1.toBigInt), 36337,
      Array(
        FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
        SeqCostItem(NamedDesc("EQ_COA_BigInt"), PerItemCost(JitCost(15), JitCost(7), 5), 1)),
      1788
    )(cloneColl(_))

    prepareSamples[Coll[GroupElement]]
    verifyNeq(Coll[GroupElement](), Coll(ge1), 36337, collNeqCost1, 1786)(cloneColl(_))
    verifyNeq(Coll[GroupElement](ge1), Coll(ge2), 36337,
      Array(
        FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
        SeqCostItem(NamedDesc("EQ_COA_GroupElement"), PerItemCost(JitCost(15), JitCost(5), 1), 1)),
      1788
    )(cloneColl(_))

    prepareSamples[Coll[AvlTree]]
    verifyNeq(Coll[AvlTree](), Coll(t1), 36337, collNeqCost1, 1786)(cloneColl(_))
    verifyNeq(Coll[AvlTree](t1), Coll(t2), 36337,
      Array(
        FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
        SeqCostItem(NamedDesc("EQ_COA_AvlTree"), PerItemCost(JitCost(15), JitCost(5), 2), 1)),
      1788
    )(cloneColl(_))

    { // since SBox.isConstantSize = false, the cost is different among cases
      prepareSamples[Coll[AvlTree]]
      val x = Coll[Box]()
      val y = Coll(b1)
      val copied_x = cloneColl(x)
      verifyOp(Seq(
          (x, x) -> Expected(Success(false), 36337, costNEQ(collNeqCost2), 1788),
          (x, copied_x) -> Expected(Success(false), 36337, costNEQ(collNeqCost2), 1788),
          (copied_x, x) -> Expected(Success(false), 36337, costNEQ(collNeqCost2), 1788),
          (x, y) -> Expected(Success(true), 36377, costNEQ(collNeqCost1), 1786),
          (y, x) -> Expected(Success(true), 36377, costNEQ(collNeqCost1), 1786)
        ),
        "!=", NEQ.apply)(_ != _, generateCases = false)

      verifyNeq(Coll[Box](b1), Coll(b2), 36417,
        Array(
          FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
          SeqCostItem(NamedDesc("EQ_COA_Box"), PerItemCost(JitCost(15), JitCost(5), 1), 1)),
        1788
      )(cloneColl(_), generateCases = false)
    }

    prepareSamples[Coll[PreHeader]]
    verifyNeq(Coll[PreHeader](), Coll(preH1), 36337, collNeqCost1, 1786)(cloneColl(_))
    verifyNeq(Coll[PreHeader](preH1), Coll(preH2), 36337,
      Array(
        FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
        SeqCostItem(NamedDesc("EQ_COA_PreHeader"), PerItemCost(JitCost(15), JitCost(3), 1), 1)),
      1788
    )(cloneColl(_))

    prepareSamples[Coll[Header]]
    verifyNeq(Coll[Header](), Coll(h1), 36337, collNeqCost1, 1786)(cloneColl(_))
    verifyNeq(Coll[Header](h1), Coll(h2), 36337,
      Array(
        FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
        SeqCostItem(NamedDesc("EQ_COA_Header"), PerItemCost(JitCost(15), JitCost(5), 1), 1)),
      1788
    )(cloneColl(_))
  }

  property("NEQ of nested collections and tuples") {
    implicit val evalSettings = suite.evalSettings.copy(isMeasureOperationTime = false)
    prepareSamples[Coll[Int]]
    prepareSamples[Coll[Coll[Int]]]
    prepareSamples[Coll[Coll[Int]]]

    val nestedNeq1 = Array(
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1)))
    )
    val nestedNeq2 = Array(
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 1)
    )
    val nestedNeq3 = Array(
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      SeqCostItem(NamedDesc("EQ_COA_Int"), PerItemCost(JitCost(15), JitCost(2), 64), 1),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 1)
    )
    verifyNeq(Coll[Coll[Int]](), Coll(Coll[Int]()), 36337, nestedNeq1, 1786)(cloneColl(_))
    verifyNeq(Coll(Coll[Int]()), Coll(Coll[Int](1)), 36337, nestedNeq2, 1787)(cloneColl(_))
    verifyNeq(Coll(Coll[Int](1)), Coll(Coll[Int](2)), 36337, nestedNeq3, 1789)(cloneColl(_))
    verifyNeq(Coll(Coll[Int](1)), Coll(Coll[Int](1, 2)), 36337, nestedNeq2, 1787)(cloneColl(_))

    prepareSamples[Coll[(Int, BigInt)]]
    prepareSamples[Coll[Coll[(Int, BigInt)]]]

    val nestedNeq4 = Array(
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
      FixedCostItem(NamedDesc("EQ_BigInt"), FixedCost(JitCost(5))),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 1),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 1)
    )
    val nestedNeq5 = Array(
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
      FixedCostItem(NamedDesc("EQ_BigInt"), FixedCost(JitCost(5))),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 1),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 1),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 1) 
    )
    val nestedNeq6 = Array(
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_AvlTree"), FixedCost(JitCost(6))),
      FixedCostItem(NamedDesc("MatchType"), FixedCost(JitCost(1))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
      FixedCostItem(NamedDesc("EQ_BigInt"), FixedCost(JitCost(5))),
      FixedCostItem(NamedDesc("EQ_Tuple"), FixedCost(JitCost(4))),
      FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
      FixedCostItem(NamedDesc("EQ_BigInt"), FixedCost(JitCost(5))),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 2),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 1),
      SeqCostItem(NamedDesc("EQ_Coll"), PerItemCost(JitCost(10), JitCost(2), 1), 1)
    )
    verifyNeq(Coll(Coll((1, 10.toBigInt))), Coll(Coll((1, 11.toBigInt))), 36337, nestedNeq4, 1790)(cloneColl(_))
    verifyNeq(Coll(Coll(Coll((1, 10.toBigInt)))), Coll(Coll(Coll((1, 11.toBigInt)))), 36337, nestedNeq5, 1791)(cloneColl(_))
    verifyNeq(
      (Coll(
         (Coll(
           (t1, Coll((1, 10.toBigInt), (1, 10.toBigInt)))
          ), ge1)
       ), preH1),
      (Coll(
         (Coll(
           (t1, Coll((1, 10.toBigInt), (1, 11.toBigInt)))
          ), ge1)
       ), preH1),
      36337,
      nestedNeq6,
      1794
    )(x => (cloneColl(x._1), x._2))
  }

  property("GroupElement.getEncoded equivalence") {
    verifyCases(
    {
      def success[T](v: T) = Expected(Success(v), 37905, methodCostDetails(SGroupElement.GetEncodedMethod, 250), 1810)
      Seq(
        (ge1, success(Helpers.decodeBytes(ge1str))),
        (ge2, success(Helpers.decodeBytes(ge2str))),
        (ge3, success(Helpers.decodeBytes(ge3str))),
        (SigmaDsl.groupGenerator,
          success(Helpers.decodeBytes("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))),
        (SigmaDsl.groupIdentity,
          success(Helpers.decodeBytes("000000000000000000000000000000000000000000000000000000000000000000")))
      )
    },
    existingFeature((x: GroupElement) => x.getEncoded,
      "{ (x: GroupElement) => x.getEncoded }",
      FuncValue(
        Vector((1, SGroupElement)),
        MethodCall(ValUse(1, SGroupElement), SGroupElement.getMethodByName("getEncoded"), Vector(), Map())
      )))
  }

  property("decodePoint(GroupElement.getEncoded) equivalence") {
    verifyCases(
    {
      val costDetails = TracedCost(
        traceBase ++ Array(
          FixedCostItem(PropertyCall),
          FixedCostItem(MethodDesc(SGroupElement.GetEncodedMethod), FixedCost(JitCost(250))),
          FixedCostItem(DecodePoint),
          FixedCostItem(ValUse),
          FixedCostItem(NamedDesc("EQ_GroupElement"), FixedCost(JitCost(172)))
        )
      )
      def success[T](v: T) = Expected(Success(v), 38340, costDetails, 1857)
      Seq(
        (ge1, success(true)),
        (ge2, success(true)),
        (ge3, success(true)),
        (SigmaDsl.groupGenerator, success(true)),
        (SigmaDsl.groupIdentity, success(true))
      )
    },
    existingFeature({ (x: GroupElement) => decodePoint(x.getEncoded) == x },
    "{ (x: GroupElement) => decodePoint(x.getEncoded) == x }",
    FuncValue(
      Vector((1, SGroupElement)),
      EQ(
        DecodePoint(
          MethodCall.typed[Value[SCollection[SByte.type]]](
            ValUse(1, SGroupElement),
            SGroupElement.getMethodByName("getEncoded"),
            Vector(),
            Map()
          )
        ),
        ValUse(1, SGroupElement)
      )
    )))
  }

  property("GroupElement.negate equivalence") {
    verifyCases(
    {
      def success[T](v: T) = Expected(Success(v), 36292, methodCostDetails(SGroupElement.NegateMethod, 45), 1805)
      Seq(
        (ge1, success(Helpers.decodeGroupElement("02358d53f01276211f92d0aefbd278805121d4ff6eb534b777af1ee8abae5b2056"))),
        (ge2, success(Helpers.decodeGroupElement("03dba7b94b111f3894e2f9120b577da595ec7d58d488485adf73bf4e153af63575"))),
        (ge3, success(Helpers.decodeGroupElement("0390449814f5671172dd696a61b8aa49aaa4c87013f56165e27d49944e98bc414d"))),
        (SigmaDsl.groupGenerator, success(Helpers.decodeGroupElement("0379be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))),
        (SigmaDsl.groupIdentity, success(Helpers.decodeGroupElement("000000000000000000000000000000000000000000000000000000000000000000")))
      )
    },
    existingFeature({ (x: GroupElement) => x.negate },
    "{ (x: GroupElement) => x.negate }",
    FuncValue(
      Vector((1, SGroupElement)),
      MethodCall(ValUse(1, SGroupElement), SGroupElement.getMethodByName("negate"), Vector(), Map())
    )))
  }

  property("GroupElement.exp equivalence") {
    val cases = {
      val costDetails = TracedCost(
        traceBase ++ Array(
          FixedCostItem(SelectField),
          FixedCostItem(ValUse),
          FixedCostItem(SelectField),
          FixedCostItem(Exponentiate)
        )
      )
      def success[T](v: T) = Expected(Success(v), 41484, costDetails, 1893)
      Seq(
        ((ge1, CBigInt(new BigInteger("-25c80b560dd7844e2efd10f80f7ee57d", 16))),
          success(Helpers.decodeGroupElement("023a850181b7b73f92a5bbfa0bfc78f5bbb6ff00645ddde501037017e1a2251e2e"))),
        ((ge2, CBigInt(new BigInteger("2488741265082fb02b09f992be3dd8d60d2bbe80d9e2630", 16))),
          success(Helpers.decodeGroupElement("032045b928fb7774a4cd9ef5fa8209f4e493cd4cc5bd536b52746a53871bf73431"))),
        ((ge3, CBigInt(new BigInteger("-33e8fbdb13d2982e92583445e1fdcb5901a178a7aa1e100", 16))),
          success(Helpers.decodeGroupElement("036128efaf14d8ac2812a662f6494dc617b87986a3dc6b4a59440048a7ac7d2729"))),
        ((ge3, CBigInt(new BigInteger("1", 16))),
          success(ge3))
      )
    }
    val scalaFunc = { (x: (GroupElement, BigInt)) => x._1.exp(x._2) }
    val script = "{ (x: (GroupElement, BigInt)) => x._1.exp(x._2) }"
    if (lowerMethodCallsInTests) {
      verifyCases(cases,
        existingFeature(
          scalaFunc, script,
          FuncValue(
            Vector((1, STuple(Vector(SGroupElement, SBigInt)))),
            Exponentiate(
              SelectField.typed[Value[SGroupElement.type]](
                ValUse(1, STuple(Vector(SGroupElement, SBigInt))),
                1.toByte
              ),
              SelectField.typed[Value[SBigInt.type]](
                ValUse(1, STuple(Vector(SGroupElement, SBigInt))),
                2.toByte
              )
            )
          )))
    } else {
      forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
        testCases(
          Seq(
            ((ge1, CBigInt(new BigInteger("-25c80b560dd7844e2efd10f80f7ee57d", 16))),
              Expected(
                // in v4.x exp method cannot be called via MethodCall ErgoTree node
                Failure(new NoSuchMethodException("sigmastate.eval.CostingRules$GroupElementCoster.exp(scalan.Base$Ref)")),
                cost = 41484,
                CostDetails.ZeroCost,
                newCost = 0,
                newVersionedResults = {
                  // in v5.0 MethodCall ErgoTree node is allowed
                  val res = ExpectedResult(
                    Success(Helpers.decodeGroupElement("023a850181b7b73f92a5bbfa0bfc78f5bbb6ff00645ddde501037017e1a2251e2e")),
                    Some(999)
                  )
                  Seq( // expected result for each version
                    0 -> ( res -> None ),
                    1 -> ( res -> None ),
                    2 -> ( res -> None )
                  )
                }
              )
              )
          ),
          changedFeature(
            (x: (GroupElement, BigInt)) =>
              throw new NoSuchMethodException("sigmastate.eval.CostingRules$GroupElementCoster.exp(scalan.Base$Ref)"),
            scalaFunc, ""/*can't be compiled in v4.x*/,
            FuncValue(
              Vector((1, STuple(Vector(SGroupElement, SBigInt)))),
              MethodCall(
                SelectField.typed[Value[SGroupElement.type]](
                  ValUse(1, STuple(Vector(SGroupElement, SBigInt))),
                  1.toByte
                ),
                SGroupElement.getMethodByName("exp"),
                Vector(
                  SelectField.typed[Value[SBigInt.type]](
                    ValUse(1, STuple(Vector(SGroupElement, SBigInt))),
                    2.toByte
                  )
                ),
                Map()
              )
            ),
            allowNewToSucceed = true,
            allowDifferentErrors = true
          ))
      }
    }
  }

  property("GroupElement.multiply equivalence") {
    val scalaFunc = { (x: (GroupElement, GroupElement)) => x._1.multiply(x._2) }
    if (lowerMethodCallsInTests) {
      verifyCases(
      {
        val costDetails = TracedCost(
          traceBase ++ Array(
            FixedCostItem(SelectField),
            FixedCostItem(ValUse),
            FixedCostItem(SelectField),
            FixedCostItem(MultiplyGroup)
          )
        )
        def success[T](v: T) = Expected(Success(v), 36457, costDetails, 1807)
        Seq(
          ((ge1, Helpers.decodeGroupElement("03e132ca090614bd6c9f811e91f6daae61f16968a1e6c694ed65aacd1b1092320e")),
              success(Helpers.decodeGroupElement("02bc48937b4a66f249a32dfb4d2efd0743dc88d46d770b8c5d39fd03325ba211df"))),
          ((ge2, Helpers.decodeGroupElement("03e132ca090614bd6c9f811e91f6daae61f16968a1e6c694ed65aacd1b1092320e")),
              success(Helpers.decodeGroupElement("0359c3bb2ac4ea4dbd7b1e09d7b11198141a3263834fb84a88039629ec1e9311d1"))),
          ((ge3, Helpers.decodeGroupElement("03e132ca090614bd6c9f811e91f6daae61f16968a1e6c694ed65aacd1b1092320e")),
              success(Helpers.decodeGroupElement("02eca42e28548d3fb9fa77cdd0c983066c3ad141ebb086b5044ce46b9ba9b5a714"))),
          ((ge3, SigmaDsl.groupIdentity),
              success(ge3))
        )
      },
      existingFeature(scalaFunc,
        "{ (x: (GroupElement, GroupElement)) => x._1.multiply(x._2) }",
        FuncValue(
          Vector((1, STuple(Vector(SGroupElement, SGroupElement)))),
          MultiplyGroup(
            SelectField.typed[Value[SGroupElement.type]](
              ValUse(1, STuple(Vector(SGroupElement, SGroupElement))),
              1.toByte
            ),
            SelectField.typed[Value[SGroupElement.type]](
              ValUse(1, STuple(Vector(SGroupElement, SGroupElement))),
              2.toByte
            )
          )
        )))
    } else {
      val cases = Seq(
        ((ge1, Helpers.decodeGroupElement("03e132ca090614bd6c9f811e91f6daae61f16968a1e6c694ed65aacd1b1092320e")),
          Expected(
            // in v4.x exp method cannot be called via MethodCall ErgoTree node
            Failure(new NoSuchMethodException("sigmastate.eval.CostingRules$GroupElementCoster.multiply(scalan.Base$Ref)")),
            cost = 41484,
            CostDetails.ZeroCost,
            newCost = 0,
            newVersionedResults = {
              // in v5.0 MethodCall ErgoTree node is allowed
              val res = ExpectedResult(
                Success(Helpers.decodeGroupElement("02bc48937b4a66f249a32dfb4d2efd0743dc88d46d770b8c5d39fd03325ba211df")),
                Some(139)
              )
              Seq( // expected result for each version
                0 -> ( res -> None ),
                1 -> ( res -> None ),
                2 -> ( res -> None )
              )
            }
          )
          )
      )
      forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
        testCases(
          cases,
          changedFeature(
            (_: (GroupElement, GroupElement)) =>
              throw new NoSuchMethodException("java.lang.NoSuchMethodException: sigmastate.eval.CostingRules$GroupElementCoster.multiply(scalan.Base$Ref)"),
            scalaFunc, ""/*can't be compiled in v4.x*/,
            FuncValue(
              Vector((1, STuple(Vector(SGroupElement, SGroupElement)))),
              MethodCall(
                SelectField.typed[Value[SGroupElement.type]](
                  ValUse(1, STuple(Vector(SGroupElement, SGroupElement))),
                  1.toByte
                ),
                SGroupElement.getMethodByName("multiply"),
                Vector(
                  SelectField.typed[Value[SGroupElement.type]](
                    ValUse(1, STuple(Vector(SGroupElement, SGroupElement))),
                    2.toByte
                  )
                ),
                Map()
              )
            ),
            allowNewToSucceed = true,
            allowDifferentErrors = true
          ))
      }
    }
  }

  // TODO v6.0 (3h): related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  //  property("GroupElement.isIdentity equivalence") {
  //    // val isIdentity = existingFeature({ (x: GroupElement) => x.isIdentity },
  //    //   "{ (x: GroupElement) => x.isIdentity }")
  //  }

  property("AvlTree properties equivalence") {
    def expectedExprFor(propName: String) = {
      FuncValue(
        Vector((1, SAvlTree)),
        MethodCall(
          ValUse(1, SAvlTree),
          SAvlTree.getMethodByName(propName),
          Vector(),
          Map()
        )
      )
    }
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36182, methodCostDetails(SAvlTree.digestMethod, 15), 1787)
        Seq(
          (t1, success(Helpers.decodeBytes("000183807f66b301530120ff7fc6bd6601ff01ff7f7d2bedbbffff00187fe89094"))),
          (t2, success(Helpers.decodeBytes("ff000d937f80ffd731ed802d24358001ff8080ff71007f00ad37e0a7ae43fff95b"))),
          (t3, success(Helpers.decodeBytes("3100d2e101ff01fc047c7f6f00ff80129df69a5090012f01ffca99f5bfff0c8036")))
        )
      },
      existingFeature((t: AvlTree) => t.digest,
        "{ (t: AvlTree) => t.digest }",
        expectedExprFor("digest")))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36260, methodCostDetails(SAvlTree.enabledOperationsMethod, 15), 1785)
        Seq(
          (t1, success(6.toByte)),
          (t2, success(0.toByte)),
          (t3, success(1.toByte))
        )
      },
      existingFeature((t: AvlTree) => t.enabledOperations,
        "{ (t: AvlTree) => t.enabledOperations }",
        expectedExprFor("enabledOperations")))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36136, methodCostDetails(SAvlTree.keyLengthMethod, 15), 1785)
        Seq(
          (t1, success(1)),
          (t2, success(32)),
          (t3, success(128))
        )
      },
      existingFeature((t: AvlTree) => t.keyLength,
        "{ (t: AvlTree) => t.keyLength }",
        expectedExprFor("keyLength")))

    verifyCases(
      {
        def success[T](v: T, newCost: Int) = Expected(Success(v), 37151, methodCostDetails(SAvlTree.valueLengthOptMethod, 15), newCost)
        Seq(
          (t1, success(Some(1), 1786)),
          (t2, success(Some(64), 1786)),
          (t3, success(None, 1785))
        )
      },
      existingFeature((t: AvlTree) => t.valueLengthOpt,
        "{ (t: AvlTree) => t.valueLengthOpt }",
        expectedExprFor("valueLengthOpt")))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36479, methodCostDetails(SAvlTree.isInsertAllowedMethod, 15), 1785)
        Seq(
          (t1, success(false)),
          (t2, success(false)),
          (t3, success(true))
        )
      },
      existingFeature((t: AvlTree) => t.isInsertAllowed,
        "{ (t: AvlTree) => t.isInsertAllowed }",
        expectedExprFor("isInsertAllowed")))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36096, methodCostDetails(SAvlTree.isUpdateAllowedMethod, 15), 1785)
        Seq(
          (t1, success(true)),
          (t2, success(false)),
          (t3, success(false))
        )
      },
      existingFeature((t: AvlTree) => t.isUpdateAllowed,
        "{ (t: AvlTree) => t.isUpdateAllowed }",
        expectedExprFor("isUpdateAllowed")))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36502, methodCostDetails(SAvlTree.isRemoveAllowedMethod, 15), 1785)
        Seq(
          (t1, success(true)),
          (t2, success(false)),
          (t3, success(false))
        )
      },
      existingFeature((t: AvlTree) => t.isRemoveAllowed,
        "{ (t: AvlTree) => t.isRemoveAllowed }",
        expectedExprFor("isRemoveAllowed")))
  }

  property("AvlTree.{contains, get, getMany, updateDigest, updateOperations} equivalence") {
    val contains = existingFeature(
      (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.contains(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (1, STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte))))))
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))))
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SBoolean.type]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))))
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("contains"),
            Vector(
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    val get = existingFeature((t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.get(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[Byte], Coll[Byte]))) => t._1.get(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (1, STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte))))))
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))))
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SOption[SCollection[SByte.type]]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(Vector(SAvlTree, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))))
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("get"),
            Vector(
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SByte), SCollectionType(SByte)))),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    val getMany = existingFeature(
      (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.getMany(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.getMany(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (
              1,
              STuple(
                Vector(
                  SAvlTree,
                  STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))
                )
              )
          )
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(
                    Vector(
                      SAvlTree,
                      STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))
                    )
                  )
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SCollection[SOption[SCollection[SByte.type]]]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(
                  Vector(
                    SAvlTree,
                    STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))
                  )
                )
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("getMany"),
            Vector(
              SelectField.typed[Value[SCollection[SCollection[SByte.type]]]](
                ValUse(3, STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SCollectionType(SCollectionType(SByte)), SCollectionType(SByte)))),
                2.toByte
              )
            ),
            Map()
          )
        )
      )
    )

    val updateDigest = existingFeature((t: (AvlTree, Coll[Byte])) => t._1.updateDigest(t._2),
      "{ (t: (AvlTree, Coll[Byte])) => t._1.updateDigest(t._2) }",
      FuncValue(
        Vector((1, STuple(Vector(SAvlTree, SCollectionType(SByte))))),
        MethodCall.typed[Value[SAvlTree.type]](
          SelectField.typed[Value[SAvlTree.type]](
            ValUse(1, STuple(Vector(SAvlTree, SCollectionType(SByte)))),
            1.toByte
          ),
          SAvlTree.getMethodByName("updateDigest"),
          Vector(
            SelectField.typed[Value[SCollection[SByte.type]]](
              ValUse(1, STuple(Vector(SAvlTree, SCollectionType(SByte)))),
              2.toByte
            )
          ),
          Map()
        )
      ))

    val updateOperations = existingFeature((t: (AvlTree, Byte)) => t._1.updateOperations(t._2),
      "{ (t: (AvlTree, Byte)) => t._1.updateOperations(t._2) }",
      FuncValue(
        Vector((1, STuple(Vector(SAvlTree, SByte)))),
        MethodCall.typed[Value[SAvlTree.type]](
          SelectField.typed[Value[SAvlTree.type]](ValUse(1, STuple(Vector(SAvlTree, SByte))), 1.toByte),
          SAvlTree.getMethodByName("updateOperations"),
          Vector(
            SelectField.typed[Value[SByte.type]](ValUse(1, STuple(Vector(SAvlTree, SByte))), 2.toByte)
          ),
          Map()
        )
      ))

    def success[T](v: T) = Expected(Success(v), 0)

    { // getMany with a bunch of existing keys
      val (keysArr, valuesArr, _, avlProver) = sampleAvlProver
      val keys = Colls.fromArray(keysArr.slice(0, 20))
      val expRes = Colls.fromArray(valuesArr.slice(0, 20).map(Option(_)))

      keys.foreach { key =>
        avlProver.performOneOperation(Lookup(ADKey @@ key.toArray))
      }
      val proof = avlProver.generateProof().toColl
      val digest = avlProver.digest.toColl
      val tree = SigmaDsl.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)

      val input = (tree, (keys, proof))
      // TODO mainnet v5.0: checkExpected is ignoring newCost/costDetails. Is that correct behaviour?
      getMany.checkExpected(input, Expected(Success(expRes), 0))//, TracedCost(traceBase), 1700))
    }

    val (keysArr, valuesArr, _, avlProver) = sampleAvlProver
    val key = keysArr(0)
    val value = valuesArr(0)
    val otherKey = key.map(x => (-x).toByte) // any other different from key

    val table = Table(("key", "contains", "valueOpt"),
      (key, true, Some(value)),
      (otherKey, false, None)
    )

    forAll(table) { (key, okContains, valueOpt) =>
      avlProver.performOneOperation(Lookup(ADKey @@ key.toArray))
      val proof = avlProver.generateProof().toColl
      val digest = avlProver.digest.toColl
      val tree = SigmaDsl.avlTree(AvlTreeFlags.ReadOnly.serializeToByte, digest, 32, None)

      // positive test
      {
        val input = (tree, (key, proof))
        contains.checkExpected(input, success(okContains))
        get.checkExpected(input, success(valueOpt))

        // TODO mainnet v5.0: Cyclic newCost here (1823/1825), also costDetails are ignored
        contains.checkVerify(input, Expected(Success(okContains), 37850))//, TracedCost(traceBase), 1823))
        get.checkVerify(input, Expected(value = Success(valueOpt), cost = 38372))
      }

      val keys = Colls.fromItems(key)
      val expRes = Colls.fromItems(valueOpt)

      {
        val input = (tree, (keys, proof))
        getMany.checkExpected(input, success(expRes))
        getMany.checkVerify(input, Expected(value = Success(expRes), cost = 38991))
      }

      {
        val input = (tree, digest)
        val (res, _) = updateDigest.checkEquality(input).getOrThrow
        res.digest shouldBe digest
        updateDigest.checkVerify(input, Expected(value = Success(res), cost = 36341))
      }

      val newOps = 1.toByte

      {
        val input = (tree, newOps)
        val (res,_) = updateOperations.checkEquality(input).getOrThrow
        res.enabledOperations shouldBe newOps
        updateOperations.checkVerify(input, Expected(value = Success(res), cost = 36341))
      }

      // negative tests: invalid proof
      val invalidProof = proof.map(x => (-x).toByte) // any other different from proof

      {
        val input = (tree, (key, invalidProof))
        val (res, _) = contains.checkEquality(input).getOrThrow
        res shouldBe false
        contains.checkVerify(input, Expected(value = Success(res), cost = 37850))
      }

      {
        val resGet = get.checkEquality((tree, (key, invalidProof)))
        resGet.isFailure shouldBe true
      }

      {
        val resGetMany = getMany.checkEquality((tree, (keys, invalidProof)))
        resGetMany.isFailure shouldBe true
      }
    }
  }

  type BatchProver = BatchAVLProver[Digest32, Blake2b256.type]

  def performInsert(avlProver: BatchProver, key: Coll[Byte], value: Coll[Byte]) = {
    avlProver.performOneOperation(Insert(ADKey @@ key.toArray, ADValue @@ value.toArray))
    val proof = avlProver.generateProof().toColl
    proof
  }

  def performUpdate(avlProver: BatchProver, key: Coll[Byte], value: Coll[Byte]) = {
    avlProver.performOneOperation(Update(ADKey @@ key.toArray, ADValue @@ value.toArray))
    val proof = avlProver.generateProof().toColl
    proof
  }

  def performRemove(avlProver: BatchProver, keys: Seq[Coll[Byte]]) = {
    keys.foreach { key =>
      avlProver.performOneOperation(Remove(ADKey @@ key.toArray))
    }
    val proof = avlProver.generateProof().toColl
    proof
  }

  def createTree(digest: Coll[Byte], insertAllowed: Boolean = false, updateAllowed: Boolean = false, removeAllowed: Boolean = false) = {
    val flags = AvlTreeFlags(insertAllowed, updateAllowed, removeAllowed).serializeToByte
    val tree = SigmaDsl.avlTree(flags, digest, 32, None)
    tree
  }

  type KV = (Coll[Byte], Coll[Byte])

  property("AvlTree.insert equivalence") {
    val insert = existingFeature((t: (AvlTree, (Coll[KV], Coll[Byte]))) => t._1.insert(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]))) => t._1.insert(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (
              1,
              STuple(
                Vector(
                  SAvlTree,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                )
              )
              )
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(
                    Vector(
                      SAvlTree,
                      STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                    )
                  )
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SOption[SAvlTree.type]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(
                  Vector(
                    SAvlTree,
                    STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                  )
                )
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("insert"),
            Vector(
              SelectField.typed[Value[SCollection[STuple]]](
                ValUse(
                  3,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                ),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(
                  3,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                ),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    val newCostDetails = TracedCost(
      traceBase ++ Array(
        FixedCostItem(PropertyCall)
      )
    )

    forAll(keyCollGen, bytesCollGen) { (key, value) =>
      val (tree, avlProver) = createAvlTreeAndProver()
      val preInsertDigest = avlProver.digest.toColl
      val insertProof = performInsert(avlProver, key, value)
      val kvs = Colls.fromItems((key -> value))

      { // positive
        val preInsertTree = createTree(preInsertDigest, insertAllowed = true)
        val input = (preInsertTree, (kvs, insertProof))
        val (res, _) = insert.checkEquality(input).getOrThrow
        res.isDefined shouldBe true
        insert.checkVerify(input, Expected(Success(res), 38501, newCostDetails, 1816))
      }

      { // negative: readonly tree
        val readonlyTree = createTree(preInsertDigest)
        val input = (readonlyTree, (kvs, insertProof))
        val (res, _) = insert.checkEquality(input).getOrThrow
        res.isDefined shouldBe false
        insert.checkVerify(input, Expected(value = Success(res), cost = 38501))
      }

      { // negative: invalid key
        val tree = createTree(preInsertDigest, insertAllowed = true)
        val invalidKey = key.map(x => (-x).toByte) // any other different from key
        val invalidKvs = Colls.fromItems((invalidKey -> value)) // NOTE, insertProof is based on `key`
        val input = (tree, (invalidKvs, insertProof))
        val (res, _) = insert.checkEquality(input).getOrThrow
        res.isDefined shouldBe true // TODO v6.0: should it really be true? (looks like a bug)
        insert.checkVerify(input, Expected(value = Success(res), cost = 38501))
      }

      { // negative: invalid proof
        val tree = createTree(preInsertDigest, insertAllowed = true)
        val invalidProof = insertProof.map(x => (-x).toByte) // any other different from proof
        val res = insert.checkEquality((tree, (kvs, invalidProof)))
        res.isFailure shouldBe true
      }
    }
  }

  property("AvlTree.update equivalence") {
    val update = existingFeature((t: (AvlTree, (Coll[KV], Coll[Byte]))) => t._1.update(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[(Coll[Byte], Coll[Byte])], Coll[Byte]))) => t._1.update(t._2._1, t._2._2) }",
      FuncValue(
        Vector(
          (
              1,
              STuple(
                Vector(
                  SAvlTree,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                )
              )
              )
        ),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(
                  1,
                  STuple(
                    Vector(
                      SAvlTree,
                      STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                    )
                  )
                ),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SOption[SAvlTree.type]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(
                1,
                STuple(
                  Vector(
                    SAvlTree,
                    STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                  )
                )
              ),
              1.toByte
            ),
            SAvlTree.getMethodByName("update"),
            Vector(
              SelectField.typed[Value[SCollection[STuple]]](
                ValUse(
                  3,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                ),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(
                  3,
                  STuple(Vector(SCollectionType(STuple(Vector(SByteArray, SByteArray))), SByteArray))
                ),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))
    val cost = 40952

    def success[T](v: T) = Expected(Success(v), 0)

    forAll(keyCollGen, bytesCollGen) { (key, value) =>
      val (_, avlProver) = createAvlTreeAndProver(key -> value)
      val preUpdateDigest = avlProver.digest.toColl
      val newValue = bytesCollGen.sample.get
      val updateProof = performUpdate(avlProver, key, newValue)
      val kvs = Colls.fromItems((key -> newValue))
      val endDigest = avlProver.digest.toColl

      { // positive: update to newValue
        val preUpdateTree = createTree(preUpdateDigest, updateAllowed = true)
        val endTree = preUpdateTree.updateDigest(endDigest)
        val input = (preUpdateTree, (kvs, updateProof))
        val res = Some(endTree)
        update.checkExpected(input, success(res))
        update.checkVerify(input, Expected(Success(res), cost))
      }

      { // positive: update to the same value (identity operation)
        val tree = createTree(preUpdateDigest, updateAllowed = true)
        val keys = Colls.fromItems((key -> value))
        val input = (tree, (keys, updateProof))
        val res = Some(tree)
        update.checkExpected(input, success(res))
        update.checkVerify(input, Expected(value = Success(res), cost = cost))
      }

      { // negative: readonly tree
        val readonlyTree = createTree(preUpdateDigest)
        val input = (readonlyTree, (kvs, updateProof))
        update.checkExpected(input, success(None))
        update.checkVerify(input, Expected(value = Success(None), cost = cost))
      }

      { // negative: invalid key
        val tree = createTree(preUpdateDigest, updateAllowed = true)
        val invalidKey = key.map(x => (-x).toByte) // any other different from key
        val invalidKvs = Colls.fromItems((invalidKey -> newValue))
        val input = (tree, (invalidKvs, updateProof))
        update.checkExpected(input, success(None))
        update.checkVerify(input, Expected(value = Success(None), cost = cost))
      }

      { // negative: invalid value (different from the value in the proof)
        val tree = createTree(preUpdateDigest, updateAllowed = true)
        val invalidValue = newValue.map(x => (-x).toByte)
        val invalidKvs = Colls.fromItems((key -> invalidValue))
        val input = (tree, (invalidKvs, updateProof))
        val (res, _) = update.checkEquality(input).getOrThrow
        res.isDefined shouldBe true  // TODO v6.0: should it really be true? (looks like a bug)
        update.checkVerify(input, Expected(value = Success(res), cost = cost))
      }

      { // negative: invalid proof
        val tree = createTree(preUpdateDigest, updateAllowed = true)
        val invalidProof = updateProof.map(x => (-x).toByte) // any other different from proof
        val input = (tree, (kvs, invalidProof))
        update.checkExpected(input, success(None))
        update.checkVerify(input, Expected(value = Success(None), cost = cost))
      }
    }
  }

  property("AvlTree.remove equivalence") {
    val remove = existingFeature((t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.remove(t._2._1, t._2._2),
      "{ (t: (AvlTree, (Coll[Coll[Byte]], Coll[Byte]))) => t._1.remove(t._2._1, t._2._2) }",
      FuncValue(
        Vector((1, STuple(Vector(SAvlTree, STuple(Vector(SByteArray2, SByteArray)))))),
        BlockValue(
          Vector(
            ValDef(
              3,
              List(),
              SelectField.typed[Value[STuple]](
                ValUse(1, STuple(Vector(SAvlTree, STuple(Vector(SByteArray2, SByteArray))))),
                2.toByte
              )
            )
          ),
          MethodCall.typed[Value[SOption[SAvlTree.type]]](
            SelectField.typed[Value[SAvlTree.type]](
              ValUse(1, STuple(Vector(SAvlTree, STuple(Vector(SByteArray2, SByteArray))))),
              1.toByte
            ),
            SAvlTree.getMethodByName("remove"),
            Vector(
              SelectField.typed[Value[SCollection[SCollection[SByte.type]]]](
                ValUse(3, STuple(Vector(SByteArray2, SByteArray))),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(3, STuple(Vector(SByteArray2, SByteArray))),
                2.toByte
              )
            ),
            Map()
          )
        )
      ))

    def success[T](v: T) = Expected(Success(v), 0)

    { // positive test with many keys in the tree and to remove
      val keys = arrayOfN(100, keyCollGen).sample.get
      val values = arrayOfN(100, bytesCollGen).sample.get
      val (_, avlProver) = createAvlTreeAndProver(keys.zip(values):_*)
      val preRemoveDigest = avlProver.digest.toColl
      val keysToRemove = keys.zipWithIndex.collect { case (k, i) if i % 2 != 0 => k }
      val removeProof = performRemove(avlProver, keysToRemove)
      val endDigest = avlProver.digest.toColl

      val preRemoveTree = createTree(preRemoveDigest, removeAllowed = true)
      val endTree = preRemoveTree.updateDigest(endDigest)
      val input = (preRemoveTree, (Colls.fromArray(keysToRemove), removeProof))
      val res = Some(endTree)
      remove.checkExpected(input, success(res))
    }

    forAll(keyCollGen, bytesCollGen) { (key, value) =>
      val (_, avlProver) = createAvlTreeAndProver(key -> value)
      val preRemoveDigest = avlProver.digest.toColl
      val removeProof = performRemove(avlProver, Array(key))
      val endDigest = avlProver.digest.toColl
      val keys = Colls.fromItems(key)
      val cost = 38270

      { // positive
        val preRemoveTree = createTree(preRemoveDigest, removeAllowed = true)
        val endTree = preRemoveTree.updateDigest(endDigest)
        val input = (preRemoveTree, (keys, removeProof))
        val res = Some(endTree)
        remove.checkExpected(input, success(res))
        remove.checkVerify(input, Expected(value = Success(res), cost = cost))
      }

      { // negative: readonly tree
        val readonlyTree = createTree(preRemoveDigest)
        val input = (readonlyTree, (keys, removeProof))
        remove.checkExpected(input, success(None))
        remove.checkVerify(input, Expected(value = Success(None), cost = cost))
      }

      { // negative: invalid key
        val tree = createTree(preRemoveDigest, removeAllowed = true)
        val invalidKey = key.map(x => (-x).toByte) // any other different from `key`
        val invalidKeys = Colls.fromItems(invalidKey)
        val input = (tree, (invalidKeys, removeProof))
        remove.checkExpected(input, success(None))
        remove.checkVerify(input, Expected(value = Success(None), cost = cost))
      }

      { // negative: invalid proof
        val tree = createTree(preRemoveDigest, removeAllowed = true)
        val invalidProof = removeProof.map(x => (-x).toByte) // any other different from `removeProof`
        val input = (tree, (keys, invalidProof))
        remove.checkExpected(input, success(None))
        remove.checkVerify(input, Expected(value = Success(None), cost = cost))
      }
    }
  }

  property("longToByteArray equivalence") {
    val costDetails = CostDetails(traceBase :+ FixedCostItem(CompanionDesc(LongToByteArray), FixedCost(JitCost(17))))
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36007, costDetails, 1787)
        Seq(
          (-9223372036854775808L, success(Helpers.decodeBytes("8000000000000000"))),
          (-1148502660425090565L, success(Helpers.decodeBytes("f00fb2ea55c579fb"))),
          (-1L, success(Helpers.decodeBytes("ffffffffffffffff"))),
          (0L, success(Helpers.decodeBytes("0000000000000000"))),
          (1L, success(Helpers.decodeBytes("0000000000000001"))),
          (238790047448232028L, success(Helpers.decodeBytes("03505a48720cf05c"))),
          (9223372036854775807L, success(Helpers.decodeBytes("7fffffffffffffff")))
        )
      },
      existingFeature((x: Long) => SigmaDsl.longToByteArray(x),
        "{ (x: Long) => longToByteArray(x) }",
        FuncValue(Vector((1, SLong)), LongToByteArray(ValUse(1, SLong)))))
  }

  property("byteArrayToBigInt equivalence") {
    val costDetails = CostDetails(traceBase :+ FixedCostItem(CompanionDesc(ByteArrayToBigInt), FixedCost(JitCost(30))))
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36536, costDetails, 1787)
        Seq(
          (Helpers.decodeBytes(""),
              Expected(new NumberFormatException("Zero length BigInteger"))),
          (Helpers.decodeBytes("00"),
              success(CBigInt(new BigInteger("0", 16)))),
          (Helpers.decodeBytes("01"),
              success(CBigInt(new BigInteger("1", 16)))),
          (Helpers.decodeBytes("ff"),
              success(CBigInt(new BigInteger("-1", 16)))),
          (Helpers.decodeBytes("80d6c201"),
              Expected(Success(CBigInt(new BigInteger("-7f293dff", 16))), 36539, costDetails, 1787)),
          (Helpers.decodeBytes("70d6c20170d6c201"),
              Expected(Success(CBigInt(new BigInteger("70d6c20170d6c201", 16))), 36543, costDetails, 1787)),
          (Helpers.decodeBytes(
            "80e0ff7f02807fff72807f0a00ff7fb7c57f75c11ba2802970fd250052807fc37f6480ffff007fff18eeba44"
          ), Expected(new ArithmeticException("BigInteger out of 256 bit range")))
        )
      },
      existingFeature((x: Coll[Byte]) => SigmaDsl.byteArrayToBigInt(x),
        "{ (x: Coll[Byte]) => byteArrayToBigInt(x) }",
        FuncValue(Vector((1, SByteArray)), ByteArrayToBigInt(ValUse(1, SByteArray)))))
  }

  property("byteArrayToLong equivalence") {
    val costDetails = CostDetails(traceBase :+ FixedCostItem(CompanionDesc(ByteArrayToLong), FixedCost(JitCost(16))))
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36092, costDetails, 1785)
        Seq(
          (Helpers.decodeBytes(""), Expected(new IllegalArgumentException("array too small: 0 < 8"))),
          (Helpers.decodeBytes("81"), Expected(new IllegalArgumentException("array too small: 1 < 8"))),
          (Helpers.decodeBytes("812d7f00ff807f"), Expected(new IllegalArgumentException("array too small: 7 < 8"))),
          (Helpers.decodeBytes("812d7f00ff807f7f"), success(-9138508426601529473L)),
          (Helpers.decodeBytes("ffffffffffffffff"), success(-1L)),
          (Helpers.decodeBytes("0000000000000000"), success(0L)),
          (Helpers.decodeBytes("0000000000000001"), success(1L)),
          (Helpers.decodeBytes("712d7f00ff807f7f"), success(8155314142501175167L)),
          (Helpers.decodeBytes("812d7f00ff807f7f0101018050757f0580ac009680f2ffc1"), success(-9138508426601529473L))
        )
      },
      existingFeature((x: Coll[Byte]) => SigmaDsl.byteArrayToLong(x),
        "{ (x: Coll[Byte]) => byteArrayToLong(x) }",
        FuncValue(Vector((1, SByteArray)), ByteArrayToLong(ValUse(1, SByteArray)))))
  }

  // TODO soft-fork: related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/427
  // TODO costing: expression t._1(t._2) cannot be costed because t is lambda argument
  //  ignore("Func context variable") {
  //    val doApply = checkEq(func[(Int => Int, Int), Int]("{ (t: (Int => Int, Int)) => t._1(t._2) }")) { (t: (Int => Int, Int)) => t._1(t._2) }
  //    val code = compileWithCosting(emptyEnv, s"{ (x: Int) => x + 1 }")
  //    val ctx = ErgoLikeContext.dummy(fakeSelf)
  //    doApply((CFunc[Int, Int](ctx, code), 10))
  //  }

  property("Box properties equivalence") {
    verifyCases(
      {
        val costDetails = CostDetails(traceBase :+ FixedCostItem(CompanionDesc(ExtractId), FixedCost(JitCost(12))))
        def success[T](v: T) = Expected(Success(v), 35984, costDetails, 1786)
        Seq(
          (b1, success(Helpers.decodeBytes("5ee78f30ae4e770e44900a46854e9fecb6b12e8112556ef1cd19aef633b4421e"))),
          (b2, success(Helpers.decodeBytes("3a0089be265460e29ca47d26e5b55a6f3e3ffaf5b4aed941410a2437913848ad")))
        )
      },
      existingFeature({ (x: Box) => x.id },
        "{ (x: Box) => x.id }",
        FuncValue(Vector((1, SBox)), ExtractId(ValUse(1, SBox)))))

    verifyCases(
      {
        val costDetails = CostDetails(traceBase :+ FixedCostItem(CompanionDesc(ExtractAmount), FixedCost(JitCost(8))))
        def success[T](v: T) = Expected(Success(v), 35882, costDetails, 1784)
        Seq(
          (b1, success(9223372036854775807L)),
          (b2, success(12345L))
        )
      },
      existingFeature({ (x: Box) => x.value },
        "{ (x: Box) => x.value }",
        FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))))

    verifyCases(
      {
        val costDetails = CostDetails(traceBase :+ FixedCostItem(CompanionDesc(ExtractScriptBytes), FixedCost(JitCost(10))))
        def success[T](v: T) = Expected(Success(v), 35938, costDetails, 1786)
        Seq(
          (b1, success(Helpers.decodeBytes(
            "100108cd0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e7300"
          ))),
          (b2, success(Helpers.decodeBytes("00d1968302010100ff83020193040204020100")))
        )
      },
      existingFeature({ (x: Box) => x.propositionBytes },
        "{ (x: Box) => x.propositionBytes }",
        FuncValue(Vector((1, SBox)), ExtractScriptBytes(ValUse(1, SBox)))))

    verifyCases(
      {
        val costDetails = CostDetails(traceBase :+ FixedCostItem(CompanionDesc(ExtractBytes), FixedCost(JitCost(12))))
        def success[T](v: T) = Expected(Success(v), 36012, costDetails, 1786)
        Seq(
          (b1, success(Helpers.decodeBytes(
            "ffffffffffffffff7f100108cd0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e73009fac29026e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f000180ade204a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600f4030201000e067fc87f7f01ff218301ae8000018008637f0021fb9e00018055486f0b514121016a00ff718080bcb001"
          ))),
          (b2, success(Helpers.decodeBytes(
            "b96000d1968302010100ff83020193040204020100c0843d000401010e32297000800b80f1d56c809a8c6affbed864b87f007f6f007f00ac00018c01c4fdff011088807f0100657f00f9ab0101ff6d6505a4a7b5a2e7a4a4dd3a05feffffffffffffffff01003bd5c630803cfff6c1ff7f7fb980ff136afc011f8080b8b04ad4dbda2d7f4e01"
          )))
        )
      },
      existingFeature({ (x: Box) => x.bytes },
        "{ (x: Box) => x.bytes }",
        FuncValue(Vector((1, SBox)), ExtractBytes(ValUse(1, SBox)))))

    verifyCases(
      {
        val costDetails = CostDetails(traceBase :+ FixedCostItem(CompanionDesc(ExtractBytesWithNoRef), FixedCost(JitCost(12))))
        def success[T](v: T) = Expected(Success(v), 35954, costDetails, 1786)
        Seq(
          (b1, success(Helpers.decodeBytes(
            "ffffffffffffffff7f100108cd0297c44a12f4eb99a85d298fa3ba829b5b42b9f63798c980ece801cc663cc5fc9e73009fac29026e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f000180ade204a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600f4030201000e067fc87f7f01ff"
          ))),
          (b2, success(Helpers.decodeBytes(
            "b96000d1968302010100ff83020193040204020100c0843d000401010e32297000800b80f1d56c809a8c6affbed864b87f007f6f007f00ac00018c01c4fdff011088807f0100657f00f9ab0101ff6d6505a4a7b5a2e7a4a4dd3a05feffffffffffffffff01"
          )))
        )
      },
      existingFeature({ (x: Box) => x.bytesWithoutRef },
        "{ (x: Box) => x.bytesWithoutRef }",
        FuncValue(Vector((1, SBox)), ExtractBytesWithNoRef(ValUse(1, SBox)))))

    verifyCases(
      {
        val costDetails = CostDetails(traceBase :+ FixedCostItem(CompanionDesc(ExtractCreationInfo), FixedCost(JitCost(16))))
        def success[T](v: T) = Expected(Success(v), 36074, costDetails, 1787)
        Seq(
          (b1, success((
              677407,
              Helpers.decodeBytes("218301ae8000018008637f0021fb9e00018055486f0b514121016a00ff718080583c")
              ))),
          (b2, success((
              1000000,
              Helpers.decodeBytes("003bd5c630803cfff6c1ff7f7fb980ff136afc011f8080b8b04ad4dbda2d7f4e0001")
              )))
        )
      },
      existingFeature({ (x: Box) => x.creationInfo },
        "{ (x: Box) => x.creationInfo }",
        FuncValue(Vector((1, SBox)), ExtractCreationInfo(ValUse(1, SBox)))))

    // TODO v6.0 (2h): fix collections equality and remove map(identity)
    //  (PairOfColl should be equal CollOverArray)
    verifyCases(
      Seq(
        b1 -> Expected(Success(Coll[(Coll[Byte], Long)](
          (Helpers.decodeBytes("6e789ab7b2fffff12280a6cd01557f6fb22b7f80ff7aff8e1f7f15973d7f0001"), 10000000L),
          (Helpers.decodeBytes("a3ff007f00057600808001ff8f8000019000ffdb806fff7cc0b6015eb37fa600"), 500L)
          ).map(identity)), 36167, methodCostDetails(SBox.tokensMethod, 15), 1792),
        b2 -> Expected(Success(Coll[(Coll[Byte], Long)]().map(identity)), 36157, methodCostDetails(SBox.tokensMethod, 15), 1786)
      ),
      existingFeature({ (x: Box) => x.tokens },
        "{ (x: Box) => x.tokens }",
        FuncValue(
          Vector((1, SBox)),
          MethodCall.typed[Value[SCollection[STuple]]](
            ValUse(1, SBox),
            SBox.getMethodByName("tokens"),
            Vector(),
            Map()
          )
        )))
  }

  property("Box properties equivalence (new features)") {
    // TODO v6.0 (4h): related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/416
    val getReg = newFeature((x: Box) => x.getReg[Int](1).get,
      "{ (x: Box) => x.getReg[Int](1).get }")

    forAll { box: Box =>
      Seq(getReg).foreach(_.checkEquality(box))
    }
  }

  property("Conditional access to registers") {
    def boxWithRegisters(regs: AdditionalRegisters): Box = {
      SigmaDsl.Box(testBox(20, TrueTree, 0, Seq(), regs))
    }
    val box1 = boxWithRegisters(Map(
      ErgoBox.R4 -> ByteConstant(0.toByte),
      ErgoBox.R5 -> ShortConstant(1024.toShort)
    ))
    val box2 = boxWithRegisters(Map(
      ErgoBox.R4 -> ByteConstant(1.toByte),
      ErgoBox.R5 -> IntConstant(1024 * 1024)
    ))
    val box3 = boxWithRegisters(Map(
      ErgoBox.R4 -> ByteConstant(2.toByte)
    ))
    val box4 = boxWithRegisters(Map.empty)
    val expCostDetails1 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(ExtractRegisterAs), FixedCost(JitCost(50))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(OptionIsDefined), FixedCost(JitCost(10))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet)
      )
    )
    val expCostDetails2 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(ExtractRegisterAs), FixedCost(JitCost(50))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(OptionIsDefined), FixedCost(JitCost(10))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        FixedCostItem(Constant)
      )
    )

    verifyCases(
      Seq(
        (box1, Expected(Success(1024.toShort), 37190, expCostDetails1, 1794)),
        (box2, Expected(
          new InvalidType("Cannot getReg[Short](5): invalid type of value TestValue(1048576) at id=5")
        )),
        (box3, Expected(Success(0.toShort), 37190, expCostDetails2, 1792))
      ),
      existingFeature(
        { (x: Box) =>
          val tagOpt = x.R5[Short]
          if (tagOpt.isDefined) {
            tagOpt.get
          } else {
            0.toShort
          }
        },
        """{ (x: Box) =>
         |  val tagOpt = x.R5[Short]
         |  if (tagOpt.isDefined) {
         |    tagOpt.get
         |  } else {
         |    0.toShort
         |  }
         |}""".stripMargin,
        FuncValue(
          Array((1, SBox)),
          BlockValue(
            Array(ValDef(3, List(), ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R5, SOption(SShort)))),
            If(
              OptionIsDefined(ValUse(3, SOption(SShort))),
              OptionGet(ValUse(3, SOption(SShort))),
              ShortConstant(0.toShort)
            )
          )
        )))

    val expCostDetails3 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(ExtractRegisterAs), FixedCost(JitCost(50))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(OptionIsDefined), FixedCost(JitCost(10))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(ExtractRegisterAs), FixedCost(JitCost(50))),
        FixedCostItem(OptionGet),
        TypeBasedCostItem(Upcast, SInt)
      )
    )

    val expCostDetails4 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(ExtractRegisterAs), FixedCost(JitCost(50))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(OptionIsDefined), FixedCost(JitCost(10))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(ExtractRegisterAs), FixedCost(JitCost(50))),
        FixedCostItem(OptionGet)
      )
    )

    val expCostDetails5 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(ExtractRegisterAs), FixedCost(JitCost(50))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(CompanionDesc(OptionIsDefined), FixedCost(JitCost(10))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(CompanionDesc(If), FixedCost(JitCost(10))),
        FixedCostItem(Constant)
      )
    )

  
    verifyCases(
      Seq(
        (box1, Expected(Success(1024), cost = 39782, expCostDetails3, 1805)),
        (box2, Expected(Success(1024 * 1024), cost = 39782, expCostDetails4, 1806)),
        (box3, Expected(Success(0), cost = 39782, expCostDetails5, 1799)),
        (box4, Expected(Success(-1), cost = 39782, expCostDetails2, 1792))
      ),
      existingFeature(
        { (x: Box) =>
          val tagOpt = x.R4[Byte]
          if (tagOpt.isDefined) {
            val tag = tagOpt.get
            if (tag == 0.toByte) {
              val short = x.R5[Short].get  // access Short in the register
              short.toInt
            } else {
              if (tag == 1.toByte) {
                x.R5[Int].get    // access Int in the register
              }
              else 0
            }
          } else {
            -1
          }
        },
        """{
         | (x: Box) =>
         |   val tagOpt = x.R4[Byte]
         |   if (tagOpt.isDefined) {
         |     val tag = tagOpt.get
         |     if (tag == 0.toByte) {
         |       val short = x.R5[Short].get  // access Short in the register
         |       short.toInt
         |     } else {
         |       if (tag == 1.toByte) {
         |         x.R5[Int].get    // access Int in the register
         |       }
         |       else 0
         |     }
         |   } else {
         |     -1
         |   }
         |}""".stripMargin,
        FuncValue(
          Array((1, SBox)),
          BlockValue(
            Array(ValDef(3, List(), ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R4, SOption(SByte)))),
            If(
              OptionIsDefined(ValUse(3, SOption(SByte))),
              BlockValue(
                Array(ValDef(4, List(), OptionGet(ValUse(3, SOption(SByte))))),
                If(
                  EQ(ValUse(4, SByte), ByteConstant(0.toByte)),
                  Upcast(OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R5, SOption(SShort))), SInt),
                  If(
                    EQ(ValUse(4, SByte), ByteConstant(1.toByte)),
                    OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R5, SOption(SInt))),
                    IntConstant(0)
                  )
                )
              ),
              IntConstant(-1)
            )
          )
        )
        ))
  }

  property("Advanced Box test") {
    val (tree, _) = createAvlTreeAndProver()

    val box1 = SigmaDsl.Box(testBox(20, TrueTree, 0, Seq(), Map(
      ErgoBox.R4 -> ByteConstant(1.toByte),
      ErgoBox.R5 -> ShortConstant(1024.toShort),
      ErgoBox.R6 -> IntConstant(1024 * 1024),
      ErgoBox.R7 -> LongConstant(1024.toLong),
      ErgoBox.R8 -> BigIntConstant(222L),
      ErgoBox.R9 -> AvlTreeConstant(tree)
    )))

    val box2 = SigmaDsl.Box(testBox(20, TrueTree, 0, Seq(), Map(
      ErgoBox.R4 -> ByteArrayConstant(Coll(1.toByte))
    )))

    val box3 = SigmaDsl.Box(testBox(20, TrueTree, 0, Seq(), Map(
      ErgoBox.R4 -> Constant((10, 20L).asInstanceOf[SType#WrappedType], STuple(SInt, SLong)),
      ErgoBox.R5 -> Constant((10, Some(20L)).asInstanceOf[SType#WrappedType], STuple(SInt, SOption(SLong)))
      // TODO v6.0 (1h): uncomment after DataSerializer support of Option type
      //  ErgoBox.R6 -> Constant[SOption[SInt.type]](Option(10), SOption(SInt)),
    )))

    val expCostDetails = TracedCost(
      traceBase ++ Array(
        FixedCostItem(CompanionDesc(ExtractRegisterAs), FixedCost(JitCost(50))), 
        FixedCostItem(OptionGet)
      )
    )
    verifyCases(
      Seq(
        (box1, Expected(Success(1.toByte), cost = 36253, expCostDetails, 1790)),
        (box2, Expected(new InvalidType("Cannot getReg[Byte](4): invalid type of value Value(Coll(1)) at id=4")))
      ),
      existingFeature((x: Box) => x.R4[Byte].get,
        "{ (x: Box) => x.R4[Byte].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R4, SOption(SByte)))
        )))

    verifyCases(
      Seq(
        (box1, Expected(Success(1024.toShort), cost = 36253, expCostDetails, 1790)),
        (box2, Expected(new NoSuchElementException("None.get")))
      ),
      existingFeature((x: Box) => x.R5[Short].get,
        "{ (x: Box) => x.R5[Short].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R5, SOption(SShort)))
        )))

    verifyCases(
      Seq(
        (box1, Expected(Success(1024 * 1024), cost = 36253, expCostDetails, 1790))
      ),
      existingFeature((x: Box) => x.R6[Int].get,
        "{ (x: Box) => x.R6[Int].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R6, SOption(SInt)))
        )))

    verifyCases(
      Seq(
        (box1, Expected(Success(1024.toLong), cost = 36253, expCostDetails, 1790))
      ),
      existingFeature((x: Box) => x.R7[Long].get,
        "{ (x: Box) => x.R7[Long].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R7, SOption(SLong)))
        )))

    verifyCases(
      Seq(
        (box1, Expected(Success(CBigInt(BigInteger.valueOf(222L))), cost = 36253, expCostDetails, 1790))
      ),
      existingFeature((x: Box) => x.R8[BigInt].get,
        "{ (x: Box) => x.R8[BigInt].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R8, SOption(SBigInt)))
        )))

    verifyCases(
      Seq(
        (box1, Expected(Success(CAvlTree(
          AvlTreeData(
            ADDigest @@ (
                ErgoAlgos.decodeUnsafe("4ec61f485b98eb87153f7c57db4f5ecd75556fddbc403b41acf8441fde8e160900")
                ),
            AvlTreeFlags(true, true, true),
            32,
            None
          )
        )),
        cost = 36253,
        expCostDetails,
        1790)
      )),
      existingFeature((x: Box) => x.R9[AvlTree].get,
        "{ (x: Box) => x.R9[AvlTree].get }",
        FuncValue(
          Vector((1, SBox)),
          OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R9, SOption(SAvlTree)))
        )))

    verifyCases(
      Seq(
        (box3, Expected(Success(10), cost = 36468))//, expCostDetails, 1790))
      ),
      existingFeature((x: Box) => x.R4[(Int, Long)].get._1,
        "{ (x: Box) => x.R4[(Int, Long)].get._1 }",
        FuncValue(
          Array((1, SBox)),
          SelectField.typed[Value[SInt.type]](
            OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R4, SOption(SPair(SInt, SLong)))),
            1.toByte
          )
        )))

    verifyCases(
      Seq(
        (box3, Expected(Success(10), cost = 36468))//, expCostDetails, 1790))
      ),
      existingFeature((x: Box) => x.R5[(Int, Option[Long])].get._1,
        "{ (x: Box) => x.R5[(Int, Option[Long])].get._1 }",
        FuncValue(
          Array((1, SBox)),
          SelectField.typed[Value[SInt.type]](
            OptionGet(ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R5, SOption(SPair(SInt, SOption(SLong))))),
            1.toByte
          )
        )))

    // TODO v6.0 (1h): uncomment after DataSerializer support of Option type
    //    verifyCases(
    //      Seq(
    //        (box3, Expected(Success(20L), cost = 36468))
    //      ),
    //      existingFeature((x: Box) => x.R5[(Int, Option[Long])].get._2.get,
    //        "{ (x: Box) => x.R5[(Int, Option[Long])].get._2.get }",
    //        FuncValue(
    //          Array((1, SBox)),
    //          OptionGet(
    //            SelectField.typed[Value[SOption[SLong.type]]](
    //              OptionGet(
    //                ExtractRegisterAs(ValUse(1, SBox), ErgoBox.R5, SOption(SPair(SInt, SOption(SLong))))
    //              ),
    //              2.toByte
    //            ))
    //        )))
  }

  def existingPropTest[A: RType, B: RType](propName: String, scalaFunc: A => B) = {
    val tA = RType[A]
    val typeName = tA.name
    val stypeA = Evaluation.rtypeToSType(tA)
    val typeCompanion = stypeA.asInstanceOf[STypeCompanion]
    existingFeature(scalaFunc,
      s"{ (x: $typeName) => x.$propName }",
      FuncValue(Vector((1, stypeA)),
        MethodCall(ValUse(1, stypeA), typeCompanion.getMethodByName(propName), Vector(), Map() )
      ))
  }

  property("PreHeader properties equivalence") {
    

    verifyCases(
      Seq((preH1, Expected(Success(0.toByte), cost = 37022, methodCostDetails(SPreHeader.versionMethod, 10), 1785))),
      existingPropTest("version", { (x: PreHeader) => x.version }))

    verifyCases(
      Seq((preH1, Expected(Success(
        Helpers.decodeBytes("7fff7fdd6f62018bae0001006d9ca888ff7f56ff8006573700a167f17f2c9f40")),
        cost = 36121, methodCostDetails(SPreHeader.parentIdMethod, 10), 1786))),
      existingPropTest("parentId", { (x: PreHeader) => x.parentId }))

    verifyCases(
      Seq((preH1, Expected(Success(6306290372572472443L), cost = 36147, methodCostDetails(SPreHeader.timestampMethod, 10), 1785))),
      existingPropTest("timestamp", { (x: PreHeader) => x.timestamp }))

    verifyCases(
      Seq((preH1, Expected(Success(-3683306095029417063L), cost = 36127, methodCostDetails(SPreHeader.nBitsMethod, 10), 1785))),
      existingPropTest("nBits", { (x: PreHeader) => x.nBits }))

    verifyCases(
      Seq((preH1, Expected(Success(1), cost = 36136, methodCostDetails(SPreHeader.heightMethod, 10), 1785))),
      existingPropTest("height", { (x: PreHeader) => x.height }))

    verifyCases(
      Seq((preH1, Expected(Success(
        Helpers.decodeGroupElement("026930cb9972e01534918a6f6d6b8e35bc398f57140d13eb3623ea31fbd069939b")),
        cost = 36255, methodCostDetails(SPreHeader.minerPkMethod, 10), 1802))),
      existingPropTest("minerPk", { (x: PreHeader) => x.minerPk }))

    verifyCases(
      Seq((preH1, Expected(Success(Helpers.decodeBytes("ff8087")), cost = 36249, methodCostDetails(SPreHeader.votesMethod,10), 1786))),
      existingPropTest("votes", { (x: PreHeader) => x.votes }))
  }

  property("Header properties equivalence") {
    verifyCases(
      Seq((h1, Expected(Success(
        Helpers.decodeBytes("957f008001808080ffe4ffffc8f3802401df40006aa05e017fa8d3f6004c804a")),
        cost = 36955, methodCostDetails(SHeader.idMethod, 10), 1786))),
      existingPropTest("id", { (x: Header) => x.id }))

    verifyCases(
      Seq((h1, Expected(Success(0.toByte), cost = 36124, methodCostDetails(SHeader.versionMethod, 10), 1785))),
      existingPropTest("version", { (x: Header) => x.version }))

    verifyCases(
      Seq((h1, Expected(Success(
        Helpers.decodeBytes("0180dd805b0000ff5400b997fd7f0b9b00de00fb03c47e37806a8186b94f07ff")),
        cost = 36097, methodCostDetails(SHeader.parentIdMethod, 10), 1786))),
      existingPropTest("parentId", { (x: Header) => x.parentId }))

    verifyCases(
      Seq((h1, Expected(Success(
        Helpers.decodeBytes("01f07f60d100ffb970c3007f60ff7f24d4070bb8fffa7fca7f34c10001ffe39d")),
        cost = 36111, methodCostDetails(SHeader.ADProofsRootMethod, 10), 1786))),
      existingPropTest("ADProofsRoot", { (x: Header) => x.ADProofsRoot}))

    verifyCases(
      Seq((h1, Expected(Success(CAvlTree(createAvlTreeData())), cost = 36092, methodCostDetails(SHeader.stateRootMethod, 10), 1785))),
      existingPropTest("stateRoot", { (x: Header) => x.stateRoot }))

    verifyCases(
      Seq((h1, Expected(Success(
        Helpers.decodeBytes("804101ff01000080a3ffbd006ac080098df132a7017f00649311ec0e00000100")),
        cost = 36094, methodCostDetails(SHeader.transactionsRootMethod, 10), 1786))),
      existingPropTest("transactionsRoot", { (x: Header) => x.transactionsRoot }))

    verifyCases(
      Seq((h1, Expected(Success(1L), cost = 36151, methodCostDetails(SHeader.timestampMethod, 10), 1785))),
      existingPropTest("timestamp", { (x: Header) => x.timestamp }))

    verifyCases(
      Seq((h1, Expected(Success(-1L), cost = 36125, methodCostDetails(SHeader.nBitsMethod, 10), 1785))),
      existingPropTest("nBits", { (x: Header) => x.nBits }))

    verifyCases(
      Seq((h1, Expected(Success(1), cost = 36134, methodCostDetails(SHeader.heightMethod, 10), 1785))),
      existingPropTest("height", { (x: Header) => x.height }))

    verifyCases(
      Seq((h1, Expected(Success(
        Helpers.decodeBytes("e57f80885601b8ff348e01808000bcfc767f2dd37f0d01015030ec018080bc62")),
        cost = 36133, methodCostDetails(SHeader.extensionRootMethod, 10), 1786))),
      existingPropTest("extensionRoot", { (x: Header) => x.extensionRoot }))

    verifyCases(
      Seq((h1, Expected(Success(
        Helpers.decodeGroupElement("039bdbfa0b49cc6bef58297a85feff45f7bbeb500a9d2283004c74fcedd4bd2904")),
        cost = 36111, methodCostDetails(SHeader.minerPkMethod, 10), 1802))),
      existingPropTest("minerPk", { (x: Header) => x.minerPk }))

    verifyCases(
      Seq((h1, Expected(Success(
        Helpers.decodeGroupElement("0361299207fa392231e23666f6945ae3e867b978e021d8d702872bde454e9abe9c")),
        cost = 36111, methodCostDetails(SHeader.powOnetimePkMethod, 10), 1802))),
      existingPropTest("powOnetimePk", { (x: Header) => x.powOnetimePk }))

    verifyCases(
      Seq((h1, Expected(Success(
        Helpers.decodeBytes("7f4f09012a807f01")),
        cost = 36176, methodCostDetails(SHeader.powNonceMethod, 10), 1786))),
      existingPropTest("powNonce", { (x: Header) => x.powNonce }))

    verifyCases(
      Seq((h1, Expected(Success(
        CBigInt(new BigInteger("-e24990c47e15ed4d0178c44f1790cc72155d516c43c3e8684e75db3800a288", 16))),
        cost = 36220, methodCostDetails(SHeader.powDistanceMethod, 10), 1785))),
      existingPropTest("powDistance", { (x: Header) => x.powDistance }))

    verifyCases(
      Seq((h1, Expected(Success(
        Helpers.decodeBytes("7f0180")),
        cost = 36100, methodCostDetails(SHeader.votesMethod, 10), 1786))),
      existingPropTest("votes", { (x: Header) => x.votes }))
  }

  def contextData() = {
    val input = CostingBox(
      false,
      new ErgoBox(
        80946L,
        new ErgoTree(
          16.toByte,
          Vector(
            SigmaPropConstant(
              CSigmaProp(
                ProveDHTuple(
                  Helpers.decodeECPoint("03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb"),
                  Helpers.decodeECPoint("023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d03"),
                  Helpers.decodeECPoint("03d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72"),
                  Helpers.decodeECPoint("037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441")
                )
              )
            )
          ),
          Right(ConstantPlaceholder(0, SSigmaProp))
        ),
        Coll(),
        Map(
          ErgoBox.R4 -> ByteArrayConstant(Helpers.decodeBytes("34")),
          ErgoBox.R5 -> TrueLeaf
        ),
        ModifierId @@ ("0000bfe96a7c0001e7a5ee00aafb80ff057fbe7f8c6680e33a3dc18001820100"),
        1.toShort,
        5
      )
    )

    val dataBox = CostingBox(
      false,
      new ErgoBox(
        -1L,
        new ErgoTree(
          0.toByte,
          Vector(),
          Right(SigmaPropConstant(CSigmaProp(ProveDlog(Helpers.decodeECPoint("02af645874c3b53465a5e9d820eb207d6001258c3b708f0d31d7c2e342833dce64")))))
        ),
        Coll((Digest32 @@ (ErgoAlgos.decodeUnsafe("8f0000ff009e7fff012427ff7fffcc35dfe680017f004ef3be1280e57fc40101")), 500L)),
        Map(
          ErgoBox.R9 -> LongConstant(-6985752043373238161L),
          ErgoBox.R4 -> LongConstant(-7374898275229807247L),
          ErgoBox.R6 -> ByteArrayConstant(Helpers.decodeBytes("00")),
          ErgoBox.R5 -> LongConstant(-135729055492651903L),
          ErgoBox.R8 -> TrueLeaf,
          ErgoBox.R7 -> ByteArrayConstant(
            Helpers.decodeBytes(
              "5a017f1f9d2e01ff004f007f807f21b87f899e3380014900010c0101da80e9809d2a85ff010125cc017f74ed8c7f96b55efffadf7f7fffa700012e8085a915007f7f0001ffd5013e0180d58bb300c5b77f231e7f1c01013d807afd387f80287f80a51900"
            )
          )
        ),
        ModifierId @@ ("ff3f4e00d400ff00ffae3680927f782affc0004b9f0092ca98010080f60100c1"),
        9495.toShort,
        1000000
      )
    )

    val header = CHeader(
      Helpers.decodeBytes("1c597f88969600d2fffffdc47f00d8ffc555a9e85001000001c505ff80ff8f7f"),
      0.toByte,
      Helpers.decodeBytes("7a7fe5347f09017818010062000001807f86808000ff7f66ffb07f7ad27f3362"),
      Helpers.decodeBytes("c1d70ad9b1ffc1fb9a715fff19807f2401017fcd8b73db017f1cff77727fff08"),
      CAvlTree(
        AvlTreeData(
          ADDigest @@ (ErgoAlgos.decodeUnsafe("54d23dd080006bdb56800100356080935a80ffb77e90b800057f00661601807f17")),
          AvlTreeFlags(true, true, false),
          2147483647,
          None
        )
      ),
      Helpers.decodeBytes("5e7f1164ccd0990080c501fc0e0181cb387fc17f00ff00c7d5ff767f91ff5e68"),
      -7421721754642387858L,
      -4826493284887861030L,
      10,
      Helpers.decodeBytes("e580c88001ff6fc89c5501017f80e001ff0101fe48c153ff7f00666b80d780ab"),
      Helpers.decodeGroupElement("03e7f2875298fddd933c2e0a38968fe85bdeeb70dd8b389559a1d36e2ff1b58fc5"),
      Helpers.decodeGroupElement("034e2d3b5f9e409e3ae8a2e768340760362ca33764eda5855f7a43487f14883300"),
      Helpers.decodeBytes("974651c9efff7f00"),
      CBigInt(new BigInteger("478e827dfa1e4b57", 16)),
      Helpers.decodeBytes("01ff13")
    )

    val ctx = CostingDataContext(
      _dataInputs = Coll[Box](dataBox),
      headers = Coll[Header](header),
      preHeader = CPreHeader(
        0.toByte,
        Helpers.decodeBytes("1c597f88969600d2fffffdc47f00d8ffc555a9e85001000001c505ff80ff8f7f"),
        -755484979487531112L,
        9223372036854775807L,
        11,
        Helpers.decodeGroupElement("0227a58e9b2537103338c237c52c1213bf44bdb344fa07d9df8ab826cca26ca08f"),
        Helpers.decodeBytes("007f00")
      ),
      inputs = Coll[Box](input),
      outputs = Coll[Box](
        CostingBox(
          false,
          new ErgoBox(
            1000000L,
            new ErgoTree(
              16.toByte,
              Vector(
                SigmaPropConstant(
                  CSigmaProp(
                    COR(
                      List(
                        ProveDHTuple(
                          Helpers.decodeECPoint("021b4c0f54304b9c427d5c87846dd56a2fa361cd34a6cb8a2090aef043c9383198"),
                          Helpers.decodeECPoint("026826a4a9d0ec937c24d72da381ee6b5e74e49fb79a6a23a03fe0aa2cab3448ba"),
                          Helpers.decodeECPoint("02535153378ce30df1b31137680748de728f8512d29dfeeb1f331ac6a787cd00d8"),
                          Helpers.decodeECPoint("03d00d0174cdffd7ce3b77ef45ef9573c18fb76929fb3340f7ceea8d0be9bf5c4a")
                        ),
                        ProveDlog(Helpers.decodeECPoint("02c702d83f83a5ec9674e17e5eb3ab3ae579768c945590f0fb10c1c4a388353c7c")),
                        ProveDHTuple(
                          Helpers.decodeECPoint("03bef02fb10347eef473730711ec313b2f013322e6dad32515bd172249420f25e5"),
                          Helpers.decodeECPoint("0365160972ed72d232f0cb5fa7909ac1647eb122942b421493def6a6051005d141"),
                          Helpers.decodeECPoint("035060119f4b47ccf12c4502657e9ee38dba92fc6b6b1807b75d5cdc1986754751"),
                          Helpers.decodeECPoint("02db7a6c1b51847ce5b1ba8e8c89b4ea5e68c5667f430e8bbe075ff4ea2877233a")
                        )
                      )
                    )
                  )
                )
              ),
              Right(ConstantPlaceholder(0, SSigmaProp))
            ),
            Coll((Digest32 @@ (ErgoAlgos.decodeUnsafe("6f070152007f00005a00893ea1e98045ffa28f72da01ff7f01ff2d48eb793fd6")), 20000L)),
            Map(ErgoBox.R5 -> LongConstant(1L), ErgoBox.R4 -> LongConstant(5008366408131208436L)),
            ModifierId @@ ("26485d14a94ef18ec36227a838b98e11e910087be4c7e634f51391e4ea4d16ff"),
            0.toShort,
            11
          )
        ),
        CostingBox(
          false,
          new ErgoBox(
            2769336982721999022L,
            new ErgoTree(
              0.toByte,
              Vector(),
              Right(SigmaPropConstant(CSigmaProp(ProveDlog(Helpers.decodeECPoint("02d13e1a8c31f32194761adc1cdcbaa746b3e049e682bba9308d8ee84576172991")))))
            ),
            Coll((Digest32 @@ (ErgoAlgos.decodeUnsafe("6f070152007f00005a00893ea1e98045ffa28f72da01ff7f01ff2d48eb793fd6")), 500L)),
            Map(),
            ModifierId @@ ("26485d14a94ef18ec36227a838b98e11e910087be4c7e634f51391e4ea4d16ff"),
            1.toShort,
            0
          )
        )
      ),
      height = 11,
      selfBox = input.copy(),  // in 3.x, 4.x implementation selfBox is never the same instance as input (see toSigmaContext)
      selfIndex = 0,
      lastBlockUtxoRootHash = CAvlTree(
        AvlTreeData(
          ADDigest @@ (ErgoAlgos.decodeUnsafe("54d23dd080006bdb56800100356080935a80ffb77e90b800057f00661601807f17")),
          AvlTreeFlags(true, true, true),
          1211925457,
          None
        )
      ),
      _minerPubKey = Helpers.decodeBytes("0227a58e9b2537103338c237c52c1213bf44bdb344fa07d9df8ab826cca26ca08f"),
      vars = Colls
          .replicate[AnyValue](10, null) // reserve 10 vars
          .append(Coll[AnyValue](
            TestValue(Helpers.decodeBytes("00"), CollType(RType.ByteType)),
            TestValue(true, RType.BooleanType))),
      activatedScriptVersion = activatedVersionInTests,
      currentErgoTreeVersion = ergoTreeVersionInTests,
      false
    )
    val ctx2 = ctx.copy(vars = Coll[AnyValue](null, null, null))
    val ctx3 = ctx.copy(vars = Coll[AnyValue]())

    (input, dataBox, header, ctx, ctx2, ctx3)
  }

  def ctxWithRegsInOutput(ctx: CostingDataContext, regs: AdditionalRegisters) = {
    ctx.copy(
      outputs = Coll({
        val box = ctx.outputs(0).asInstanceOf[CostingBox]
        box.copy(ebox = copyBox(box.ebox)(additionalRegisters = regs))
      })
    )
  }

  def ctxWithRegsInDataInput(ctx: CostingDataContext, regs: AdditionalRegisters) = {
    ctx.copy(
      _dataInputs = Coll({
        val box = ctx.dataInputs(0).asInstanceOf[CostingBox]
        box.copy(ebox = copyBox(box.ebox)(additionalRegisters = regs))
      })
    )
  }

  property("Context properties equivalence") {
    val samples = genSamples[Context](MinSuccessful(5))
    val (input, dataBox, header, ctx, ctx2, ctx3) = contextData()

    test(samples, existingPropTest("dataInputs", { (x: Context) => x.dataInputs }))

    val testTraceBase = traceBase ++ Array(
      FixedCostItem(PropertyCall),
      FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
      FixedCostItem(Constant),
      FixedCostItem(ByIndex)
    )
        
    val costDetails = TracedCost(testTraceBase)
    verifyCases(
      Seq(
        (ctx, Expected(Success(dataBox), cost = 37087, costDetails, 1789)),
        (ctx.copy(_dataInputs = Coll()), Expected(new ArrayIndexOutOfBoundsException("0")))
      ),
      existingFeature({ (x: Context) => x.dataInputs(0) },
        "{ (x: Context) => x.dataInputs(0) }",
        FuncValue(
          Vector((1, SContext)),
          ByIndex(
            MethodCall.typed[Value[SCollection[SBox.type]]](
              ValUse(1, SContext),
              SContext.getMethodByName("dataInputs"),
              Vector(),
              Map()
            ),
            IntConstant(0),
            None
          )
        )),
      preGeneratedSamples = Some(samples))

    val idCostDetails = TracedCost(testTraceBase :+ FixedCostItem(CompanionDesc(ExtractId), FixedCost(JitCost(12))))
    verifyCases(
      Seq(
        (ctx, Expected(Success(
          Helpers.decodeBytes("7da4b55971f19a78d007638464580f91a020ab468c0dbe608deb1f619e245bc3")),
          cost = 37193, idCostDetails, 1792))
      ),
      existingFeature({ (x: Context) => x.dataInputs(0).id },
        "{ (x: Context) => x.dataInputs(0).id }",
        FuncValue(
          Vector((1, SContext)),
          ExtractId(
            ByIndex(
              MethodCall.typed[Value[SCollection[SBox.type]]](
                ValUse(1, SContext),
                SContext.getMethodByName("dataInputs"),
                Vector(),
                Map()
              ),
              IntConstant(0),
              None
            )
          )
        )),
      preGeneratedSamples = Some(samples))

    // NOTE: verifyCases is not used below because PreHeader/Header instances cannot be put in
    // registers and context vars (which are used in `checkVerify` method)
    testCases(
      Seq(ctx -> Success(ctx.preHeader)),
      existingPropTest("preHeader", { (x: Context) => x.preHeader }),
      preGeneratedSamples = Some(samples))

    testCases(
      Seq(ctx -> Success(ctx.headers)),
      existingPropTest("headers", { (x: Context) => x.headers }),
      preGeneratedSamples = Some(samples))

    // TODO: verifyCases doesn't work because of equality (check the reason)

    testCases(
      Seq(ctx -> Success(ctx.OUTPUTS)),
      existingFeature(
        { (x: Context) => x.OUTPUTS },
        "{ (x: Context) => x.OUTPUTS }",
        FuncValue(Vector((1, SContext)), Outputs)),
      preGeneratedSamples = Some(samples))

    // NOTE: verifyCases is not supported because SELF modified to pass input
    test(samples, existingFeature({ (x: Context) => x.INPUTS },
      "{ (x: Context) => x.INPUTS }", FuncValue(Vector((1, SContext)), Inputs)))

    test(samples, existingFeature({ (x: Context) => x.SELF },
    "{ (x: Context) => x.SELF }", FuncValue(Vector((1, SContext)), Self)))

    val heightCostDetails = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(Height)
      )
    )
    verifyCases(
      Seq(ctx -> Expected(Success(ctx.HEIGHT), cost = 35885, heightCostDetails, 1786)),
      existingFeature(
        { (x: Context) => x.HEIGHT },
        "{ (x: Context) => x.HEIGHT }",
        FuncValue(Vector((1, SContext)), Height)),
      preGeneratedSamples = Some(samples))

    val inputsCostDetails1 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(Inputs),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(MapCollection), PerItemCost(JitCost(20), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount)))
    verifyCases(
      Seq((ctx, Expected(Success(Coll[Long](80946L)), cost = 39152, inputsCostDetails1, 1790))),
      existingFeature(
        { (x: Context) => x.INPUTS.map { (b: Box) => b.value } },
        "{ (x: Context) => x.INPUTS.map { (b: Box) => b.value } }",
        FuncValue(
          Vector((1, SContext)),
          MapCollection(Inputs, FuncValue(Vector((3, SBox)), ExtractAmount(ValUse(3, SBox))))
        )),
      preGeneratedSamples = Some(samples))

    val inputsCostDetails2 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(Inputs),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(MapCollection), PerItemCost(JitCost(20), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        FixedCostItem(Tuple)))
    verifyCases(
      Seq((ctx, Expected(Success(Coll((80946L, 80946L))), cost = 39959, inputsCostDetails2, 1794))),
      existingFeature(
        { (x: Context) => x.INPUTS.map { (b: Box) => (b.value, b.value) } },
        """{ (x: Context) =>
         |  x.INPUTS.map { (b: Box) => (b.value, b.value) }
         |}""".stripMargin,
        FuncValue(
          Vector((1, SContext)),
          MapCollection(
            Inputs,
            FuncValue(
              Vector((3, SBox)),
              BlockValue(
                Vector(ValDef(5, List(), ExtractAmount(ValUse(3, SBox)))),
                Tuple(Vector(ValUse(5, SLong), ValUse(5, SLong)))
              )
            )
          )
        )),
      preGeneratedSamples = Some(samples))

    verifyCases(
      Seq((ctx, Expected(new InvalidType("Cannot getReg[Int](4): invalid type of value Value(Coll(52)) at id=4")))),
      existingFeature(
        { (x: Context) =>
          x.INPUTS.map { (b: Box) =>
            val pk = b.R4[Int].get
            val value = longToByteArray(b.value)
            (pk, value)
          }
        },
        """{ (x: Context) =>
         |  x.INPUTS.map { (b: Box) =>
         |    val pk = b.R4[Int].get
         |    val value = longToByteArray(b.value)
         |    (pk, value)
         |  }
         |}""".stripMargin,
        FuncValue(
          Vector((1, SContext)),
          MapCollection(
            Inputs,
            FuncValue(
              Vector((3, SBox)),
              Tuple(
                Vector(
                  OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SInt))),
                  LongToByteArray(ExtractAmount(ValUse(3, SBox)))
                )
              )
            )
          )
        )),
      preGeneratedSamples = Some(samples))

    val selfCostDetails = TracedCost(
      traceBase ++ Array(
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.selfBoxIndexMethod, FixedCost(JitCost(20)))
      )
    )
    verifyCases(
      Seq(
        (ctx, Expected(
          Success(-1), cost = 36318,
          // TODO mainnet v5.0: Change to concrete details does not change test result
          expectedDetails = CostDetails.ZeroCost,
          newCost = 1786,
          newVersionedResults = {
            val res = (ExpectedResult(Success(0), Some(1786)) -> Some(selfCostDetails))
            Seq(0, 1, 2).map(version => version -> res)
          }))
      ),
      changedFeature({ (x: Context) => x.selfBoxIndex },
        { (x: Context) => x.selfBoxIndex }, // see versioning in selfBoxIndex implementation
        "{ (x: Context) => x.selfBoxIndex }",
        FuncValue(
          Vector((1, SContext)),
          MethodCall.typed[Value[SInt.type]](
            ValUse(1, SContext),
            SContext.getMethodByName("selfBoxIndex"),
            Vector(),
            Map()
          )
        )),
      preGeneratedSamples = Some(samples))

    // test vectors to reproduce v4.x bug (see https://github.com/ScorexFoundation/sigmastate-interpreter/issues/603)
    samples.foreach { c =>
      if (VersionContext.current.isJitActivated) {
        // fixed in v5.0
        c.selfBoxIndex should not be(-1)
      } else {
        // should be reproduced in v4.x
        c.selfBoxIndex shouldBe -1
      }
    }

    verifyCases(
      Seq(ctx -> Expected(Success(ctx.LastBlockUtxoRootHash), cost = 35990, methodCostDetails(SContext.lastBlockUtxoRootHashMethod, 15), 1786)),
      existingPropTest("LastBlockUtxoRootHash", { (x: Context) => x.LastBlockUtxoRootHash }),
      preGeneratedSamples = Some(samples))

    val isUpdateAllowedCostDetails = TracedCost(
      traceBase ++ Array(
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.lastBlockUtxoRootHashMethod, FixedCost(JitCost(15))),
        FixedCostItem(PropertyCall),
        FixedCostItem(SAvlTree.isUpdateAllowedMethod, FixedCost(JitCost(15)))
      )
    )
    verifyCases(
      Seq(ctx -> Expected(Success(ctx.LastBlockUtxoRootHash.isUpdateAllowed), cost = 36288, isUpdateAllowedCostDetails, 1787)),
      existingFeature(
        { (x: Context) => x.LastBlockUtxoRootHash.isUpdateAllowed },
        "{ (x: Context) => x.LastBlockUtxoRootHash.isUpdateAllowed }",
        FuncValue(
          Vector((1, SContext)),
          MethodCall.typed[Value[SBoolean.type]](
            MethodCall.typed[Value[SAvlTree.type]](
              ValUse(1, SContext),
              SContext.getMethodByName("LastBlockUtxoRootHash"),
              Vector(),
              Map()
            ),
            SAvlTree.getMethodByName("isUpdateAllowed"),
            Vector(),
            Map()
          )
        )),
      preGeneratedSamples = Some(samples))

    verifyCases(
      Seq(ctx -> Expected(Success(ctx.minerPubKey), cost = 36047, methodCostDetails(SContext.minerPubKeyMethod, 20), 1787)),
      existingPropTest("minerPubKey", { (x: Context) => x.minerPubKey }),
      preGeneratedSamples = Some(samples))

// TODO v6.0 (2h): implement support of Option[T] in DataSerializer
//  this will allow passing optional values in registers and also in constants
//    testCases2(
//      Seq(
//        ctx -> Expected(Success(Some(true), cost = 0)),
//        ctx2 -> Expected(Success(None, cost = 0)),
//        ctx3 -> Expected(Success(None, cost = 0))
//      ),
    testCases(
      Seq(
        ctx -> Success(Some(true)),
        ctx2 -> Success(None),
        ctx3 -> Success(None)
      ),
      existingFeature((x: Context) => x.getVar[Boolean](11),
      "{ (x: Context) => getVar[Boolean](11) }",
        FuncValue(Vector((1, SContext)), GetVar(11.toByte, SOption(SBoolean)))),
      preGeneratedSamples = Some(samples))
 
    verifyCases(
      Seq((ctx, Expected(new InvalidType("Cannot getVar[Int](11): invalid type of value Value(true) at id=2")))),
      existingFeature((x: Context) => x.getVar[Int](11).get,
      "{ (x: Context) => getVar[Int](11).get }",
        FuncValue(Vector((1, SContext)), OptionGet(GetVar(11.toByte, SOption(SInt))))),
      preGeneratedSamples = Some(samples))

    val getVarCostDetails = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet)))
    verifyCases(
      Seq((ctx, Expected(Success(true), cost = 36750, getVarCostDetails, 1785))),
      existingFeature((x: Context) => x.getVar[Boolean](11).get,
      "{ (x: Context) => getVar[Boolean](11).get }",
        FuncValue(Vector((1, SContext)), OptionGet(GetVar(11.toByte, SOption(SBoolean))))),
      preGeneratedSamples = Some(samples))
  }

  property("Conditional access to data box register using isDefined") {
    val (_, _, _, ctx, _, _) = contextData()

    val registerIsDefinedCostDetails = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet)
      )
    )
    verifyCases(
      Seq(
        ctx -> Expected(Success(-135729055492651903L), 38399, registerIsDefinedCostDetails, 1799)
      ),
      existingFeature(
      { (x: Context) =>
        val tagOpt = x.dataInputs(0).R5[Long]
        if (tagOpt.isDefined) {
          tagOpt.get
        } else {
          0L
        }
      },
      """{ (x: Context) =>
       |  val tagOpt = x.dataInputs(0).R5[Long]
       |  if (tagOpt.isDefined) {
       |    tagOpt.get
       |  } else {
       |    0L
       |  }
       |}""".stripMargin,
      FuncValue(
        Array((1, SContext)),
        BlockValue(
          Array(
            ValDef(
              3,
              List(),
              ExtractRegisterAs(
                ByIndex(
                  MethodCall.typed[Value[SCollection[SBox.type]]](
                    ValUse(1, SContext),
                    SContext.getMethodByName("dataInputs"),
                    Vector(),
                    Map()
                  ),
                  IntConstant(0),
                  None
                ),
                ErgoBox.R5,
                SOption(SLong)
              )
            )
          ),
          If(
            OptionIsDefined(ValUse(3, SOption(SLong))),
            OptionGet(ValUse(3, SOption(SLong))),
            LongConstant(0L)
          )
        )
      )
      ),
      preGeneratedSamples = Some(mutable.WrappedArray.empty))
  }

  property("Conditional access (data box register)") {
    val (_, _, _, ctx, _, _) = contextData()

    val expectedError = new IllegalArgumentException("assertion failed: Unexpected register type found at register #4")

    verifyCases(
      Seq(
        ctx -> Expected(expectedError)
      ),
      changedFeature(
        scalaFunc = { (x: Context) =>
          // this error is expected in v3.x, v4.x
          throw expectedError
        },
        scalaFuncNew = { (x: Context) =>
          // this is expected in v5.0, so the semantics of the script should correspond
          // to this Scala code
          val dataBox = x.dataInputs(0)
          val ok = if (x.OUTPUTS(0).R5[Long].get == 1L) {
            dataBox.R4[Long].get <= x.SELF.value
          } else {
            dataBox.R4[Coll[Byte]].get != x.SELF.propositionBytes
          }
          ok
        },
        s"""{ (x: Context) =>
          |  val dataBox = x.dataInputs(0)
          |  val ok = if (x.OUTPUTS(0).R5[Long].get == 1L) {
          |    dataBox.R4[Long].get <= x.SELF.value
          |  } else {
          |    dataBox.R4[Coll[Byte]].get != x.SELF.propositionBytes
          |  }
          |  ok
          |}
          |""".stripMargin,
        FuncValue(
          Array((1, SContext)),
          BlockValue(
            Array(
              ValDef(
                3,
                List(),
                ByIndex(
                  MethodCall.typed[Value[SCollection[SBox.type]]](
                    ValUse(1, SContext),
                    SContext.getMethodByName("dataInputs"),
                    Vector(),
                    Map()
                  ),
                  IntConstant(0),
                  None
                )
              )
            ),
            If(
              EQ(
                OptionGet(
                  ExtractRegisterAs(ByIndex(Outputs, IntConstant(0), None), ErgoBox.R5, SOption(SLong))
                ),
                LongConstant(1L)
              ),
              LE(
                OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SLong))),
                ExtractAmount(Self)
              ),
              NEQ(
                OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SByteArray))),
                ExtractScriptBytes(Self)
              )
            )
          )
        ),
        allowNewToSucceed = true
      ),
      preGeneratedSamples = Some(mutable.WrappedArray.empty))
  }

  property("Conditional access OUTPUTS(0).R4 using tag in R5") {
    val (_, _, _, ctx, _, _) = contextData()

    val registerTagCostDetails1 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(Outputs),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(OptionGet)
      )
    )
    val registerTagCostDetails2 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(Outputs),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(OptionGet),
        TypeBasedCostItem(Upcast, SLong)
      )
    )

    val registerTagCostDetails3 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(Outputs),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )
    verifyCases(
      Seq(
        ctx -> Expected(Success(5008366408131208436L), 40406, registerTagCostDetails1, 1811),
        ctxWithRegsInOutput(ctx, Map(
          ErgoBox.R5 -> LongConstant(0L),
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(10L), 40396, registerTagCostDetails2, 1810),
        ctxWithRegsInOutput(ctx, Map(
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(-1L), 40396, registerTagCostDetails3, 1797)
      ),
      existingFeature(
      { (x: Context) =>
        val tagOpt = x.OUTPUTS(0).R5[Long]
        val res = if (tagOpt.isDefined) {
          val tag = tagOpt.get
          if (tag == 0L) {
            val short = x.OUTPUTS(0).R4[Short].get  // access Short in the register
            short.toLong
          } else {
            if (tag == 1L) {
              val long = x.OUTPUTS(0).R4[Long].get    // access Long in the register
              long
            }
            else 0L
          }
        } else {
          -1L
        }
        res
      },
      """{
       |(x: Context) =>
       |  val tagOpt = x.OUTPUTS(0).R5[Long]
       |  val res = if (tagOpt.isDefined) {
       |    val tag = tagOpt.get
       |    if (tag == 0L) {
       |      val short = x.OUTPUTS(0).R4[Short].get  // access Short in the register
       |      short.toLong
       |    } else {
       |      if (tag == 1L) {
       |        val long = x.OUTPUTS(0).R4[Long].get    // access Long in the register
       |        long
       |      }
       |      else 0L
       |    }
       |  } else {
       |    -1L
       |  }
       |  res
       |}""".stripMargin,
      FuncValue(
        Array((1, SContext)),
        BlockValue(
          Array(
            ValDef(3, List(), ByIndex(Outputs, IntConstant(0), None)),
            ValDef(4, List(), ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R5, SOption(SLong)))
          ),
          If(
            OptionIsDefined(ValUse(4, SOption(SLong))),
            BlockValue(
              Array(ValDef(5, List(), OptionGet(ValUse(4, SOption(SLong))))),
              If(
                EQ(ValUse(5, SLong), LongConstant(0L)),
                Upcast(OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SShort))), SLong),
                If(
                  EQ(ValUse(5, SLong), LongConstant(1L)),
                  OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SLong))),
                  LongConstant(0L)
                )
              )
            ),
            LongConstant(-1L)
          )
        )
      )
      ),
      preGeneratedSamples = Some(mutable.WrappedArray.empty))
  }

  property("Conditional access OUTPUTS(0).R4 using tag in R5 (plus action)") {
    val (_, _, _, ctx, _, _) = contextData()
    val tagRegisterCostDetails1 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(Outputs),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(OptionGet),
        FixedCostItem(Self),
        FixedCostItem(ExtractAmount),
        TypeBasedCostItem(ArithOp.Plus, SLong)
      )
    )
    val tagRegisterCostDetails2 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(Outputs),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(OptionGet),
        TypeBasedCostItem(Upcast, SLong),
        FixedCostItem(Self),
        FixedCostItem(ExtractAmount),
        TypeBasedCostItem(ArithOp.Plus, SLong)
      )
    )
    val tagRegisterCostDetails3 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(Outputs),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )
    val tagRegisterCostDetails4 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(Outputs),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )
    verifyCases(
      Seq(
        // case 1L
        ctx -> Expected(Success(5008366408131289382L), 41016, tagRegisterCostDetails1, 1814),
        // case 0L
        ctxWithRegsInOutput(ctx, Map(
          ErgoBox.R5 -> LongConstant(0L),
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(80956L), 41006, tagRegisterCostDetails2, 1813),

        // case returning 0L
        ctxWithRegsInOutput(ctx, Map(
          ErgoBox.R5 -> LongConstant(2L),
          // note R4 is required to avoid
          // "RuntimeException: Set of non-mandatory indexes is not densely packed"
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(0L), 41006, tagRegisterCostDetails3, 1804),

        // case returning -1L
        ctxWithRegsInOutput(ctx, Map(
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(-1L), 41006, tagRegisterCostDetails4, 1797)
      ),
      existingFeature(
      { (x: Context) =>
        val tagOpt = x.OUTPUTS(0).R5[Long]
        val res = if (tagOpt.isDefined) {
          val tag = tagOpt.get
          if (tag == 0L) {
            val short = x.OUTPUTS(0).R4[Short].get  // access Short in the register
            short.toLong + x.SELF.value
          } else {
            if (tag == 1L) {
              val long = x.OUTPUTS(0).R4[Long].get    // access Long in the register
              long + x.SELF.value
            }
            else 0L
          }
        } else {
          -1L
        }
        res
      },
      """{
       |(x: Context) =>
       |  val tagOpt = x.OUTPUTS(0).R5[Long]
       |  val res = if (tagOpt.isDefined) {
       |    val tag = tagOpt.get
       |    if (tag == 0L) {
       |      val short = x.OUTPUTS(0).R4[Short].get  // access Short in the register
       |      short.toLong + x.SELF.value
       |    } else {
       |      if (tag == 1L) {
       |        val long = x.OUTPUTS(0).R4[Long].get    // access Long in the register
       |        long + x.SELF.value
       |      }
       |      else 0L
       |    }
       |  } else {
       |    -1L
       |  }
       |  res
       |}""".stripMargin,
      FuncValue(
        Array((1, SContext)),
        BlockValue(
          Array(
            ValDef(3, List(), ByIndex(Outputs, IntConstant(0), None)),
            ValDef(4, List(), ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R5, SOption(SLong)))
          ),
          If(
            OptionIsDefined(ValUse(4, SOption(SLong))),
            BlockValue(
              Array(ValDef(5, List(), OptionGet(ValUse(4, SOption(SLong))))),
              If(
                EQ(ValUse(5, SLong), LongConstant(0L)),
                ArithOp(
                  Upcast(
                    OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SShort))),
                    SLong
                  ),
                  ExtractAmount(Self),
                  OpCode @@ (-102.toByte)
                ),
                If(
                  EQ(ValUse(5, SLong), LongConstant(1L)),
                  ArithOp(
                    OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SLong))),
                    ExtractAmount(Self),
                    OpCode @@ (-102.toByte)
                  ),
                  LongConstant(0L)
                )
              )
            ),
            LongConstant(-1L)
          )
        )
      )
      ),
      preGeneratedSamples = Some(mutable.WrappedArray.empty))
  }

  property("Conditional access dataInputs(0).R4 using tag in R5") {
    val (_, _, _, ctx, _, _) = contextData()
    val tagRegisterCostDetails1 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(OptionGet)
      )
    )
    val tagRegisterCostDetails2 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(OptionGet),
        TypeBasedCostItem(Upcast, SLong)
      )
    )
    val tagRegisterCostDetails3 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )
    val tagRegisterCostDetails4 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )

    verifyCases(
      Seq(
        ctxWithRegsInDataInput(ctx, Map(
          ErgoBox.R5 -> LongConstant(1L),
          ErgoBox.R4 -> LongConstant(10))) -> Expected(Success(10L), 41084, tagRegisterCostDetails1, 1812),
        ctxWithRegsInDataInput(ctx, Map(
          ErgoBox.R5 -> LongConstant(0L),
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(10L), 41084, tagRegisterCostDetails2, 1811),
        ctxWithRegsInDataInput(ctx, Map(
          ErgoBox.R5 -> LongConstant(2L),
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(0L), 41084, tagRegisterCostDetails3, 1806),
        ctxWithRegsInDataInput(ctx, Map(
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(-1L), 41084, tagRegisterCostDetails4, 1799)
      ),
      existingFeature(
      { (x: Context) =>
        val tagOpt = x.dataInputs(0).R5[Long]
        val res = if (tagOpt.isDefined) {
          val tag = tagOpt.get
          if (tag == 0L) {
            val short = x.dataInputs(0).R4[Short].get  // access Short in the register
            short.toLong
          } else {
            if (tag == 1L) {
              val long = x.dataInputs(0).R4[Long].get    // access Long in the register
              long
            }
            else 0L
          }
        } else {
          -1L
        }
        res
      },
      """{
       |(x: Context) =>
       |  val tagOpt = x.dataInputs(0).R5[Long]
       |  val res = if (tagOpt.isDefined) {
       |    val tag = tagOpt.get
       |    if (tag == 0L) {
       |      val short = x.dataInputs(0).R4[Short].get  // access Short in the register
       |      short.toLong
       |    } else {
       |      if (tag == 1L) {
       |        val long = x.dataInputs(0).R4[Long].get    // access Long in the register
       |        long
       |      }
       |      else 0L
       |    }
       |  } else {
       |    -1L
       |  }
       |  res
       |}""".stripMargin,
      FuncValue(
        Array((1, SContext)),
        BlockValue(
          Array(
            ValDef(
              3,
              List(),
              ByIndex(
                MethodCall.typed[Value[SCollection[SBox.type]]](
                  ValUse(1, SContext),
                  SContext.getMethodByName("dataInputs"),
                  Vector(),
                  Map()
                ),
                IntConstant(0),
                None
              )
            ),
            ValDef(4, List(), ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R5, SOption(SLong)))
          ),
          If(
            OptionIsDefined(ValUse(4, SOption(SLong))),
            BlockValue(
              Array(ValDef(5, List(), OptionGet(ValUse(4, SOption(SLong))))),
              If(
                EQ(ValUse(5, SLong), LongConstant(0L)),
                Upcast(OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SShort))), SLong),
                If(
                  EQ(ValUse(5, SLong), LongConstant(1L)),
                  OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SLong))),
                  LongConstant(0L)
                )
              )
            ),
            LongConstant(-1L)
          )
        )
      )
      ),
      preGeneratedSamples = Some(mutable.WrappedArray.empty))
  }

  property("Conditional access dataInputs(0).R4 using tag in R5 (plus action)") {
    val (_, _, _, ctx, _, _) = contextData()
    val costDetails1 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(OptionGet),
        FixedCostItem(Self),
        FixedCostItem(ExtractAmount),
        TypeBasedCostItem(ArithOp.Plus, SLong)
      )
    )
    val costDetails2 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(OptionGet),
        TypeBasedCostItem(Upcast, SLong),
        FixedCostItem(Self),
        FixedCostItem(ExtractAmount),
        TypeBasedCostItem(ArithOp.Plus, SLong)
      )
    ) 
    val costDetails3 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )
    val costDetails4 = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.dataInputsMethod, FixedCost(JitCost(15))),
        FixedCostItem(Constant),
        FixedCostItem(ByIndex),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractRegisterAs),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(OptionIsDefined),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )
    verifyCases(
      Seq(
        ctxWithRegsInDataInput(ctx, Map(
          ErgoBox.R5 -> LongConstant(1L),
          ErgoBox.R4 -> LongConstant(10))) -> Expected(Success(80956L), 41694, costDetails1, 1816),
        ctxWithRegsInDataInput(ctx, Map(
          ErgoBox.R5 -> LongConstant(0L),
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(80956L), 41694, costDetails2, 1814),
        ctxWithRegsInDataInput(ctx, Map(
          ErgoBox.R5 -> LongConstant(2L),
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(0L), 41694, costDetails3, 1806),
        ctxWithRegsInDataInput(ctx, Map(
          ErgoBox.R4 -> ShortConstant(10))) -> Expected(Success(-1L), 41694, costDetails4, 1799)
      ),
      existingFeature(
      { (x: Context) =>
        val tagOpt = x.dataInputs(0).R5[Long]
        val res = if (tagOpt.isDefined) {
          val tag = tagOpt.get
          if (tag == 0L) {
            val short = x.dataInputs(0).R4[Short].get  // access Short in the register
            short.toLong + x.SELF.value
          } else {
            if (tag == 1L) {
              val long = x.dataInputs(0).R4[Long].get    // access Long in the register
              long + x.SELF.value
            }
            else 0L
          }
        } else {
          -1L
        }
        res
      },
      """{
       |(x: Context) =>
       |  val tagOpt = x.dataInputs(0).R5[Long]
       |  val res = if (tagOpt.isDefined) {
       |    val tag = tagOpt.get
       |    if (tag == 0L) {
       |      val short = x.dataInputs(0).R4[Short].get  // access Short in the register
       |      short.toLong + x.SELF.value
       |    } else {
       |      if (tag == 1L) {
       |        val long = x.dataInputs(0).R4[Long].get    // access Long in the register
       |        long + x.SELF.value
       |      }
       |      else 0L
       |    }
       |  } else {
       |    -1L
       |  }
       |  res
       |}""".stripMargin,
      FuncValue(
        Array((1, SContext)),
        BlockValue(
          Array(
            ValDef(
              3,
              List(),
              ByIndex(
                MethodCall.typed[Value[SCollection[SBox.type]]](
                  ValUse(1, SContext),
                  SContext.getMethodByName("dataInputs"),
                  Vector(),
                  Map()
                ),
                IntConstant(0),
                None
              )
            ),
            ValDef(4, List(), ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R5, SOption(SLong)))
          ),
          If(
            OptionIsDefined(ValUse(4, SOption(SLong))),
            BlockValue(
              Array(ValDef(5, List(), OptionGet(ValUse(4, SOption(SLong))))),
              If(
                EQ(ValUse(5, SLong), LongConstant(0L)),
                ArithOp(
                  Upcast(
                    OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SShort))),
                    SLong
                  ),
                  ExtractAmount(Self),
                  OpCode @@ (-102.toByte)
                ),
                If(
                  EQ(ValUse(5, SLong), LongConstant(1L)),
                  ArithOp(
                    OptionGet(ExtractRegisterAs(ValUse(3, SBox), ErgoBox.R4, SOption(SLong))),
                    ExtractAmount(Self),
                    OpCode @@ (-102.toByte)
                  ),
                  LongConstant(0L)
                )
              )
            ),
            LongConstant(-1L)
          )
        )
      )
      ),
      preGeneratedSamples = Some(mutable.WrappedArray.empty))

  }

  property("xorOf equivalence") {
    def costDetails(i: Int) = TracedCost(traceBase :+ SeqCostItem(CompanionDesc(XorOf), PerItemCost(JitCost(20), JitCost(5), 32), i))
    verifyCases(
      {
        def successNew[T](v: T, c: Int, newV: T, cd: CostDetails) = Expected(
          value = Success(v),
          cost = c,
          expectedDetails = CostDetails.ZeroCost,
          newCost = 1786,
          newVersionedResults = Seq(0, 1, 2).map(i => i -> (ExpectedResult(Success(newV), Some(1786)) -> Some(cd)))
        )
        Seq(
          (Coll[Boolean](), successNew(false, 37061, newV = false, costDetails(0))),
          (Coll[Boolean](false), successNew(false, 37071, newV = false, costDetails(1))),
          (Coll[Boolean](true), successNew(false, 37071, newV = true, costDetails(1))),
          (Coll[Boolean](false, false), successNew(false, 37081, newV = false, costDetails(2))),
          (Coll[Boolean](false, true), successNew(true, 37081, newV = true, costDetails(2))),
          (Coll[Boolean](true, false), successNew(true, 37081, newV = true, costDetails(2))),
          (Coll[Boolean](true, true), successNew(false, 37081, newV = false, costDetails(2))),
          (Coll[Boolean](false, false, false), successNew(false, 37091, newV = false, costDetails(3))),
          (Coll[Boolean](false, false, true), successNew(true, 37091, newV = true, costDetails(3))),
          (Coll[Boolean](false, true, false), successNew(true, 37091, newV = true, costDetails(3))),
          (Coll[Boolean](false, true, true), successNew(true, 37091, newV = false, costDetails(3))),
          (Coll[Boolean](true, false, false), successNew(true, 37091, newV = true, costDetails(3))),
          (Coll[Boolean](true, false, true), successNew(true, 37091, newV = false, costDetails(3))),
          (Coll[Boolean](true, true, false), successNew(true, 37091, newV = false, costDetails(3))),
          (Coll[Boolean](true, true, true), successNew(false, 37091, newV = true, costDetails(3))),
          (Coll[Boolean](false, false, false, false), successNew(false, 37101, newV = false, costDetails(4))),
          (Coll[Boolean](false, false, false, true), successNew(true, 37101, newV = true, costDetails(4))),
          (Coll[Boolean](false, false, true, false), successNew(true, 37101, newV = true, costDetails(4))),
          (Coll[Boolean](false, false, true, true), successNew(true, 37101, newV = false, costDetails(4)))
        )
      },
      changedFeature(
        (x: Coll[Boolean]) => SigmaDsl.xorOf(x),
        (x: Coll[Boolean]) => SigmaDsl.xorOf(x),
        "{ (x: Coll[Boolean]) => xorOf(x) }",
        FuncValue(Vector((1, SBooleanArray)), XorOf(ValUse(1, SBooleanArray)))))
  }

  property("LogicalNot equivalence") {
    val costDetails = TracedCost(traceBase :+ FixedCostItem(LogicalNot))
    verifyCases(
      Seq(
        (true, Expected(Success(false), 35864, costDetails, 1785)),
        (false, Expected(Success(true), 35864, costDetails, 1785))),
      existingFeature((x: Boolean) => !x,
        "{ (x: Boolean) => !x }",
        FuncValue(Vector((1, SBoolean)), LogicalNot(ValUse(1, SBoolean)))))
  }

  property("Numeric Negation equivalence") {
    val costDetails = TracedCost(traceBase :+ FixedCostItem(Negation))
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36136, costDetails, 1786)
        Seq(
          (Byte.MinValue, success(Byte.MinValue)), // !!!
          ((Byte.MinValue + 1).toByte, success(Byte.MaxValue)),
          (-40.toByte, success(40.toByte)),
          (-1.toByte, success(1.toByte)),
          (0.toByte, success(0.toByte)),
          (1.toByte, success(-1.toByte)),
          (45.toByte, success(-45.toByte)),
          (127.toByte, success(-127.toByte)),
          (Byte.MaxValue, success(-127.toByte))
        )
      },
      existingFeature((x: Byte) => (-x).toByte,
        "{ (x: Byte) => -x }",
        FuncValue(Vector((1, SByte)), Negation(ValUse(1, SByte)))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36136, costDetails, 1786)
        Seq(
          (Short.MinValue, success(Short.MinValue)), // special case!
          ((Short.MinValue + 1).toShort, success(Short.MaxValue)),
          (-1528.toShort, success(1528.toShort)),
          (-1.toShort, success(1.toShort)),
          (0.toShort, success(0.toShort)),
          (1.toShort, success(-1.toShort)),
          (7586.toShort, success(-7586.toShort)),
          (32767.toShort, success(-32767.toShort)),
          (Short.MaxValue, success(-32767.toShort)))
      },
      existingFeature((x: Short) => (-x).toShort,
        "{ (x: Short) => -x }",
        FuncValue(Vector((1, SShort)), Negation(ValUse(1, SShort)))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36136, costDetails, 1786)
        Seq(
          (Int.MinValue, success(Int.MinValue)),  // special case!
          (Int.MinValue + 1, success(Int.MaxValue)),
          (-63509744, success(63509744)),
          (-1, success(1)),
          (0, success(0)),
          (1, success(-1)),
          (677062351, success(-677062351)),
          (Int.MaxValue, success(-2147483647)))
      },
      existingFeature((x: Int) => -x,
        "{ (x: Int) => -x }",
        FuncValue(Vector((1, SInt)), Negation(ValUse(1, SInt)))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36136, costDetails, 1786)
        Seq(
          (Long.MinValue, success(Long.MinValue)),   // special case!
          (Long.MinValue + 1, success(Long.MaxValue)),
          (-957264171003115006L, success(957264171003115006L)),
          (-1L, success(1L)),
          (0L, success(0L)),
          (1L, success(-1L)),
          (340835904095777627L, success(-340835904095777627L)),
          (Long.MaxValue, success(-9223372036854775807L)))
      },
      existingFeature((x: Long) => -x,
        "{ (x: Long) => -x }",
        FuncValue(Vector((1, SLong)), Negation(ValUse(1, SLong)))))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36136, costDetails, 1787)
        Seq(
          (CBigInt(new BigInteger("-1655a05845a6ad363ac88ea21e88b97e436a1f02c548537e12e2d9667bf0680", 16)), success(CBigInt(new BigInteger("1655a05845a6ad363ac88ea21e88b97e436a1f02c548537e12e2d9667bf0680", 16)))),
          (CBigInt(new BigInteger("-1b24ba8badba8abf347cce054d9b9f14f229321507245b8", 16)), success(CBigInt(new BigInteger("1b24ba8badba8abf347cce054d9b9f14f229321507245b8", 16)))),
          (CBigInt(new BigInteger("-1ec9cca2c346cb72a1e65481eaa0627d", 16)), success(CBigInt(new BigInteger("1ec9cca2c346cb72a1e65481eaa0627d", 16)))),
          (CBigInt(new BigInteger("-8000000000000001", 16)), success(CBigInt(new BigInteger("8000000000000001", 16)))),
          (CBigInt(new BigInteger("-8000000000000000", 16)), success(CBigInt(new BigInteger("8000000000000000", 16)))),
          (CBigInt(new BigInteger("-48afe3e059821cd6", 16)), success(CBigInt(new BigInteger("48afe3e059821cd6", 16)))),
          (CBigInt(new BigInteger("-80000001", 16)), success(CBigInt(new BigInteger("80000001", 16)))),
          (CBigInt(new BigInteger("-80000000", 16)), success(CBigInt(new BigInteger("80000000", 16)))),
          (CBigInt(new BigInteger("-1", 16)), success(CBigInt(new BigInteger("1", 16)))),
          (CBigInt(new BigInteger("0", 16)), success(CBigInt(new BigInteger("0", 16)))),
          (CBigInt(new BigInteger("1", 16)), success(CBigInt(new BigInteger("-1", 16)))),
          (CBigInt(new BigInteger("7fffffff", 16)), success(CBigInt(new BigInteger("-7fffffff", 16)))),
          (CBigInt(new BigInteger("80000000", 16)), success(CBigInt(new BigInteger("-80000000", 16)))),
          (CBigInt(new BigInteger("90e8c3b6e8df65c", 16)), success(CBigInt(new BigInteger("-90e8c3b6e8df65c", 16)))),
          (CBigInt(new BigInteger("36aa93288257dcca141d0c01c5cef14c9d1c0f8507872e3fdd839a759636c78", 16)), success(CBigInt(new BigInteger("-36aa93288257dcca141d0c01c5cef14c9d1c0f8507872e3fdd839a759636c78", 16)))))
      },
      existingFeature((x: BigInt) => x.negate(),
        "{ (x: BigInt) => -x }",
        FuncValue(Vector((1, SBigInt)), Negation(ValUse(1, SBigInt)))))
  }

  property("groupGenerator equivalence") {
    val costDetails = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(Global),
        FixedCostItem(PropertyCall),
        FixedCostItem(SGlobal.groupGeneratorMethod, FixedCost(JitCost(10)))
      )
    )
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35981, costDetails, 1802)
        Seq(
          (-1, success(Helpers.decodeGroupElement("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))),
          (1, success(Helpers.decodeGroupElement("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))))
      },
      existingFeature({ (x: Int) => SigmaDsl.groupGenerator },
      "{ (x: Int) => groupGenerator }",
      FuncValue(
        Vector((1, SInt)),
        MethodCall.typed[Value[SGroupElement.type]](
          Global,
          SGlobal.getMethodByName("groupGenerator"),
          Vector(),
          Map()
        )
      )))

    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35981, costDetails, 1802)
        Seq(
          (-1, success(Helpers.decodeGroupElement("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))),
          (1, success(Helpers.decodeGroupElement("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))))
      },
      existingFeature({ (x: Int) => SigmaDsl.groupGenerator },
        "{ (x: Int) => Global.groupGenerator }",
        FuncValue(
          Vector((1, SInt)),
          MethodCall.typed[Value[SGroupElement.type]](
            Global,
            SGlobal.getMethodByName("groupGenerator"),
            Vector(),
            Map()
          )
        )))

    if (lowerMethodCallsInTests) {
      val expCostDetails = TracedCost(
        costDetails.trace ++ Array(
          FixedCostItem(ValUse),
          FixedCostItem(Exponentiate)
        )
      )
      verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 41237, expCostDetails, 1892)
        Seq(
          (CBigInt(new BigInteger("-e5c1a54694c85d644fa30a6fc5f3aa209ed304d57f72683a0ebf21038b6a9d", 16)), success(Helpers.decodeGroupElement("023395bcba3d7cf21d73c50f8af79d09a8c404c15ce9d04f067d672823bae91a54"))),
          (CBigInt(new BigInteger("-bc2d08f935259e0eebf272c66c6e1dbd484c6706390215", 16)), success(Helpers.decodeGroupElement("02ddcf4c48105faf3c16f7399b5dbedd82ab0bb50ae292d8f88f49a3f86e78974e"))),
          (CBigInt(new BigInteger("-35cbe9a7a652e5fe85f735ee9909fdd8", 16)), success(Helpers.decodeGroupElement("03b110ec9c7a8c20ed873818e976a0e96e5a17be979d3422d59b362de2a3ae043e"))),
          (CBigInt(new BigInteger("-3f05ffca6bd4b15c", 16)), success(Helpers.decodeGroupElement("02acf2657d0714cef8d65ae15c362faa09c0934c0bce872a23398e564c090b85c8"))),
          (CBigInt(new BigInteger("-80000001", 16)), success(Helpers.decodeGroupElement("0391b418fd1778356ce947a5cbb46539fd29842aea168486fae91fc5317177a575"))),
          (CBigInt(new BigInteger("-80000000", 16)), success(Helpers.decodeGroupElement("025318f9b1a2697010c5ac235e9af475a8c7e5419f33d47b18d33feeb329eb99a4"))),
          (CBigInt(new BigInteger("-1", 16)), success(Helpers.decodeGroupElement("0379be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))),
          (CBigInt(new BigInteger("0", 16)), success(Helpers.decodeGroupElement("000000000000000000000000000000000000000000000000000000000000000000"))),
          (CBigInt(new BigInteger("1", 16)), success(Helpers.decodeGroupElement("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"))),
          (CBigInt(new BigInteger("80000000", 16)), success(Helpers.decodeGroupElement("035318f9b1a2697010c5ac235e9af475a8c7e5419f33d47b18d33feeb329eb99a4"))),
          (CBigInt(new BigInteger("1251b7fcd8a01e95", 16)), success(Helpers.decodeGroupElement("030fde7238b8dddfafab8f5481dc17b880505d6bacbe3cdf2ce975afdcadf66354"))),
          (CBigInt(new BigInteger("12f6bd76d8fe1d035bdb14bf2f696e52", 16)), success(Helpers.decodeGroupElement("028f2ccf13669461cb3cfbea281e2db08fbb67b38493a1628855203d3f69b82763"))),
          (CBigInt(new BigInteger("102bb404f5e36bdba004fdefa34df8cfa02e7912f3caf79", 16)), success(Helpers.decodeGroupElement("03ce82f431d115d45ad555084f8b2861ce5c4561d154e931e9f778594896e46a25"))))
      },
      existingFeature({ (n: BigInt) => SigmaDsl.groupGenerator.exp(n) },
      "{ (n: BigInt) => groupGenerator.exp(n) }",
      FuncValue(
        Vector((1, SBigInt)),
        Exponentiate(
          MethodCall.typed[Value[SGroupElement.type]](
            Global,
            SGlobal.getMethodByName("groupGenerator"),
            Vector(),
            Map()
          ),
          ValUse(1, SBigInt)
        )
      )))
    }
  }

  property("Global.xor equivalence") {
    if (lowerMethodCallsInTests) {
      val costDetails = TracedCost(
        traceBase ++ Array(
          FixedCostItem(SelectField),
          FixedCostItem(ValUse),
          FixedCostItem(SelectField),
          SeqCostItem(CompanionDesc(Xor), PerItemCost(JitCost(10), JitCost(2), 128), 1)
        )
      )
      verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36903)
        Seq(
          ((Helpers.decodeBytes(""), Helpers.decodeBytes("")), success(Helpers.decodeBytes(""))),
          ((Helpers.decodeBytes("01"), Helpers.decodeBytes("01")), success(Helpers.decodeBytes("00"))),
          ((Helpers.decodeBytes("0100"), Helpers.decodeBytes("0101")), success(Helpers.decodeBytes("0001"))),
          ((Helpers.decodeBytes("01"), Helpers.decodeBytes("0101")), success(Helpers.decodeBytes("00"))),
          ((Helpers.decodeBytes("0100"), Helpers.decodeBytes("01")) ->
            Expected(Failure(new ArrayIndexOutOfBoundsException("1")),
              cost = 0,
              expectedDetails = CostDetails.ZeroCost,
              newCost = 1789,
              newVersionedResults =  {
                val res = (ExpectedResult(Success(Helpers.decodeBytes("00")), Some(1789)), Some(costDetails))
                Seq(0, 1, 2).map(version => version -> res)
              }
            )),
          ((Helpers.decodeBytes("800136fe89afff802acea67128a0ff007fffe3498c8001806080012b"),
              Helpers.decodeBytes("648018010a5d5800f5b400a580e7b4809b0cd273ff1230bfa800017f7fdb002749b3ac2b86ff")),
              success(Helpers.decodeBytes("e4812eff83f2a780df7aa6d4a8474b80e4f3313a7392313fc8800054")))
        )
      },
      changedFeature(
        (x: (Coll[Byte], Coll[Byte])) => SigmaDsl.xor(x._1, x._2),
        (x: (Coll[Byte], Coll[Byte])) => SigmaDsl.xor(x._1, x._2),
        "{ (x: (Coll[Byte], Coll[Byte])) => xor(x._1, x._2) }",
        FuncValue(
          Vector((1, STuple(Vector(SByteArray, SByteArray)))),
          Xor(
            SelectField.typed[Value[SCollection[SByte.type]]](
              ValUse(1, STuple(Vector(SByteArray, SByteArray))),
              1.toByte
            ),
            SelectField.typed[Value[SCollection[SByte.type]]](
              ValUse(1, STuple(Vector(SByteArray, SByteArray))),
              2.toByte
            )
          )
        )))
    } else {
      val cases = Seq(
        ((Helpers.decodeBytes("01"), Helpers.decodeBytes("01")) ->
          Expected(
            // in v4.x exp method cannot be called via MethodCall ErgoTree node
//            Failure(new NoSuchMethodException("")),
            Success(Helpers.decodeBytes("00")),
            cost = 41484,
            CostDetails.ZeroCost,
            newCost = 0,
            newVersionedResults = {
              // in v5.0 MethodCall ErgoTree node is allowed
              val res = ExpectedResult(
                Success(Helpers.decodeBytes("00")),
                Some(116)
              )
              Seq( // expected result for each version
                0 -> ( res -> None ),
                1 -> ( res -> None ),
                2 -> ( res -> None )
              )
            }
          )
        )
      )
      forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
        testCases(
          cases,
          changedFeature(
            (x: (Coll[Byte], Coll[Byte])) => SigmaDsl.xor(x._1, x._2),
            (x: (Coll[Byte], Coll[Byte])) => SigmaDsl.xor(x._1, x._2),
            "", // NOTE, the script for this test case cannot be compiled, thus expectedExpr is used
            expectedExpr = FuncValue(
              Vector((1, STuple(Vector(SByteArray, SByteArray)))),
              MethodCall(
                Global,
                SGlobal.getMethodByName("xor"),
                Vector(
                  SelectField.typed[Value[SCollection[SByte.type]]](
                    ValUse(1, STuple(Vector(SByteArray, SByteArray))),
                    1.toByte
                  ),
                  SelectField.typed[Value[SCollection[SByte.type]]](
                    ValUse(1, STuple(Vector(SByteArray, SByteArray))),
                    2.toByte
                  )),
                Map()
              )
            ),
            allowNewToSucceed = true,
            allowDifferentErrors = false
          ))
      }
    }
  }

  def sampleCollBoxes = genSamples[Coll[Box]](collOfN[Box](5), MinSuccessful(20))

  def create_b1 = CostingBox(
    false,
    new ErgoBox(
      1L,
      new ErgoTree(
        0.toByte,
        Vector(),
        Right(
          SigmaPropConstant(
            CSigmaProp(
              ProveDHTuple(
                Helpers.decodeECPoint("02c1a9311ecf1e76c787ba4b1c0e10157b4f6d1e4db3ef0d84f411c99f2d4d2c5b"),
                Helpers.decodeECPoint("027d1bd9a437e73726ceddecc162e5c85f79aee4798505bc826b8ad1813148e419"),
                Helpers.decodeECPoint("0257cff6d06fe15d1004596eeb97a7f67755188501e36adc49bd807fe65e9d8281"),
                Helpers.decodeECPoint("033c6021cff6ba5fdfc4f1742486030d2ebbffd9c9c09e488792f3102b2dcdabd5")
              )
            )
          )
        )
      ),
      Coll(),
      Map(
        ErgoBox.R4 -> ByteArrayConstant(
          Helpers.decodeBytes(
            "7200004cccdac3008001bc80ffc7ff9633bca3e501801380ff007900019d7f0001a8c9dfff5600d964011617ca00583f989c7f80007fee7f99b07f7f870067dc315180828080307fbdf400"
          )
        ),
        ErgoBox.R7 -> LongConstant(0L),
        ErgoBox.R6 -> FalseLeaf,
        ErgoBox.R5 -> ByteArrayConstant(Helpers.decodeBytes("7f"))
      ),
      ModifierId @@ ("7dffff48ab0000c101a2eac9ff17017f6180aa7fc6f2178000800179499380a5"),
      21591.toShort,
      638768
    )
  )

  def create_b2 = CostingBox(
    false,
    new ErgoBox(
      1000000000L,
      new ErgoTree(
        0.toByte,
        Vector(),
        Right(BoolToSigmaProp(OR(ConcreteCollection(Array(FalseLeaf, AND(ConcreteCollection(Array(FalseLeaf, FalseLeaf), SBoolean))), SBoolean))))
      ),
      Coll(),
      Map(),
      ModifierId @@ ("008677ffff7ff36dff00f68031140400007689ff014c9201ce8000a9ffe6ceff"),
      32767.toShort,
      32827
    )
  )

  property("Coll.filter equivalence") {
    val samples = sampleCollBoxes
    val b1 = create_b1
    val b2 = create_b2

    val costDetails = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Filter), PerItemCost(JitCost(20), JitCost(1), 10), 0)
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Filter), PerItemCost(JitCost(20), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Filter), PerItemCost(JitCost(20), JitCost(1), 10), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong)
      )
    )
    verifyCases(
      {
        def success[T](v: T, c: Int, costDetails: CostDetails, newCost: Int) = Expected(Success(v), c, costDetails, newCost)
        Seq(
          (Coll[Box](), success(Coll[Box](), 37297, costDetails, 1787)),
          (Coll[Box](b1), success(Coll[Box](), 37397, costDetails2, 1792)),
          (Coll[Box](b1, b2), success(Coll[Box](b2), 37537, costDetails3, 1796))
        )
      },
      existingFeature({ (x: Coll[Box]) => x.filter({ (b: Box) => b.value > 1 }) },
      "{ (x: Coll[Box]) => x.filter({(b: Box) => b.value > 1 }) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        Filter(
          ValUse(1, SCollectionType(SBox)),
          FuncValue(Vector((3, SBox)), GT(ExtractAmount(ValUse(3, SBox)), LongConstant(1L)))
        )
      )),
      preGeneratedSamples = Some(samples))
  }

  property("Coll.flatMap equivalence") {
    val samples = sampleCollBoxes
    val b1 = create_b1
    val b2 = create_b2

    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        SeqCostItem(MethodDesc(SCollection.FlatMapMethod), PerItemCost(JitCost(60), JitCost(10), 8), 0)
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(NamedDesc("MatchSingleArgMethodCall"), FixedCost(JitCost(30))),
        SeqCostItem(NamedDesc("CheckFlatmapBody"), PerItemCost(JitCost(20), JitCost(20), 1), 2),
        SeqCostItem(MethodDesc(SCollection.FlatMapMethod), PerItemCost(JitCost(60), JitCost(10), 8), 0)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(NamedDesc("MatchSingleArgMethodCall"), FixedCost(JitCost(30))),
        SeqCostItem(NamedDesc("CheckFlatmapBody"), PerItemCost(JitCost(20), JitCost(20), 1), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractScriptBytes),
        SeqCostItem(MethodDesc(SCollection.FlatMapMethod), PerItemCost(JitCost(60), JitCost(10), 8), 135)
      )
    )
    // TODO mainnet v5.0: Failing test with cyclic results
    verifyCases(
      {
        def success[T](v: T, c: Int) = Expected(Success(v), c)
        Seq(
          (Coll[Box](), success(Coll[Byte](), 38126)),
          (Coll[Box](b1), success(Helpers.decodeBytes(
            "0008ce02c1a9311ecf1e76c787ba4b1c0e10157b4f6d1e4db3ef0d84f411c99f2d4d2c5b027d1bd9a437e73726ceddecc162e5c85f79aee4798505bc826b8ad1813148e4190257cff6d06fe15d1004596eeb97a7f67755188501e36adc49bd807fe65e9d8281033c6021cff6ba5fdfc4f1742486030d2ebbffd9c9c09e488792f3102b2dcdabd5"
          ), 38206)),
          (Coll[Box](b1, b2), success(Helpers.decodeBytes(
            "0008ce02c1a9311ecf1e76c787ba4b1c0e10157b4f6d1e4db3ef0d84f411c99f2d4d2c5b027d1bd9a437e73726ceddecc162e5c85f79aee4798505bc826b8ad1813148e4190257cff6d06fe15d1004596eeb97a7f67755188501e36adc49bd807fe65e9d8281033c6021cff6ba5fdfc4f1742486030d2ebbffd9c9c09e488792f3102b2dcdabd500d197830201010096850200"
          ), 38286))
        )
      },
      existingFeature({ (x: Coll[Box]) => x.flatMap({ (b: Box) => b.propositionBytes }) },
      "{ (x: Coll[Box]) => x.flatMap({(b: Box) => b.propositionBytes }) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        MethodCall.typed[Value[SCollection[SByte.type]]](
          ValUse(1, SCollectionType(SBox)),
          SCollection.getMethodByName("flatMap").withConcreteTypes(
            Map(STypeVar("IV") -> SBox, STypeVar("OV") -> SByte)
          ),
          Vector(FuncValue(Vector((3, SBox)), ExtractScriptBytes(ValUse(3, SBox)))),
          Map()
        )
      )),
      preGeneratedSamples = Some(samples))
  }

  property("Coll.zip equivalence") {
    val samples = sampleCollBoxes
    val b1 = create_b1
    val b2 = create_b2
    def costDetails(zipElements: Int) = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(ValUse),
        SeqCostItem(MethodDesc(SCollection.ZipMethod), PerItemCost(JitCost(10), JitCost(1), 10), zipElements)
      )
    )

    verifyCases(
      {
        def success[T](v: T, c: Int, cd: CostDetails, nc: Int) = Expected(Success(v), c, cd, nc)
        Seq(
          (Coll[Box](), success(Coll[(Box, Box)](), 37399, costDetails(0), 1786)),
          (Coll[Box](b1), success(Coll[(Box, Box)]((b1, b1)), 37559, costDetails(1), 1788)),
          (Coll[Box](b1, b2), success(Coll[(Box, Box)]((b1, b1), (b2, b2)), 37719, costDetails(2), 1790))
        )
      },
      existingFeature({ (x: Coll[Box]) => x.zip(x) },
      "{ (x: Coll[Box]) => x.zip(x) }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        MethodCall.typed[Value[SCollection[STuple]]](
          ValUse(1, SCollectionType(SBox)),
          SCollection.getMethodByName("zip").withConcreteTypes(
            Map(STypeVar("IV") -> SBox, STypeVar("OV") -> SBox)
          ),
          Vector(ValUse(1, SCollectionType(SBox))),
          Map()
        )
      )),
      preGeneratedSamples = Some(samples))
  }

  property("Coll.size equivalence") {
    val samples = sampleCollBoxes
    val b1 = create_b1
    val b2 = create_b2
    val costDetails = TracedCost(traceBase :+ FixedCostItem(SizeOf))
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35954, costDetails, 1785)
        Seq(
          (Coll[Box](), success(0)),
          (Coll[Box](b1), success(1)),
          (Coll[Box](b1, b2), success(2))
        )
      },
      existingFeature({ (x: Coll[Box]) => x.size },
      "{ (x: Coll[Box]) => x.size }",
      FuncValue(Vector((1, SCollectionType(SBox))), SizeOf(ValUse(1, SCollectionType(SBox))))),
      preGeneratedSamples = Some(samples))
  }

  property("Coll.indices equivalence") {
    val samples = sampleCollBoxes
    val b1 = create_b1
    val b2 = create_b2
    def costDetails(indicesCount: Int) = TracedCost(
      traceBase ++ Array(
        FixedCostItem(PropertyCall),
        SeqCostItem(MethodDesc(SCollection.IndicesMethod), PerItemCost(JitCost(20), JitCost(2), 16), indicesCount)
      )
    )
    verifyCases(
      {
        def success[T](v: T, i: Int) = Expected(Success(v), 36036, costDetails(i), 1788)
        Seq(
          (Coll[Box](), success(Coll[Int](), 0)),
          (Coll[Box](b1), success(Coll[Int](0), 1)),
          (Coll[Box](b1, b2), success(Coll[Int](0, 1), 2))
        )
      },
      existingFeature({ (x: Coll[Box]) => x.indices },
      "{ (x: Coll[Box]) => x.indices }",
      FuncValue(
        Vector((1, SCollectionType(SBox))),
        MethodCall.typed[Value[SCollection[SInt.type]]](
          ValUse(1, SCollectionType(SBox)),
          SCollection.getMethodByName("indices").withConcreteTypes(Map(STypeVar("IV") -> SBox)),
          Vector(),
          Map()
        )
      )),
      preGeneratedSamples = Some(samples))

  }

  property("Coll.forall equivalence") {
    val samples = sampleCollBoxes
    val b1 = create_b1
    val b2 = create_b2
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(ForAll), PerItemCost(JitCost(3), JitCost(1), 10), 0)
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(ForAll), PerItemCost(JitCost(3), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(ForAll), PerItemCost(JitCost(3), JitCost(1), 10), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong)
      )
    )

    if (lowerMethodCallsInTests) {
      verifyCases(
        {
          Seq(
            (Coll[Box](), Expected(Success(true), 37909, costDetails1, 1784)),
            (Coll[Box](b1), Expected(Success(false), 37969, costDetails2, 1789)),
            (Coll[Box](b1, b2), Expected(Success(false), 38029, costDetails3, 1789))
          )
        },
        existingFeature({ (x: Coll[Box]) => x.forall({ (b: Box) => b.value > 1 }) },
        "{ (x: Coll[Box]) => x.forall({(b: Box) => b.value > 1 }) }",
        FuncValue(
          Vector((1, SCollectionType(SBox))),
          ForAll(
            ValUse(1, SCollectionType(SBox)),
            FuncValue(Vector((3, SBox)), GT(ExtractAmount(ValUse(3, SBox)), LongConstant(1L)))
          )
        )),
        preGeneratedSamples = Some(samples))
    } else {
      assertExceptionThrown(
        verifyCases(
          Seq( (Coll[Box](), Expected(Success(true), 37909)) ),
          existingFeature(
            { (x: Coll[Box]) => x.forall({ (b: Box) => b.value > 1 }) },
            "{ (x: Coll[Box]) => x.forall({(b: Box) => b.value > 1 }) }"
          )),
        rootCauseLike[NoSuchMethodException]("sigmastate.eval.CostingRules$CollCoster.forall(scalan.Base$Ref)")
      )
    }
  }

  property("Coll.exists equivalence") {
    val samples = sampleCollBoxes
    val b1 = create_b1
    val b2 = create_b2
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Exists), PerItemCost(JitCost(3), JitCost(1), 10), 0)
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Exists), PerItemCost(JitCost(3), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Exists), PerItemCost(JitCost(3), JitCost(1), 10), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(ExtractAmount),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong)
      )
    )

    if (lowerMethodCallsInTests) {
      verifyCases(
        {
          Seq(
            (Coll[Box](), Expected(Success(false), 38455, costDetails1, 1784)),
            (Coll[Box](b1), Expected(Success(false), 38515, costDetails2, 1789)),
            (Coll[Box](b1, b2), Expected(Success(true), 38575, costDetails3, 1793))
          )
        },
        existingFeature({ (x: Coll[Box]) => x.exists({ (b: Box) => b.value > 1 }) },
        "{ (x: Coll[Box]) => x.exists({(b: Box) => b.value > 1 }) }",
        FuncValue(
          Vector((1, SCollectionType(SBox))),
          Exists(
            ValUse(1, SCollectionType(SBox)),
            FuncValue(Vector((3, SBox)), GT(ExtractAmount(ValUse(3, SBox)), LongConstant(1L)))
          )
        )),
        preGeneratedSamples = Some(samples))
    } else {
      assertExceptionThrown(
        verifyCases(
          Seq( (Coll[Box](), Expected(Success(false), 38455)) ),
          existingFeature(
            { (x: Coll[Box]) => x.exists({ (b: Box) => b.value > 1 }) },
            "{ (x: Coll[Box]) => x.exists({(b: Box) => b.value > 1 }) }"
          )),
        rootCauseLike[NoSuchMethodException]("sigmastate.eval.CostingRules$CollCoster.exists(scalan.Base$Ref)")
      )
    }
  }

  property("Coll exists with nested If") {
    val o = NumericOps.BigIntIsExactOrdering
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Exists), PerItemCost(JitCost(3), JitCost(1), 10), 0)
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Exists), PerItemCost(JitCost(3), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SBigInt),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Exists), PerItemCost(JitCost(3), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SBigInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(LT, SBigInt)
      )
    )
    val costDetails4 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Exists), PerItemCost(JitCost(3), JitCost(1), 10), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SBigInt),
        FixedCostItem(If),
        FixedCostItem(Constant),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SBigInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(LT, SBigInt)  
      )
    )
    
    if (lowerMethodCallsInTests) {
      verifyCases(
        {
          Seq(
            (Coll[BigInt](), Expected(Success(false), 38955, costDetails1, 1784)),
            (Coll[BigInt](BigIntZero), Expected(Success(false), 39045, costDetails2, 1789)),
            (Coll[BigInt](BigIntOne), Expected(Success(true), 39045, costDetails3, 1792)),
            (Coll[BigInt](BigIntZero, BigIntOne), Expected(Success(true), 39135, costDetails4, 1797)),
            (Coll[BigInt](BigIntZero, BigInt10), Expected(Success(false), 39135, costDetails4, 1797))
          )
        },
        existingFeature(
          { (x: Coll[BigInt]) => x.exists({ (b: BigInt) =>
              if (o.gt(b, BigIntZero)) o.lt(b, BigInt10) else false
            })
          },
          "{ (x: Coll[BigInt]) => x.exists({(b: BigInt) => if (b > 0) b < 10 else false }) }",
          FuncValue(
            Array((1, SCollectionType(SBigInt))),
            Exists(
              ValUse(1, SCollectionType(SBigInt)),
              FuncValue(
                Array((3, SBigInt)),
                If(
                  GT(ValUse(3, SBigInt), BigIntConstant(CBigInt(new BigInteger("0", 16)))),
                  LT(ValUse(3, SBigInt), BigIntConstant(CBigInt(new BigInteger("a", 16)))),
                  FalseLeaf
                )
              )
            )
          )))
    } else {
      assertExceptionThrown(
        verifyCases(
          Seq( (Coll[BigInt](), Expected(Success(false), 38955)) ),
          existingFeature(
            { (x: Coll[BigInt]) => x.exists({ (b: BigInt) =>
              if (o.gt(b, BigIntZero)) o.lt(b, BigInt10) else false
            })
            },
            "{ (x: Coll[BigInt]) => x.exists({(b: BigInt) => if (b > 0) b < 10 else false }) }"
          )),
        rootCauseLike[NoSuchMethodException]("sigmastate.eval.CostingRules$CollCoster.exists(scalan.Base$Ref)")
      )
    }
  }

  property("Coll forall with nested If") {
    val o = NumericOps.BigIntIsExactOrdering
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(ForAll), PerItemCost(JitCost(3), JitCost(1), 10), 0)
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(ForAll), PerItemCost(JitCost(3), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GE, SBigInt),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(ForAll), PerItemCost(JitCost(3), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GE, SBigInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(LE, SBigInt)
      )
    )
    val costDetails4 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(ForAll), PerItemCost(JitCost(3), JitCost(1), 10), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GE, SBigInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(LE, SBigInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GE, SBigInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(LE, SBigInt)  
      )
    )
    if (lowerMethodCallsInTests) {
      verifyCases(
        {
          Seq(
            (Coll[BigInt](), Expected(Success(true), 38412, costDetails1, 1784)),
            (Coll[BigInt](BigIntMinusOne), Expected(Success(false), 38502, costDetails2, 1789)),
            (Coll[BigInt](BigIntOne), Expected(Success(true), 38502, costDetails3, 1792)),
            (Coll[BigInt](BigIntZero, BigIntOne), Expected(Success(true), 38592, costDetails4, 1799)),
            (Coll[BigInt](BigIntZero, BigInt11), Expected(Success(false), 38592, costDetails4, 1799))
          )
        },
        existingFeature(
          { (x: Coll[BigInt]) => x.forall({ (b: BigInt) =>
            if (o.gteq(b, BigIntZero)) o.lteq(b, BigInt10) else false
          })
          },
          "{ (x: Coll[BigInt]) => x.forall({(b: BigInt) => if (b >= 0) b <= 10 else false }) }",
        FuncValue(
          Array((1, SCollectionType(SBigInt))),
          ForAll(
            ValUse(1, SCollectionType(SBigInt)),
            FuncValue(
              Array((3, SBigInt)),
              If(
                GE(ValUse(3, SBigInt), BigIntConstant(CBigInt(new BigInteger("0", 16)))),
                LE(ValUse(3, SBigInt), BigIntConstant(CBigInt(new BigInteger("a", 16)))),
                FalseLeaf
              )
            )
          )
        )))
    } else {
      assertExceptionThrown(
        verifyCases(
          Seq( (Coll[BigInt](), Expected(Success(true), 38412)) ),
          existingFeature(
            { (x: Coll[BigInt]) => x.forall({ (b: BigInt) =>
              if (o.gteq(b, BigIntZero)) o.lteq(b, BigInt10) else false
            })
            },
            "{ (x: Coll[BigInt]) => x.forall({(b: BigInt) => if (b >= 0) b <= 10 else false }) }"
          )),
        rootCauseLike[NoSuchMethodException]("sigmastate.eval.CostingRules$CollCoster.forall(scalan.Base$Ref)")
      )
    }

  }

  val collWithRangeGen = for {
    arr <- collGen[Int]
    l <- Gen.choose(0, arr.length - 1)
    r <- Gen.choose(l, arr.length - 1) } yield (arr, (l, r))

  property("Coll flatMap method equivalence") {
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(NamedDesc("MatchSingleArgMethodCall"), FixedCost(JitCost(30))),
        SeqCostItem(NamedDesc("CheckFlatmapBody"), PerItemCost(JitCost(20), JitCost(20), 1), 1),
        SeqCostItem(MethodDesc(SCollection.FlatMapMethod), PerItemCost(JitCost(60), JitCost(10), 8), 0)
      )
    )
    val costDetails1Copy = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        SeqCostItem(MethodDesc(SCollection.FlatMapMethod), PerItemCost(JitCost(60), JitCost(10), 8), 0)
      )
    )
    verifyCases(
      {
        def success[T](v: T, c: Int) = Expected(Success(v), c)
        Seq(
          // TODO mainnet v5.0: different results - costDetails1(Copy) and cost 1793/1800 (problematic version 2/2 ?)
          // Coll[GroupElement]() -> Expected(Success(Coll[Byte]()), 40133, CostDetails.ZeroCost, 1793),
          Coll[GroupElement](
            Helpers.decodeGroupElement("02d65904820f8330218cf7318b3810d0c9ab9df86f1ee6100882683f23c0aee587"),
            Helpers.decodeGroupElement("0390e9daa9916f30d0bc61a8e381c6005edfb7938aee5bb4fc9e8a759c7748ffaa")) ->
              success(Helpers.decodeBytes(
                "02d65904820f8330218cf7318b3810d0c9ab9df86f1ee6100882683f23c0aee5870390e9daa9916f30d0bc61a8e381c6005edfb7938aee5bb4fc9e8a759c7748ffaa"
              ), 40213),
          Coll[GroupElement](
            Helpers.decodeGroupElement("02d65904820f8330218cf7318b3810d0c9ab9df86f1ee6100882683f23c0aee587"),
            Helpers.decodeGroupElement("0390e9daa9916f30d0bc61a8e381c6005edfb7938aee5bb4fc9e8a759c7748ffaa"),
            Helpers.decodeGroupElement("03bd839b969b02d218fd1192f2c80cbda9c6ce9c7ddb765f31b748f4666203df85")) ->
              success(Helpers.decodeBytes(
                "02d65904820f8330218cf7318b3810d0c9ab9df86f1ee6100882683f23c0aee5870390e9daa9916f30d0bc61a8e381c6005edfb7938aee5bb4fc9e8a759c7748ffaa03bd839b969b02d218fd1192f2c80cbda9c6ce9c7ddb765f31b748f4666203df85"
              ), 40253)
        )
      },
      existingFeature(
        { (x: Coll[GroupElement]) => x.flatMap({ (b: GroupElement) => b.getEncoded }) },
        "{ (x: Coll[GroupElement]) => x.flatMap({ (b: GroupElement) => b.getEncoded }) }",
        FuncValue(
          Vector((1, SCollectionType(SGroupElement))),
          MethodCall.typed[Value[SCollection[SByte.type]]](
            ValUse(1, SCollectionType(SGroupElement)),
            SCollection.getMethodByName("flatMap").withConcreteTypes(
              Map(STypeVar("IV") -> SGroupElement, STypeVar("OV") -> SByte)
            ),
            Vector(
              FuncValue(
                Vector((3, SGroupElement)),
                MethodCall.typed[Value[SCollection[SByte.type]]](
                  ValUse(3, SGroupElement),
                  SGroupElement.getMethodByName("getEncoded"),
                  Vector(),
                  Map()
                )
              )
            ),
            Map()
          )
        )))

    { // changed behavior of flatMap in v5.0
      val cases = Seq(
        Coll[GroupElement](
          Helpers.decodeGroupElement("02d65904820f8330218cf7318b3810d0c9ab9df86f1ee6100882683f23c0aee587"),
          Helpers.decodeGroupElement("0390e9daa9916f30d0bc61a8e381c6005edfb7938aee5bb4fc9e8a759c7748ffaa")
        ) -> Expected(Try(SCollection.throwInvalidFlatmap(null)), 0,
              expectedDetails = CostDetails.ZeroCost,
              newCost = 0,
              newVersionedResults = (0 to 2).map(version =>
                // successful result for each version
                version -> (ExpectedResult(Success({
                         val is = Coll((0 to 32):_*)
                         is.append(is)
                       }),
                       verificationCost = Some(817)) -> None)
              ))
      )
      val f = changedFeature(
        scalaFunc = { (x: Coll[GroupElement]) => SCollection.throwInvalidFlatmap(null) },
        scalaFuncNew = { (x: Coll[GroupElement]) =>
          // NOTE, v5.0 interpreter accepts any lambda in flatMap
          x.flatMap({ (b: GroupElement) => b.getEncoded.indices })
        },
        "", // NOTE, the script for this test case cannot be compiled
        FuncValue(
          Vector((1, SCollectionType(SGroupElement))),
          MethodCall.typed[Value[SCollection[SInt.type]]](
            ValUse(1, SCollectionType(SGroupElement)),
            SCollection.getMethodByName("flatMap").withConcreteTypes(
              Map(STypeVar("IV") -> SGroupElement, STypeVar("OV") -> SInt)
            ),
            Vector(
              FuncValue(
                Vector((3, SGroupElement)),
                MethodCall.typed[Value[SCollection[SInt.type]]](
                  MethodCall.typed[Value[SCollection[SByte.type]]](
                    ValUse(3, SGroupElement),
                    SGroupElement.getMethodByName("getEncoded"),
                    Vector(),
                    Map()
                  ),
                  SCollection.getMethodByName("indices").withConcreteTypes(
                    Map(STypeVar("IV") -> SByte)
                  ),
                  Vector(),
                  Map()
                )
              )
            ),
            Map()
          )
        ),
        allowNewToSucceed = true, // the new 5.0 interpreter can succeed (after activation) when v4.0 fails
        allowDifferentErrors = true
      )

      val table = Table(("x", "y"), cases:_*)
      forAll(table) { (x, expectedRes) =>
        val res = f.checkEquality(x)
        val resValue = res.map(_._1)
        if (activatedVersionInTests >= VersionContext.JitActivationVersion) {
          val expected = expectedRes.newResults(ergoTreeVersionInTests)._1
          checkResult(resValue, expected.value, failOnTestVectors = true)
          if (res.isSuccess) {
            res.get._2.cost shouldBe JitCost(expected.verificationCost.get)
          }
        }
      }
    }

    val f = existingFeature(
      { (x: Coll[GroupElement]) => x.flatMap({ (b: GroupElement) => b.getEncoded.indices }) },
      "{ (x: Coll[GroupElement]) => x.flatMap({ (b: GroupElement) => b.getEncoded.indices }) }" )
    assertExceptionThrown(
      f.oldF,
      t => t match {
        case e: InvocationTargetException =>
          e.getTargetException.getMessage.contains("Unsupported lambda in flatMap")
      }
    )
  }

  property("Coll patch method equivalence") {
    val samples = genSamples(collWithRangeGen, MinSuccessful(50))
    def costDetails(i: Int) = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(MethodCall),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        SeqCostItem(MethodDesc(SCollection.PatchMethod), PerItemCost(JitCost(30), JitCost(2), 10), i)
      )
    )

    verifyCases(
      {
        def success[T](v: T, cd: CostDetails) = Expected(Success(v), 37514, cd, 1796)
        Seq(
          ((Coll[Int](), (0, 0)), success(Coll[Int](), costDetails(0))),
          ((Coll[Int](1), (0, 0)), success(Coll[Int](1, 1), costDetails(2))),
          ((Coll[Int](1), (0, 1)), success(Coll[Int](1), costDetails(2))),
          ((Coll[Int](1, 2), (0, 0)), success(Coll[Int](1, 2, 1, 2), costDetails(4))),
          ((Coll[Int](1, 2), (1, 0)), success(Coll[Int](1, 1, 2, 2), costDetails(4))),
          ((Coll[Int](1, 2), (0, 2)), success(Coll[Int](1, 2), costDetails(4))),
          ((Coll[Int](1, 2), (0, 3)), success(Coll[Int](1, 2), costDetails(4))),
          ((Coll[Int](1, 2), (1, 2)), success(Coll[Int](1, 1, 2), costDetails(4))),
          ((Coll[Int](1, 2), (2, 0)), success(Coll[Int](1, 2, 1, 2), costDetails(4))),
          ((Coll[Int](1, 2), (3, 0)), success(Coll[Int](1, 2, 1, 2), costDetails(4))),
          ((Coll[Int](1, 2), (3, 1)), success(Coll[Int](1, 2, 1, 2), costDetails(4))),
          ((Coll[Int](1, 2), (-1, 1)), success(Coll[Int](1, 2, 2), costDetails(4)))
        )
      },
      existingFeature(
        { (x: (Coll[Int], (Int, Int))) =>
          val coll = x._1
          val l = x._2._1; val r = x._2._2
          coll.patch(l, coll, r)
        },
        """{ (x: (Coll[Int], (Int, Int))) =>
          |  val coll = x._1
          |  val l = x._2._1; val r = x._2._2
          |  coll.patch(l, coll, r)
          |}""".stripMargin,
        FuncValue(
          Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[Value[SCollection[SInt.type]]](
                  ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                  1.toByte
                )
              ),
              ValDef(
                4,
                List(),
                SelectField.typed[Value[STuple]](
                  ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                  2.toByte
                )
              )
            ),
            MethodCall.typed[Value[SCollection[SInt.type]]](
              ValUse(3, SCollectionType(SInt)),
              SCollection.getMethodByName("patch").withConcreteTypes(Map(STypeVar("IV") -> SInt)),
              Vector(
                SelectField.typed[Value[SInt.type]](ValUse(4, SPair(SInt, SInt)), 1.toByte),
                ValUse(3, SCollectionType(SInt)),
                SelectField.typed[Value[SInt.type]](ValUse(4, SPair(SInt, SInt)), 2.toByte)
              ),
              Map()
            )
          )
        )),
      preGeneratedSamples = Some(samples))

  }

  property("Coll updated method equivalence") {
     def costDetails(i: Int) = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(MethodCall),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        SeqCostItem(MethodDesc(SCollection.UpdatedMethod), PerItemCost(JitCost(20), JitCost(1), 10), i)
      )
    )
    verifyCases(
      // (coll, (index, elem))
      {
        def success[T](v: T, cd: CostDetails) = Expected(Success(v), 37180, cd, 1794)
        Seq(
          ((Coll[Int](), (0, 0)), Expected(new IndexOutOfBoundsException("0"))),
          ((Coll[Int](1), (0, 0)), success(Coll[Int](0), costDetails(1))),
          ((Coll[Int](1, 2), (0, 0)), success(Coll[Int](0, 2), costDetails(2))),
          ((Coll[Int](1, 2), (1, 0)), success(Coll[Int](1, 0), costDetails(2))),
          ((Coll[Int](1, 2, 3), (2, 0)), success(Coll[Int](1, 2, 0), costDetails(3))),
          ((Coll[Int](1, 2), (2, 0)), Expected(new IndexOutOfBoundsException("2"))),
          ((Coll[Int](1, 2), (3, 0)), Expected(new IndexOutOfBoundsException("3"))),
          ((Coll[Int](1, 2), (-1, 0)), Expected(new IndexOutOfBoundsException("-1")))
        )
      },
      existingFeature(
        (x: (Coll[Int], (Int, Int))) => x._1.updated(x._2._1, x._2._2),
        "{ (x: (Coll[Int], (Int, Int))) => x._1.updated(x._2._1, x._2._2) }",
        FuncValue(
          Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[Value[STuple]](
                  ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                  2.toByte
                )
              )
            ),
            MethodCall.typed[Value[SCollection[SInt.type]]](
              SelectField.typed[Value[SCollection[SInt.type]]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                1.toByte
              ),
              SCollection.getMethodByName("updated").withConcreteTypes(Map(STypeVar("IV") -> SInt)),
              Vector(
                SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 1.toByte),
                SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 2.toByte)
              ),
              Map()
            )
          )
        )))
  }

  property("Coll updateMany method equivalence") {
    val samples = genSamples(
      for {
        coll <- collGen[Int]
        is <- genIndices(coll.length)
        vs <- collOfN[Int](is.length)
      } yield (coll, (is.toColl, vs)),
      MinSuccessful(20))

    def costDetails(i: Int) = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(MethodCall),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        SeqCostItem(MethodDesc(SCollection.UpdateManyMethod), PerItemCost(JitCost(20), JitCost(2), 10), i)
      )
    )
    verifyCases(
      // (coll, (indexes, values))
      {
        def success[T](v: T, i: Int) = Expected(Success(v), 37817, costDetails(i), 1794)
        Seq(
          ((Coll[Int](), (Coll(0), Coll(0))), Expected(new IndexOutOfBoundsException("0"))),
          ((Coll[Int](), (Coll(0, 1), Coll(0, 0))), Expected(new IndexOutOfBoundsException("0"))),
          ((Coll[Int](), (Coll(0, 1), Coll(0))), Expected(new IllegalArgumentException("requirement failed: Collections should have same length but was 2 and 1:\n xs=Coll(0,1);\n ys=Coll(0)"))),
          ((Coll[Int](1), (Coll(0), Coll(0))), success(Coll[Int](0), 1)),
          ((Coll[Int](1), (Coll(0, 1), Coll(0, 0))), Expected(new IndexOutOfBoundsException("1"))),
          ((Coll[Int](1, 2), (Coll(0), Coll(0))), success(Coll[Int](0, 2), 2)),
          ((Coll[Int](1, 2), (Coll(0, 1), Coll(0, 0))), success(Coll[Int](0, 0), 2)),
          ((Coll[Int](1, 2), (Coll(0, 1, 2), Coll(0, 0, 0))), Expected(new IndexOutOfBoundsException("2"))),
          ((Coll[Int](1, 2), (Coll(1), Coll(0))), success(Coll[Int](1, 0), 2)),
          ((Coll[Int](1, 2, 3), (Coll(2), Coll(0))), success(Coll[Int](1, 2, 0), 3)),
          ((Coll[Int](1, 2), (Coll(2), Coll(0))), Expected(new IndexOutOfBoundsException("2"))),
          ((Coll[Int](1, 2), (Coll(3), Coll(0))), Expected(new IndexOutOfBoundsException("3"))),
          ((Coll[Int](1, 2), (Coll(-1), Coll(0))), Expected(new IndexOutOfBoundsException("-1"))),
          ((Coll[Int](10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140),
              (Coll[Int](12, 12, 4, 11, 1, 8, 0, 1), Coll[Int](-10, -20, -30, -40, -50, -60, -70, -80))),
              success(Coll[Int](-70, -80, 30, 40, -30, 60, 70, 80, -60, 100, 110, -40, -20, 140), 14))
        )
      },
      existingFeature(
        (x: (Coll[Int], (Coll[Int], Coll[Int]))) => x._1.updateMany(x._2._1, x._2._2),
        "{ (x: (Coll[Int], (Coll[Int], Coll[Int]))) => x._1.updateMany(x._2._1, x._2._2) }",
        FuncValue(
          Vector((1, SPair(SCollectionType(SInt), SPair(SCollectionType(SInt), SCollectionType(SInt))))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[Value[STuple]](
                  ValUse(
                    1,
                    SPair(SCollectionType(SInt), SPair(SCollectionType(SInt), SCollectionType(SInt)))
                  ),
                  2.toByte
                )
              )
            ),
            MethodCall.typed[Value[SCollection[SInt.type]]](
              SelectField.typed[Value[SCollection[SInt.type]]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SCollectionType(SInt), SCollectionType(SInt)))),
                1.toByte
              ),
              SCollection.getMethodByName("updateMany").withConcreteTypes(Map(STypeVar("IV") -> SInt)),
              Vector(
                SelectField.typed[Value[SCollection[SInt.type]]](
                  ValUse(3, SPair(SCollectionType(SInt), SCollectionType(SInt))),
                  1.toByte
                ),
                SelectField.typed[Value[SCollection[SInt.type]]](
                  ValUse(3, SPair(SCollectionType(SInt), SCollectionType(SInt))),
                  2.toByte
                )
              ),
              Map()
            )
          )
        )),
      preGeneratedSamples = Some(samples))
  }

  // TODO v6.0 (3h): https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  property("Coll find method equivalence") {
    val find = newFeature((x: Coll[Int]) => x.find({ (v: Int) => v > 0 }),
      "{ (x: Coll[Int]) => x.find({ (v: Int) => v > 0} ) }")
    forAll { x: Coll[Int] =>
      find.checkEquality(x)
    }
  }

  // TODO v6.0 (3h): https://github.com/ScorexFoundation/sigmastate-interpreter/issues/418
  property("Coll bitwise methods equivalence") {
    val shiftRight = newFeature(
      { (x: Coll[Boolean]) =>
        if (x.size > 2) x.slice(0, x.size - 2) else Colls.emptyColl[Boolean]
      },
      "{ (x: Coll[Boolean]) => x >> 2 }")
    forAll { x: Array[Boolean] =>
      shiftRight.checkEquality(Colls.fromArray(x))
    }
  }

  // TODO v6.0 (3h): https://github.com/ScorexFoundation/sigmastate-interpreter/issues/479
  property("Coll diff methods equivalence") {
    val diff = newFeature((x: (Coll[Int], Coll[Int])) => x._1.diff(x._2),
      "{ (x: (Coll[Int], Coll[Int])) => x._1.diff(x._2) }")
    forAll { (x: Coll[Int], y: Coll[Int]) =>
      diff.checkEquality((x, y))
    }
  }

  property("Coll fold method equivalence") {
    val n = ExactNumeric.IntIsExactNumeric
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 0)
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        TypeBasedCostItem(ArithOp.Plus, SInt)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        TypeBasedCostItem(ArithOp.Plus, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        TypeBasedCostItem(ArithOp.Plus, SInt)
      )
    )
    val costDetails4 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 3),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        TypeBasedCostItem(ArithOp.Plus, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        TypeBasedCostItem(ArithOp.Plus, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        TypeBasedCostItem(ArithOp.Plus, SInt)
      )
    )
    if (lowerMethodCallsInTests) {
      verifyCases(
        // (coll, initState)
        {
          Seq(
            ((Coll[Byte](),  0), Expected(Success(0), 41266, costDetails1, 1787)),
            ((Coll[Byte](),  Int.MaxValue), Expected(Success(Int.MaxValue), 41266, costDetails1, 1787)),
            ((Coll[Byte](1),  Int.MaxValue - 1), Expected(Success(Int.MaxValue), 41396, costDetails2, 1793)),
            ((Coll[Byte](1),  Int.MaxValue), Expected(new ArithmeticException("integer overflow"))),
            ((Coll[Byte](-1),  Int.MinValue + 1), Expected(Success(Int.MinValue), 41396, costDetails2, 1793)),
            ((Coll[Byte](-1),  Int.MinValue), Expected(new ArithmeticException("integer overflow"))),
            ((Coll[Byte](1, 2), 0), Expected(Success(3), 41526, costDetails3, 1799)),
            ((Coll[Byte](1, -1), 0), Expected(Success(0), 41526, costDetails3, 1799)),
            ((Coll[Byte](1, -1, 1), 0), Expected(Success(1), 41656, costDetails4, 1805))
          )
        },
        existingFeature(
          { (x: (Coll[Byte], Int)) => x._1.foldLeft(x._2, { i: (Int, Byte) => n.plus(i._1, i._2) }) },
          "{ (x: (Coll[Byte], Int)) => x._1.fold(x._2, { (i1: Int, i2: Byte) => i1 + i2 }) }",
          FuncValue(
            Vector((1, SPair(SByteArray, SInt))),
            Fold(
              SelectField.typed[Value[SCollection[SByte.type]]](ValUse(1, SPair(SByteArray, SInt)), 1.toByte),
              SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SByteArray, SInt)), 2.toByte),
              FuncValue(
                Vector((3, SPair(SInt, SByte))),
                ArithOp(
                  SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SByte)), 1.toByte),
                  Upcast(SelectField.typed[Value[SByte.type]](ValUse(3, SPair(SInt, SByte)), 2.toByte), SInt),
                  OpCode @@ (-102.toByte)
                )
              )
            )
          )))
    } else {
      assertExceptionThrown(
        verifyCases(
          Seq( ((Coll[Byte](),  0), Expected(Success(0), 41266)) ),
          existingFeature(
            { (x: (Coll[Byte], Int)) => x._1.foldLeft(x._2, { i: (Int, Byte) => n.plus(i._1, i._2) }) },
            "{ (x: (Coll[Byte], Int)) => x._1.fold(x._2, { (i1: Int, i2: Byte) => i1 + i2 }) }"
          )),
        rootCauseLike[CosterException]("Don't know how to evalNode(Lambda(List()")
      )
    }
  }

  property("Coll fold with nested If") {
    val n = ExactNumeric.IntIsExactNumeric
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 0)
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        TypeBasedCostItem(ArithOp.Plus, SInt)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse)
      )
    )
    val costDetails4 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        TypeBasedCostItem(ArithOp.Plus, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        TypeBasedCostItem(ArithOp.Plus, SInt)
      ),
      Some(232417L)
    )
    val costDetails5 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        TypeBasedCostItem(ArithOp.Plus, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse)
      )
    )
    val costDetails6 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Fold), PerItemCost(JitCost(3), JitCost(1), 10), 3),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        TypeBasedCostItem(ArithOp.Plus, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 2),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        TypeBasedCostItem(Upcast, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        TypeBasedCostItem(ArithOp.Plus, SInt)
      )
    )
    if (lowerMethodCallsInTests) {
      verifyCases(
        // (coll, initState)
        {
          Seq(
            ((Coll[Byte](),  0), Expected(Success(0), 42037, costDetails1, 1787)),
            ((Coll[Byte](),  Int.MaxValue), Expected(Success(Int.MaxValue), 42037, costDetails1, 1787)),
            ((Coll[Byte](1),  Int.MaxValue - 1), Expected(Success(Int.MaxValue), 42197, costDetails2, 1799)),
            ((Coll[Byte](1),  Int.MaxValue), Expected(new ArithmeticException("integer overflow"))),
            ((Coll[Byte](-1),  Int.MinValue + 1), Expected(Success(Int.MinValue + 1), 42197, costDetails3, 1797)),
            ((Coll[Byte](-1),  Int.MinValue), Expected(Success(Int.MinValue), 42197, costDetails3, 1797)),
            ((Coll[Byte](1, 2), 0), Expected(Success(3), 42357, costDetails4, 1811)),
            ((Coll[Byte](1, -1), 0), Expected(Success(1), 42357, costDetails5, 1809)),
            ((Coll[Byte](1, -1, 1), 0), Expected(Success(2), 42517, costDetails6, 1821))
          )
        },
        existingFeature(
          { (x: (Coll[Byte], Int)) => x._1.foldLeft(x._2, { i: (Int, Byte) => if (i._2 > 0) n.plus(i._1, i._2) else i._1 }) },
          "{ (x: (Coll[Byte], Int)) => x._1.fold(x._2, { (i1: Int, i2: Byte) => if (i2 > 0) i1 + i2 else i1 }) }",
          FuncValue(
            Array((1, SPair(SByteArray, SInt))),
            Fold(
              SelectField.typed[Value[SCollection[SByte.type]]](ValUse(1, SPair(SByteArray, SInt)), 1.toByte),
              SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SByteArray, SInt)), 2.toByte),
              FuncValue(
                Array((3, SPair(SInt, SByte))),
                BlockValue(
                  Array(
                    ValDef(
                      5,
                      List(),
                      Upcast(
                        SelectField.typed[Value[SByte.type]](ValUse(3, SPair(SInt, SByte)), 2.toByte),
                        SInt
                      )
                    ),
                    ValDef(
                      6,
                      List(),
                      SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SByte)), 1.toByte)
                    )
                  ),
                  If(
                    GT(ValUse(5, SInt), IntConstant(0)),
                    ArithOp(ValUse(6, SInt), ValUse(5, SInt), OpCode @@ (-102.toByte)),
                    ValUse(6, SInt)
                  )
                )
              )
            )
          ) ))
    } else {
      assertExceptionThrown(
        verifyCases(
          Seq( ((Coll[Byte](),  0), Expected(Success(0), 42037)) ),
          existingFeature(
            { (x: (Coll[Byte], Int)) => x._1.foldLeft(x._2, { i: (Int, Byte) => if (i._2 > 0) n.plus(i._1, i._2) else i._1 }) },
            "{ (x: (Coll[Byte], Int)) => x._1.fold(x._2, { (i1: Int, i2: Byte) => if (i2 > 0) i1 + i2 else i1 }) }"
          )),
        rootCauseLike[CosterException]("Don't know how to evalNode(Lambda(List()")
      )
    }
  }

  property("Coll indexOf method equivalence") {
    def costDetails(i: Int) = TracedCost( 
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(MethodCall),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField)
      ) 
      ++ Array.fill(i)(FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3))))
      :+ SeqCostItem(MethodDesc(SCollection.IndexOfMethod), PerItemCost(JitCost(20), JitCost(10), 2), i)
    )
    verifyCases(
      // (coll, (elem: Byte, from: Int))
      {
        def success0[T](v: T) = Expected(Success(v), 37649, costDetails(0), 1793)
        def success1[T](v: T) = Expected(Success(v), 37649, costDetails(1), 1793)
        def success2[T](v: T) = Expected(Success(v), 37649, costDetails(2), 1794)
        def success3[T](v: T) = Expected(Success(v), 37649, costDetails(3), 1795)
        def success12[T](v: T) = Expected(Success(v), 37649, costDetails(12), 1802)
        Seq(
          ((Coll[Byte](),  (0.toByte, 0)), success0(-1)),
          ((Coll[Byte](),  (0.toByte, -1)), success0(-1)),
          ((Coll[Byte](),  (0.toByte, 1)), success0(-1)),
          ((Coll[Byte](1),  (0.toByte, 0)), success1(-1)),
          ((Coll[Byte](1),  (1.toByte, 0)), success1(0)),
          ((Coll[Byte](1),  (1.toByte, 1)), success0(-1)),
          ((Coll[Byte](1, 1),  (0.toByte, -1)), success2(-1)),
          ((Coll[Byte](1, 1),  (0.toByte, 0)), success2(-1)),
          ((Coll[Byte](1, 1),  (1.toByte, -1)), success1(0)),
          ((Coll[Byte](1, 1),  (1.toByte, 0)), success1(0)),
          ((Coll[Byte](1, 1),  (1.toByte, 1)), success1(1)),
          ((Coll[Byte](1, 1),  (1.toByte, 2)), success0(-1)),
          ((Coll[Byte](1, 1),  (1.toByte, 3)), success0(-1)),
          ((Coll[Byte](1, 2, 3),  (3.toByte, 0)), success3(2)),
          ((Coll[Byte](1, 2, 3),  (3.toByte, 1)), success2(2)),
          ((Coll[Byte](1, 2, 3),  (3.toByte, 2)), success1(2)),
          ((Coll[Byte](1, 2, 3),  (3.toByte, 3)), success0(-1)),
          ((Helpers.decodeBytes("8085623fb7cd6b7f01801f00800100"), (0.toByte, -1)), success12(11))
        )
      },
      existingFeature(
        { (x: (Coll[Byte], (Byte, Int))) => x._1.indexOf(x._2._1, x._2._2) },
        "{ (x: (Coll[Byte], (Byte, Int))) => x._1.indexOf(x._2._1, x._2._2) }",
        FuncValue(
          Vector((1, SPair(SByteArray, SPair(SByte, SInt)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[Value[STuple]](ValUse(1, SPair(SByteArray, SPair(SByte, SInt))), 2.toByte)
              )
            ),
            MethodCall.typed[Value[SInt.type]](
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(1, SPair(SByteArray, SPair(SByte, SInt))),
                1.toByte
              ),
              SCollection.getMethodByName("indexOf").withConcreteTypes(Map(STypeVar("IV") -> SByte)),
              Vector(
                SelectField.typed[Value[SByte.type]](ValUse(3, SPair(SByte, SInt)), 1.toByte),
                SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SByte, SInt)), 2.toByte)
              ),
              Map()
            )
          )
        )), preGeneratedSamples = Some(Seq()))
  }

  property("Coll apply method equivalence") {
    val costDetails = TracedCost( 
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ByIndex)
      )
    )
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 36410, costDetails, 1789)
        Seq(
          ((Coll[Int](), 0), Expected(new ArrayIndexOutOfBoundsException("0"))),
          ((Coll[Int](), -1), Expected(new ArrayIndexOutOfBoundsException("-1"))),
          ((Coll[Int](1), 0), success(1)),
          ((Coll[Int](1), 1), Expected(new ArrayIndexOutOfBoundsException("1"))),
          ((Coll[Int](1), -1), Expected(new ArrayIndexOutOfBoundsException("-1"))),
          ((Coll[Int](1, 2), 1), success(2)),
          ((Coll[Int](1, 2), 1), success(2)),
          ((Coll[Int](1, 2), 2), Expected(new ArrayIndexOutOfBoundsException("2")))
        )
      },
      existingFeature((x: (Coll[Int], Int)) => x._1(x._2),
        "{ (x: (Coll[Int], Int)) => x._1(x._2) }",
        FuncValue(
          Vector((1, SPair(SCollectionType(SInt), SInt))),
          ByIndex(
            SelectField.typed[Value[SCollection[SInt.type]]](
              ValUse(1, SPair(SCollectionType(SInt), SInt)),
              1.toByte
            ),
            SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SCollectionType(SInt), SInt)), 2.toByte),
            None
          )
        )))
  }

  property("Coll getOrElse method equivalence") {
    val default = 10
    val costDetails = TracedCost( 
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        FixedCostItem(ByIndex)
      )
    )
    if (lowerMethodCallsInTests) {
      verifyCases(
      // (coll, (index, default))
      {
        def success[T](v: T) = Expected(Success(v), 37020, costDetails, 1793)
        Seq(
          ((Coll[Int](), (0, default)), success(default)),
          ((Coll[Int](), (-1, default)), success(default)),
          ((Coll[Int](1), (0, default)), success(1)),
          ((Coll[Int](1), (1, default)), success(default)),
          ((Coll[Int](1), (-1, default)), success(default)),
          ((Coll[Int](1, 2), (0, default)), success(1)),
          ((Coll[Int](1, 2), (1, default)), success(2)),
          ((Coll[Int](1, 2), (2, default)), success(default)),
          ((Coll[Int](1, 2), (-1, default)), success(default))
        )
      },
      existingFeature((x: (Coll[Int], (Int, Int))) => x._1.getOrElse(x._2._1, x._2._2),
        "{ (x: (Coll[Int], (Int, Int))) => x._1.getOrElse(x._2._1, x._2._2) }",
        FuncValue(
          Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[Value[STuple]](
                  ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                  2.toByte
                )
              )
            ),
            ByIndex(
              SelectField.typed[Value[SCollection[SInt.type]]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                1.toByte
              ),
              SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 1.toByte),
              Some(SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 2.toByte))
            )
          )
        )))
    } else {
      forEachScriptAndErgoTreeVersion(activatedVersions, ergoTreeVersions) {
        testCases(
          Seq(
            ((Coll[Int](1, 2), (0, default)),
              Expected(
                // in v4.x exp method cannot be called via MethodCall ErgoTree node
                Failure(new NoSuchMethodException("")),
                cost = 41484,
                CostDetails.ZeroCost,
                newCost = 0,
                newVersionedResults = {
                  // in v5.0 MethodCall ErgoTree node is allowed
                  val res = ExpectedResult( Success(1), Some(166) )
                  Seq( // expected result for each version
                    0 -> ( res -> None ),
                    1 -> ( res -> None ),
                    2 -> ( res -> None )
                  )
                }
              )
            )
          ),
          changedFeature(
            (x: (Coll[Int], (Int, Int))) =>
              throw new NoSuchMethodException(""),
            (x: (Coll[Int], (Int, Int))) => x._1.getOrElse(x._2._1, x._2._2),
            ""/*can't be compiled in v4.x*/,
            FuncValue(
              Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
              BlockValue(
                Vector(
                  ValDef(
                    3,
                    List(),
                    SelectField.typed[Value[STuple]](
                      ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                      2.toByte
                    )
                  )
                ),
                MethodCall(
                  SelectField.typed[Value[SCollection[SInt.type]]](
                    ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                    1.toByte
                  ),
                  SCollection.getMethodByName("getOrElse").withConcreteTypes(
                    Map(STypeVar("IV") -> SInt)
                  ),
                  Vector(
                    SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 1.toByte),
                    SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 2.toByte)
                  ),
                  Map()
                )
              )
            ),
            allowNewToSucceed = true,
            allowDifferentErrors = false
          ))
      }
    }
  }

  property("Tuple size method equivalence") {
    val costDetails = TracedCost( 
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(Constant)
      )
    )
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 35905, costDetails, 1783)
        Seq(
          ((0, 0), success(2)),
          ((1, 2), success(2))
        )
      },
      existingFeature((x: (Int, Int)) => 2,
        "{ (x: (Int, Int)) => x.size }",
        FuncValue(Vector((1, SPair(SInt, SInt))), IntConstant(2))))
  }

  property("Tuple apply method equivalence") {
    val samples = genSamples[(Int, Int)](DefaultMinSuccessful)
    val costDetails = TracedCost(traceBase :+ FixedCostItem(SelectField))
    verifyCases(
      Seq(((1, 2), Expected(Success(1), cost = 36013, costDetails, 1784))),
      existingFeature((x: (Int, Int)) => x._1,
        "{ (x: (Int, Int)) => x(0) }",
        FuncValue(
          Vector((1, SPair(SInt, SInt))),
          SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 1.toByte)
        )),
      preGeneratedSamples = Some(samples))
    verifyCases(
      Seq(((1, 2), Expected(Success(2), cost = 36013, costDetails, 1784))),
      existingFeature((x: (Int, Int)) => x._2,
        "{ (x: (Int, Int)) => x(1) }",
        FuncValue(
          Vector((1, SPair(SInt, SInt))),
          SelectField.typed[Value[SInt.type]](ValUse(1, SPair(SInt, SInt)), 2.toByte)
        )),
      preGeneratedSamples = Some(samples))
  }

  property("Coll map method equivalence") {
    def repeatPlusChunk(i: Int): Array[CostItem] = Array.fill(i){
      Array(
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(ArithOp.Plus, SInt)
      )
    }.flatten
    def costDetails(i: Int) = TracedCost(
      traceBase
      ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(MapCollection), PerItemCost(JitCost(20), JitCost(1), 10), i)
      )
      ++ repeatPlusChunk(i)
    )

    val n = ExactNumeric.IntIsExactNumeric
    verifyCases(
      {
        def success[T](v: T, c: Int) = Expected(Success(v), c)
        Seq(
          (Coll[Int](), Expected(Success(Coll[Int]()), 38886, costDetails(0), 1788)),
          (Coll[Int](1), Expected(Success(Coll[Int](2)), 38936, costDetails(1), 1791)),
          (Coll[Int](1, 2), Expected(Success(Coll[Int](2, 3)), 38986, costDetails(2), 1794)),
          (Coll[Int](1, 2, Int.MaxValue), Expected(new ArithmeticException("integer overflow")))
        )
      },
      existingFeature((x: Coll[Int]) => x.map({ (v: Int) => n.plus(v, 1) }),
        "{ (x: Coll[Int]) => x.map({ (v: Int) => v + 1 }) }",
        FuncValue(
          Vector((1, SCollectionType(SInt))),
          MapCollection(
            ValUse(1, SCollectionType(SInt)),
            FuncValue(Vector((3, SInt)), ArithOp(ValUse(3, SInt), IntConstant(1), OpCode @@ (-102.toByte)))
          )
        )))
  }

  property("Coll map with nested if") {
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(MapCollection), PerItemCost(JitCost(20), JitCost(1), 10), 0)
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(MapCollection), PerItemCost(JitCost(20), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(ArithOp.Plus, SInt)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(MapCollection), PerItemCost(JitCost(20), JitCost(1), 10), 1),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(Constant),
        FixedCostItem(ValUse),
        TypeBasedCostItem(ArithOp.Multiply, SInt)
      )
    )
    val costDetails4 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(MapCollection), PerItemCost(JitCost(20), JitCost(1), 10), 2),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(ArithOp.Plus, SInt),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SInt),
        FixedCostItem(If),
        FixedCostItem(Constant),
        FixedCostItem(ValUse),
        TypeBasedCostItem(ArithOp.Multiply, SInt)
      )
    )
    val n = ExactNumeric.IntIsExactNumeric
    verifyCases(
      {
        def success[T](v: T, c: Int) = Expected(Success(v), c)
        Seq(
          (Coll[Int](), Expected(Success(Coll[Int]()), 39571, costDetails1, 1788)),
          (Coll[Int](1), Expected(Success(Coll[Int](2)), 39671, costDetails2, 1795)),
          (Coll[Int](-1), Expected(Success(Coll[Int](1)), 39671, costDetails3, 1795)),
          (Coll[Int](1, -2), Expected(Success(Coll[Int](2, 2)), 39771, costDetails4, 1802)),
          (Coll[Int](1, 2, Int.MaxValue), Expected(new ArithmeticException("integer overflow"))),
          (Coll[Int](1, 2, Int.MinValue), Expected(new ArithmeticException("integer overflow")))
        )
      },
      existingFeature(
        (x: Coll[Int]) => x.map({ (v: Int) => if (v > 0) n.plus(v, 1) else n.times(-1, v) }),
        "{ (x: Coll[Int]) => x.map({ (v: Int) => if (v > 0) v + 1 else -1 * v }) }",
        FuncValue(
          Array((1, SCollectionType(SInt))),
          MapCollection(
            ValUse(1, SCollectionType(SInt)),
            FuncValue(
              Array((3, SInt)),
              If(
                GT(ValUse(3, SInt), IntConstant(0)),
                ArithOp(ValUse(3, SInt), IntConstant(1), OpCode @@ (-102.toByte)),
                ArithOp(IntConstant(-1), ValUse(3, SInt), OpCode @@ (-100.toByte))
              )
            )
          )
        ) ))
  }

  property("Coll filter") {
    def costDetails(i: Int) = {
      val gtChunk = Array.fill(i)(
        Array(
          FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
          FixedCostItem(ValUse),
          FixedCostItem(Constant),
          TypeBasedCostItem(GT, SInt)
        )
      ).flatten

      TracedCost(
        traceBase ++
        Array(
          FixedCostItem(FuncValue),
          SeqCostItem(CompanionDesc(Filter), PerItemCost(JitCost(20), JitCost(1), 10), i)
        ) ++
        gtChunk
      )
    }

    val o = ExactOrdering.IntIsExactOrdering
    verifyCases(
    {
      Seq(
        (Coll[Int](), Expected(Success(Coll[Int]()), 37223, costDetails(0), 1788)),
        (Coll[Int](1), Expected(Success(Coll[Int](1)), 37273, costDetails(1), 1791)),
        (Coll[Int](1, 2), Expected(Success(Coll[Int](1, 2)), 37323, costDetails(2), 1795)),
        (Coll[Int](1, 2, -1), Expected(Success(Coll[Int](1, 2)), 37373, costDetails(3), 1798)),
        (Coll[Int](1, -1, 2, -2), Expected(Success(Coll[Int](1, 2)), 37423, costDetails(4), 1802))
      )
    },
    existingFeature((x: Coll[Int]) => x.filter({ (v: Int) => o.gt(v, 0) }),
      "{ (x: Coll[Int]) => x.filter({ (v: Int) => v > 0 }) }",
      FuncValue(
        Array((1, SCollectionType(SInt))),
        Filter(
          ValUse(1, SCollectionType(SInt)),
          FuncValue(Array((3, SInt)), GT(ValUse(3, SInt), IntConstant(0)))
        )
      )))
  }

  property("Coll filter with nested If") {
    val leftBranch = Array(
      FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
      FixedCostItem(ValUse),
      FixedCostItem(Constant),
      TypeBasedCostItem(GT, SInt),
      FixedCostItem(If),
      FixedCostItem(ValUse),
      FixedCostItem(Constant),
      TypeBasedCostItem(LT, SInt)
    )
    val rightBranch = Array(
      FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
      FixedCostItem(ValUse),
      FixedCostItem(Constant),
      TypeBasedCostItem(GT, SInt),
      FixedCostItem(If),
      FixedCostItem(Constant)
    )
    def repeatLeftBranch(i: Int) = Array.fill(i)(leftBranch).flatten
    
    def costDetails(i: Int) = {
      TracedCost(
        traceBase ++
        Array(
          FixedCostItem(FuncValue),
          SeqCostItem(CompanionDesc(Filter), PerItemCost(JitCost(20), JitCost(1), 10), i)
        ) ++ 
        repeatLeftBranch(i)
      )
    }
    val costDetails3 = TracedCost(
      traceBase ++ 
      Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Filter), PerItemCost(JitCost(20), JitCost(1), 10), 3)
      ) ++ 
      repeatLeftBranch(2) ++ 
      rightBranch
    )
    val costDetails5 = TracedCost(
      traceBase ++ 
      Array(
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(Filter), PerItemCost(JitCost(20), JitCost(1), 10), 5)
      ) ++
      leftBranch ++
      rightBranch ++
      leftBranch ++
      rightBranch ++
      leftBranch
    )

    val o = ExactOrdering.IntIsExactOrdering
    verifyCases(
    {
      def success[T](v: T, c: Int) = Expected(Success(v), c)
      Seq(
        (Coll[Int](), Expected(Success(Coll[Int]()), 37797, costDetails(0), 1788)),
        (Coll[Int](1), Expected(Success(Coll[Int](1)), 37887, costDetails(1), 1795)),
        (Coll[Int](10), Expected(Success(Coll[Int]()), 37887, costDetails(1), 1795)),
        (Coll[Int](1, 2), Expected(Success(Coll[Int](1, 2)), 37977, costDetails(2), 1803)),
        (Coll[Int](1, 2, 0), Expected(Success(Coll[Int](1, 2)), 38067, costDetails3, 1808)),
        (Coll[Int](1, -1, 2, -2, 11), Expected(Success(Coll[Int](1, 2)), 38247, costDetails5, 1820))
      )
    },
    existingFeature((x: Coll[Int]) => x.filter({ (v: Int) => if (o.gt(v, 0)) v < 10 else false }),
      "{ (x: Coll[Int]) => x.filter({ (v: Int) => if (v > 0) v < 10 else false }) }",
      FuncValue(
        Array((1, SCollectionType(SInt))),
        Filter(
          ValUse(1, SCollectionType(SInt)),
          FuncValue(
            Array((3, SInt)),
            If(GT(ValUse(3, SInt), IntConstant(0)), LT(ValUse(3, SInt), IntConstant(10)), FalseLeaf)
          )
        )
      )))
  }

  // TODO mainnet v5.0: NoSuchMethodException thrown inside the test.
  property("Coll slice method equivalence") {
    val samples = genSamples(collWithRangeGen, DefaultMinSuccessful)
    if (lowerMethodCallsInTests) {
      verifyCases(
      // (coll, (from, until))
      {
        def success[T](v: T) = Expected(Success(v), 36964)
        Seq(
          ((Coll[Int](), (-1, 0)), success(Coll[Int]())),
          ((Coll[Int](), (0, 0)), success(Coll[Int]())),
          ((Coll[Int](1), (0, 0)), success(Coll[Int]())),
          ((Coll[Int](1), (0, -1)), success(Coll[Int]())),
          ((Coll[Int](1), (1, 1)), success(Coll[Int]())),
          ((Coll[Int](1), (-1, 1)), success(Coll[Int](1))),
          ((Coll[Int](1, 2), (1, 1)), success(Coll[Int]())),
          ((Coll[Int](1, 2), (1, 0)), success(Coll[Int]())),
          ((Coll[Int](1, 2), (1, 2)), success(Coll[Int](2))),
          ((Coll[Int](1, 2, 3, 4), (1, 3)), success(Coll[Int](2, 3)))
        )
      },
      existingFeature((x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2),
        "{ (x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2) }",
        FuncValue(
          Vector((1, SPair(SCollectionType(SInt), SPair(SInt, SInt)))),
          BlockValue(
            Vector(
              ValDef(
                3,
                List(),
                SelectField.typed[Value[STuple]](
                  ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                  2.toByte
                )
              )
            ),
            Slice(
              SelectField.typed[Value[SCollection[SInt.type]]](
                ValUse(1, SPair(SCollectionType(SInt), SPair(SInt, SInt))),
                1.toByte
              ),
              SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 1.toByte),
              SelectField.typed[Value[SInt.type]](ValUse(3, SPair(SInt, SInt)), 2.toByte)
            )
          )
        )),
        preGeneratedSamples = Some(samples))
    } else {
      assertExceptionThrown(
        verifyCases(
          Seq( (Coll[Int](), (-1, 0)) -> Expected(Success(Coll[Int]()), 37765) ),
          existingFeature((x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2),
            "{ (x: (Coll[Int], (Int, Int))) => x._1.slice(x._2._1, x._2._2) }"
          )),
        rootCauseLike[NoSuchMethodException]("sigmastate.eval.CostingRules$CollCoster.slice")
      )
    }
  }

  property("Coll.append equivalence") {
    def costDetails(i: Int) = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SelectField),
        FixedCostItem(ValUse),
        FixedCostItem(SelectField),
        SeqCostItem(CompanionDesc(Append), PerItemCost(JitCost(20), JitCost(2), 100), i)
      )
    )
    if (lowerMethodCallsInTests) {
      verifyCases(
      {
        def success[T](v: T, size: Int) = Expected(Success(v), 37765, costDetails(size), 1790)
        val arr1 = Gen.listOfN(100, arbitrary[Int]).map(_.toArray).sample.get
        val arr2 = Gen.listOfN(200, arbitrary[Int]).map(_.toArray).sample.get
        Seq(
          (Coll[Int](), Coll[Int]()) -> success(Coll[Int](), 0),
          (Coll[Int](), Coll[Int](1)) -> success(Coll[Int](1), 1),
          (Coll[Int](1), Coll[Int]()) -> success(Coll[Int](1), 1),
          (Coll[Int](1), Coll[Int](2)) -> success(Coll[Int](1, 2), 2),
          (Coll[Int](1), Coll[Int](2, 3)) -> success(Coll[Int](1, 2, 3), 3),
          (Coll[Int](1, 2), Coll[Int](3)) -> success(Coll[Int](1, 2, 3), 3),
          (Coll[Int](1, 2), Coll[Int](3, 4)) -> success(Coll[Int](1, 2, 3, 4), 4),
          (Coll[Int](arr1:_*), Coll[Int](arr2:_*)) -> Expected(Success(Coll[Int](arr1 ++ arr2:_*)), 37785, costDetails(300), 1791)
        )
      },
      existingFeature(
        { (x: (Coll[Int], Coll[Int])) => x._1.append(x._2) },
        "{ (x: (Coll[Int], Coll[Int])) => x._1.append(x._2) }",
        FuncValue(
          Vector((1, SPair(SCollectionType(SInt), SCollectionType(SInt)))),
          Append(
            SelectField.typed[Value[SCollection[SInt.type]]](
              ValUse(1, SPair(SCollectionType(SInt), SCollectionType(SInt))),
              1.toByte
            ),
            SelectField.typed[Value[SCollection[SInt.type]]](
              ValUse(1, SPair(SCollectionType(SInt), SCollectionType(SInt))),
              2.toByte
            )
          )
        )))
    } else {
      assertExceptionThrown(
        verifyCases(
          Seq( (Coll[Int](), Coll[Int]()) -> Expected(Success(Coll[Int]()), 37765) ),
          existingFeature(
            { (x: (Coll[Int], Coll[Int])) => x._1.append(x._2) },
            "{ (x: (Coll[Int], Coll[Int])) => x._1.append(x._2) }"
            )),
        rootCauseLike[NoSuchMethodException]("sigmastate.eval.CostingRules$CollCoster.append(scalan.Base$Ref)")
      )
    }
  }

  property("Option methods equivalence") {
    val costDetails1 = TracedCost(traceBase :+ FixedCostItem(OptionGet))
    val costDetails2 = TracedCost(traceBase :+ FixedCostItem(OptionIsDefined))
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(Constant),
        FixedCostItem(OptionGetOrElse)
      )
    )
    val costDetails4 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.FilterMethod, FixedCost(JitCost(20)))
      )
    )
    val costDetails5 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.FilterMethod, FixedCost(JitCost(20))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        FixedCostItem(NamedDesc("EQ_Prim"), FixedCost(JitCost(3)))
      )
    )
    val costDetails6 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.MapMethod, FixedCost(JitCost(20)))
      )
    )
    val costDetails7 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.MapMethod, FixedCost(JitCost(20))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(ArithOp.Plus, SLong)
      )
    )

    verifyCases(
      Seq(
        (None -> Expected(new NoSuchElementException("None.get"))),
        (Some(10L) -> Expected(Success(10L), 36046, costDetails1, 1785))),
      existingFeature({ (x: Option[Long]) => x.get },
        "{ (x: Option[Long]) => x.get }",
        FuncValue(Vector((1, SOption(SLong))), OptionGet(ValUse(1, SOption(SLong))))))

    verifyCases(
      Seq(
        (None -> Expected(Success(false), 36151, costDetails2, 1784)),
        (Some(10L) -> Expected(Success(true), 36151, costDetails2, 1784))),
      existingFeature({ (x: Option[Long]) => x.isDefined },
        "{ (x: Option[Long]) => x.isDefined }",
        FuncValue(Vector((1, SOption(SLong))), OptionIsDefined(ValUse(1, SOption(SLong))))))

    verifyCases(
      Seq(
        (None -> Expected(Success(1L), 36367, costDetails3, 1786)),
        (Some(10L) -> Expected(Success(10L), 36367, costDetails3, 1786))),
      existingFeature({ (x: Option[Long]) => x.getOrElse(1L) },
        "{ (x: Option[Long]) => x.getOrElse(1L) }",
        FuncValue(Vector((1, SOption(SLong))), OptionGetOrElse(ValUse(1, SOption(SLong)), LongConstant(1L)))))

    verifyCases(
      Seq(
        (None -> Expected(Success(None), 38239, costDetails4, 1786)),
        (Some(10L) -> Expected(Success(None), 38239, costDetails5, 1788)),
        (Some(1L) -> Expected(Success(Some(1L)), 38239, costDetails5, 1789))),
      existingFeature({ (x: Option[Long]) => x.filter({ (v: Long) => v == 1} ) },
        "{ (x: Option[Long]) => x.filter({ (v: Long) => v == 1 }) }",
        FuncValue(
          Vector((1, SOption(SLong))),
          MethodCall.typed[Value[SOption[SLong.type]]](
            ValUse(1, SOption(SLong)),
            SOption.getMethodByName("filter").withConcreteTypes(Map(STypeVar("T") -> SLong)),
            Vector(FuncValue(Vector((3, SLong)), EQ(ValUse(3, SLong), LongConstant(1L)))),
            Map()
          )
        )))

    val n = ExactNumeric.LongIsExactNumeric
    verifyCases(
      Seq(
        (None -> Expected(Success(None), 38575, costDetails6, 1786)),
        (Some(10L) -> Expected(Success(Some(11L)), 38575, costDetails7, 1790)),
        (Some(Long.MaxValue) -> Expected(new ArithmeticException("long overflow")))),
      existingFeature({ (x: Option[Long]) => x.map( (v: Long) => n.plus(v, 1) ) },
        "{ (x: Option[Long]) => x.map({ (v: Long) => v + 1 }) }",
        FuncValue(
          Vector((1, SOption(SLong))),
          MethodCall.typed[Value[SOption[SLong.type]]](
            ValUse(1, SOption(SLong)),
            SOption.getMethodByName("map").withConcreteTypes(
              Map(STypeVar("T") -> SLong, STypeVar("R") -> SLong)
            ),
            Vector(
              FuncValue(
                Vector((3, SLong)),
                ArithOp(ValUse(3, SLong), LongConstant(1L), OpCode @@ (-102.toByte))
              )
            ),
            Map()
          )
        )))
  }

  property("Option filter,map with nested If") {
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.FilterMethod, FixedCost(JitCost(20)))
      )
    )
    val costDetails2 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.FilterMethod, FixedCost(JitCost(20))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong),
        FixedCostItem(If),
        FixedCostItem(Constant)
      )
    )
    val costDetails3 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.FilterMethod, FixedCost(JitCost(20))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(GT, SLong),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(LE, SLong)
      )
    )

    val o = ExactOrdering.LongIsExactOrdering
    verifyCases(
      Seq(
        (None -> Expected(Success(None), 38736, costDetails1, 1786)),
        (Some(0L) -> Expected(Success(None), 38736, costDetails2, 1791)),
        (Some(10L) -> Expected(Success(Some(10L)), 38736, costDetails3, 1794)),
        (Some(11L) -> Expected(Success(None), 38736, costDetails3, 1794))),
      existingFeature(
        { (x: Option[Long]) => x.filter({ (v: Long) => if (o.gt(v, 0L)) v <= 10 else false } ) },
        "{ (x: Option[Long]) => x.filter({ (v: Long) => if (v > 0) v <= 10 else false }) }",
        FuncValue(
          Array((1, SOption(SLong))),
          MethodCall.typed[Value[SOption[SLong.type]]](
            ValUse(1, SOption(SLong)),
            SOption.getMethodByName("filter").withConcreteTypes(Map(STypeVar("T") -> SLong)),
            Vector(
              FuncValue(
                Array((3, SLong)),
                If(
                  GT(ValUse(3, SLong), LongConstant(0L)),
                  LE(ValUse(3, SLong), LongConstant(10L)),
                  FalseLeaf
                )
              )
            ),
            Map()
          )
        )))

    val costDetails4 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.MapMethod, FixedCost(JitCost(20)))
      )
    )
    val costDetails5 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.MapMethod, FixedCost(JitCost(20))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(LT, SLong),
        FixedCostItem(If),
        FixedCostItem(ValUse)
      )
    )
    val costDetails6 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.MapMethod, FixedCost(JitCost(20))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(LT, SLong),
        FixedCostItem(If),
        FixedCostItem(ValUse),
        FixedCostItem(Constant),
        TypeBasedCostItem(ArithOp.Minus, SLong)
      )
    )
    val n = ExactNumeric.LongIsExactNumeric
    verifyCases(
      Seq(
        (None -> Expected(Success(None), 39077, costDetails4, 1786)),
        (Some(0L) -> Expected(Success(Some(0L)), 39077, costDetails5, 1792)),
        (Some(10L) -> Expected(Success(Some(10L)), 39077, costDetails5, 1792)),
        (Some(-1L) -> Expected(Success(Some(-2L)), 39077, costDetails6, 1794)),
        (Some(Long.MinValue) -> Expected(new ArithmeticException("long overflow")))),
      existingFeature(
        { (x: Option[Long]) => x.map( (v: Long) => if (o.lt(v, 0)) n.minus(v, 1) else v ) },
        "{ (x: Option[Long]) => x.map({ (v: Long) => if (v < 0) v - 1 else v }) }",
        FuncValue(
          Array((1, SOption(SLong))),
          MethodCall.typed[Value[SOption[SLong.type]]](
            ValUse(1, SOption(SLong)),
            SOption.getMethodByName("map").withConcreteTypes(
              Map(STypeVar("T") -> SLong, STypeVar("R") -> SLong)
            ),
            Vector(
              FuncValue(
                Array((3, SLong)),
                If(
                  LT(ValUse(3, SLong), LongConstant(0L)),
                  ArithOp(ValUse(3, SLong), LongConstant(1L), OpCode @@ (-103.toByte)),
                  ValUse(3, SLong)
                )
              )
            ),
            Map()
          )
        ) ))
  }

  // TODO v6.0 (3h): implement Option.fold
  property("Option new methods") {
    val isEmpty = newFeature({ (x: Option[Long]) => x.isEmpty },
    "{ (x: Option[Long]) => x.isEmpty }")

    val n = ExactNumeric.LongIsExactNumeric
    val fold = newFeature({ (x: Option[Long]) => x.fold(5.toLong)( (v: Long) => n.plus(v, 1) ) },
      "{ (x: Option[Long]) => x.fold(5, { (v: Long) => v + 1 }) }")

    forAll { x: Option[Long] =>
      Seq(isEmpty, fold).map(_.checkEquality(x))
    }
  }

  property("Option fold workaround method") {
    val costDetails1 = TracedCost(
      traceBase ++ Array(
        FixedCostItem(MethodCall),
        FixedCostItem(FuncValue),
        FixedCostItem(SOption.FilterMethod, FixedCost(JitCost(20)))
      )
    )
    val n = ExactNumeric.LongIsExactNumeric
    verifyCases(
      Seq(
        (None -> Expected(
            value = Failure(new NoSuchElementException("None.get")),
            cost = 0,
            expectedDetails = CostDetails.ZeroCost,
            newCost = 1786,
            newVersionedResults = Seq(
              2 -> (ExpectedResult(Success(5L), Some(1786)) -> None)
            )
          )),
        (Some(0L) -> Expected(Success(1L), cost = 39012,
          expectedDetails = CostDetails.ZeroCost,
          expectedNewCost = 1794)),
        (Some(Long.MaxValue) -> Expected(new ArithmeticException("long overflow")))
      ),
      changedFeature(
        scalaFunc = { (x: Option[Long]) =>
          def f(opt: Long): Long = n.plus(opt, 1)
          if (x.isDefined) f(x.get)
          else {
            f(x.get); // simulate non-lazy 'if': f is called in both branches
            5L
          }
        },
        scalaFuncNew = { (x: Option[Long]) =>
          def f(opt: Long): Long = n.plus(opt, 1)
          if (x.isDefined) f(x.get) else 5L
        },
        """{(x: Option[Long]) =>
          |  def f(opt: Long): Long = opt + 1
          |  if (x.isDefined) f(x.get) else 5L
          |}""".stripMargin,
        FuncValue(
          Vector((1, SOption(SLong))),
          If(
            OptionIsDefined(ValUse(1, SOption(SLong))),
            Apply(
              FuncValue(
                Vector((3, SLong)),
                ArithOp(ValUse(3, SLong), LongConstant(1L), OpCode @@ (-102.toByte))
              ),
              Array(OptionGet(ValUse(1, SOption(SLong))))
            ),
            LongConstant(5L)
          )
        ),
        allowNewToSucceed = true))
  }

  def formatter(costKind: PerItemCost) = (info: MeasureInfo[Coll[Byte]]) => {
    val numChunks = costKind.chunks(info.input.length)
    val timeUs = info.measuredTime / 1000
    val timePerBlock = info.measuredTime / info.nIters / numChunks
    s"case ${info.iteration}: ${timeUs} usec; numChunks: $numChunks; timePerBlock: $timePerBlock"
  }

  property("blake2b256 benchmark: to estimate timeout") {
    val cases = (1 to 10).map { i =>
      val block = Colls.fromArray(Array.fill(ErgoTreeEvaluator.DataBlockSize * i)(0.toByte))
      block
    }
    benchmarkCases(cases,
      existingFeature((x: Coll[Byte]) => SigmaDsl.blake2b256(x),
        "{ (x: Coll[Byte]) => blake2b256(x) }",
        FuncValue(Vector((1, SByteArray)), CalcBlake2b256(ValUse(1, SByteArray)))),
      nIters = 1000,
      formatter(CalcBlake2b256.costKind))
  }

  property("blake2b256, sha256 equivalence") {
    def costDetailsBlake(i: Int) = TracedCost(traceBase :+ SeqCostItem(CompanionDesc(CalcBlake2b256), PerItemCost(JitCost(20), JitCost(7), 128), i))
    def costDetailsSha(i: Int) = TracedCost(traceBase :+ SeqCostItem(CompanionDesc(CalcSha256), PerItemCost(JitCost(80), JitCost(8), 64), i))

    verifyCases(
      Seq(
        Coll[Byte]() -> Expected(
          Success(Helpers.decodeBytes("0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8")),
          36269,
          costDetailsBlake(0),
          1788
        ),
        Helpers.decodeBytes("e0ff0105ffffac31010017ff33") -> Expected(
          Success(Helpers.decodeBytes("33707eed9aab64874ff2daa6d6a378f61e7da36398fb36c194c7562c9ff846b5")),
          36269,
          costDetailsBlake(13),
          1788
        ),
        Colls.replicate(1024, 1.toByte) -> Expected(
          Success(Helpers.decodeBytes("45d8456fc5d41d1ec1124cb92e41192c1c3ec88f0bf7ae2dc6e9cf75bec22045")),
          36369,
          costDetailsBlake(1024),
          1793
        )
      ),
      existingFeature((x: Coll[Byte]) => SigmaDsl.blake2b256(x),
        "{ (x: Coll[Byte]) => blake2b256(x) }",
        FuncValue(Vector((1, SByteArray)), CalcBlake2b256(ValUse(1, SByteArray)))))

    verifyCases(
      Seq(
        Coll[Byte]() -> Expected(
          Success(Helpers.decodeBytes("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")),
          36393,
          costDetailsSha(0),
          1794
        ),
        Helpers.decodeBytes("e0ff0105ffffac31010017ff33") -> Expected(
          Success(Helpers.decodeBytes("367d0ec2cdc14aac29d5beb60c2bfc86d5a44a246308659af61c1b85fa2ca2cc")),
          36393,
          costDetailsSha(13),
          1794
        ),
        Colls.replicate(1024, 1.toByte) -> Expected(
          Success(Helpers.decodeBytes("5a648d8015900d89664e00e125df179636301a2d8fa191c1aa2bd9358ea53a69")),
          36493,
          costDetailsSha(1024),
          1806
        )
      ),
      existingFeature((x: Coll[Byte]) => SigmaDsl.sha256(x),
        "{ (x: Coll[Byte]) => sha256(x) }",
        FuncValue(Vector((1, SByteArray)), CalcSha256(ValUse(1, SByteArray)))))
  }

  property("sigmaProp equivalence") {
    val costDetails = TracedCost(traceBase :+ FixedCostItem(BoolToSigmaProp))
    verifyCases(
      Seq(
        (false, Expected(Success(CSigmaProp(TrivialProp.FalseProp)), 35892, costDetails, 1785)),
        (true, Expected(Success(CSigmaProp(TrivialProp.TrueProp)), 35892, costDetails, 1785))),
      existingFeature((x: Boolean) => sigmaProp(x),
       "{ (x: Boolean) => sigmaProp(x) }",
        FuncValue(Vector((1, SBoolean)), BoolToSigmaProp(ValUse(1, SBoolean)))))
  }

  property("atLeast equivalence") {
    def costDetails(i: Int) = TracedCost(
      traceBase ++ Array(
        FixedCostItem(SizeOf),
        FixedCostItem(Constant),
        TypeBasedCostItem(ArithOp.Minus, SInt),
        FixedCostItem(ValUse),
        SeqCostItem(CompanionDesc(AtLeast), PerItemCost(JitCost(20), JitCost(3), 5), i)
      )
    )

    verifyCases(
      Seq(
        Coll[SigmaProp](
          CSigmaProp(
            ProveDHTuple(
              Helpers.decodeECPoint("028cf1686a3275e54441c68d789151bdec40d34d62188bbadce170d96d4ce399b0"),
              Helpers.decodeECPoint("03e53afbe18efff6586f5b8bda1c3262aac65ede384f12b4a56ecb74e16d73efc5"),
              Helpers.decodeECPoint("02614b14a8c6c6b4b7ce017d72fbca7f9218b72c16bdd88f170ffb300b106b9014"),
              Helpers.decodeECPoint("034cc5572276adfa3e283a3f1b0f0028afaadeaa362618c5ec43262d8cefe7f004")
            )
          )) -> Expected(Success(CSigmaProp(TrivialProp.TrueProp)), 36462, costDetails(1), 1790),
        Coll[SigmaProp](
          CSigmaProp(
            ProveDHTuple(
              Helpers.decodeECPoint("028cf1686a3275e54441c68d789151bdec40d34d62188bbadce170d96d4ce399b0"),
              Helpers.decodeECPoint("03e53afbe18efff6586f5b8bda1c3262aac65ede384f12b4a56ecb74e16d73efc5"),
              Helpers.decodeECPoint("02614b14a8c6c6b4b7ce017d72fbca7f9218b72c16bdd88f170ffb300b106b9014"),
              Helpers.decodeECPoint("034cc5572276adfa3e283a3f1b0f0028afaadeaa362618c5ec43262d8cefe7f004")
            )
          ),
          CSigmaProp(ProveDlog(Helpers.decodeECPoint("03f7eacae7476a9ef082513a6a70ed6b208aafad0ade5f614ac6cfa2176edd0d69"))),
          CSigmaProp(ProveDlog(Helpers.decodeECPoint("023bddd50b917388cd2c4f478f3ea9281bf03a252ee1fefe9c79f800afaa8d86ad")))
        ) -> Expected(Success(
          CSigmaProp(
            CTHRESHOLD(
              2,
              Array(
                ProveDHTuple(
                  Helpers.decodeECPoint("028cf1686a3275e54441c68d789151bdec40d34d62188bbadce170d96d4ce399b0"),
                  Helpers.decodeECPoint("03e53afbe18efff6586f5b8bda1c3262aac65ede384f12b4a56ecb74e16d73efc5"),
                  Helpers.decodeECPoint("02614b14a8c6c6b4b7ce017d72fbca7f9218b72c16bdd88f170ffb300b106b9014"),
                  Helpers.decodeECPoint("034cc5572276adfa3e283a3f1b0f0028afaadeaa362618c5ec43262d8cefe7f004")
                ),
                ProveDlog(Helpers.decodeECPoint("03f7eacae7476a9ef082513a6a70ed6b208aafad0ade5f614ac6cfa2176edd0d69")),
                ProveDlog(Helpers.decodeECPoint("023bddd50b917388cd2c4f478f3ea9281bf03a252ee1fefe9c79f800afaa8d86ad"))
              )
            )
          )
        ), 36462, costDetails(3), 1893),
        Colls.replicate[SigmaProp](AtLeast.MaxChildrenCount + 1, CSigmaProp(TrivialProp.TrueProp)) ->
          Expected(new IllegalArgumentException("Expected input elements count should not exceed 255, actual: 256"))
      ),
      existingFeature((x: Coll[SigmaProp]) => SigmaDsl.atLeast(x.size - 1, x),
        "{ (x: Coll[SigmaProp]) => atLeast(x.size - 1, x) }",
        FuncValue(
          Vector((1, SCollectionType(SSigmaProp))),
          AtLeast(
            ArithOp(SizeOf(ValUse(1, SCollectionType(SSigmaProp))), IntConstant(1), OpCode @@ (-103.toByte)),
            ValUse(1, SCollectionType(SSigmaProp))
          )
        )))
  }

  property("&& sigma equivalence") {
    val testTraceBase = traceBase ++ Array(
      FixedCostItem(SelectField),
      FixedCostItem(ValUse),
      FixedCostItem(SelectField)
    )

    val costDetails1 = TracedCost(testTraceBase :+ SeqCostItem(CompanionDesc(SigmaAnd), PerItemCost(JitCost(10), JitCost(2), 1), 2))
    val costDetails2 = TracedCost(
      testTraceBase ++ Array(
        FixedCostItem(BoolToSigmaProp),
        SeqCostItem(CompanionDesc(SigmaAnd), PerItemCost(JitCost(10), JitCost(2), 1), 2)
      )
    )

    verifyCases(
      {
        def success[T](v: T, newCost: Int) = Expected(Success(v), 36428, costDetails1, newCost)
        Seq(
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("02ea9bf6da7f512386c6ca509d40f8c5e7e0ffb3eea5dc3c398443ea17f4510798"))),
              CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606")))) ->
              success(
                CSigmaProp(
                  CAND(
                    Seq(
                      ProveDlog(Helpers.decodeECPoint("02ea9bf6da7f512386c6ca509d40f8c5e7e0ffb3eea5dc3c398443ea17f4510798")),
                      ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))
                    )
                  )
                ), 1822),
          (CSigmaProp(TrivialProp.TrueProp),
              CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606")))) ->
              success(CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))), 1804),
          (CSigmaProp(TrivialProp.FalseProp),
              CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606")))) ->
              success(CSigmaProp(TrivialProp.FalseProp), 1787),
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))),
              CSigmaProp(TrivialProp.TrueProp)) ->
              success(CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))), 1804),
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))),
              CSigmaProp(TrivialProp.FalseProp)) ->
              success(CSigmaProp(TrivialProp.FalseProp), 1787)
        )
      },
      existingFeature(
        (x: (SigmaProp, SigmaProp)) => x._1 && x._2,
        "{ (x:(SigmaProp, SigmaProp)) => x._1 && x._2 }",
        FuncValue(
          Vector((1, SPair(SSigmaProp, SSigmaProp))),
          SigmaAnd(
            Seq(
              SelectField.typed[Value[SSigmaProp.type]](ValUse(1, SPair(SSigmaProp, SSigmaProp)), 1.toByte),
              SelectField.typed[Value[SSigmaProp.type]](ValUse(1, SPair(SSigmaProp, SSigmaProp)), 2.toByte)
            )
          )
        )))

    verifyCases(
      {
        Seq(
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))), true) ->
              Expected(Success(CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606")))), 36522, costDetails2, 1806),
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))), false) ->
              Expected(Success(CSigmaProp(TrivialProp.FalseProp)), 36522, costDetails2, 1789)
        )
      },
      existingFeature(
        (x: (SigmaProp, Boolean)) => x._1 && sigmaProp(x._2),
        "{ (x:(SigmaProp, Boolean)) => x._1 && sigmaProp(x._2) }",
        FuncValue(
          Vector((1, SPair(SSigmaProp, SBoolean))),
          SigmaAnd(
            Seq(
              SelectField.typed[Value[SSigmaProp.type]](ValUse(1, SPair(SSigmaProp, SBoolean)), 1.toByte),
              BoolToSigmaProp(
                SelectField.typed[Value[SBoolean.type]](ValUse(1, SPair(SSigmaProp, SBoolean)), 2.toByte)
              )
            )
          )
        )))
  }

  property("|| sigma equivalence") {
    val testTraceBase = traceBase ++ Array(
      FixedCostItem(SelectField),
      FixedCostItem(ValUse),
      FixedCostItem(SelectField)
    )

    val costDetails1 = TracedCost(testTraceBase :+ SeqCostItem(CompanionDesc(SigmaOr), PerItemCost(JitCost(10), JitCost(2), 1), 2))
    verifyCases(
      {
        def success[T](v: T, newCost: Int) = Expected(Success(v), 36494, costDetails1, newCost)
        Seq(
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("02ea9bf6da7f512386c6ca509d40f8c5e7e0ffb3eea5dc3c398443ea17f4510798"))),
              CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606")))) ->
              success(
                CSigmaProp(
                  COR(
                    Seq(
                      ProveDlog(Helpers.decodeECPoint("02ea9bf6da7f512386c6ca509d40f8c5e7e0ffb3eea5dc3c398443ea17f4510798")),
                      ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))
                    )
                  )
                ),
                1822),
          (CSigmaProp(TrivialProp.FalseProp),
              CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606")))) ->
              success(CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))), 1804),
          (CSigmaProp(TrivialProp.TrueProp),
              CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606")))) ->
              success(CSigmaProp(TrivialProp.TrueProp), 1787),
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))),
              CSigmaProp(TrivialProp.FalseProp)) ->
              success(CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))), 1804),
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))),
              CSigmaProp(TrivialProp.TrueProp)) ->
              success(CSigmaProp(TrivialProp.TrueProp), 1787)
        )
      },
      existingFeature(
        (x: (SigmaProp, SigmaProp)) => x._1 || x._2,
        "{ (x:(SigmaProp, SigmaProp)) => x._1 || x._2 }",
        FuncValue(
          Vector((1, SPair(SSigmaProp, SSigmaProp))),
          SigmaOr(
            Seq(
              SelectField.typed[SigmaPropValue](ValUse(1, SPair(SSigmaProp, SSigmaProp)), 1.toByte),
              SelectField.typed[SigmaPropValue](ValUse(1, SPair(SSigmaProp, SSigmaProp)), 2.toByte)
            )
          )
        )))

    val costDetails2 = TracedCost(
      testTraceBase ++ Array(
        FixedCostItem(BoolToSigmaProp),
        SeqCostItem(CompanionDesc(SigmaOr), PerItemCost(JitCost(10), JitCost(2), 1), 2)
      )
    )
    verifyCases(
      {
        def success[T](v: T, newCost: Int) = Expected(Success(v), 36588, costDetails2, newCost)
        Seq(
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))), false) ->
              success(CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))), 1806),
          (CSigmaProp(ProveDlog(Helpers.decodeECPoint("03a426a66fc1af2792b35d9583904c3fb877b49ae5cea45b7a2aa105ffa4c68606"))), true) ->
              success(CSigmaProp(TrivialProp.TrueProp), 1789)
        )
      },
      existingFeature(
        (x: (SigmaProp, Boolean)) => x._1 || sigmaProp(x._2),
        "{ (x:(SigmaProp, Boolean)) => x._1 || sigmaProp(x._2) }",
        FuncValue(
          Vector((1, SPair(SSigmaProp, SBoolean))),
          SigmaOr(
            Seq(
              SelectField.typed[SigmaPropValue](ValUse(1, SPair(SSigmaProp, SBoolean)), 1.toByte),
              BoolToSigmaProp(
                SelectField.typed[BoolValue](ValUse(1, SPair(SSigmaProp, SBoolean)), 2.toByte)
              )
            )
          )
        )))
  }

  property("SigmaProp.propBytes equivalence") {
    verifyCases(
      {
        def newDetails(nItems: Int) = TracedCost(traceBase :+ SeqCostItem(SigmaPropBytes, nItems))
        def pk = ProveDlog(Helpers.decodeECPoint("039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6f"))
        def dht = ProveDHTuple(
          Helpers.decodeECPoint("03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb"),
          Helpers.decodeECPoint("023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d03"),
          Helpers.decodeECPoint("03d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72"),
          Helpers.decodeECPoint("037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441")
        )
        def and = CAND(Array(pk, dht))
        def or = COR(Array(pk, dht))
        def threshold = CTHRESHOLD(2, Array(pk, dht, or, and))
        Seq(
          CSigmaProp(dht) -> Expected(Success(
            Helpers.decodeBytes(
              "0008ce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441"
            )
          ), cost = 35902, newDetails(4), expectedNewCost = 1791),
          CSigmaProp(pk) -> Expected(Success(
            Helpers.decodeBytes("0008cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6f")),
            cost = 35902, newDetails(1), expectedNewCost = 1789),
          CSigmaProp(and) -> Expected(Success(
            Helpers.decodeBytes(
              "00089602cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441"
            )
          ), cost = 35902, newDetails(6), expectedNewCost = 1792),
          CSigmaProp(threshold) -> Expected(Success(
            Helpers.decodeBytes(
              "0008980204cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b04419702cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b04419602cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441"
            )
          ), cost = 35902, newDetails(18), expectedNewCost = 1800),
          CSigmaProp(COR(Array(pk, dht, and, or, threshold))) -> Expected(Success(
            Helpers.decodeBytes(
              "00089705cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b04419602cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b04419702cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441980204cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b04419702cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b04419602cd039d0b1e46c21540d033143440d2fb7dd5d650cf89981c99ee53c6e0374d2b1b6fce03c046fccb95549910767d0543f5e8ce41d66ae6a8720a46f4049cac3b3d26dafb023479c9c3b86a0d3c8be3db0a2d186788e9af1db76d55f3dad127d15185d83d0303d7898641cb6653585a8e1dabfa7f665e61e0498963e329e6e3744bd764db2d72037ae057d89ec0b46ff8e9ff4c37e85c12acddb611c3f636421bef1542c11b0441"
            )
          ), cost = 35902, newDetails(36), expectedNewCost = 1811)
        )
      },
      existingFeature((x: SigmaProp) => x.propBytes,
        "{ (x: SigmaProp) => x.propBytes }",
        FuncValue(Vector((1, SSigmaProp)), SigmaPropBytes(ValUse(1, SSigmaProp)))),
      preGeneratedSamples = Some(Seq()))
  }

  // TODO v6.0 (3h): implement allZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  property("allZK equivalence") {
    lazy val allZK = newFeature((x: Coll[SigmaProp]) => SigmaDsl.allZK(x),
      "{ (x: Coll[SigmaProp]) => allZK(x) }")
    forAll { x: Coll[SigmaProp] =>
      allZK.checkEquality(x)
    }
  }

  // TODO v6.0 (3h): implement anyZK func https://github.com/ScorexFoundation/sigmastate-interpreter/issues/543
  property("anyZK equivalence") {
    lazy val anyZK = newFeature((x: Coll[SigmaProp]) => SigmaDsl.anyZK(x),
      "{ (x: Coll[SigmaProp]) => anyZK(x) }")
    forAll { x: Coll[SigmaProp] =>
      anyZK.checkEquality(x)
    }
  }

  property("allOf equivalence") {
    def costDetails(i: Int) = TracedCost(traceBase :+ SeqCostItem(CompanionDesc(AND), PerItemCost(JitCost(10), JitCost(5), 32), i))
    verifyCases(
      Seq(
        (Coll[Boolean]()                   -> Expected(Success(true), 36018, costDetails(0), 1785)),
        (Coll[Boolean](true)               -> Expected(Success(true), 36028, costDetails(1), 1785)),
        (Coll[Boolean](false)              -> Expected(Success(false), 36028, costDetails(1), 1785)),
        (Coll[Boolean](false, false)       -> Expected(Success(false), 36038, costDetails(1), 1785)),
        (Coll[Boolean](false, true)        -> Expected(Success(false), 36038, costDetails(1), 1785)),
        (Coll[Boolean](true, false)        -> Expected(Success(false), 36038, costDetails(2), 1785)),
        (Coll[Boolean](true, true)         -> Expected(Success(true), 36038, costDetails(2), 1785)),
        (Coll[Boolean](true, false, false) -> Expected(Success(false), 36048, costDetails(2), 1785)),
        (Coll[Boolean](true, false, true)  -> Expected(Success(false), 36048, costDetails(2), 1785)),
        (Coll[Boolean](true, true, false)  -> Expected(Success(false), 36048, costDetails(3), 1785)),
        (Coll[Boolean](true, true, true)   -> Expected(Success(true), 36048, costDetails(3), 1785))
      ),
      existingFeature((x: Coll[Boolean]) => SigmaDsl.allOf(x),
        "{ (x: Coll[Boolean]) => allOf(x) }",
        FuncValue(Vector((1, SBooleanArray)), AND(ValUse(1, SBooleanArray)))))
  }

  property("anyOf equivalence") {
    def costDetails(i: Int) = TracedCost(traceBase :+ SeqCostItem(CompanionDesc(OR), PerItemCost(JitCost(5), JitCost(5), 64), i))
    verifyCases(
      Seq(
        (Coll[Boolean]()                   -> Expected(Success(false), 36062, costDetails(0), 1784)),
        (Coll[Boolean](true)               -> Expected(Success(true), 36072, costDetails(1), 1784)),
        (Coll[Boolean](false)              -> Expected(Success(false), 36072, costDetails(1), 1784)),
        (Coll[Boolean](false, false)       -> Expected(Success(false), 36082, costDetails(2), 1784)),
        (Coll[Boolean](false, true)        -> Expected(Success(true), 36082, costDetails(2), 1784)),
        (Coll[Boolean](true, false)        -> Expected(Success(true), 36082, costDetails(1), 1784)),
        (Coll[Boolean](true, true)         -> Expected(Success(true), 36082, costDetails(1), 1784)),
        (Coll[Boolean](true, false, false) -> Expected(Success(true), 36092, costDetails(1), 1784)),
        (Coll[Boolean](true, false, true)  -> Expected(Success(true), 36092, costDetails(1), 1784)),
        (Coll[Boolean](true, true, false)  -> Expected(Success(true), 36092, costDetails(1), 1784)),
        (Coll[Boolean](true, true, true)   -> Expected(Success(true), 36092, costDetails(1), 1784))
      ),
      existingFeature((x: Coll[Boolean]) => SigmaDsl.anyOf(x),
        "{ (x: Coll[Boolean]) => anyOf(x) }",
        FuncValue(Vector((1, SBooleanArray)), OR(ValUse(1, SBooleanArray)))))
  }

  property("proveDlog equivalence") {
    val costDetails = TracedCost(traceBase :+ FixedCostItem(CreateProveDlog))
    verifyCases(
      Seq(
        (Helpers.decodeGroupElement("02288f0e55610c3355c89ed6c5de43cf20da145b8c54f03a29f481e540d94e9a69")
          -> Expected(Success(
               CSigmaProp(ProveDlog(Helpers.decodeECPoint("02288f0e55610c3355c89ed6c5de43cf20da145b8c54f03a29f481e540d94e9a69")))),
               cost = 45935,
               costDetails,
               1802))
      ),
      existingFeature({ (x: GroupElement) => SigmaDsl.proveDlog(x) },
        "{ (x: GroupElement) => proveDlog(x) }",
        FuncValue(Vector((1, SGroupElement)), CreateProveDlog(ValUse(1, SGroupElement)))))
  }

  property("proveDHTuple equivalence") {
    val costDetails = TracedCost(
      traceBase ++ Array(
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        FixedCostItem(ValUse),
        FixedCostItem(CreateProveDHTuple)
      )
    )
    verifyCases(
      Seq(
        (Helpers.decodeGroupElement("039c15221a318d27c186eba84fa8d986c1f63bbd9f8060380c9bfc2ef455d8346a")
          -> Expected(Success(
            CSigmaProp(
              ProveDHTuple(
                Helpers.decodeECPoint("039c15221a318d27c186eba84fa8d986c1f63bbd9f8060380c9bfc2ef455d8346a"),
                Helpers.decodeECPoint("039c15221a318d27c186eba84fa8d986c1f63bbd9f8060380c9bfc2ef455d8346a"),
                Helpers.decodeECPoint("039c15221a318d27c186eba84fa8d986c1f63bbd9f8060380c9bfc2ef455d8346a"),
                Helpers.decodeECPoint("039c15221a318d27c186eba84fa8d986c1f63bbd9f8060380c9bfc2ef455d8346a")
              )
            )),
            cost = 76215,
            costDetails,
            1856
          ))
      ),
      existingFeature({ (x: GroupElement) => SigmaDsl.proveDHTuple(x, x, x, x) },
        "{ (x: GroupElement) => proveDHTuple(x, x, x, x) }",
        FuncValue(
          Vector((1, SGroupElement)),
          CreateProveDHTuple(
            ValUse(1, SGroupElement),
            ValUse(1, SGroupElement),
            ValUse(1, SGroupElement),
            ValUse(1, SGroupElement)
          )
        )))
  }

  property("substConstants equivalence") {
    // tree without constant segregation
    val t1 = ErgoTree(ErgoTree.DefaultHeader, Vector(), TrueSigmaProp)
    // tree with constant segregation, but without constants
    val t2 = ErgoTree(ErgoTree.ConstantSegregationHeader, Vector(), TrueSigmaProp)
    // tree with one segregated constant
    val t3 = ErgoTree(ErgoTree.ConstantSegregationHeader, Vector(TrueSigmaProp), ConstantPlaceholder(0, SSigmaProp))
    // tree with one segregated constant of different type
    val t4 = ErgoTree(
      ErgoTree.ConstantSegregationHeader,
      Vector(IntConstant(10)),
      BoolToSigmaProp(EQ(ConstantPlaceholder(0, SInt), IntConstant(20))))
    
    verifyCases(
      {
        def success[T](v: T) = Expected(Success(v), 37694)
        Seq(
          (Helpers.decodeBytes(""), 0) -> Expected(new java.nio.BufferUnderflowException()),

          // NOTE: in v4.x the constants count is serialized erroneously in the following 2 cases
          // in v5.0 (i.e. ET v2) the bug is fixed https://github.com/ScorexFoundation/sigmastate-interpreter/issues/769
          (Coll(t1.bytes:_*), 0) -> Expected(
            Success(Helpers.decodeBytes("000008d3")),
            cost = 37694,
            expectedDetails = CostDetails.ZeroCost,
            newCost = 1803,
            newVersionedResults = {
              val res = (ExpectedResult(Success(Helpers.decodeBytes("0008d3")), Some(1803)) -> None)
              Seq(0, 1, 2).map(version => version -> res)
            }),

          (Helpers.decodeBytes("000008d3"), 0) -> Expected(
            Success(Helpers.decodeBytes("00000008d3")),
            cost = 37694,
            expectedDetails = CostDetails.ZeroCost,
            newCost = 1803,
            newVersionedResults = {
              // since the tree without constant segregation, substitution has no effect
              val res = (ExpectedResult(Success(Helpers.decodeBytes("000008d3")), Some(1803)) -> None)
              Seq(0, 1, 2).map(version => version -> res)
            }),
          // tree with segregation flag, empty constants array
          (Coll(t2.bytes:_*), 0) -> success(Helpers.decodeBytes("100008d3")),
          (Helpers.decodeBytes("100008d3"), 0) -> success(Helpers.decodeBytes("100008d3")),
          // tree with one segregated constant
          (Coll(t3.bytes:_*), 0) -> success(Helpers.decodeBytes("100108d27300")),
          (Helpers.decodeBytes("100108d37300"), 0) -> success(Helpers.decodeBytes("100108d27300")),
          (Coll(t3.bytes:_*), 1) -> success(Helpers.decodeBytes("100108d37300")),
          (Coll(t4.bytes:_*), 0) -> Expected(new IllegalArgumentException("requirement failed: expected new constant to have the same SInt$ tpe, got SSigmaProp"))
        )
      },
      changedFeature(
        { (x: (Coll[Byte], Int)) =>
          SigmaDsl.substConstants(x._1, Coll[Int](x._2), Coll[Any](SigmaDsl.sigmaProp(false))(RType.AnyType))
        },
        { (x: (Coll[Byte], Int)) =>
          SigmaDsl.substConstants(x._1, Coll[Int](x._2), Coll[Any](SigmaDsl.sigmaProp(false))(RType.AnyType))
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
        )))
  }

  /* Original issue: https://github.com/ScorexFoundation/sigmastate-interpreter/issues/604
   *
   * Test fails for v4.x with
   * `new StagingException("Don't know how to evaluate(s1252 -> Placeholder(BaseElemLiftable<Int>))")`
   * but that cannot be explicitly stated in cases. Therefore we use different exception (None.get)
   * and also `allowDifferentErrors` flag.
   * Test passes for v5.x so we use flag `allowNewToSucceed`.
   */
  property("Random headers access and comparison (originaly from spam tests)") {
    val (_, _, _, ctx, _, _) = contextData()
    val costDetails = TracedCost(
      Array(
        FixedCostItem(Apply),
        FixedCostItem(FuncValue),
        FixedCostItem(GetVar),
        FixedCostItem(OptionGet),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        SeqCostItem(CompanionDesc(BlockValue), PerItemCost(JitCost(1), JitCost(1), 10), 1),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        FixedCostItem(SContext.headersMethod, FixedCost(JitCost(15))),
        FixedCostItem(FuncValue.AddToEnvironmentDesc, FixedCost(JitCost(5))),
        FixedCostItem(ValUse),
        FixedCostItem(PropertyCall),
        SeqCostItem(MethodDesc(SCollection.IndicesMethod), PerItemCost(JitCost(20), JitCost(2), 16), 1),
        FixedCostItem(Constant),
        FixedCostItem(ValUse),
        FixedCostItem(SizeOf),
        FixedCostItem(Constant),
        TypeBasedCostItem(ArithOp.Minus, SInt),
        SeqCostItem(CompanionDesc(Slice), PerItemCost(JitCost(10), JitCost(2), 100), 0),
        FixedCostItem(FuncValue),
        SeqCostItem(CompanionDesc(ForAll), PerItemCost(JitCost(3), JitCost(1), 10), 0)
      )
    )

    if (lowerMethodCallsInTests) {
      verifyCases(
        Seq(
          ctx -> Expected(
            Failure(new NoSuchElementException("None.get")),
            cost = 37694,
            expectedDetails = CostDetails.ZeroCost,
            newCost = 1796,
            newVersionedResults = (0 to 2).map(i => i -> (ExpectedResult(Success(true), Some(1796)) -> Some(costDetails)))
          )
        ),
        changedFeature(
        { (x: Context) =>
          Option.empty[Int].get
          true
        },
        { (x: Context) =>
          val headers = x.headers
          val ids = headers.map({ (h: Header) => h.id })
          val parentIds = headers.map({ (h: Header) => h.parentId })
          headers.indices.slice(0, headers.size - 1).forall({ (i: Int) =>
            val parentId = parentIds(i)
            val id = ids(i + 1)
            parentId == id
          })
        },
        """{
         |(x: Context) =>
         |  val headers = x.headers
         |  val ids = headers.map({(h: Header) => h.id })
         |  val parentIds = headers.map({(h: Header) => h.parentId })
         |  headers.indices.slice(0, headers.size - 1).forall({ (i: Int) =>
         |    val parentId = parentIds(i)
         |    val id = ids(i + 1)
         |    parentId == id
         |  })
         |}""".stripMargin,
        FuncValue(
          Array((1, SContext)),
          BlockValue(
            Array(
              ValDef(
                3,
                List(),
                MethodCall.typed[Value[SCollection[SHeader.type]]](
                  ValUse(1, SContext),
                  SContext.getMethodByName("headers"),
                  Vector(),
                  Map()
                )
              )
            ),
            ForAll(
              Slice(
                MethodCall.typed[Value[SCollection[SInt.type]]](
                  ValUse(3, SCollectionType(SHeader)),
                  SCollection.getMethodByName("indices").withConcreteTypes(Map(STypeVar("IV") -> SHeader)),
                  Vector(),
                  Map()
                ),
                IntConstant(0),
                ArithOp(
                  SizeOf(ValUse(3, SCollectionType(SHeader))),
                  IntConstant(1),
                  OpCode @@ (-103.toByte)
                )
              ),
              FuncValue(
                Array((4, SInt)),
                EQ(
                  ByIndex(
                    MapCollection(
                      ValUse(3, SCollectionType(SHeader)),
                      FuncValue(
                        Array((6, SHeader)),
                        MethodCall.typed[Value[SCollection[SByte.type]]](
                          ValUse(6, SHeader),
                          SHeader.getMethodByName("parentId"),
                          Vector(),
                          Map()
                        )
                      )
                    ),
                    ValUse(4, SInt),
                    None
                  ),
                  ByIndex(
                    MapCollection(
                      ValUse(3, SCollectionType(SHeader)),
                      FuncValue(
                        Array((6, SHeader)),
                        MethodCall.typed[Value[SCollection[SByte.type]]](
                          ValUse(6, SHeader),
                          SHeader.getMethodByName("id"),
                          Vector(),
                          Map()
                        )
                      )
                    ),
                    ArithOp(ValUse(4, SInt), IntConstant(1), OpCode @@ (-102.toByte)),
                    None
                  )
                )
              )
            )
          )
        ),
        allowDifferentErrors = true,
        allowNewToSucceed = true
        ),
        preGeneratedSamples = Some(mutable.WrappedArray.empty)
      )
    }
  }

  // related issue https://github.com/ScorexFoundation/sigmastate-interpreter/issues/464
  property("nested loops: map inside fold") {
    val keys = Colls.fromArray(Array(Coll[Byte](1, 2, 3, 4, 5)))
    val initial = Coll[Byte](0, 0, 0, 0, 0)
    val cases =  Seq(
      (keys, initial) -> Expected(Success(Coll[Byte](1, 2, 3, 4, 5)), cost = 46522, expectedDetails = CostDetails.ZeroCost, 1821)
    )
    val scalaFunc = { (x: (Coll[Coll[Byte]], Coll[Byte])) =>
      x._1.foldLeft(x._2, { (a: (Coll[Byte], Coll[Byte])) =>
        a._1.zip(a._2).map({ (c: (Byte, Byte)) => (c._1 + c._2).toByte })
      })
    }
    val script =
      """{
       | (x: (Coll[Coll[Byte]], Coll[Byte])) =>
       |  x._1.fold(x._2, { (a: Coll[Byte], b: Coll[Byte]) =>
       |    a.zip(b).map({ (c: (Byte, Byte)) => (c._1 + c._2).toByte })
       |  })
       |}""".stripMargin
    if (lowerMethodCallsInTests) {
      verifyCases(cases,
        existingFeature(scalaFunc, script,
          FuncValue(
            Array((1, SPair(SByteArray2, SByteArray))),
            Fold(
              SelectField.typed[Value[SCollection[SCollection[SByte.type]]]](
                ValUse(1, SPair(SByteArray2, SByteArray)),
                1.toByte
              ),
              SelectField.typed[Value[SCollection[SByte.type]]](
                ValUse(1, SPair(SByteArray2, SByteArray)),
                2.toByte
              ),
              FuncValue(
                Array((3, SPair(SByteArray, SByteArray))),
                MapCollection(
                  MethodCall.typed[Value[SCollection[STuple]]](
                    SelectField.typed[Value[SCollection[SByte.type]]](
                      ValUse(3, SPair(SByteArray, SByteArray)),
                      1.toByte
                    ),
                    SCollection.getMethodByName("zip").withConcreteTypes(
                      Map(STypeVar("IV") -> SByte, STypeVar("OV") -> SByte)
                    ),
                    Vector(
                      SelectField.typed[Value[SCollection[SByte.type]]](
                        ValUse(3, SPair(SByteArray, SByteArray)),
                        2.toByte
                      )
                    ),
                    Map()
                  ),
                  FuncValue(
                    Array((5, SPair(SByte, SByte))),
                    ArithOp(
                      SelectField.typed[Value[SByte.type]](ValUse(5, SPair(SByte, SByte)), 1.toByte),
                      SelectField.typed[Value[SByte.type]](ValUse(5, SPair(SByte, SByte)), 2.toByte),
                      OpCode @@ (-102.toByte)
                    )
                  )
                )
              )
            )
          )
        ),
        preGeneratedSamples = Some(Seq.empty)
      )
    } else {
      assertExceptionThrown(
        verifyCases(cases,
          existingFeature(scalaFunc, script)
        ),
        rootCauseLike[CosterException]("Don't know how to evalNode(Lambda(List(),Vector((a,Coll[SByte$]), ")
      )
    }
  }

  override protected def afterAll(): Unit = {
    println(ErgoTreeEvaluator.DefaultProfiler.generateReport)
    println("==========================================================")
    println(Interpreter.verifySignatureProfiler.generateReport)
  }
}
