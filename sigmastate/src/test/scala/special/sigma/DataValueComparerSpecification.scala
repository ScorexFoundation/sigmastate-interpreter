package special.sigma

import org.ergoplatform.SigmaConstants.ScriptCostLimit
import org.scalatest.BeforeAndAfterAll
import scalan.util.BenchmarkUtil
import sigmastate.DataValueComparer
import sigmastate.Values.ErgoTree
import sigmastate.eval.Profiler
import sigmastate.helpers.SigmaPPrint
import sigmastate.interpreter.{TracedCost, ErgoTreeEvaluator, CostAccumulator, EvalSettings}

import scala.util.{Success, Try}

class DataValueComparerSpecification extends SigmaDslTesting
    with BeforeAndAfterAll { suite =>

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 30)

  implicit override val evalSettings: EvalSettings =
    ErgoTreeEvaluator.DefaultEvalSettings.copy(
    isMeasureOperationTime = true,
    isMeasureScriptTime = true,
    isLogEnabled = false, // don't commit the `true` value (CI log is too high)
    costTracingEnabled = true  // should always be enabled in tests (and false by default)
  )
  override val nBenchmarkIters = 50

  implicit val suiteProfiler = new Profiler

  import TestData._

  def createEvaluator(settings: EvalSettings, profiler: Profiler): ErgoTreeEvaluator = {
    val accumulator = new CostAccumulator(initialCost = 0, Some(ScriptCostLimit.value))
    val evaluator = new ErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator, profiler, settings)
    evaluator
  }

  /** Checks (on positive cases) that EQ.equalDataValues used in v5.0 is equivalent to
    * `==` used in v4.0
    * @param x computation which produced first argument
    * @param y computation which produced second argument
    */
  def check(x: => Any, y: => Any, expected: Boolean)(implicit settings: EvalSettings, profiler: Profiler) = {
    val evaluator = createEvaluator(settings, profiler)
    withClue(s"EQ.equalDataValues($x, $y)") {
      val res = sameResultOrError(
        repeatAndReturnLast(nBenchmarkIters) {
          DataValueComparer.equalDataValues(x, y)(evaluator)
        },
        x == y)
      res match {
        case Success(res) => res shouldBe expected
        case _ =>
      }
    }
    if (evaluator.settings.isMeasureScriptTime) {
      val evaluator = createEvaluator(settings, profiler)
      val _x = x
      val _y = y
      val (res, actualTime) = BenchmarkUtil.measureTimeNano {
        Try(DataValueComparer.equalDataValues(_x, _y)(evaluator))
      }
      if (res.isSuccess) {
        val costDetails = TracedCost(evaluator.costTrace, Some(actualTime))
        val xStr = SigmaPPrint(_x).plainText
        val yStr = SigmaPPrint(_y).plainText
        val script = s"$xStr == $yStr"
        evaluator.profiler.addEstimation(script, costDetails.cost, actualTime)
      }
    }

  }

  /** It is important for profiling to return a new array on every method call. */
  def zeros = Array[Any](0.toByte, 0.toShort, 0, 0.toLong)
  def ones = Array[Any](1.toByte, 1.toShort, 1, 1.toLong)

  override protected def beforeAll(): Unit = {
    // warm up DataValueComparer
    val warmUpProfiler = new Profiler
    repeatAndReturnLast(nIters = 50000 / nBenchmarkIters) {
      runBaseCases(warmUpProfiler)(evalSettings = evalSettings.copy(isLogEnabled = false))
    }
    System.gc()
    Thread.sleep(1000)
  }

  /** This is NOT comprehensive list of possible checks.
    * See also DataSerializerSpecification.roundtrip where comprehensive
    * checking of positive cases is done.
    * This method also used to warm up DataValueComparer in the beforeAll method. */
  def runBaseCases(profiler: Profiler)(implicit evalSettings: EvalSettings) = {
    implicit val suiteProfiler = profiler  // hide suite's profiler and use explicitly passed
    ones.foreach { x =>
      ones.foreach { y =>
        check(x, y, true)  // numeric values are equal regardless of their type
        check(Option(x), Option(y), true)  // numeric values in Option
        check(Option(x), y, false)
        check(Option(x), None, false)
        check(None, Option(x), false)
        check((x, 1), (y, 1), true)        // and in Tuple
        check((1, x), (1, y), true)
        check((1, x), y, false)
      }
    }
    check(createBigIntMaxValue(), createBigIntMaxValue(), true)
    check(create_ge1(), create_ge1(), true)
    check(create_t1, create_t1(), true)
    check(create_b1(), create_b1(), true)
    check(create_preH1(), create_preH1(), true)
    check(create_h1(), create_h1(), true)
  }

  property("equalDataValues base cases") {
    runBaseCases(suiteProfiler)(evalSettings)
  }

  property("equalDataValues positive cases (Coll)") {
    val empty1 = Coll[Int]()
    val empty2 = Coll[Int]()
    check(empty1, empty2, true)
    val single1 = Coll[Int](1)
    val single2 = Coll[Int](1)
    check(single1, single2, true)
  }

  property("equalDataValues negative cases") {
    check(BigIntZero, BigIntOne, false)
    check(ge1, ge2, false)
    check(t1, t2, false)
    check(b1, b2, false)
    check(preH1, preH2, false)
    check(h1, h2, false)

    ones.foreach { x =>
      zeros.foreach { y =>
        check(x, y, false)
      }
    }
    ones.foreach { x =>
      check(BigIntOne, x, false)
    }
    val values = Array[AnyRef](
      1.asInstanceOf[AnyRef], BigIntOne, ge1, t1, b1, preH1, h1,
      Coll[Int](1), None, Option(1), (1, 1))
    values.foreach { x =>
      values.foreach { y =>
        if (!(x eq y)) {
          check(x, y, false)
          check(y, x, false)
        }
      }
    }
  }

  property("equalDataValues negative cases (in Coll)") {
    val onesInColl = Array[AnyRef](Coll(1.toByte), Coll(1.toShort), Coll(1), Coll(1.toLong))
    onesInColl.foreach { x =>
      onesInColl.foreach { y =>
        if (!(x eq y)) {
          check(x, y, false)
          check(y, x, false)
        }
      }
    }
    check(Coll(1), Coll(1, 2), false)
  }

  override protected def afterAll(): Unit = {
    println(suiteProfiler.generateReport)
  }

}
