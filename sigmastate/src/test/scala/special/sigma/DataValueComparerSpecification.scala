package special.sigma

import java.math.BigInteger

import org.ergoplatform.SigmaConstants.ScriptCostLimit
import org.scalatest.BeforeAndAfterAll
import scalan.util.BenchmarkUtil
import sigmastate.DataValueComparer
import sigmastate.Values.ErgoTree
import sigmastate.eval.{CBigInt, Profiler}
import sigmastate.helpers.SigmaPPrint
import sigmastate.interpreter.{TracedCost, ErgoTreeEvaluator, CostAccumulator, EvalSettings, CostItem}

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
  override val nBenchmarkIters = 5000

  val profiler = new Profiler

  import TestData._

  def createEvaluator(settings: EvalSettings): ErgoTreeEvaluator = {
    val accumulator = new CostAccumulator(initialCost = 0, Some(ScriptCostLimit.value))
    val evaluator = new ErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator, profiler, settings)
    evaluator
  }

  /** Checks (on positive cases) that EQ.equalDataValues used in v5.0 is equivalent to
    * `==` used in v4.0 */
  def check(x: Any, y: Any, expected: Boolean)(implicit settings: EvalSettings) = {
    val evaluator = createEvaluator(settings)
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
      val evaluator = createEvaluator(settings)
      val (res, actualTime) = BenchmarkUtil.measureTimeNano(
        Try(DataValueComparer.equalDataValues(x, y)(evaluator)))
      if (res.isSuccess) {
        val costDetails = TracedCost(evaluator.costTrace, Some(actualTime))
        val xStr = SigmaPPrint(x).plainText
        val yStr = SigmaPPrint(y).plainText
        val script = s"$xStr == $yStr"
        evaluator.profiler.addEstimation(script, costDetails.cost, actualTime)
      }
    }

  }

  val zeros = Array[Any](0.toByte, 0.toShort, 0, 0.toLong)
  val ones = Array[Any](1.toByte, 1.toShort, 1, 1.toLong)

  override protected def beforeAll(): Unit = {
    // warm up DataValueComparer
    repeatAndReturnLast(nIters = 20000 / nBenchmarkIters) {
      runPosititveCases(
        evalSettings = evalSettings.copy(
          isMeasureOperationTime = false,
          isMeasureScriptTime = false,
          costTracingEnabled = true))
    }
  }

  /** This is not comprehensive list of positive checks.
    * See also DataSerializerSpecification.roundtrip where comprehensive
    * checking of positive cases is done.
    * This method also used to warm up DataValueComparer. */
  def runPosititveCases(implicit evalSettings: EvalSettings) = {
    ones.foreach { x =>
      ones.foreach { y =>
        check(x, y, true)  // numeric values are equal regardless of their type
        check(Option(x), Option(y), true)  // numeric values in Option
        check((x, 1), (y, 1), true)        // and in Tuple
        check((1, x), (1, y), true)
      }
    }
    check(BigIntZero, CBigInt(new BigInteger("0", 16)), true)
  }

  property("equalDataValues positive cases") {
    runPosititveCases(evalSettings)
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
    println(profiler.generateReport)
  }

}
