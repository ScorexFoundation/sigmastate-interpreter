package special.sigma

import java.math.BigInteger

import org.ergoplatform.SigmaConstants.ScriptCostLimit
import sigmastate.EQ
import sigmastate.Values.ErgoTree
import sigmastate.eval.CBigInt
import sigmastate.interpreter.ErgoTreeEvaluator.{DefaultProfiler, DefaultEvalSettings}
import sigmastate.interpreter.{CostAccumulator, ErgoTreeEvaluator}

import scala.util.Success

class EqualitySpecification extends SigmaTestingData {
  import TestData._

  def createEvaluator() = {
    val accumulator = new CostAccumulator(initialCost = 0, Some(ScriptCostLimit.value))
    val evaluator = new ErgoTreeEvaluator(
      context = null,
      constants = ErgoTree.EmptyConstants,
      coster = accumulator, DefaultProfiler, DefaultEvalSettings)
    evaluator
  }

  /** Checks (on positive cases) that EQ.equalDataValues used in v5.0 is equivalent to
    * `==` used in v4.0 */
  def check(x: Any, y: Any, expected: Boolean) = {
    val evaluator = createEvaluator()
    withClue(s"EQ.equalDataValues($x, $y)") {
      val res = sameResultOrError(
        EQ.equalDataValues(x, y)(evaluator),
        x == y)
      res match {
        case Success(res) => res shouldBe expected
        case _ =>
      }
    }
  }

  val zeros = Array[Any](0.toByte, 0.toShort, 0, 0.toLong)
  val ones = Array[Any](1.toByte, 1.toShort, 1, 1.toLong)

  /** This is not comprehensive list of positive checks.
    * See also DataSerializerSpecification.roundtrip where comprehensive
    * checking of positive cases is done. */
  property("EQ.equalDataValues positive cases") {
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

  property("EQ.equalDataValues positive cases (Coll)") {
    val empty1 = Coll[Int]()
    val empty2 = Coll[Int]()
    check(empty1, empty2, true)
    val single1 = Coll[Int](1)
    val single2 = Coll[Int](1)
    check(single1, single2, true)
  }

  property("EQ.equalDataValues negative cases") {
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

  property("EQ.equalDataValues negative cases (in Coll)") {
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

}
