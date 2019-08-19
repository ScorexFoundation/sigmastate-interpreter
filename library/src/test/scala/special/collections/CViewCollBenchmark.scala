package special.collections

import org.scalameter.{execution, Executor}
import org.scalameter.api._
import org.scalameter.picklers.Implicits._
import special.collection.Coll
import special.collection.ExtensionMethods._
import spire.syntax.all._

trait CViewCollBenchmarkCases extends CollGens { suite: Bench[Double] =>
  val sizes = Gen.exponential("size")(10, 100000, 10)

  val ranges = for { size <- sizes } yield (0 until size, 100000 / size)

  val arrays = ranges.map { case (r, i) => (r.toArray, i) }

  val colls = arrays.map { case (arr, i) => (builder.fromArray(arr), i) }

  val incColls = arrays.map { case (arr, i) => (builder.makeView(builder.fromArray(arr), identity[Int]), i) }

  performance of "map" in {
    measure method "of CViewColl" in {
      using(incColls) in { case (coll, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          coll.map(inc)
        }

      }
    }
    measure method "of Coll" in {
      using(colls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.map(inc)
        }
      }
    }
  }
  performance of "map creation" in {
    measure method "of CViewColl" in {
      using(colls) in { case (coll, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          builder.makeView(coll, inc)
        }

      }
    }
    measure method "of Coll" in {
      using(colls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          c.map(inc)
        }
      }
    }
  }
  performance of "map usage" in {

    measure method "of CViewColl" in {
      using(colls) in { case (coll, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          val view = builder.makeView[Int, Int](coll, x => x * 10)
          var sum = 0
          cfor(0)(_ < view.length, _ + 2) { i =>
            sum += view(i)
          }
        }

      }
    }
    measure method "of Coll" in {
      using(colls) in { case (c, n) =>
        cfor(0)(_ < n, _ + 1) { _ =>
          val coll = c.map(x => x * 10)
          var sum = 0
          cfor(0)(_ < c.length, _ + 2) { i =>
            sum += c(i)
          }
        }
      }
    }
  }
  // TODO: make more performance tests
}

object FastCViewCollBenchmark extends Bench.LocalTime with CViewCollBenchmarkCases {
}

object CViewCollBenchmark extends Bench.OfflineRegressionReport with CViewCollBenchmarkCases {
  override def executor: Executor[Double] = new execution.LocalExecutor(
    warmer,
    aggregator,
    measurer
  )
}


