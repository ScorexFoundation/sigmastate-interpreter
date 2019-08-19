package special.collections

import org.scalameter.KeyValue
import org.scalameter.api.{Gen, Bench, _}

trait BenchmarkGens extends CollGens { suite: Bench[Double] =>
  val maxSize = 100000

  val sizes = Gen.exponential("size")(10, maxSize, 10)

  val ranges = for { size <- sizes } yield (0 until size, maxSize / size)

  val arrays = ranges.map { case (r, i) => (r.toArray, i) }

  val colls = arrays.map { case (arr, i) => (builder.fromArray(arr), i) }

  private val config = Seq[KeyValue](
    exec.minWarmupRuns -> 5,
    exec.maxWarmupRuns -> 10,
    exec.benchRuns -> 30,
    exec.requireGC -> true
  )
}
