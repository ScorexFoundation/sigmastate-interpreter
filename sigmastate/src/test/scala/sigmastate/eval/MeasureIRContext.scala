package sigmastate.eval

import scalan.{BaseCtxTests, Benchmark}
import scalan.util.BenchmarkUtil.measure

object MeasureIRContext extends App {
  var ctx: RuntimeIRContext = null
  measure(1, false) { i =>
    ctx = new RuntimeIRContext
  }
  measure(10000, false) { i =>
    ctx = new RuntimeIRContext
  }
  println(s"Def count: ${ctx.defCount}")
  /*
  Total time: 2485 ms
  Total time: 2714 ms
  Def count: 20
  */
}

class SigmaLibraryTests extends BaseCtxTests {

  test("Benchmark SigmaLibrary creation time") {
    new Benchmark(new RuntimeIRContext).run()
  }
}

object MeasureSigmaLibraryCreate extends App {
  new Benchmark(new RuntimeIRContext).run()
  /*
  Total time: 12932 ms
  Def count: 20, total: 15774 msec
  */
}

