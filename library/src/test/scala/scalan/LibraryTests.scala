package scalan

import scalan.util.BenchmarkUtil._

class Benchmark[T <: Scalan](createContext: => T) {
  def run() = {
    val (ctx, total) = measureTime {
      var ctx = createContext
      measure(100000, false) { i =>
        ctx = createContext
      }
      ctx
    }
    println(s"Def count: ${ctx.defCount}, total: $total msec")
  }
}

class LibraryTests extends BaseCtxTests {
  test("Benchmark Library creation time") {
    new Benchmark(new TestLibrary {}).run()
  }
}

object MeasureLibraryCreate extends App {
  new Benchmark(new TestLibrary {}).run()
}
