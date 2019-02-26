package scalan

private class TestSigmaLibrary extends SigmaLibrary {
  import TestSigmaDslBuilder._
  lazy val sigmaDslBuilder = RTestSigmaDslBuilder()
}

class SigmaLibraryTests extends BaseCtxTests {

  test("Benchmark SigmaLibrary creation time") {
    new Benchmark(new TestSigmaLibrary).run()
  }
}

object MeasureSigmaLibraryCreate extends App {
  new Benchmark(new TestSigmaLibrary).run()
}

