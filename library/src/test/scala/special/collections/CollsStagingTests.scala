package special.collections

import scala.language.reflectiveCalls
import special.wrappers.WrappersTests
import scalan._
import scalan.util.BenchmarkUtil._

class CollsStagingTests extends WrappersTests {
  class Ctx extends TestContext with TestLibrary {
    import Coll._
    import CollBuilder._
    lazy val t2 = fun { (c: Ref[Coll[Double]]) =>
      c.map(fun { x => x + 1.0 })
    }
    lazy val t3 = fun { (x: Ref[Int]) =>
      val b = colBuilder
      b.fromItems(x, x + 1, x + 2)
    }
  }

  test("Coll methods") {
    val ctx = new Ctx {
      def test() = {
//        { val Def(Lambda(_, _, x, RColOverArray(M.map(in, _)))) = t2; assert(in == x) }
      }
    }
    ctx.test()
    ctx.emit("t2", ctx.t2)
    ctx.emit("t3", ctx.t3)
  }

  test("measure: build graph and resetContext") {
    val ctx = new Ctx {
      useAlphaEquality = false
    }
    import ctx._
    import Coll._
    import CollBuilder._
//    import CollOverArrayBuilder._

    var res: Sym = null
    val nIters = 10
    measure(nIters) { i =>
      var sum: Int = 0
      for (j <- 0 until 3000) {
        val col = colBuilder.replicate(i*j, 0)
        res = col.map(fun {x => x + 1})
        sum += ctx.defCount
      }
      println(s"Defs: ${ctx.defCount}")
      if (i == nIters - 1) emit("res", res)
      ctx.resetContext()
    }
  }

  test("measure: build graph with new context") {
    measure(10) { i =>
      var sum: Int = 0
      for (j <- 0 until 3000) {
        val ctx = new Ctx {
          useAlphaEquality = false
        }
        import ctx._
        import Coll._
        import CollBuilder._
        val col = colBuilder.replicate(i*j, 0)
        val res = col.map(fun {x => x + 1})
        sum += ctx.defCount
      }
      println(s"Defs: ${sum}")
    }
  }

  def runMeasure(nRepeats: Int, name: String, alphaEq: Boolean, keepOrig: Boolean, unfoldWithOrig: Boolean) = {
    println(s"runMeasure($name, alphaEq = $alphaEq, keepOrig = $keepOrig, unfoldWithOrig = $unfoldWithOrig)")
    val nIters = 10
    def warmUp(i: Int) = {
      val ctx = new Ctx {
        useAlphaEquality = alphaEq
        keepOriginalFunc = keepOrig
        unfoldWithOriginalFunc = unfoldWithOrig
      }
      import ctx._
      import Coll._
      import CollBuilder._
      var outGraph: Sym = null
      for (j <- 0 until nRepeats) {
        val f = fun { in: Ref[(CollBuilder, Int)] =>
          val Pair(colBuilder, delta) = in
          val col = colBuilder.replicate(i*j, 0)
          val res = col.map(fun {x => x + delta})
          res
        }
        outGraph = Pair(f, f(Pair(colBuilder, 1)))
      }
    }
    def measureUp(i: Int) = {
      val ctx = new Ctx {
        useAlphaEquality = alphaEq
        keepOriginalFunc = keepOrig
        unfoldWithOriginalFunc = unfoldWithOrig
      }
      import ctx._
      import Coll._
      import CollBuilder._
      var outGraph: Sym = null
      for (j <- 0 until nRepeats) {
        val f = fun { in: Ref[(CollBuilder, Int)] =>
          val Pair(colBuilder, delta) = in
          val col = colBuilder.replicate(i*j, delta)
          val col2 = colBuilder.replicate(j+i, delta)
          val res = col.map(fun {x => x + delta}).zip(col2)
          res
        }
        outGraph = Pair(f, f(Pair(colBuilder, 1)))
      }
      println(s"Defs: ${ctx.defCount}")

      if (i == nIters - 1)
        emit(name, outGraph)
    }
    measure(nIters)(warmUp)
    System.gc()
    measure(nIters)(measureUp)
  }

  test("measure: unfoldLambda") {
    val dummyCtx = new Ctx  // to force class loading
    runMeasure(100, "default", true, true, true)
    runMeasure(1000, "noAlpha", false, true, true)
    runMeasure(1000, "noAlpha_noKeepOrig", false, false, true)
  }

  test("invokeTransformedAdapterMethodCall") {
    val ctx = new Ctx {
      useAlphaEquality = true
      keepOriginalFunc = false
    }
    import ctx._
    import Coll._
    val f = fun { col: Ref[Coll[Int]] =>  col.length }
    val g = fun { col: Ref[Coll[Int]] => f(col) }
    val exp = fun { col: Ref[Coll[Int]] => col.length }
    emit("graphs", f, g, exp)
    g shouldBe exp
  }

  test("invokeUnlifted for Col") {
    val ctx = new WrappersCtx with Library
    import ctx._
    import Liftables._
    import Coll._
    import CollBuilder._
    import EnvRep._

    val Cols: SCollBuilder = new special.collection.CollOverArrayBuilder
    val arr = Array(1, 2, 3)
    val col = Cols.fromArray(arr)

    check(col, { env: EnvRep[Coll[Int]] => for {xs <- env; arg <- lifted(2) } yield xs.apply(arg) }, col.apply(2))

    val inc = (x: Int) => x + 1
    check(col, { env: EnvRep[Coll[Int]] => for { xs <- env; incL <- lifted(inc) } yield xs.map(incL) }, col.map(inc))

//    check(Cols, { env: EnvRep[CollBuilder] => for { b <- env; arrL <- lifted(arr) } yield b.fromArray(arrL) }, Cols.fromArray(arr))

    measure(10) { i =>
      (1 to 100).foreach { j =>
        check(Cols,
          {env: EnvRep[CollBuilder] => for {
            b <- env; x1 <- lifted(1); x2 <- lifted(j); x3 <- lifted(i)
          } yield b.fromItems(x1, x2, x3) },
          Cols.fromItems(1, j, i))
      }
      println(s"Defs: ${ctx.defCount}")
    }
  }

  test("invokeUnlifted for method of Ctor") {
    val ctx = new WrappersCtx with Library
    import ctx._
    import Liftables._
    import Coll._
    import CollBuilder._
    import EnvRep._

    val Cols: SCollBuilder = new special.collection.CollOverArrayBuilder
    val colData = Cols.replicate(10, 10)
    val colSym = colBuilder.replicate(10, 10)
    val resSym = colSym.append(colSym)
    val resData = colData.append(colData)
    val env = Map[Sym, AnyRef]()
    val resEnvSym = EnvRep.add(colSym -> colData.asInstanceOf[AnyRef])
    val (resEnv, _) = resEnvSym.run(env)
    resSym match {
      case Def(mc: MethodCall) =>
        val res = invokeUnlifted(colSym.elem, mc, resEnv)
        res shouldBe resData
    }
  }
}
