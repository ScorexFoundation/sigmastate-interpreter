package sigmastate

import org.scalatest.{PropSpecLike, Tag}
import org.scalactic.source.Position
import spire.syntax.all.cfor

trait CrossVersionProps extends PropSpecLike with TestsBase {

  val printVersions: Boolean = false
  /** Number of times each test property is warmed up (i.e. executed before final execution). */
  val perTestWarmUpIters: Int = 0

  protected def testFun_Run(testName: String, testFun: => Any): Unit = {
    def msg = s"""property("$testName")(ActivatedVersion = $activatedVersionInTests; ErgoTree version = $ergoTreeVersionInTests)"""
    if (printVersions) println(msg)
    try testFun
    catch {
      case t: Throwable =>
        if (!printVersions) {
          // wasn't printed, print it now
          println(msg)
        }
        throw t
    }
  }

  override protected def property(testName: String, testTags: Tag*)
                                 (testFun: => Any)
                                 (implicit pos: Position): Unit = {
    super.property(testName, testTags:_*) {
      // do warmup if necessary
      if (perTestWarmUpIters > 0) {
        cfor(0)(_ < perTestWarmUpIters, _ + 1) { _ =>
          testFun_Run(testName, testFun)
        }
        System.gc()
        Thread.sleep(100) // give it some time to finish warm-up
      }

      cfor(0)(_ < activatedVersions.length, _ + 1) { i =>
        val activatedVersion = activatedVersions(i)
        _currActivatedVersion.withValue(activatedVersion) {

          cfor(0)(
            i => i < ergoTreeVersions.length && ergoTreeVersions(i) <= activatedVersion,
            _ + 1) { j =>
            val treeVersion = ergoTreeVersions(j)
            _currErgoTreeVersion.withValue(treeVersion) {
              testFun_Run(testName, testFun)
            }
          }

        }
      }

      if (okRunTestsWithoutMCLowering) {
        _lowerMethodCalls.withValue(false) {
          testFun_Run(testName, testFun)
        }
      }
    }
  }
}
