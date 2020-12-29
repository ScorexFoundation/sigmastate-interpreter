package sigmastate

import org.scalatest.{PropSpecLike, Tag}
import org.scalactic.source.Position
import spire.syntax.all.cfor

trait CrossVersionProps extends PropSpecLike with TestsBase {

  val printVersions: Boolean = false

  override protected def property(testName: String, testTags: Tag*)
                                 (testFun: => Any)
                                 (implicit pos: Position): Unit = {

    super.property(testName, testTags:_*) {
      cfor(0)(_ < activatedVersions.length, _ + 1) { i =>
        val activatedVersion = activatedVersions(i)
        _currActivatedVersion.withValue(activatedVersion) {

          cfor(0)(
            i => i < ergoTreeVersions.length && ergoTreeVersions(i) <= activatedVersion,
            _ + 1) { j =>
            val treeVersion = ergoTreeVersions(j)
            _currErgoTreeVersion.withValue(treeVersion) {
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
          }

        }
      }
    }
  }
}
