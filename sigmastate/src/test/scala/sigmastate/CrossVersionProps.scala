package sigmastate

import scala.util.DynamicVariable
import sigmastate.interpreter.{Interpreter, VersionContext}
import org.scalatest.{PropSpecLike, Tag}
import org.scalactic.source.Position

trait CrossVersionProps extends PropSpecLike {

  val versionContextsToTest: Seq[VersionContext] =
    (0 to Interpreter.MaxSupportedScriptVersion)
      .map(i => VersionContext(i.toByte)).toArray[VersionContext]

  private val _activatedVersion = new DynamicVariable[VersionContext](VersionContext(0))

  def activatedVersion: VersionContext = _activatedVersion.value

  override protected def property(testName: String, testTags: Tag*)
                                 (testFun: => Any)
                                 (implicit pos: Position): Unit = {

    super.property(testName, testTags:_*) {
      versionContextsToTest.foreach { vc =>
        _activatedVersion.withValue(vc) {
          try testFun
          catch {
            case t: Throwable =>
              println(s"activatedVersion = $vc")
              throw t
          }
        }
      }
    }
  }
}
