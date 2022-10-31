package sigmastate

import org.scalatest.Tag
import org.scalactic.source.Position


trait CompilerCrossVersionProps extends CrossVersionProps with CompilerTestsBase {

  override protected def property(testName: String, testTags: Tag*)
                                 (testFun: => Any)
                                 (implicit pos: Position): Unit = {
    super.property(testName, testTags:_*)(testFun)

    val testName2 = s"${testName}_MCLowering"
    super.property2(testName2, testTags:_*) {
      if (okRunTestsWithoutMCLowering) {
        _lowerMethodCalls.withValue(false) {
          testFun_Run(testName2, testFun)
        }
      }
    }
  }
}
