package special.sigma

import scalan.util.FileUtil
import scalan.{SigmaLibrary, BaseCtxTests}

class SigmaExamplesStagedTests extends BaseCtxTests {
  lazy val ctx = new TestContext with SigmaLibrary {
    import TestSigmaDslBuilder._
    val sigmaDslBuilder = RTestSigmaDslBuilder()

    def emitWithMD(name: String, ss: Sym*) = {
      val directory = FileUtil.file(prefix, testName)
      implicit val graphVizConfig = defaultGraphVizConfig.copy(emitMetadata = true)
      emitDepGraph(ss, directory, name)
    }
  }
  import ctx._

  test("SigmaLibrary cake") {
    assert(ctx != null)
  }

  lazy val nameKey = MetaKey[String]("name")

//  test("CrowdFunding.asFunction") {
//    import CrowdFundingContract._
//    import ProveDlogEvidence._
//    import ProveDlog._
//    import WECPoint._
//    val backer: Rep[ProveDlog] = RProveDlogEvidence(fresh[WECPoint].setMetadata(nameKey)("backer"))
//    val project: Rep[ProveDlog] = RProveDlogEvidence(fresh[WECPoint].setMetadata(nameKey)("project"))
//    val timeout = 100L
//    val minToRaise = 1000L
//    val c = RCrowdFundingContract(timeout, minToRaise, backer, project)
//    val f = c.asFunction
//
//    emitWithMD("f", f)
//  }
//  test("DemurrageCurrency.asFunction") {;
//  }
}
