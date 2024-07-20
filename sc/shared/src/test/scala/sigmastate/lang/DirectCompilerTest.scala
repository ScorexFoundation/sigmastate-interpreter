package sigmastate.lang

import org.ergoplatform.sdk.NetworkType
import sigma.serialization.generators.ObjectGenerators
import sigmastate.helpers.CompilerTestingCommons

class DirectCompilerTest extends CompilerTestingCommons with LangTests with ObjectGenerators {

  val directCompiler = new DirectCompiler(NetworkType.Mainnet.networkPrefix)

  // Test case for compile method
  property("compile method should correctly compile valid input") {

    forAll(ergoTreeGen) { tree =>
      val prop = tree.toProposition(tree.isConstantSegregation)
      val result = directCompiler.compileNode(DirectCompiler.emptyEnv, prop, 0)
      (result eq prop) shouldBe false
      result shouldBe prop
    }
  }

}
