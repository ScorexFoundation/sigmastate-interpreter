package sigmastate.lang

import org.ergoplatform.sdk.NetworkType
import sigma.ast.SigmaPredef.PredefinedFuncRegistry
import sigma.ast.StdSigmaBuilder
import sigma.serialization.generators.ObjectGenerators
import sigmastate.helpers.CompilerTestingCommons
import sigmastate.interpreter.Interpreter

class DirectCompilerTest extends CompilerTestingCommons with LangTests with ObjectGenerators {

  private val predefFuncRegistry = new PredefinedFuncRegistry(StdSigmaBuilder)
  import predefFuncRegistry._
  val directCompiler = new DirectCompiler(NetworkType.Mainnet.networkPrefix, predefFuncRegistry)

  // Test case for compile method
  property("compile method should correctly compile valid input") {

    forAll(ergoTreeGen) { tree =>
      val prop = tree.toProposition(tree.isConstantSegregation)
      val result = directCompiler.compileNode(Interpreter.emptyEnv, prop)
      (result eq prop) shouldBe false
      result shouldBe prop
    }
  }

}
