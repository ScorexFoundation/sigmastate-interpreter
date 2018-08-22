package sigmastate.eval

import sigmastate.Values.{IntConstant, SValue}
import sigmastate.lang.LangTests
import special.sigma.Context

import scalan.BaseCtxTests

class CompilerItTest extends BaseCtxTests
    with LangTests with ExampleContracts with ErgoScriptTestkit {


  test("operations") {
    val ctx = newContext(height = 1, boxA1)
    reduce(noEnv, "one+one", "1 + 1", ctx, IntConstant(2))
  }

}
