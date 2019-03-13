package sigmastate

import sigmastate.Values.{BooleanConstant, IntConstant}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptEnv, ScriptNameProp}
import sigmastate.utxo.CostTable

class CostingSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext

  val env: ScriptEnv = Map(
    (ScriptNameProp -> s"filename_verify")
  )
  val extension: ContextExtension = ContextExtension(Map(
    1.toByte -> IntConstant(1),
    2.toByte -> BooleanConstant(true)
  ))
  val context = fakeContext.withExtension(extension)

  def cost(script: String): Long = {
    val ergoTree = compiler.compile(env, script)
    val interpreter = new ContextEnrichingTestProvingInterpreter
    interpreter.reduceToCrypto(context, env, ergoTree).get._2
  }

  property("math operations costing") {

    cost("{getVar[Boolean](2).get}") shouldBe (CostTable.extractCost + CostTable.extractCost)

    //    cost("1 + 2") shouldBe (2 * CostTable.constCost + CostTable.comparisonCost)


  }

}