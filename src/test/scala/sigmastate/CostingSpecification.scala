package sigmastate

import sigmastate.Values.{BooleanConstant, IntConstant}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.utxo.CostTable
import sigmastate.utxo.CostTable._

class CostingSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries = true
  }

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
    cost("{getVar[Int](1).get > 1}") shouldBe
      (extractCost + extractCost + comparisonCost + constCost)
    cost("{getVar[Int](1).get + 1 > getVar[Int](1).get}") shouldBe
        (extractCost + extractCost + comparisonCost + plusMinus + constCost)
//    cost("{ getVar[Int](1).get + 1 > getVar[Int](1).get && getVar[Boolean](2).get }") shouldBe
//        ((extractCost + extractCost) * 2 + comparisonCost + plusMinus + constCost + logicCost)

    //    cost("1 + 2") shouldBe (2 * CostTable.constCost + CostTable.comparisonCost)


  }

}