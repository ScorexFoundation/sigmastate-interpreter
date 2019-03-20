package sigmastate

import org.ergoplatform.{ErgoLikeContext, ErgoBox}
import org.ergoplatform.ErgoScriptPredef.TrueProp
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{BooleanConstant, IntConstant, ByteArrayConstant}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.utxo.CostTable
import sigmastate.utxo.CostTable._

class CostingSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries = false
    substFromCostTable = false
  }

  val env: ScriptEnv = Map(
    (ScriptNameProp -> s"filename_verify")
  )
  val extension: ContextExtension = ContextExtension(Map(
    1.toByte -> IntConstant(1),
    2.toByte -> BooleanConstant(true)
  ))
  val tokenId = Blake2b256("tokenA")
  val selfBox = createBox(0, TrueProp, Seq(tokenId -> 10L),
      Map(ErgoBox.R4 -> ByteArrayConstant(Array[Byte](1, 2, 3)),
          ErgoBox.R5 -> IntConstant(3)))
  val context = ErgoLikeContext.dummy(selfBox).withExtension(extension)

  def cost(script: String): Long = {
    val ergoTree = compiler.compile(env, script)
    val interpreter = new ContextEnrichingTestProvingInterpreter
    interpreter.reduceToCrypto(context, env, ergoTree).get._2
  }

  val ContextVarAccess = accessContextVar + selectField  // `getVar(id)` + `.get`
  val RegisterAccess = accessRegister + selectField  // `getReg(id)` + `.get`
  val GTConstCost = comparisonCost + constCost

  property("basic (smoke) tests") {

    cost("{getVar[Boolean](2).get}") shouldBe ContextVarAccess

    cost("{getVar[Int](1).get > 1}") shouldBe (ContextVarAccess + GTConstCost)

    // accessing two context variables
    cost("{ getVar[Int](1).get > 1 && getVar[Boolean](2).get }") shouldBe
        (ContextVarAccess * 2 + GTConstCost + logicCost)

    // the same var is used twice doesn't lead to double cost
    cost("{getVar[Int](1).get + 1 > getVar[Int](1).get}") shouldBe
        (ContextVarAccess + plusMinus + constCost + comparisonCost)

    // cost is accumulated along the expression tree
    cost("{ getVar[Int](1).get + 1 > getVar[Int](1).get && getVar[Boolean](2).get }") shouldBe
        (ContextVarAccess * 2 + plusMinus + constCost + comparisonCost + logicCost)
  }

  property("logical op costs") {
    cost("{ val cond = getVar[Boolean](2).get; cond && cond }") shouldBe (ContextVarAccess + logicCost)
    cost("{ val cond = getVar[Boolean](2).get; cond || cond }") shouldBe (ContextVarAccess + logicCost)
    cost("{ val cond = getVar[Boolean](2).get; cond || cond && true }") shouldBe (ContextVarAccess + logicCost * 2 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; cond || cond && true || cond }") shouldBe (ContextVarAccess + logicCost * 3 + constCost)
    cost("{ val cond = getVar[Boolean](2).get; allOf(Coll(cond, true, cond)) }") shouldBe (ContextVarAccess + logicCost * 2 + constCost)
  }

  property("SELF box operations cost") {
    cost("{ SELF.value > 0 }") shouldBe (accessBox + extractCost + GTConstCost)
    cost("{ SELF.id.size > 0 }") shouldBe (accessBox + extractCost + collLength + GTConstCost)
    cost("{ SELF.tokens.size > 0 }") shouldBe (accessBox + extractCost + collLength + GTConstCost)
//    cost("{ SELF.R0[Long].get > 0 }") shouldBe (accessBox + RegisterAccess + GTConstCost)
//    cost("{ SELF.getReg[Long](0.toByte).get > 0 }") shouldBe (accessBox + RegisterAccess + GTConstCost)
  }

}