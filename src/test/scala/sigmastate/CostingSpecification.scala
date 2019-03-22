package sigmastate

import org.ergoplatform.ErgoLikeContext.{dummyPubkey, dummyPreHeader, noHeaders, noBoxes}
import org.ergoplatform.{ErgoLikeContext, ErgoBox}
import org.ergoplatform.ErgoScriptPredef.TrueProp
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{ByteArrayConstant, BooleanConstant, IntConstant}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, ScriptEnv}
import sigmastate.utxo.CostTable
import sigmastate.utxo.CostTable._
import sigmastate.eval._
import special.sigma.SigmaTestingData

class CostingSpecification extends SigmaTestingData {
  implicit lazy val IR = new TestingIRContext {
    override val okPrintEvaluatedEntries = false
    substFromCostTable = false
  }
  lazy val interpreter = new ContextEnrichingTestProvingInterpreter
  lazy val pkA = interpreter.dlogSecrets(0).publicImage
  lazy val pkB = interpreter.dlogSecrets(1).publicImage

  val printCosts = true
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
  lazy val outBoxA = ErgoBox(10, pkA, 0)
  lazy val outBoxB = ErgoBox(20, pkB, 0)
  lazy val tx = createTransaction(IndexedSeq(outBoxA, outBoxB))
  lazy val context =
    new ErgoLikeContext(
      currentHeight = preHeader.height,
      lastBlockUtxoRoot = header2.stateRoot.asInstanceOf[CAvlTree].treeData,
      minerPubkey = preHeader.minerPk.getEncoded.toArray,
      headers = headers, preHeader = preHeader,
      dataBoxes = IndexedSeq(dataBox),
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx, self = selfBox, extension)

  def cost(script: String): Long = {
    val ergoTree = compiler.compile(env, script)
    val res = interpreter.reduceToCrypto(context, env, ergoTree).get._2
    if (printCosts)
      println(script + s" --> cost $res")
    res
  }

  val ContextVarAccess = accessContextVar + selectField  // `getVar(id)` + `.get`
  val RegisterAccess = accessRegister + selectField  // `getReg(id)` + `.get`
  val GTConstCost = comparisonCost + constCost

  property("basic (smoke) tests") {

    cost("{ getVar[Boolean](2).get }") shouldBe ContextVarAccess

    cost("{ getVar[Int](1).get > 1 }") shouldBe (ContextVarAccess + GTConstCost)

    // accessing two context variables
    cost("{ getVar[Int](1).get > 1 && getVar[Boolean](2).get }") shouldBe
        (ContextVarAccess * 2 + GTConstCost + logicCost)

    // the same var is used twice doesn't lead to double cost
    cost("{ getVar[Int](1).get + 1 > getVar[Int](1).get }") shouldBe
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
    cost("{ SELF.creationInfo._1 > 0 }") shouldBe (accessBox + accessRegister + selectField + GTConstCost)
    cost("{ SELF.R5[Int].get > 0 }") shouldBe (accessBox + RegisterAccess + GTConstCost)

//    cost("{ SELF.getReg[Long](0.toByte).get > 0 }") shouldBe (accessBox + RegisterAccess + GTConstCost)
  }

  lazy val OutputsCost = selectField + accessBox * tx.outputs.length
  lazy val InputsCost = selectField + accessBox * context.boxesToSpend.length
  lazy val DataInputsCost = selectField + accessBox * context.dataBoxes.length
  lazy val HeadersCost = selectField
  lazy val PreHeaderCost = selectField

  property("Context operations cost") {
    cost("{ HEIGHT > 0 }") shouldBe (selectField + GTConstCost)
    cost("{ OUTPUTS.size > 0 }") shouldBe (OutputsCost + collLength + GTConstCost)
    cost("{ INPUTS.size > 0 }") shouldBe (InputsCost + collLength + GTConstCost)
    cost("{ CONTEXT.dataInputs.size > 0 }") shouldBe (DataInputsCost + collLength + GTConstCost)
    cost("{ LastBlockUtxoRootHash.isUpdateAllowed }") shouldBe (selectField + selectField)
    cost("{ MinerPubkey.size > 0 }") shouldBe (selectField + collLength + GTConstCost)
    cost("{ CONTEXT.headers.size > 0 }") shouldBe (HeadersCost + collLength + GTConstCost)
    cost("{ CONTEXT.preHeader.height > 0 }") shouldBe (PreHeaderCost + selectField + GTConstCost)
  }

//  property("PreHeader operations cost") {
//    cost("{ CONTEXT.preHeader.version > 0 }") shouldBe (PreHeaderCost + selectField + castOp + GTConstCost)
//    cost("{ CONTEXT.preHeader.parentId.size > 0 }") shouldBe (PreHeaderCost + selectField + collLength + GTConstCost)
//  }
}