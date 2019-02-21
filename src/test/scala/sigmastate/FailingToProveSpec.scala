package sigmastate

import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransaction, ErgoBox, ErgoLikeInterpreter}
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import org.scalatest.TryValues._
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.utxo.ErgoLikeTestInterpreter
import org.ergoplatform.ErgoScriptPredef._

class FailingToProveSpec extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  /**
    * Both properties should work fine.
    * Currently the problem in first case is that even if we evaluate `withdrawCondition1 == true`
    * we are still try to evaluate `withdrawCondition2` which leads to something like IndexOutOfBoundsError.
    * Cause second condition has 3 outputs in body, while we are have only two in tx.
    */
  property("successfully evaluate proof 1") {
    val interpreter = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter()

    val env = Map.empty[String, Any]
    val compiledScript = compileWithCosting(env,
      s"""
         | {
         |  val withdrawCondition1 =
         |        OUTPUTS(0).value == 101L && OUTPUTS(1).value == 99L
         |  val withdrawCondition2 =
         |        OUTPUTS(0).value == 102L && OUTPUTS(1).value == 98L && OUTPUTS(2).value == 100L
         |
         |  withdrawCondition1 || withdrawCondition2
         | }
       """.stripMargin).asBoolValue.toSigmaProp

    val selfBox = ErgoBox(200L, compiledScript, 0)
    val o1 = ErgoBox(101L, TrueProp, 5001)
    val o2 = ErgoBox(99L, TrueProp, 5001)
    val tx =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(o1, o2))
    val ctx = ErgoLikeContext(
      currentHeight = 5001,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx,
      self = selfBox,
      minerPubkey = ErgoLikeContext.dummyPubkey)
    val proof = interpreter.prove(emptyEnv + (ScriptNameProp -> "prove"), compiledScript, ctx, fakeMessage).success.value.proof
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), compiledScript, ctx, proof, fakeMessage) should be a 'success
  }

  property("successfully evaluate proof 2") {
    val interpreter = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter()

    val env = Map.empty[String, Any]
    val compiledScript = compileWithCosting(env,
      s"""
         | {
         |
         |  val withdrawCondition1 =
         |              OUTPUTS(0).value == 101L && OUTPUTS(1).value == 99L
         |  val withdrawCondition2 =
         |              OUTPUTS(0).value == 102L && OUTPUTS(1).value == 98L && OUTPUTS(2).value == 100L
         |
         |  withdrawCondition1 || withdrawCondition2
         | }
       """.stripMargin).asBoolValue.toSigmaProp

    val selfBox = ErgoBox(200L, compiledScript, 0)
    val o1 = ErgoBox(102L, TrueProp, 5001)
    val o2 = ErgoBox(98L, TrueProp, 5001)
    val o3 = ErgoBox(100L, TrueProp, 5001)
    val tx =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(o1, o2, o3))
    val ctx = ErgoLikeContext(
      currentHeight = 5001,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(selfBox),
      spendingTransaction = tx,
      self = selfBox,
      minerPubkey = ErgoLikeContext.dummyPubkey)
    val proof = interpreter.prove(emptyEnv + (ScriptNameProp -> "prove"), compiledScript, ctx, fakeMessage).success.value.proof
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), compiledScript, ctx, proof, fakeMessage) should be a 'success
  }

}
