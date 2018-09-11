package sigmastate

import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeInterpreter, ErgoLikeTransaction}
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._

import org.scalatest.TryValues._

class FailingToProveSpec extends SigmaTestingCommons {

  /**
    * Bot properties should work fine.
    * Currently the problem in first case is that even if we evaluate `withdrawCondition1 == true`
    * we are still try to evaluate `withdrawCondition2` which leads to something like indexOfBoundsError.
    * Cause second condition has 3 outputs in body, while we are have only two in tx.
    *
    * try `val proof = interpreter.prove(compiledScript, ctx, fakeMessage).success.value.proof` to see real stacktrace
    * which leads to ByIndex transformer -> matchCase -> cc => cc.items.lift(i).orElse(default).get
    * here is lift(i) will return None, at the same time default are being set to None too.
    * So it's basically None.get exception.
    */


  property("successfully evaluate proof 1") {
    val interpreter = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter()

    val env = Map.empty[String, Any]
    val compiledScript = compile(env,
      s"""
         | {
         |
         |  let withdrawCondition1 =
         |              OUTPUTS(0).value == 101L && OUTPUTS(1).value == 99L
         |  let withdrawCondition2 =
         |              OUTPUTS(0).value == 102L && OUTPUTS(1).value == 98L && OUTPUTS(2).value == 100L
         |
         |  withdrawCondition1 || withdrawCondition2
         | }
       """.stripMargin).asBoolValue

    val self = ErgoBox(200L, compiledScript)
    val o1 = ErgoBox(101L, SBoolean.mkConstant(true))
    val o2 = ErgoBox(99L, SBoolean.mkConstant(true))
    val tx =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(o1, o2))
    val ctx = ErgoLikeContext(
      currentHeight = 5001L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx,
      self = self)
    val proof = interpreter.prove(compiledScript, ctx, fakeMessage).success.value.proof
    verifier.verify(compiledScript, ctx, proof, fakeMessage) should be a 'success
  }

  property("successfully evaluate proof 2") {
    val interpreter = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter()

    val env = Map.empty[String, Any]
    val compiledScript = compile(env,
      s"""
         | {
         |
         |  let withdrawCondition1 =
         |              OUTPUTS(0).value == 101L && OUTPUTS(1).value == 99L
         |  let withdrawCondition2 =
         |              OUTPUTS(0).value == 102L && OUTPUTS(1).value == 98L && OUTPUTS(2).value == 100L
         |
         |  withdrawCondition1 || withdrawCondition2
         | }
       """.stripMargin).asBoolValue

    val self = ErgoBox(200L, compiledScript)
    val o1 = ErgoBox(102L, SBoolean.mkConstant(true))
    val o2 = ErgoBox(98L, SBoolean.mkConstant(true))
    val o3 = ErgoBox(100L, SBoolean.mkConstant(true))
    val tx =  ErgoLikeTransaction(IndexedSeq(), IndexedSeq(o1, o2, o3))
    val ctx = ErgoLikeContext(
      currentHeight = 5001L,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx,
      self = self)
    val proof = interpreter.prove(compiledScript, ctx, fakeMessage).success.value.proof
    verifier.verify(compiledScript, ctx, proof, fakeMessage) should be a 'success
  }

}
