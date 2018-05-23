package sigmastate.utxo.benchmarks


import org.ergoplatform.{ErgoBox, ErgoContext, ErgoTransaction}
import sigmastate._
import sigmastate.helpers.{ErgoProvingInterpreter, SigmaTestingCommons}
import sigmastate.Values._
import sigmastate.utxo._


class CrowdfundingBenchmark extends SigmaTestingCommons with BenchmarkingCommons {

  def createTestContext(contract: CrowdFundingContract): ErgoContext = {
    val outputToSpend = ErgoBox(10, TrueLeaf)
    //First case: height < timeout, project is able to claim amount of tokens not less than required threshold
    val tx1Output1 = ErgoBox(contract.minToRaise, contract.projectPubKey)
    val tx1Output2 = ErgoBox(1, contract.projectPubKey)
    //normally this transaction would invalid, but we're not checking it in this test
    val tx = ErgoTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
    val ctx = ErgoContext(
      currentHeight = contract.timeout - 1, // HEIGHT < timeout,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx,
      self = outputToSpend)
    ctx
  }

  val timeout = 100L
  val minToRaise = 1000L
  val nIters = 10000
  val nTasks = 1

  ignore("Evaluation by Precompiled Kernel") {
    runTasks(nTasks) { iTask =>
      //backer's prover with his private key
      val backerProver = new ErgoProvingInterpreter
      //project's prover with his private key
      val projectProver = new ErgoProvingInterpreter
      val contract = new CrowdFundingKernelContract(timeout, minToRaise, backerProver, projectProver)
      val ctx = createTestContext(contract)

      val (ok, time) = measureTime {
        var res = true
        for ( i <- 1 to nIters ) {
          val proof = contract.prove(ctx, fakeMessage)
          res = contract.verify(proof, ctx, fakeMessage).get._1
          res shouldBe true
        }
        res
      }
      ok shouldBe true
      println(s"Task $iTask: Thread ${Thread.currentThread().getId}: Completed $nIters iterations in $time msec")
    }
  }

  ignore("Evaluation by Script Interpretation") {
    runTasks(nTasks) { iTask =>
      //backer's prover with his private key
      val backerProver = new ErgoProvingInterpreter
      //project's prover with his private key
      val projectProver = new ErgoProvingInterpreter
      val contract = new CrowdFundingScriptContract(timeout, minToRaise, backerProver, projectProver)
      val ctx = createTestContext(contract)

      val (ok, time) = measureTime {
        var res = true
        for ( i <- 1 to nIters ) {
          val proof = contract.prove(ctx, fakeMessage)
          res = contract.verify(proof, ctx, fakeMessage).get._1
          res shouldBe true
        }
        res
      }
      ok shouldBe true
      println(s"Task $iTask: Thread ${Thread.currentThread().getId}: Completed $nIters iterations in $time msec")
    }
  }


}
