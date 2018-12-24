package sigmastate.utxo.benchmarks


import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransaction}
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import scalan.util.BenchmarkUtil._

class CrowdfundingBenchmark extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  def createTestContext(contract: CrowdFundingContract): ErgoLikeContext = {
    val outputToSpend = ErgoBox(10, TrueLeaf, 0)
    //First case: height < timeout, project is able to claim amount of tokens not less than required threshold
    val tx1Output1 = ErgoBox(contract.minToRaise, contract.projectPubKey, 0)
    val tx1Output2 = ErgoBox(1, contract.projectPubKey, 0)
    //normally this transaction would invalid, but we're not checking it in this test
    val tx = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
    val ctx = ErgoLikeContext(
      currentHeight = contract.timeout - 1, // HEIGHT < timeout,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = tx,
      self = outputToSpend)
    ctx
  }

  val timeout = 100
  val minToRaise = 1000L
  val nIters = 10000
  val nTasks = 1

  ignore("Evaluation by Precompiled Kernel(!!! ignore)") {
    runTasks(nTasks) { iTask =>
      //backer's prover with his private key
      val backerProver = new ErgoLikeTestProvingInterpreter
      //project's prover with his private key
      val projectProver = new ErgoLikeTestProvingInterpreter
      val contract = new CrowdFundingKernelContract(timeout, minToRaise, backerProver, projectProver)
      val ctx = createTestContext(contract)

      val (ok, time) = measureTime {
        var res = true
        for (i <- 1 to nIters) {
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

  ignore("Evaluation by Script Interpretation(!!! ignore)") {
    runTasks(nTasks) { iTask =>
      //backer's prover with his private key
      val backerProver = new ErgoLikeTestProvingInterpreter
      //project's prover with his private key
      val projectProver = new ErgoLikeTestProvingInterpreter
      val contract = new CrowdFundingScriptContract(timeout, minToRaise, backerProver, projectProver)
      val ctx = createTestContext(contract)

      val (ok, time) = measureTime {
        var res = true
        for (i <- 1 to nIters) {
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
