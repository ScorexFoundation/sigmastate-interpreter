package sigmastate.utxo

import scapi.sigma.DLogProtocol.ProveDlog
import sigmastate._
import sigmastate.helpers.{SigmaTestingCommons, ErgoProvingInterpreter}
import sigmastate.Values._
import sigmastate.lang.SigmaCompiler
import sigmastate.lang.Terms._

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

abstract class SmartContract {
  val compiler = new SigmaCompiler
}

class CrowdFundingContract(
    val timeout: Long,
    val minToRaise: Long,
    val backerProver: ErgoProvingInterpreter,
    val projectProver: ErgoProvingInterpreter
    ) extends SmartContract {
  //a blockchain node verifying a block containing a spending transaction
  val verifier = new ErgoInterpreter
  val backerPubKey = backerProver.dlogSecrets.head.publicImage
  val projectPubKey = projectProver.dlogSecrets.head.publicImage

  val compiledProposition: Value[SBoolean.type] = {
    val env = Map(
      "timeout" -> timeout,
      "minToRaise" -> minToRaise,
      "backerPubKey" -> backerPubKey,
      "projectPubKey" -> projectPubKey
    )
    val compiledScript = compiler.compile(env,
      """{
       | let c1 = HEIGHT >= timeout && backerPubKey
       | let c2 = allOf(Array(
       |   HEIGHT < timeout,
       |   projectPubKey,
       |   OUTPUTS.exists(fun (out: Box) = {
       |     out.value >= minToRaise && out.propositionBytes == projectPubKey.propBytes
       |   })
       | ))
       | c1 || c2
       | }
      """.stripMargin).asBoolValue
    compiledScript
  }

  def prove(ctx: ErgoContext, fakeMessage: Array[Byte]): projectProver.ProofT = {
    val proofP = projectProver.prove(compiledProposition, ctx, fakeMessage).get.proof
    proofP
  }

  def verify(proof: projectProver.ProofT, ctx: ErgoContext, fakeMessage: Array[Byte]): Try[Boolean] = {
    val res = verifier.verify(compiledProposition, ctx, proof, fakeMessage)
    res
  }
}

class CrowdfundingBenchmark extends SigmaTestingCommons {
  def measureTime[T](action: => T): (T, Long) = {
    val t0 = System.currentTimeMillis()
    val res = action
    val t = System.currentTimeMillis()
    (res, t - t0)
  }

  def runTasks(nTasks: Int)(block: Int => Unit) = {
    val (_, total) = measureTime {
      val tasks = (1 to nTasks).map(iTask => Future(block(iTask)) )
      val res = Await.result(Future.sequence(tasks), Duration.Inf)
    }
    println(s"Completed $nTasks tasks in $total msec")
  }

  property("Evaluation by Interpretation") {
    val nIters = 100
    runTasks(64) { iTask =>
      //backer's prover with his private key
      val backerProver = new ErgoProvingInterpreter
      //project's prover with his private key
      val projectProver = new ErgoProvingInterpreter

      val contract = new CrowdFundingContract(100, 1000, backerProver, projectProver)

      val outputToSpend = ErgoBox(10, contract.compiledProposition)

      //First case: height < timeout, project is able to claim amount of tokens not less than required threshold
      val tx1Output1 = ErgoBox(contract.minToRaise, contract.projectPubKey)
      val tx1Output2 = ErgoBox(1, contract.projectPubKey)
      //normally this transaction would invalid, but we're not checking it in this test
      val tx1 = ErgoTransaction(IndexedSeq(), IndexedSeq(tx1Output1, tx1Output2))
      val ctx1 = ErgoContext(
        currentHeight = contract.timeout - 1, // HEIGHT < timeout,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        boxesToSpend = IndexedSeq(),
        spendingTransaction = tx1,
        self = outputToSpend)

      val (ok, time) = measureTime {
        var res = true
        for ( i <- 1 to nIters ) {
          val proof = contract.prove(ctx1, fakeMessage)
          res = contract.verify(proof, ctx1, fakeMessage).get
          res shouldBe true
        }
        res
      }
      ok shouldBe true
      println(s"Task $iTask: Thread ${Thread.currentThread().getId}: Completed $nIters iterations in $time msec")
    }
  }
}
