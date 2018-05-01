package sigmastate.utxo

import java.math.BigInteger

import scapi.sigma.Challenge
import scapi.sigma.DLogProtocol.{DLogInteractiveProver, ProveDlog, DLogProverInput, FirstDLogProverMessage}
import scorex.crypto.hash.Blake2b256
import sigmastate._
import sigmastate.helpers.{SigmaTestingCommons, ErgoProvingInterpreter}
import sigmastate.Values._
import sigmastate.interpreter.GroupSettings
import sigmastate.lang.SigmaCompiler
import sigmastate.lang.Terms._

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

abstract class SmartContract {
  val compiler = new SigmaCompiler
}

abstract class CrowdFundingContract(
    val timeout: Long,
    val minToRaise: Long,
    val backerProver: ErgoProvingInterpreter,
    val projectProver: ErgoProvingInterpreter
) extends SmartContract {
  //a blockchain node verifying a block containing a spending transaction
  val verifier = new ErgoInterpreter
  val backerPubKey = backerProver.dlogSecrets.head.publicImage
  val projectPubKey = projectProver.dlogSecrets.head.publicImage
  
  def prove(ctx: ErgoContext, fakeMessage: Array[Byte]): projectProver.ProofT

  def verify(proof: projectProver.ProofT, ctx: ErgoContext, fakeMessage: Array[Byte]): Try[Boolean]
}

class CrowdFundingScriptContract(
    timeout: Long,
    minToRaise: Long,
    override val backerProver: ErgoProvingInterpreter,
    override val projectProver: ErgoProvingInterpreter
) extends CrowdFundingContract(timeout, minToRaise, backerProver, projectProver) {

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

  def prove(ctx: ErgoContext, fakeMessage: Array[Byte]): this.projectProver.ProofT = {
    val proofP = projectProver.prove(compiledProposition, ctx, fakeMessage).get.proof
    proofP
  }

  def verify(proof: projectProver.ProofT, ctx: ErgoContext, fakeMessage: Array[Byte]): Try[Boolean] = {
    val res = verifier.verify(compiledProposition, ctx, proof, fakeMessage)
    res
  }
}

class CrowdFundingKernelContract(
    timeout: Long,
    minToRaise: Long,
    override val backerProver: ErgoProvingInterpreter,
    override val projectProver: ErgoProvingInterpreter
) extends CrowdFundingContract(timeout, minToRaise, backerProver, projectProver) {

  def isValid(pubKey: ProveDlog, message: Array[Byte]): projectProver.ProofT = {
    import projectProver._
    var su = SchnorrUnproven(pubKey, None, None, None, simulated = false)
    val secretKnown = secrets
        .filter(_.isInstanceOf[DLogProverInput])
        .exists(_.asInstanceOf[DLogProverInput].publicImage == su.proposition)

    val step4: UnprovenTree = if (!secretKnown) {
      assert(su.challengeOpt.isDefined)
      SchnorrSigner(su.proposition, None).prove(su.challengeOpt.get).asInstanceOf[UnprovenTree]
    } else {
      val (r, commitment) = DLogInteractiveProver.firstMessage(su.proposition)
      su.copy(commitmentOpt = Some(commitment), randomnessOpt = Some(r))
    }
    val commitments = step4 match {
      case ul: UnprovenLeaf => ul.commitmentOpt.toSeq
      case uc: UnprovenConjecture => uc.childrenCommitments
    }

    val rootChallenge = Blake2b256(commitments.map(_.bytes).reduce(_ ++ _) ++ message)

    su = step4.withChallenge(rootChallenge).asInstanceOf[SchnorrUnproven]
    assert(su.challengeOpt.isDefined)
    val privKey = secrets
        .filter(_.isInstanceOf[DLogProverInput])
        .find(_.asInstanceOf[DLogProverInput].publicImage == su.proposition)
        .get.asInstanceOf[DLogProverInput]
    val z = DLogInteractiveProver.secondMessage(privKey, su.randomnessOpt.get, Challenge(su.challengeOpt.get))
    SchnorrNode(su.proposition, None, su.challengeOpt.get, z)
  }

  def prove(ctx: ErgoContext, message: Array[Byte]): projectProver.ProofT = {
    val c1 = ctx.currentHeight >= timeout //&& isValid(backerPubKey, fakeMessage)
    val c2 = Array(
      ctx.currentHeight < timeout,
      ctx.spendingTransaction.outputs.exists(out => {
        out.value >= minToRaise && (out.propositionBytes sameElements projectPubKey.propBytes)
      })
    ).forall(identity)
    var proof: projectProver.ProofT = null
    c1 || (c2 && { proof = isValid(projectPubKey, message); true})
    proof
  }

  def verify(proof: projectProver.ProofT, ctx: ErgoContext, message: Array[Byte]): Try[Boolean] = Try {
    var sn = proof.asInstanceOf[SchnorrNode]
    val dlog = GroupSettings.dlogGroup
    val g = dlog.generator
    val h = sn.proposition.h

    val a = dlog.multiplyGroupElements(
      dlog.exponentiate(g, sn.secondMessage.z.underlying()),
      dlog.getInverse(dlog.exponentiate(h, new BigInteger(1, sn.challenge))))

    sn = sn.copy(firstMessageOpt = Some(FirstDLogProverMessage(a)))

    val (challenge, rootCommitments) = (sn.challenge, sn.firstMessageOpt.toSeq)

    val expectedChallenge = Blake2b256(rootCommitments.map(_.bytes).reduce(_ ++ _) ++ message)
    challenge.sameElements(expectedChallenge)
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
      val tasks = (1 to nTasks).map(iTask => Future(block(iTask)))
      val res = Await.result(Future.sequence(tasks), Duration.Inf)
    }
    println(s"Completed $nTasks tasks in $total msec")
  }

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
  val nIters = 100
  val nTasks = 100

  property("Evaluation by Precompiled Kernel") {
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
          res = contract.verify(proof, ctx, fakeMessage).get
          res shouldBe true
        }
        res
      }
      ok shouldBe true
      println(s"Task $iTask: Thread ${Thread.currentThread().getId}: Completed $nIters iterations in $time msec")
    }
  }

  property("Evaluation by Script Interpretation") {
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
          res = contract.verify(proof, ctx, fakeMessage).get
          res shouldBe true
        }
        res
      }
      ok shouldBe true
      println(s"Task $iTask: Thread ${Thread.currentThread().getId}: Completed $nIters iterations in $time msec")
    }
  }


}
