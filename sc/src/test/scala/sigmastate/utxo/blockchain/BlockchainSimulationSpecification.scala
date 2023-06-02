package sigmastate.utxo.blockchain

import java.io.{FileWriter, File}

import sigmastate.CompilerCrossVersionProps
import sigmastate.Values.{TrueLeaf, GetVarBoolean}
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestProvingInterpreter}
import sigmastate.interpreter.ContextExtension
import sigmastate.utxo.blockchain.BlockchainSimulationTestingCommons._

import scala.collection.concurrent.TrieMap


class BlockchainSimulationSpecification extends BlockchainSimulationTestingCommons
  with CompilerCrossVersionProps {

  implicit lazy val IR = new TestingIRContext

  import ValidationState._

  property("apply one valid block") {
    val state = ValidationState.initialState(activatedVersionInTests, initBlock(ergoTreeVersionInTests))
    val miner = new ErgoLikeTestProvingInterpreter()
    val block = generateBlock(state, miner, 0)
    val updStateTry = state.applyBlock(block)
    updStateTry.isSuccess shouldBe true
  }

  property("too costly block") {
    val state = ValidationState.initialState(activatedVersionInTests, initBlock(ergoTreeVersionInTests))
    val miner = new ErgoLikeTestProvingInterpreter()
    val block = generateBlock(state, miner, 0)
    val updStateTry = state.applyBlock(block, maxCost = 1)
    updStateTry.isSuccess shouldBe false
  }

  property("apply many blocks") {
    val state = ValidationState.initialState(activatedVersionInTests, initBlock(ergoTreeVersionInTests))
    val miner = new ErgoLikeTestProvingInterpreter()
    checkState(state, miner, 0, randomDeepness)
  }

  property("apply many blocks with enriched context") {
    val state = ValidationState.initialState(activatedVersionInTests, initBlock(ergoTreeVersionInTests))
    val miner = new ErgoLikeTestProvingInterpreter()
    val varId = 1.toByte
    val prop = GetVarBoolean(varId).get.toSigmaProp
    // unable to spend boxes without correct context extension
    an[RuntimeException] should be thrownBy {
      checkState(state, miner, 0, randomDeepness, Some(mkTestErgoTree(prop)))
    }

    // spend boxes with context extension
    val contextExtension = ContextExtension(Map(varId -> TrueLeaf))
    checkState(state, miner, 0, randomDeepness, Some(mkTestErgoTree(prop)), contextExtension)
  }

  ignore(s"benchmarking applying many blocks (!!! ignored)") {
    val results = new TrieMap[Int, Long]

    def bench(numberOfBlocks: Int): Unit = {

      val state = ValidationState.initialState(activatedVersionInTests, initBlock(ergoTreeVersionInTests))
      val miner = new ContextEnrichingTestProvingInterpreter()

      val (_, time) = (0 until numberOfBlocks).foldLeft(state -> 0L) { case ((s, timeAcc), h) =>
        val b = generateBlock(state, miner, h)

        val t0 = System.currentTimeMillis()
        val updStateTry = s.applyBlock(b)
        val t = System.currentTimeMillis()

        updStateTry shouldBe 'success
        updStateTry.get -> (timeAcc + (t - t0))
      }

      println(s"Total time for $numberOfBlocks blocks: $time ms")
      results.put(numberOfBlocks, time)
    }

    bench(100)
    bench(200)
    bench(300)
    bench(400)

    printResults(results.toMap)

    def printResults(results: Map[Int, Long]): Unit = {
      val file = new File("target/bench")
      file.mkdirs()
      val writer = new FileWriter(s"target/bench/result.csv", false)
      val sorted = results.toList.sortBy { case (i, _) => i }
      val header = sorted.map(_._1).mkString(",")
      writer.write(s"$header\n")
      val values = sorted.map(_._2).mkString(",")
      writer.write(s"$values\n")
      writer.flush()
      writer.close()
    }
  }
}