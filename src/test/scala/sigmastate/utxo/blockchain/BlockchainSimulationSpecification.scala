package sigmastate.utxo.blockchain

import java.io.{File, FileWriter}

import org.ergoplatform._
import org.scalacheck.Gen
import sigmastate.Values.LongConstant
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeTestProvingInterpreter}
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.utxo.blockchain.BlockchainSimulationTestingCommons._

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap


class BlockchainSimulationSpecification extends BlockchainSimulationTestingCommons {

  implicit lazy val IR = new TestingIRContext

  property("apply one valid block") {
    val state = ValidationState.initialState()
    val miner = new ErgoLikeTestProvingInterpreter()
    val block = generateBlock(state, miner, 0)
    val updStateTry = state.applyBlock(block)
    updStateTry.isSuccess shouldBe true
  }

  property("too costly block") {
    val state = ValidationState.initialState()
    val miner = new ErgoLikeTestProvingInterpreter()
    val block = generateBlock(state, miner, 0)
    val updStateTry = state.applyBlock(block, maxCost = 1)
    updStateTry.isSuccess shouldBe false
  }

  property("apply many blocks") {
    val state = ValidationState.initialState()
    val miner = new ErgoLikeTestProvingInterpreter()
    val randomDeepness = Gen.chooseNum(10, 20).sample.getOrElse(15)
    checkState(state, miner, 0, randomDeepness)
  }

  ignore(s"benchmarking applying many blocks (!!! ignored)") {
    val results = new TrieMap[Int, Long]

    def bench(numberOfBlocks: Int): Unit = {

      val state = ValidationState.initialState()
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