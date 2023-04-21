package sigmastate.utxo

import java.io.{FileWriter, File}

import org.ergoplatform
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform._
import org.scalacheck.Gen
import scorex.crypto.authds.avltree.batch.{Remove, BatchAVLProver, Insert}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.hash.{Digest32, Blake2b256}
import scorex.util._
import sigmastate.Values.{LongConstant, IntConstant, ErgoTree}
import sigmastate.helpers.{ErgoTransactionValidator, ErgoLikeContextTesting, ErgoLikeTestProvingInterpreter, SigmaTestingCommons, BlockchainState}
import sigmastate.helpers.TestingHelpers._
import sigmastate.interpreter.ContextExtension
import sigmastate.eval._
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.{GE, AvlTreeData, CrossVersionProps, AvlTreeFlags}

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.Try


class BlockchainSimulationSpecification extends SigmaTestingCommons
  with CrossVersionProps {
  import BlockchainSimulationSpecification._
  implicit lazy val IR = new TestingIRContext

  def generateBlock(state: ValidationState, miner: ErgoLikeTestProvingInterpreter, height: Int): Block = {
    val minerPubKey = miner.dlogSecrets.head.publicImage
    val boxesToSpend = state.boxesReader.byHeightRegValue(height)

    val txs = boxesToSpend.map { box =>
      val newBoxCandidate =
        new ErgoBoxCandidate(10, minerPubKey, height, Colls.emptyColl[(TokenId, Long)], Map(heightReg -> IntConstant(height + windowSize)))
      val unsignedInput = new UnsignedInput(box.id)
      val tx = UnsignedErgoLikeTransaction(IndexedSeq(unsignedInput), IndexedSeq(newBoxCandidate))
      val context = ErgoLikeContextTesting(height + 1,
        state.state.lastBlockUtxoRoot,
        ErgoLikeContextTesting.dummyPubkey,
        IndexedSeq(box),
        tx,
        box,
        activatedVersionInTests,
        ContextExtension.empty)
      val env = emptyEnv + (ScriptNameProp -> s"height_${state.state.currentHeight}_prove")
      val proverResult = miner.prove(env, box.ergoTree, context, tx.messageToSign).get

      tx.toSigned(IndexedSeq(proverResult))
    }.toIndexedSeq.ensuring(_.nonEmpty, s"Failed to create txs from boxes $boxesToSpend at height $height")

    Block(txs, minerPubKey.pkBytes)
  }

  import ValidationState._

  property("apply one valid block") {
    val state = initialState(activatedVersionInTests, initBlock(ergoTreeVersionInTests))
    val miner = new ErgoLikeTestProvingInterpreter()
    val block = generateBlock(state, miner, 0)
    val updStateTry = state.applyBlock(block)
    updStateTry.isSuccess shouldBe true
  }

  property("too costly block") {
    val state = initialState(activatedVersionInTests, initBlock(ergoTreeVersionInTests))
    val miner = new ErgoLikeTestProvingInterpreter()
    val block = generateBlock(state, miner, 0)
    val updStateTry = state.applyBlock(block, maxCost = 1)
    updStateTry.isSuccess shouldBe false
  }

  property("apply many blocks") {
    val state = initialState(activatedVersionInTests, initBlock(ergoTreeVersionInTests))
    val miner = new ErgoLikeTestProvingInterpreter()

    @tailrec
    def checkState(state: ValidationState,
                   miner: ErgoLikeTestProvingInterpreter,
                   currentLevel: Int,
                   limit: Int): Unit = currentLevel match {
      case i if i >= limit => ()
      case _ =>
        val block = generateBlock(state, miner, currentLevel)
        val updStateTry = state.applyBlock(block)
        updStateTry.isSuccess shouldBe true
        checkState(updStateTry.get, miner, currentLevel + 1, limit)
    }

    val randomDeepness = Gen.chooseNum(10, 20).sample.getOrElse(15)
    checkState(state, miner, 0, randomDeepness)
  }

  ignore(s"benchmarking applying many blocks (!!! ignored)") {
    val results = new TrieMap[Int, Long]

    def bench(numberOfBlocks: Int): Unit = {

      val state = initialState(activatedVersionInTests, initBlock(ergoTreeVersionInTests))
      val miner = new ErgoLikeTestProvingInterpreter()

      val (_, time) = (0 until numberOfBlocks).foldLeft(state -> 0L) { case ((s, timeAcc), h) =>
        val b = generateBlock(state, miner, h)

        val t0 = System.currentTimeMillis()
        val updStateTry = s.applyBlock(b)
        val t = System.currentTimeMillis()

        updStateTry.isSuccess shouldBe true
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

object BlockchainSimulationSpecification {
  private lazy val hash = Blake2b256

  val heightReg = ErgoBox.nonMandatoryRegisters.head

  val windowSize = 10

  val MaxBlockCost = 1000000

  case class Block(txs: IndexedSeq[ErgoLikeTransaction], minerPubkey: Array[Byte])

  class InMemoryErgoBoxReader(prover: ValidationState.BatchProver) extends ErgoBoxReader {
    private type KeyType = mutable.WrappedArray.ofByte

    private def getKey(id: Array[Byte]): KeyType = new mutable.WrappedArray.ofByte(id)

    private val boxes = mutable.Map[KeyType, ErgoBox]()

    override def byId(boxId: ADKey): Try[ErgoBox] = byId(getKey(boxId))

    def byId(boxId: KeyType): Try[ErgoBox] = Try(boxes(boxId))

    def byHeightRegValue(i: Int): Iterable[ErgoBox] =
      boxes.values.filter(_.get(heightReg).getOrElse(IntConstant(i + 1)) == IntConstant(i))

    def byTwoInts(r1Id: ErgoBox.RegisterId, int1: Int,
                  r2Id: ErgoBox.RegisterId, int2: Int): Option[ErgoBox] =
      boxes.values.find { box =>
        box.get(r1Id).getOrElse(LongConstant(int1 + 1)) == LongConstant(int1) &&
          box.get(r2Id).getOrElse(LongConstant(int2 + 1)) == LongConstant(int2)
      }

    def allIds: Iterable[KeyType] = boxes.keys

    def applyBlock(block: Block): Unit = {
      val toRemove = block.txs.flatMap(_.inputs).map(_.boxId)
      toRemove.foreach(k => prover.performOneOperation(Remove(k)))
      toRemove.foreach(k => boxes.remove(getKey(k)))

      val toAdd = block.txs.flatMap(_.outputs)
      toAdd.foreach(b => prover.performOneOperation(Insert(b.id, ADValue @@ b.bytes)))
      toAdd.foreach(b => boxes.put(getKey(b.id), b))

      prover.generateProof()
    }

    def digest: ADDigest = prover.digest
  }


  case class ValidationState(state: BlockchainState, boxesReader: InMemoryErgoBoxReader, activatedVersion: Byte)(implicit IR: IRContext) {
    val validator = new ErgoTransactionValidator(activatedVersion)
    def applyBlock(block: Block, maxCost: Int = MaxBlockCost): Try[ValidationState] = Try {
      val height = state.currentHeight + 1

      val blockCost = block.txs.foldLeft(0L) { case (accCost, tx) =>
        validator.validate(tx, state.copy(currentHeight = height),
          block.minerPubkey,
          boxesReader) match {
          case Left(throwable) => throw throwable
          case Right(cost) => accCost + cost
        }
      }

      assert(blockCost <= maxCost, s"Block cost $blockCost exceeds limit $maxCost")

      boxesReader.applyBlock(block)
      val newState = BlockchainState(height, state.lastBlockUtxoRoot.copy(digest = boxesReader.digest))
      ValidationState(newState, boxesReader, activatedVersion)
    }
  }

  object ValidationState {
    type BatchProver = BatchAVLProver[Digest32, Blake2b256.type]

    def initBlock(scriptVersion: Byte) = Block(
      (0 until windowSize).map { i =>
        val txId = hash.hash(i.toString.getBytes ++ scala.util.Random.nextString(12).getBytes).toModifierId
        val boxes = (1 to 30).map(_ =>
          testBox(10,
            ErgoTree.fromProposition(
              ErgoTree.headerWithVersion(scriptVersion),
              GE(Height, IntConstant(i)).toSigmaProp),
            0, Seq(), Map(heightReg -> IntConstant(i)), txId))
        ergoplatform.ErgoLikeTransaction(IndexedSeq(), boxes)
      },
      ErgoLikeContextTesting.dummyPubkey
    )

    def initialState(activatedVersion: Byte, block: Block)(implicit IR: IRContext): ValidationState = {
      val keySize = 32
      val prover = new BatchProver(keySize, None)

      val digest = prover.digest
      val utxoRoot = AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, keySize)

      val bs = BlockchainState(currentHeight = -2, utxoRoot)

      val boxReader = new InMemoryErgoBoxReader(prover)

      ValidationState(bs, boxReader, activatedVersion).applyBlock(block).get
    }
  }

}
