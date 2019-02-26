package sigmastate.utxo.blockchain

import org.ergoplatform
import org.ergoplatform._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Remove}
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.{AvlTreeData, GE}
import sigmastate.Values.LongConstant
import sigmastate.eval.IRContext
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, ErgoTransactionValidator, SigmaTestingCommons}

import scala.collection.mutable
import scala.util.Try
import scorex.util._
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.utxo.blockchain.BlockchainSimulationTestingCommons.{FullBlock, ValidationState, heightReg}

import scala.annotation.tailrec

trait BlockchainSimulationTestingCommons extends SigmaTestingCommons {

  private val windowSize = 10

  @tailrec
  protected final def checkState(state: ValidationState,
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

  protected def generateBlock(state: ValidationState, miner: ErgoLikeTestProvingInterpreter, height: Int): FullBlock = {
    val minerPubKey = miner.dlogSecrets.head.publicImage
    val boxesToSpend = state.boxesReader.byHeightRegValue(height)

    val txs = boxesToSpend.map { box =>
      val newBoxCandidate =
        new ErgoBoxCandidate(10, minerPubKey, height, Seq(), Map(heightReg -> LongConstant(height + windowSize)))
      val unsignedInput = new UnsignedInput(box.id)
      val tx = UnsignedErgoLikeTransaction(IndexedSeq(unsignedInput), IndexedSeq(newBoxCandidate))
      val context = ErgoLikeContext(height + 1,
        state.state.lastBlockUtxoRoot,
        ErgoLikeContext.dummyPubkey,
        IndexedSeq(box),
        tx,
        box,
        ContextExtension.empty)
      val env = emptyEnv + (ScriptNameProp -> s"height_${state.state.currentHeight}_prove")
      val proverResult = miner.prove(env, box.ergoTree, context, tx.messageToSign).get
      proverResult.extension shouldBe ContextExtension.empty

      tx.toSigned(IndexedSeq(proverResult))
    }.toIndexedSeq.ensuring(_.nonEmpty, s"Failed to create txs from boxes $boxesToSpend at height $height")

    FullBlock(txs, minerPubKey.pkBytes)
  }

}

object BlockchainSimulationTestingCommons {

  val heightReg = ErgoBox.nonMandatoryRegisters.head



  val MaxBlockCost = 700000

  case class FullBlock(txs: IndexedSeq[ErgoLikeTransaction], minerPubkey: Array[Byte])

  class InMemoryErgoBoxReader(prover: ValidationState.BatchProver) extends ErgoBoxReader {
    private type KeyType = mutable.WrappedArray.ofByte

    private def getKey(id: Array[Byte]): KeyType = new mutable.WrappedArray.ofByte(id)

    private val boxes = mutable.Map[KeyType, ErgoBox]()

    override def byId(boxId: ADKey): Try[ErgoBox] = byId(getKey(boxId))

    def byId(boxId: KeyType): Try[ErgoBox] = Try(boxes(boxId))

    def byHeightRegValue(i: Int): Iterable[ErgoBox] =
      boxes.values.filter(_.get(heightReg).getOrElse(LongConstant(i + 1)) == LongConstant(i))

    def byTwoInts(r1Id: ErgoBox.RegisterId, int1: Int,
                  r2Id: ErgoBox.RegisterId, int2: Int): Option[ErgoBox] =
      boxes.values.find { box =>
        box.get(r1Id).getOrElse(LongConstant(int1 + 1)) == LongConstant(int1) &&
          box.get(r2Id).getOrElse(LongConstant(int2 + 1)) == LongConstant(int2)
      }

    def allIds: Iterable[KeyType] = boxes.keys

    def applyBlock(block: FullBlock): Unit = {
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


  case class ValidationState(state: BlockchainState, boxesReader: InMemoryErgoBoxReader)(implicit IR: IRContext) {
    val validator = new ErgoTransactionValidator

    def applyBlock(block: FullBlock, maxCost: Int = MaxBlockCost): Try[ValidationState] = Try {
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
      val newState = BlockchainState(height, state.lastBlockUtxoRoot.copy(startingDigest = boxesReader.digest))
      ValidationState(newState, boxesReader)
    }
  }

  object ValidationState {
    type BatchProver = BatchAVLProver[Digest32, Blake2b256.type]

    val initBlock = FullBlock(
      (0 until 10).map { i =>
        val txId = Blake2b256.hash(i.toString.getBytes ++ scala.util.Random.nextString(12).getBytes).toModifierId
        val boxes = (1 to 50).map(_ => ErgoBox(10, GE(Height, LongConstant(i)).toSigmaProp, 0, Seq(), Map(heightReg -> LongConstant(i)), txId))
        ergoplatform.ErgoLikeTransaction(IndexedSeq(), boxes)
      },
      ErgoLikeContext.dummyPubkey
    )

    def initialState(block: FullBlock = initBlock)(implicit IR: IRContext): ValidationState = {
      val keySize = 32
      val prover = new BatchProver(keySize, None)

      val digest = prover.digest
      val utxoRoot = AvlTreeData(digest, keySize)

      val bs = BlockchainState(currentHeight = -2, utxoRoot)

      val boxReader = new InMemoryErgoBoxReader(prover)

      ValidationState(bs, boxReader).applyBlock(block).get
    }
  }
}