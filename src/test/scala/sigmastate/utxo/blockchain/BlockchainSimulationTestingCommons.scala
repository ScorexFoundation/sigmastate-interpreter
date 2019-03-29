package sigmastate.utxo.blockchain

import org.ergoplatform
import org.ergoplatform._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{Remove, BatchAVLProver, Insert}
import scorex.crypto.hash.{Digest32, Blake2b256}
import sigmastate.{GE, AvlTreeData, Values, AvlTreeFlags}
import sigmastate.Values.{LongConstant, ErgoTree}
import sigmastate.eval.IRContext
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons, ErgoTransactionValidator}

import scala.collection.mutable
import scala.util.{Random, Try}
import scorex.util._
import sigmastate.interpreter.ContextExtension
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.utxo.blockchain.BlockchainSimulationTestingCommons.{ValidationState, FullBlock}

import scala.annotation.tailrec

trait BlockchainSimulationTestingCommons extends SigmaTestingCommons {

  @tailrec
  protected final def checkState(state: ValidationState,
                                 miner: ErgoLikeTestProvingInterpreter,
                                 currentLevel: Int,
                                 limit: Int,
                                 propOpt: Option[ErgoTree] = None,
                                 extension: ContextExtension = ContextExtension.empty): Unit = currentLevel match {
    case i if i >= limit => ()
    case _ =>
      val block = generateBlock(state, miner, currentLevel, propOpt, extension)
      val updStateTry = state.applyBlock(block)
      updStateTry.isSuccess shouldBe true
      checkState(updStateTry.get, miner, currentLevel + 1, limit, propOpt, extension)
  }

  protected def generateBlock(state: ValidationState,
                              prover: ErgoLikeTestProvingInterpreter,
                              height: Int,
                              propOpt: Option[ErgoTree] = None,
                              extension: ContextExtension = ContextExtension.empty): FullBlock = {
    val prop: ErgoTree = propOpt.getOrElse(prover.dlogSecrets.head.publicImage.toSigmaProp)
    val minerPubkey = prover.dlogSecrets.head.publicImage.pkBytes

    val boxesToSpend = state.boxesReader.randomBoxes(50 + height)

    val txs = boxesToSpend.map { box =>
      val newBoxCandidate =
        new ErgoBoxCandidate(10, prop, height, Seq(), Map())
      val unsignedInput = new UnsignedInput(box.id)
      val tx = UnsignedErgoLikeTransaction(IndexedSeq(unsignedInput), IndexedSeq(newBoxCandidate))
      val context = ErgoLikeContext(height + 1,
        state.state.lastBlockUtxoRoot,
        minerPubkey,
        IndexedSeq(box),
        tx,
        box,
        extension)
      val env = emptyEnv + (ScriptNameProp -> s"height_${state.state.currentHeight}_prove")
      val proverResult = prover.prove(env, box.ergoTree, context, tx.messageToSign).get
      proverResult.extension shouldBe extension

      tx.toSigned(IndexedSeq(proverResult))
    }.toIndexedSeq.ensuring(_.nonEmpty, s"Failed to create txs from boxes $boxesToSpend at height $height")

    FullBlock(txs, minerPubkey)
  }

  protected def randomDeepness: Int = 10 + Random.nextInt(10)

}

object BlockchainSimulationTestingCommons extends SigmaTestingCommons {

  private val MaxBlockCost = 700000

  case class FullBlock(txs: IndexedSeq[ErgoLikeTransaction], minerPubkey: Array[Byte])

  class InMemoryErgoBoxReader(prover: ValidationState.BatchProver) extends ErgoBoxReader {
    private type KeyType = mutable.WrappedArray.ofByte

    private def getKey(id: Array[Byte]): KeyType = new mutable.WrappedArray.ofByte(id)

    private val boxes = mutable.Map[KeyType, ErgoBox]()

    override def byId(boxId: ADKey): Try[ErgoBox] = byId(getKey(boxId))

    def byId(boxId: KeyType): Try[ErgoBox] = Try(boxes(boxId))

    def randomBoxes(howMany: Int): Iterable[ErgoBox] = {
      scala.util.Random.shuffle(boxes.values).take(howMany)
    }

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
      val newState = BlockchainState(height, state.lastBlockUtxoRoot.copy(digest = boxesReader.digest))
      ValidationState(newState, boxesReader)
    }
  }

  object ValidationState {
    type BatchProver = BatchAVLProver[Digest32, Blake2b256.type]

    val initBlock = FullBlock(
      (0 until 10).map { i =>
        val txId = Blake2b256.hash(i.toString.getBytes ++ scala.util.Random.nextString(12).getBytes).toModifierId
        val boxes = (1 to 50).map(_ => ErgoBox(10, Values.TrueLeaf.toSigmaProp, i, Seq(), Map(), txId))
        createTransaction(boxes)
      },
      ErgoLikeContext.dummyPubkey
    )

    def initialState(block: FullBlock = initBlock)(implicit IR: IRContext): ValidationState = {
      val keySize = 32
      val prover = new BatchProver(keySize, None)

      val digest = prover.digest
      val utxoRoot = AvlTreeData(digest, AvlTreeFlags.AllOperationsAllowed, keySize)

      val bs = BlockchainState(currentHeight = -2, utxoRoot)

      val boxReader = new InMemoryErgoBoxReader(prover)

      ValidationState(bs, boxReader).applyBlock(block).get
    }
  }

}