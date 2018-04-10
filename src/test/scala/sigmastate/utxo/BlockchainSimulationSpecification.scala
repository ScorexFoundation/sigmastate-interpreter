package sigmastate.utxo

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Remove}
import scorex.crypto.hash.{Blake2b256, Blake2b256Unsafe, Digest32}
import sigmastate.Values.IntConstant
import sigmastate.interpreter.ContextExtension
import sigmastate.utxo.ErgoBox.R3
import sigmastate.{AvlTreeData, GE}

import scala.collection.mutable
import scala.util.Try

class BlockchainSimulationSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  private lazy val hash = Blake2b256

  class InMemoryErgoBoxReader(prover: ValidationState.BatchProver) extends ErgoBoxReader {
    private val boxes = mutable.Map[ErgoBox.BoxId, ErgoBox]()

    override def byId(boxId: ADKey): Try[ErgoBox] = Try(boxes(boxId))

    def byR3Value(i: Int): Iterable[ErgoBox] =
      boxes.values.filter(_.get(R3).getOrElse(IntConstant(i+1)) == IntConstant(i))

    def allIds: Iterable[ErgoBox.BoxId] = boxes.keys

    def applyBlock(block: Block): Unit = {
      val toRemove = block.txs.flatMap(_.inputs).map(_.boxId)
      toRemove.foreach(k => prover.performOneOperation(Remove(k)))
      toRemove.foreach(k => boxes.remove(k))

      val toAdd = block.txs.flatMap(_.outputs)
      toAdd.foreach(b => prover.performOneOperation(Insert(b.id, ADValue @@ b.bytes)))
      toAdd.foreach(b => boxes.put(b.id, b))

      prover.generateProof()
    }

    def digest: ADDigest = prover.digest
  }

  case class Block(txs: IndexedSeq[ErgoTransaction])

  case class ValidationState(state: BlockchainState, boxesReader: InMemoryErgoBoxReader) {
    def applyBlock(block: Block): Try[ValidationState] = Try {
      val height = state.currentHeight + 1

      block.txs.foreach {tx =>
        ErgoTransactionValidator.validate(tx, state.copy(currentHeight = height), boxesReader) match {
          case Left(throwable) => throw throwable
          case Right(_) =>
        }
      }

      boxesReader.applyBlock(block)
      val newState = BlockchainState(height, state.lastBlockUtxoRoot.copy(startingDigest = boxesReader.digest))
      ValidationState(newState, boxesReader)
    }
  }

  object ValidationState {
    type BatchProver = BatchAVLProver[Digest32, Blake2b256Unsafe]

    val initBlock = Block {
      (1 to 20).map{ i =>
        val txId = hash.hash(i.toString.getBytes ++ scala.util.Random.nextString(12).getBytes)
        val boxes = (1 to 50).map(_ => ErgoBox(10, GE(Height, IntConstant(i)), Map(R3 -> IntConstant(i)), txId))
        ErgoTransaction(IndexedSeq(), boxes)
      }
    }

    def initialState(): ValidationState = {
      val keySize = 32
      val prover = new BatchProver(keySize, None)

      val digest = prover.digest
      val utxoRoot = AvlTreeData(digest, keySize)

      val bs =  BlockchainState(currentHeight = 0, utxoRoot)

      val boxReader = new InMemoryErgoBoxReader(prover)

      ValidationState(bs, boxReader).applyBlock(initBlock).get
    }
  }

  property("apply one block") {
    val state = ValidationState.initialState()
    val boxesToSpend = state.boxesReader.byR3Value(1)

    val miner = new ErgoProvingInterpreter()
    val minerPubKey = miner.dlogSecrets.head.publicImage

    val txs = boxesToSpend.map{box =>
      //todo: real txId in newBox
      val txId = hash.hash(scala.util.Random.nextString(16).getBytes)
      val newBox = ErgoBox(10, minerPubKey, Map(), txId)
      val fakeInput = Input(box.id, null)
      val tx = ErgoTransaction(IndexedSeq(fakeInput), IndexedSeq(newBox))
      val context = ErgoContext(state.state.currentHeight + 1,
                                state.state.lastBlockUtxoRoot,
                                IndexedSeq(box),
                                tx,
                                box,
                                ContextExtension.empty)
      val proverResult = miner.prove(box.proposition, context, tx.messageToSign).get

      val realInput = Input(box.id, proverResult)
      ErgoTransaction(IndexedSeq(realInput), IndexedSeq(newBox))
    }.toIndexedSeq

    val block = Block(txs)

    val updStateTry = state.applyBlock(block)
    updStateTry.isSuccess shouldBe true
  }
}
