package sigmastate.utxo

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Remove}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import sigmastate.Values.IntConstant
import sigmastate.utxo.ErgoBox.R3
import sigmastate.{AvlTreeData, GE}

import scala.collection.mutable
import scala.util.Try

class BlockchainSimulationSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  class InMemoryErgoBoxReader(prover: ValidationState.BatchProver) extends ErgoBoxReader {
    private val boxes = mutable.Map[ErgoBox.BoxId, ErgoBox]()

    override def byId(boxId: ADKey): Try[ErgoBox] = Try(boxes(boxId))

    def byR3Value(i: Int): Iterable[ErgoBox] = boxes.values.filter(_.get(R3).getOrElse(IntConstant(i+1)) == IntConstant(i))

    def applyBlock(block: Block): Unit = {
      val toRemove = block.txs.flatMap(_.inputs).map(_.boxId)
      toRemove.foreach(k => prover.performOneOperation(Remove(k)))
      toRemove.foreach(k => boxes.remove(k))

      val toAdd = block.txs.flatMap(_.outputs)
      toAdd.foreach(b => prover.performOneOperation(Insert(b.id, ADValue @@ b.bytes)))
      toAdd.foreach(b => boxes.put(b.id, b))

      prover.generateProof()
    }

    def digest = prover.digest
  }

  case class Block(txs: IndexedSeq[ErgoTransaction])

  case class ValidationState(state: BlockchainState, boxesReader: InMemoryErgoBoxReader) {
    def applyBlock(block: Block): ValidationState = {
      boxesReader.applyBlock(block)
      val height = state.currentHeight + 1
      val newState = BlockchainState(height, state.lastBlockUtxoRoot.copy(startingDigest = boxesReader.digest))
      ValidationState(newState, boxesReader)
    }
  }

  object ValidationState {
    type BatchProver = BatchAVLProver[Digest32, Blake2b256Unsafe]

    private lazy val hash = new Blake2b256Unsafe()

    val initBlock = Block {
      (1 to 100).map{ i =>
        val h = hash.hash(scala.util.Random.nextString(12).getBytes)
        val boxes = (1 to 100).map(_ => ErgoBox(10, GE(Height, IntConstant(i)), Map(R3 -> IntConstant(i)), h))
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

      ValidationState(bs, boxReader).applyBlock(initBlock)
    }
  }

  property("apply one block") {
    val state = ValidationState.initialState()
    val boxesToSpend = state.boxesReader.byR3Value(1)

    println(boxesToSpend.size)
  }
}
