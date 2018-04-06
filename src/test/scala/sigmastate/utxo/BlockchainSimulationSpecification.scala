package sigmastate.utxo

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, Insert, Remove}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

import scala.collection.mutable
import scala.util.Try

class BlockchainSimulationSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  class InMemoryErgoBoxReader(prover: BatchAVLProver[Digest32, Blake2b256Unsafe]) extends ErgoBoxReader {
    private val boxes = mutable.Map[ErgoBox.BoxId, ErgoBox]()

    override def byId(boxId: ADKey): Try[ErgoBox] = Try(boxes(boxId))

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

  property("apply one block") {

  }
}
