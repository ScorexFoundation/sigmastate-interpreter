package sigmastate.utxo

import scala.util.{Failure, Success}

object ErgoTransactionValidator {
  val verifier = new ErgoInterpreter()

  //todo: check that outputs are well-formed?
  def validate(tx: ErgoTransaction,
               blockchainState: BlockchainState,
               boxesReader: ErgoBoxReader): Either[Throwable, Unit] = {

    val msg = tx.messageToSign
    val inputs = tx.inputs

    val boxes: IndexedSeq[ErgoBox] = tx.inputs.map(_.boxId).map{id =>
      boxesReader.byId(id) match {
        case Success(box) => box
        case Failure(e) => return Left[Throwable, Unit](e)
      }
    }

    boxes.zipWithIndex.foreach { case (box, idx) =>
      val input = inputs(idx)
      val proof = input.spendingProof

      val proverExtension = tx.inputs(idx).spendingProof.extension

      val context =
        ErgoContext(blockchainState.currentHeight, blockchainState.lastBlockUtxoRoot, boxes, tx, box, proverExtension)

      verifier.verify(box.proposition, context, proof, msg) match {
        case Success(res) => if(!res) return Left[Throwable, Unit](new Exception(s"Validation failed for input #$idx"))
        case Failure(e) => return Left[Throwable, Unit](e)
      }
    }
    Right(Unit)
  }
}