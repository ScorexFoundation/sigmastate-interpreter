package sigmastate.utxo

import scala.util.{Failure, Success}

object ErgoTransactionValidator {
  val verifier = new ErgoInterpreter()

  //todo: check that outputs are well-formed?
  def validate(tx: ErgoTransaction,
               blockchainState: BlockchainState,
               boxesReader: ErgoBoxReader): Either[Throwable, Int] = {

    val msg = tx.messageToSign
    val inputs = tx.inputs

    val boxes: IndexedSeq[ErgoBox] = tx.inputs.map(_.boxId).map{id =>
      boxesReader.byId(id) match {
        case Success(box) => box
        case Failure(e) => return Left[Throwable, Int](e)
      }
    }

    val txCost = boxes.zipWithIndex.foldLeft(0) { case (accCost, (box, idx)) =>
      val input = inputs(idx)
      val proof = input.spendingProof

      val proverExtension = tx.inputs(idx).spendingProof.extension

      val context =
        ErgoContext(blockchainState.currentHeight, blockchainState.lastBlockUtxoRoot, boxes, tx, box, proverExtension)

      val scriptCost: Int = verifier.verify(box.proposition, context, proof, msg) match {
        case Success((res, cost)) =>
          if(!res) return Left[Throwable, Int](new Exception(s"Validation failed for input #$idx"))
          else cost
        case Failure(e) =>
          return Left[Throwable, Int](e)
      }
      accCost + scriptCost
    }
    Right(txCost)
  }
}