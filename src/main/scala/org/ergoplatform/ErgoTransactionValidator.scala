package org.ergoplatform

import sigmastate.eval.{RuntimeIRContext, IRContext}
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}

import scala.util.{Success, Failure}

class ErgoTransactionValidator(implicit IR: IRContext) {
  val verifier = new ErgoLikeInterpreter()

  //todo: check that outputs are well-formed?
  def validate(tx: ErgoLikeTransaction,
               blockchainState: BlockchainState,
               boxesReader: ErgoBoxReader): Either[Throwable, Long] = {

    val msg = tx.messageToSign
    val inputs = tx.inputs

    val boxes: IndexedSeq[ErgoBox] = tx.inputs.map(_.boxId).map{id =>
      boxesReader.byId(id) match {
        case Success(box) => box
        case Failure(e) => return Left[Throwable, Long](e)
      }
    }

    val txCost = boxes.zipWithIndex.foldLeft(0L) { case (accCost, (box, idx)) =>
      val input = inputs(idx)
      val proof = input.spendingProof

      val proverExtension = tx.inputs(idx).spendingProof.extension

      val context =
        ErgoLikeContext(blockchainState.currentHeight, blockchainState.lastBlockUtxoRoot, boxes,
          tx, box, proverExtension)
      val verificationResult = verifier.verify(
        emptyEnv + (ScriptNameProp -> s"height_${blockchainState.currentHeight }_verify"),
        box.proposition, context, proof, msg)
      val scriptCost: Long = verificationResult match {
        case Success((res, cost)) =>
          if(!res) return Left[Throwable, Long](new Exception(s"Validation failed for input #$idx"))
          else cost
        case Failure(e) =>
          return Left[Throwable, Long](e)
      }
      accCost + scriptCost
    }
    Right(txCost)
  }
}