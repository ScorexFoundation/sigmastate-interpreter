package sigmastate.utxo

import scorex.core.transaction.box.Box
import sigmastate.BlockchainProposition
import BlockchainProposition.Height

trait UtxoBlockchainProposition extends BlockchainProposition

case class TransactionContainsBox(minAmountOpt: Option[Box.Amount],
                                  maxAmountOpt: Option[Box.Amount]) extends UtxoBlockchainProposition

case class BlockIdEquals(height: Height, id: Array[Byte]) extends UtxoBlockchainProposition


