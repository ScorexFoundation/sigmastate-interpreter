package sigmastate.utxo

import scorex.core.transaction.box.Box
import sigmastate.BlockchainProposition
import UtxoBlockchainProposition.Height

trait UtxoBlockchainProposition extends BlockchainProposition

object UtxoBlockchainProposition {
  type Height = Int
}