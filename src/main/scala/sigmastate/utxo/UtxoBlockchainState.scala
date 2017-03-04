package sigmastate.utxo

import sigmastate.BlockchainState
import sigmastate.utxo.UtxoBlockchainProposition.Height


class BlockHeader(id: Array[Byte])

case class UtxoBlockchainState(override val height: Height,
                               outputToSpend: SigmaStateBox,
                               spendingTransaction: SigmaStateTransaction,
                               lastHeaders: Map[Height, BlockHeader]) extends BlockchainState

