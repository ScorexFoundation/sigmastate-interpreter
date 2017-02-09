package sigmastate.utxo

import sigmastate.BlockchainProposition.Height
import sigmastate.BlockchainState


class BlockHeader(id: Array[Byte])

case class UtxoBlockchainState(override val height: Height,
                               transaction: SigmaStateTransaction,
                               lastHeaders: Map[Height, BlockHeader]) extends BlockchainState

