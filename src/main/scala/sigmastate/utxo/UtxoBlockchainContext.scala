package sigmastate.utxo

import sigmastate.Context
import sigmastate.utxo.UtxoBlockchainProposition.Height


class BlockHeader(id: Array[Byte])

case class UtxoBlockchainContext(height: Height,
                                 outputToSpend: SigmaStateBox,
                                 spendingTransaction: SigmaStateTransaction,
                                 lastHeaders: Map[Height, BlockHeader]) extends Context

