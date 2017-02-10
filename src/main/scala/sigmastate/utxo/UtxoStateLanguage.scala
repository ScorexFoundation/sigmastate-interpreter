package sigmastate.utxo

import scorex.core.transaction.box.Box
import sigmastate.BlockchainProposition
import UtxoBlockchainProposition.Height

trait UtxoBlockchainProposition extends BlockchainProposition

object UtxoBlockchainProposition {
  type Height = Int
}

case class HeightFromProposition(from: Height) extends BlockchainProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class HeightUntilProposition(until: Height) extends BlockchainProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class HeightBetweenProposition(from: Height, until: Height) extends BlockchainProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class TransactionContainsBox(minAmountOpt: Option[Box.Amount],
                                  maxAmountOpt: Option[Box.Amount]) extends UtxoBlockchainProposition

case class BlockIdEquals(height: Height, id: Array[Byte]) extends UtxoBlockchainProposition


