package sigmastate.utxo

import scorex.core.transaction.box.Box


case class TransactionContainsBox(minAmountOpt: Option[Box.Amount], maxAmountOpt: Option[Box.Amount])


