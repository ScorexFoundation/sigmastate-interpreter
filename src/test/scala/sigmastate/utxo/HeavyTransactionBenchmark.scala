package sigmastate.utxo

import scapi.sigma.DLogProtocol.DLogNode
import sigmastate._

/*
object HeavyTransactionBenchmark extends App {

  val prover = new UtxoProvingInterpreter

  val crowdFundingScript = OR(
    AND(GE(Height, timeout), DLogNode(backerPubKey)),
    AND(
      Seq(
        LT(Height, timeout),
        DLogNode(projectPubKey),
        TxHasOutput(GE(OutputAmount, minToRaise), EQ(OutputScript, PropLeaf(DLogNode(projectPubKey))))
      )
    )
  )

}
*/