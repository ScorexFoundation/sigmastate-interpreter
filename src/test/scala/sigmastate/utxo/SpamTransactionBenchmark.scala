package sigmastate.utxo

import scorex.crypto.hash.Blake2b256
import sigmastate._


object SpamTransactionBenchmark extends App {

  val prover = new UtxoProvingInterpreter

  val secret = prover.dlogSecrets.head
  val pubKey = secret.publicImage

  val propToCompare = OR((1 to 99).map(_ => pubKey))

  val spamPubKey = prover.dlogSecrets.tail.head.publicImage
  val spamProp = OR((1 to 98).map(_ => pubKey) :+ spamPubKey)

  val spamScript =
    TxHasOutput(GE(OutputAmount, IntLeaf(10)), EQ(OutputScript, PropLeaf(propToCompare)))

  println(spamScript.cost)

  val txOutputs = ((1 to 9999) map (_ => SigmaStateBox(11, spamProp))) :+ SigmaStateBox(11, propToCompare)
  val tx = SigmaStateTransaction(Seq(), txOutputs)

  //fake message, in a real-life a message is to be derived from a spending transaction
  val message = Blake2b256("Hello World")
  val fakeSelf = SigmaStateBox(0, propToCompare) -> 0L
  val ctx = UtxoContext(currentHeight = 100, spendingTransaction = tx, self = fakeSelf)

  val pt0 = System.currentTimeMillis()
  val proof = prover.prove(spamScript, ctx, message).get
  val pt = System.currentTimeMillis()
  println(s"Prover time: ${(pt-pt0)/1000.0} seconds")

  val verifier = new UtxoInterpreter
  val vt0 = System.currentTimeMillis()
  verifier.verify(spamScript, ctx, proof, message)
  val vt = System.currentTimeMillis()
  println(s"Verifier time: ${(vt-vt0)/1000.0} seconds")
}
