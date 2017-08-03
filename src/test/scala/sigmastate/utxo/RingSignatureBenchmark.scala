package sigmastate.utxo

import scorex.crypto.hash.Blake2b256
import sigmastate.{OR, TrueConstantNode}

object RingSignatureBenchmark extends App {

  val prover = new UtxoProvingInterpreter
  val verifier = new UtxoInterpreter
  val secret = prover.dlogSecrets.head

  val simulated = (1 to 99).map {_ =>
    new UtxoProvingInterpreter().dlogSecrets.head.publicImage
  }

  //fake message, in a real-life a message is to be derived from a spending transaction
  val message = Blake2b256("Hello World")
  val fakeSelf = SigmaStateBox(0, TrueConstantNode) -> 0L
  val ctx = UtxoContext(currentHeight = 1, spendingTransaction = null, self = fakeSelf)

  val publicImages = secret.publicImage +: simulated
  val prop = OR(publicImages)

  println("cost of ring sig: " + prop.cost)

  println(s"Ring signature benchmark for a ring of size ${publicImages.length}")

  val pt0 = System.currentTimeMillis()
  val proof = prover.prove(prop, ctx, message).get
  val pt = System.currentTimeMillis()
  println(s"Prover time: ${(pt-pt0)/1000.0} seconds")

  val vt0 = System.currentTimeMillis()
  verifier.verify(prop, ctx, proof, message)
  val vt = System.currentTimeMillis()
  println(s"Verifier time: ${(vt-vt0)/1000.0} seconds")
}
