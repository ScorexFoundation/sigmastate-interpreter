package sigmastate.utxo

import scorex.crypto.hash.Blake2b256
import scorex.utils.Random
import sigmastate._
import sigmastate.utils.Helpers


object CustomByteArrayBenchmark extends App {
  val ba = Random.randomBytes(100000000)

  val tag = Helpers.tagInt(ba)

  val prover = new UtxoProvingInterpreter().withContextExtender(tag, ByteArrayLeaf(ba))

  val spamScript = EQ(CalcBlake2b256(CustomByteArray(tag)), CalcBlake2b256(CustomByteArray(tag)))

  println("cost: " + spamScript.cost)

  val message = Blake2b256("Hello World")
  val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = SigmaStateBox(0, TrueConstantNode) -> 0)
  val pr = prover.prove(spamScript, ctx, message).get

  val ctxv = ctx.withExtension(pr.extension)

  val verifier = new UtxoInterpreter
  verifier.verify(spamScript, ctxv, pr.proof, message).get
}
