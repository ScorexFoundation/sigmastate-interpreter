package sigmastate.utxo

import scorex.crypto.hash.Blake2b256
import scorex.utils.Random
import sigmastate._
import sigmastate.utils.Helpers


//todo: convert to test
object CustomByteArrayBenchmark extends App {
  val ba = Random.randomBytes(10000000)

  val tag = Helpers.tagInt(ba)

  val prover = new UtxoProvingInterpreter(CostTable.ScriptLimit * 10).withContextExtender(tag, ByteArrayLeaf(ba))

  val spamScript = EQ(CalcBlake2b256(CustomByteArray(tag)), CalcBlake2b256(CustomByteArray(tag)))

  println("cost: " + spamScript.cost)

  val message = Blake2b256("Hello World")
  val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = SigmaStateBox(0, TrueConstantNode) -> 0)

  val prt = prover.prove(spamScript, ctx, message)
  assert(prt.isSuccess)

  val pr = prt.get

  val ctxv = ctx.withExtension(pr.extension)

  val verifier = new UtxoInterpreter
  assert(verifier.verify(spamScript, ctxv, pr.proof, message).isFailure)
}
