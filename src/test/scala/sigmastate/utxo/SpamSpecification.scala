package sigmastate.utxo

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.hash.Blake2b256
import scorex.utils.Random
import sigmastate.utils.Helpers
import sigmastate._


/**
  * Suite of tests where a malicious prover tries to feed a verifier with a script which is costly to verify
  */
class SpamSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  property("huge byte array") {
    //todo: make value dependent on CostTable constants, not magic constant
    val ba = Random.randomBytes(10000000)

    val tag = Helpers.tagInt(ba)

    val prover = new UtxoProvingInterpreter(CostTable.ScriptLimit * 10).withContextExtender(tag, ByteArrayLeaf(ba))

    val spamScript = EQ(CalcBlake2b256(CustomByteArray(tag)), CalcBlake2b256(CustomByteArray(tag)))

    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = SigmaStateBox(0, TrueConstantNode) -> 0)

    val prt = prover.prove(spamScript, ctx, message)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    verifier.verify(spamScript, ctxv, pr.proof, message).isFailure shouldBe true
  }

  property("big byte array with a lot of operations") {
    val ba = Random.randomBytes(5000000)

    val tag = Helpers.tagInt(ba)

    val prover = new UtxoProvingInterpreter(CostTable.ScriptLimit * 10).withContextExtender(tag, ByteArrayLeaf(ba))

    val bigSubScript = (1 to 289).foldLeft(CalcBlake2b256(CustomByteArray(tag))){case (script, _) =>
      CalcBlake2b256(script)
    }

    val spamScript = NEQ(bigSubScript, CalcBlake2b256(ByteArrayLeaf(Array.fill(32)(0: Byte))))

    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 0, spendingTransaction = null, self = SigmaStateBox(0, TrueConstantNode) -> 0)

    val prt = prover.prove(spamScript, ctx, message)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val vt0 = System.currentTimeMillis()
    val verifier = new UtxoInterpreter
    println(verifier.verify(spamScript, ctxv, pr.proof, message))
    val vt = System.currentTimeMillis()
    println(s"Verifier time: ${(vt - vt0) / 1000.0} seconds")
  }

  property("ring signature") {
    val prover = new UtxoProvingInterpreter(maxCost = CostTable.ScriptLimit * 2)
    val verifier = new UtxoInterpreter
    val secret = prover.dlogSecrets.head

    val simulated = (1 to 99).map { _ =>
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
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

    val vt0 = System.currentTimeMillis()
    verifier.verify(prop, ctx, proof, message).isFailure shouldBe true
    val vt = System.currentTimeMillis()
    println(s"Verifier time: ${(vt - vt0) / 1000.0} seconds")
  }

  property("transaction with many outputs") {
    val prover = new UtxoProvingInterpreter(maxCost = CostTable.ScriptLimit * 10)

    val propToCompare = OR((1 to 99).map(_ => IntLeaf(5)))

    val spamProp = OR((1 to 98).map(_ => IntLeaf(5)) :+ IntLeaf(6))

    val spamScript =
      TxHasOutput(GE(OutputAmount, IntLeaf(10)), EQ(OutputScript, PropLeaf(propToCompare)))

    println(spamScript.cost)

    val txOutputs = ((1 to 1599) map (_ => SigmaStateBox(11, spamProp))) :+ SigmaStateBox(11, propToCompare)
    val tx = SigmaStateTransaction(Seq(), txOutputs)

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val fakeSelf = SigmaStateBox(0, propToCompare) -> 0L
    val ctx = UtxoContext(currentHeight = 100, spendingTransaction = tx, self = fakeSelf)

    val pt0 = System.currentTimeMillis()
    val proof = prover.prove(spamScript, ctx, message).get
    val pt = System.currentTimeMillis()
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

    val verifier = new UtxoInterpreter
    val vt0 = System.currentTimeMillis()
    verifier.verify(spamScript, ctx, proof, message)
    val vt = System.currentTimeMillis()
    println(s"Verifier time: ${(vt - vt0) / 1000.0} seconds")
  }
}