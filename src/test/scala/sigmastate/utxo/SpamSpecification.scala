package sigmastate.utxo

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.hash.{Blake2b256, Blake2b256Unsafe}
import scorex.utils.Random
import sigmastate.utils.Helpers
import sigmastate._
import BoxHelpers.boxWithMetadata


/**
  * Suite of tests where a malicious prover tries to feed a verifier with a script which is costly to verify
  */
class SpamSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  //we assume that verifier must finish verification of any script in less time than 3M hash calculations
  // (for the Blake2b256 hash function over a single block input)
  val Timeout: Long = {
    val block = Array.fill(16)(0: Byte)
    val hf = new Blake2b256Unsafe

    //just in case to heat up JVM
    (1 to 1000000).foreach(_ => hf(block))

    val t0 = System.currentTimeMillis()
    (1 to 3000000).foreach(_ => hf(block))
    val t = System.currentTimeMillis()
    t - t0
  }

  def termination[T](fn: () => T): (T, Boolean) = {
    val t0 = System.currentTimeMillis()
    val res = fn()
    val t = System.currentTimeMillis()
    (res, (t - t0) < Timeout)
  }


  property("huge byte array") {
    //todo: make value dependent on CostTable constants, not magic constant
    val ba = Random.randomBytes(10000000)

    val tag = Helpers.tagInt(ba)

    val prover = new UtxoProvingInterpreter(CostTable.ScriptLimit * 10).withContextExtender(tag, ByteArrayLeafConstant(ba))

    val spamScript = EQ(CalcBlake2b256Inst(CustomByteArray(tag)), CalcBlake2b256Inst(CustomByteArray(tag)))

    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 0, Seq(), spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))

    val prt = prover.prove(spamScript, ctx, message)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    val (res, terminated) = termination(() => verifier.verify(spamScript, ctxv, pr.proof, message))

    res.isFailure shouldBe true
    terminated shouldBe true
  }

  property("big byte array with a lot of operations") {
    val ba = Random.randomBytes(5000000)

    val tag = Helpers.tagInt(ba)

    val prover = new UtxoProvingInterpreter(CostTable.ScriptLimit * 10).withContextExtender(tag, ByteArrayLeafConstant(ba))

    val bigSubScript = (1 to 289).foldLeft(CalcBlake2b256Inst(CustomByteArray(tag))) { case (script, _) =>
      CalcBlake2b256Inst(script)
    }

    val spamScript = NEQ(bigSubScript, CalcBlake2b256Inst(ByteArrayLeafConstant(Array.fill(32)(0: Byte))))

    val message = Blake2b256("Hello World")
    val ctx = UtxoContext(currentHeight = 0, Seq(), spendingTransaction = null, self = boxWithMetadata(0, TrueLeaf))

    val prt = prover.prove(spamScript, ctx, message)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new UtxoInterpreter
    val (_, terminated) = termination(() => verifier.verify(spamScript, ctxv, pr.proof, message))
    terminated shouldBe true
  }

  property("ring signature - maximum ok ring size") {
    val prover = new UtxoProvingInterpreter(maxCost = CostTable.ScriptLimit * 2)
    val verifier = new UtxoInterpreter
    val secret = prover.dlogSecrets.head

    val simulated = (1 to 98).map { _ =>
      new UtxoProvingInterpreter().dlogSecrets.head.publicImage
    }

    //fake message, in a real-life a message is to be derived from a spending transaction
    val message = Blake2b256("Hello World")
    val fakeSelf = boxWithMetadata(0, TrueLeaf)
    val ctx = UtxoContext(currentHeight = 1, Seq(), spendingTransaction = null, self = fakeSelf)

    val publicImages = secret.publicImage +: simulated
    val prop = OR(publicImages)

    println("cost of ring sig: " + prop.cost)

    println(s"Ring signature benchmark for a ring of size ${publicImages.length}")

    val pt0 = System.currentTimeMillis()
    val proof = prover.prove(prop, ctx, message).get
    val pt = System.currentTimeMillis()
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

    val (_, terminated) = termination(() => verifier.verify(prop, ctx, proof, message))
    terminated shouldBe true
  }

  property("transaction with many outputs") {
    forAll(Gen.choose(10, 200), Gen.choose(200, 5000)) { case (orCnt, outCnt) =>
      whenever(orCnt > 10 && outCnt > 200) {
        val prover = new UtxoProvingInterpreter(maxCost = CostTable.ScriptLimit * 1000)

        val propToCompare = OR((1 to orCnt).map(_ => EQ(IntLeafConstant(6), IntLeafConstant(5)) ))

        val spamProp = OR((1 until orCnt).map(_ => EQ(IntLeafConstant(6), IntLeafConstant(5))) :+
                            EQ(IntLeafConstant(6), IntLeafConstant(6)))

        val spamScript =
          Exists(Outputs, GE(ExtractAmountFn, IntLeafConstant(10)),
            EQ(ExtractScriptFn, PropLeafConstant(propToCompare.toString.getBytes)))


        val txOutputs = ((1 to outCnt) map (_ => SigmaStateBox(11, spamProp))) :+ SigmaStateBox(11, propToCompare)
        val tx = SigmaStateTransaction(Seq(), txOutputs)

        //fake message, in a real-life a message is to be derived from a spending transaction
        val message = Blake2b256("Hello World")
        val fakeSelf = boxWithMetadata(0, propToCompare)
        val ctx = UtxoContext(currentHeight = 100, Seq(), spendingTransaction = tx, self = fakeSelf)

        val pt0 = System.currentTimeMillis()
        prover.prove(spamScript, ctx, message).map { proof =>
          val pt = System.currentTimeMillis()
          println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

          val verifier = new UtxoInterpreter
          val (_, terminated) = termination(() => verifier.verify(spamScript, ctx, proof, message))
          terminated shouldBe true
        }
      }
    }
  }
}