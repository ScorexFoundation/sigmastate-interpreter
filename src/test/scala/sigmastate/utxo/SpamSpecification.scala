package sigmastate.utxo

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Blake2b256
import scorex.utils.Random
import sigmastate.Values._
import sigmastate._
import sigmastate.utxo.BoxHelpers.createBox


/**
  * Suite of tests where a malicious prover tries to feed a verifier with a script which is costly to verify
  */
class SpamSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  private val fakeSelf = createBox(0, TrueLeaf)

  //fake message, in a real-life a message is to be derived from a spending transaction
  private val message = Blake2b256("Hello World")

  //we assume that verifier must finish verification of any script in less time than 3M hash calculations
  // (for the Blake2b256 hash function over a single block input)
  val Timeout: Long = {
    val block = Array.fill(16)(0: Byte)
    val hf = Blake2b256

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

    val id = 11: Byte

    val prover = new ErgoProvingInterpreter(CostTable.ScriptLimit * 10).withContextExtender(id, ByteArrayConstant(ba))

    val spamScript = EQ(CalcBlake2b256(TaggedByteArray(id)), CalcBlake2b256(TaggedByteArray(id)))

    val ctx = ErgoContext.dummy(fakeSelf)

    val prt = prover.prove(spamScript, ctx, message)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoInterpreter
    val (res, terminated) = termination(() => verifier.verify(spamScript, ctxv, pr.proof, message))

    res.isFailure shouldBe true
    terminated shouldBe true
  }

  property("big byte array with a lot of operations") {
    val ba = Random.randomBytes(5000000)

    val id = 21: Byte

    val prover = new ErgoProvingInterpreter(CostTable.ScriptLimit * 10).withContextExtender(id, ByteArrayConstant(ba))

    val bigSubScript = (1 to 289).foldLeft(CalcBlake2b256(TaggedByteArray(id))) { case (script, _) =>
      CalcBlake2b256(script)
    }

    val spamScript = NEQ(bigSubScript, CalcBlake2b256(ByteArrayConstant(Array.fill(32)(0: Byte))))

    val ctx = ErgoContext.dummy(fakeSelf)

    val prt = prover.prove(spamScript, ctx, message)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoInterpreter
    val (_, terminated) = termination(() => verifier.verify(spamScript, ctxv, pr.proof, message))
    terminated shouldBe true
  }

  property("ring signature - maximum ok ring size") {
    val prover = new ErgoProvingInterpreter(maxCost = CostTable.ScriptLimit * 2)
    val verifier = new ErgoInterpreter
    val secret = prover.dlogSecrets.head

    val simulated = (1 to 98).map { _ =>
      new ErgoProvingInterpreter().dlogSecrets.head.publicImage
    }

    val ctx = ErgoContext.dummy(fakeSelf)

    val publicImages = secret.publicImage +: simulated
    val prop = OR.fromSeq(publicImages)

    val pt0 = System.currentTimeMillis()
    val proof = prover.prove(prop, ctx, message).get
    val pt = System.currentTimeMillis()

    val (_, terminated) = termination(() => verifier.verify(prop, ctx, proof, message))
    terminated shouldBe true
  }

  property("transaction with many outputs") {
    forAll(Gen.choose(10, 200), Gen.choose(200, 5000)) { case (orCnt, outCnt) =>
      whenever(orCnt > 10 && outCnt > 200) {
        val prover = new ErgoProvingInterpreter(maxCost = CostTable.ScriptLimit * 1000)

        val propToCompare = OR.fromSeq((1 to orCnt).map(_ => EQ(IntConstant(6), IntConstant(5))))

        val spamProp = OR.fromSeq((1 until orCnt).map(_ => EQ(IntConstant(6), IntConstant(5))) :+
          EQ(IntConstant(6), IntConstant(6)))

        val spamScript =
          Exists(Outputs, 21,
            AND(
              GE(ExtractAmount(TaggedBox(21)), IntConstant(10)),
              EQ(ExtractScriptBytes(TaggedBox(21)), ByteArrayConstant(propToCompare.toString.getBytes))
            )
          )

        val txOutputs = ((1 to outCnt) map (_ => ErgoBox(11, spamProp))) :+ ErgoBox(11, propToCompare)
        val tx = ErgoTransaction(IndexedSeq(), txOutputs)

        val ctx = ErgoContext.dummy(createBox(0, propToCompare)).copy(spendingTransaction = tx)

        val pt0 = System.currentTimeMillis()
        val proof = prover.prove(spamScript, ctx, message).get
        val pt = System.currentTimeMillis()
        println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

        val verifier = new ErgoInterpreter
        val (_, terminated) = termination(() => verifier.verify(spamScript, ctx, proof, message))
        terminated shouldBe true
      }
    }
  }

  property("transaction with many inputs and outputs") {
    val prover = new ErgoProvingInterpreter(maxCost = Int.MaxValue)

    val prop = Exists(Inputs, 21, Exists(Outputs, 22,
      EQ(ExtractScriptBytes(TaggedBox(21)), ExtractScriptBytes(TaggedBox(22)))))

    val inputScript = OR.fromSeq((1 to 200).map(_ => EQ(IntConstant(6), IntConstant(5))))
    val outputScript = OR.fromSeq((1 to 200).map(_ => EQ(IntConstant(6), IntConstant(6))))

    val inputs = ((1 to 999) map (_ => ErgoBox(11, inputScript))) :+ ErgoBox(11, outputScript)
    val outputs = (1 to 1000) map (_ => ErgoBox(11, outputScript))

    val tx = ErgoTransaction(IndexedSeq(), outputs)

    val ctx = new ErgoContext(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = inputs,
      spendingTransaction = tx,
      self = ErgoBox(11, prop))

    val pt0 = System.currentTimeMillis()
    val proof = prover.prove(prop, ctx, message).get
    val pt = System.currentTimeMillis()
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

    val verifier = new ErgoInterpreter
    val (res, terminated) = termination(() => verifier.verify(prop, ctx, proof, message))
    terminated shouldBe true
    res.isFailure shouldBe true
  }
}