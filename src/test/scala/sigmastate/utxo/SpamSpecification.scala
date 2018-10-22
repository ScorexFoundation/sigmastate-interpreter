package sigmastate.utxo

import org.ergoplatform
import org.ergoplatform.ErgoLikeContext.Metadata
import org.ergoplatform.ErgoLikeContext.Metadata._
import org.ergoplatform._
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256
import scorex.utils.Random
import sigmastate.Values._
import sigmastate._
import sigmastate.interpreter.Interpreter._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}


/**
  * Suite of tests where a malicious prover tries to feed a verifier with a script which is costly to verify
  */
class SpamSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  //we assume that verifier must finish verification of any script in less time than 3M hash calculations
  // (for the Blake2b256 hash function over a single block input)
  lazy val Timeout: Long = {
    val block = Array.fill(16)(0: Byte)
    val hf = Blake2b256

    //just in case to heat up JVM
    (1 to 2000000).foreach(_ => hf(block))

    val t0 = System.currentTimeMillis()
    (1 to 4000000).foreach(_ => hf(block))
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
    val id2 = 12: Byte

    val prover = new ErgoLikeProvingInterpreter(CostTable.ScriptLimit * 10)
      .withContextExtender(id, ByteArrayConstant(ba))
      .withContextExtender(id2, ByteArrayConstant(ba))

    val spamScript = EQ(CalcBlake2b256(GetVarByteArray(id).get), CalcBlake2b256(GetVarByteArray(id2).get))

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val prt = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage)
//    prt.isSuccess shouldBe true

    val pr = prt.get

    val verifier = new ErgoLikeInterpreter
    val (res, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, pr, fakeMessage)
    )

    res.isFailure shouldBe true
    terminated shouldBe true
  }

  /** This case verifies behavior of script interpreter when given enormously deep tree.
    * Below it is at least 150 levels.
    * When transaction is validated the script is deserialized for execution.
    * It should be checked by deserializer for it's depth.
    * The scripts with more than 150 levels are considered malicious.
  */
  property("big byte array with a lot of operations") {
//    fail("fix the stack overflow in this test")

    val ba = Random.randomBytes(5000000)

    val id = 21: Byte

    val prover = new ErgoLikeProvingInterpreter(CostTable.ScriptLimit * 10).withContextExtender(id, ByteArrayConstant(ba))

    val bigSubScript = (1 to 150).foldLeft(CalcBlake2b256(GetVarByteArray(id).get)) { case (script, _) =>
      CalcBlake2b256(script)
    }

    val spamScript = NEQ(bigSubScript, CalcBlake2b256(ByteArrayConstant(Array.fill(32)(0: Byte))))

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val prt = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeInterpreter
    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctxv, pr.proof, fakeMessage)
    )
    terminated shouldBe true
  }

  property("ring signature - maximum ok ring size") {
    val prover = new ErgoLikeProvingInterpreter(maxCost = CostTable.ScriptLimit * 2)
    val verifier = new ErgoLikeInterpreter
    val secret = prover.dlogSecrets.head

    val simulated = (1 to 98).map { _ =>
      new ErgoLikeProvingInterpreter().dlogSecrets.head.publicImage
    }

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val publicImages = secret.publicImage +: simulated
    val prop = OR(publicImages.map(image => SigmaPropConstant(image).isValid))

    val pt0 = System.currentTimeMillis()
    val proof = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
    val pt = System.currentTimeMillis()

    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, proof, fakeMessage)
    )
    terminated shouldBe true
  }

  // TODO solve: key not found: s4501
  ignore("transaction with many outputs") {
//    forAll(Gen.choose(10, 200), Gen.choose(200, 5000)) { case (orCnt, outCnt) =>
//      whenever(orCnt > 10 && outCnt > 200) {
    val orCnt = 10
    val outCnt = 5
    val prover = new ErgoLikeProvingInterpreter(maxCost = CostTable.ScriptLimit * 1000000L)

    val propToCompare = OR((1 to orCnt).map(_ => EQ(LongConstant(6), LongConstant(5))))

    val spamProp = OR((1 until orCnt).map(_ => EQ(LongConstant(6), LongConstant(5))) :+
      EQ(LongConstant(6), LongConstant(6)))

    val spamScript =
      Exists(Outputs, 21,
        AND(
          GE(ExtractAmount(TaggedBox(21)), LongConstant(10)),
          EQ(ExtractScriptBytes(TaggedBox(21)), ByteArrayConstant(propToCompare.bytes))
        )
      )

    val txOutputs = ((1 to outCnt) map (_ => ErgoBox(11, spamProp))) :+ ErgoBox(11, propToCompare)
    val tx = ErgoLikeTransaction(IndexedSeq(), txOutputs)

    val ctx = ErgoLikeContext.dummy(createBox(0, propToCompare)).withTransaction(tx)

    val pt0 = System.currentTimeMillis()
    val proof = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage).get
    val pt = System.currentTimeMillis()
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

    val verifier = new ErgoLikeInterpreter
    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, proof, fakeMessage))
    terminated shouldBe true
//      }
//    }
  }

  // TODO solve: key not found: s718
  ignore("transaction with many inputs and outputs") {
    implicit lazy val IR = new TestingIRContext { override val okPrintEvaluatedEntries = false }
    val prover = new ErgoLikeProvingInterpreter(maxCost = Long.MaxValue)

    val prop = Exists(Inputs, 21, Exists(Outputs, 22,
      EQ(ExtractScriptBytes(TaggedBox(21)), ExtractScriptBytes(TaggedBox(22)))))

    val inputScript = OR((1 to 200).map(_ => EQ(LongConstant(6), LongConstant(5))))
    val outputScript = OR((1 to 200).map(_ => EQ(LongConstant(6), LongConstant(6))))

    val inputs = ((1 to 999) map (_ => ErgoBox(11, inputScript))) :+ ErgoBox(11, outputScript)
    val outputs = (1 to 1000) map (_ => ErgoBox(11, outputScript))

    val tx = ergoplatform.ErgoLikeTransaction(IndexedSeq(), outputs)

    val ctx = new ErgoLikeContext(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = inputs,
      spendingTransaction = tx,
      self = ErgoBox(11, prop),
      metadata = Metadata(TestnetNetworkPrefix))

    val pt0 = System.currentTimeMillis()
    val proof = prover.prove(prop, ctx, fakeMessage).get
    val pt = System.currentTimeMillis()
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

    val verifier = new ErgoLikeInterpreter
    val (res, terminated) = termination(() => verifier.verify(prop, ctx, proof, fakeMessage))
    terminated shouldBe true
    res.isFailure shouldBe true
  }
}