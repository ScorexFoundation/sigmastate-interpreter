package sigmastate.utxo

import org.ergoplatform._
import org.scalacheck.Gen
import scalan.util.BenchmarkUtil
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.authds.avltree.batch.{Lookup, BatchAVLProver, Insert}
import scorex.crypto.hash.{Digest32, Blake2b256}
import scorex.utils.Random
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate._
import sigmastate.eval._
import sigmastate.interpreter.Interpreter._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, SigmaTestingCommons, ErgoLikeTestInterpreter}
import sigmastate.lang.exceptions.CosterException


/**
  * Suite of tests where a malicious prover tries to feed a verifier with a script which is costly to verify
  */
class SpamSpecification extends SigmaTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext
  //we assume that verifier must finish verification of any script in less time than 3M hash calculations
  // (for the Blake2b256 hash function over a single block input)
  lazy val Timeout: Long = {
    val block = Array.fill(16)(0: Byte)
    val hf = Blake2b256

    //just in case to heat up JVM
    (1 to 1000000).foreach(_ => hf(block))

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

    val prover = new ContextEnrichingTestProvingInterpreter(CostTable.ScriptLimit * 10)
      .withContextExtender(id, ByteArrayConstant(ba))
      .withContextExtender(id2, ByteArrayConstant(ba))

    val spamScript = EQ(CalcBlake2b256(GetVarByteArray(id).get), CalcBlake2b256(GetVarByteArray(id2).get)).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage).get

    val verifier = new ErgoLikeTestInterpreter
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

    val ba = Random.randomBytes(5000000)

    val id = 21: Byte

    val prover = new ContextEnrichingTestProvingInterpreter(CostTable.ScriptLimit * 10).withContextExtender(id, ByteArrayConstant(ba))

    val bigSubScript = (1 to 100).foldLeft(CalcBlake2b256(GetVarByteArray(id).get)) { case (script, _) =>
      CalcBlake2b256(script)
    }

    val spamScript = NEQ(bigSubScript, CalcBlake2b256(ByteArrayConstant(Array.fill(32)(0: Byte)))).toSigmaProp

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val prt = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage)
    prt.isSuccess shouldBe true

    val pr = prt.get

    val ctxv = ctx.withExtension(pr.extension)

    val verifier = new ErgoLikeTestInterpreter
    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctxv, pr.proof, fakeMessage)
    )
    terminated shouldBe true
  }

  property("ring signature - maximum ok ring size") {
    val prover = new ContextEnrichingTestProvingInterpreter(maxCost = CostTable.ScriptLimit * 2)
    val verifier = new ErgoLikeTestInterpreter
    val secret = prover.dlogSecrets.head

    val simulated = (1 to 98).map { _ =>
      new ContextEnrichingTestProvingInterpreter().dlogSecrets.head.publicImage
    }

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val publicImages = secret.publicImage +: simulated
    val prop = OR(publicImages.map(image => SigmaPropConstant(image).isProven)).toSigmaProp

    val proof = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get

    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, proof, fakeMessage)
    )
    terminated shouldBe true
  }

  property("transaction with many outputs") {
    forAll(Gen.choose(10, 200), Gen.choose(200, 5000)) { case (orCnt, outCnt) =>
      whenever(orCnt > 10 && outCnt > 200) {
    val orCnt = 10
    val outCnt = 5
    val prover = new ContextEnrichingTestProvingInterpreter(maxCost = CostTable.ScriptLimit * 1000000L)

    val propToCompare = OR((1 to orCnt).map(_ => EQ(LongConstant(6), LongConstant(5)))).toSigmaProp

    val spamProp = OR((1 until orCnt).map(_ => EQ(LongConstant(6), LongConstant(5))) :+
      EQ(LongConstant(6), LongConstant(6))).toSigmaProp

    val spamScript =
      Exists(Outputs,
        FuncValue(Vector((1, SBox)),
          AND(
            GE(ExtractAmount(ValUse(1, SBox)), LongConstant(10)),
            EQ(
              ExtractScriptBytes(ValUse(1, SBox)),
              ByteArrayConstant(propToCompare.treeWithSegregation.bytes))
          )
        )
      ).toSigmaProp

    val txOutputs = ((1 to outCnt) map (_ => ErgoBox(11, spamProp, 0))) :+ ErgoBox(11, propToCompare, 0)
    val tx = createTransaction(txOutputs)

    val ctx = ErgoLikeContext.dummy(createBox(0, propToCompare)).withTransaction(tx)

    val pt0 = System.currentTimeMillis()
    val proof = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), spamScript, ctx, fakeMessage).get
    val pt = System.currentTimeMillis()
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")

    val verifier = new ErgoLikeTestInterpreter
    val (_, terminated) = termination(() =>
      verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), spamScript, ctx, proof, fakeMessage))
    terminated shouldBe true
      }
    }
  }

  property("transaction with many inputs and outputs") {
    implicit lazy val IR = new TestingIRContext {
      override val okPrintEvaluatedEntries = false
      override def onEvaluatedGraphNode(env: DataEnv, sym: Sym, value: AnyRef): Unit = {
        if (okPrintEvaluatedEntries)
          println(printEnvEntry(sym, value))
      }
    }
    val prover = new ContextEnrichingTestProvingInterpreter()
    val limitlessProver = new ContextEnrichingTestProvingInterpreter(maxCost = Long.MaxValue)

    val prop = Exists(Inputs,
      FuncValue(Vector((1, SBox)),
        Exists(Outputs,
          FuncValue(Vector((2, SBox)), EQ(ExtractScriptBytes(ValUse(1, SBox)), ExtractScriptBytes(ValUse(2, SBox))))))).toSigmaProp

    val inputScript = OR((1 to 200).map(_ => EQ(LongConstant(6), LongConstant(5)))).toSigmaProp
    val outputScript = OR((1 to 200).map(_ => EQ(LongConstant(6), LongConstant(6)))).toSigmaProp

    val inputs = ((1 to 999) map (_ => ErgoBox(11, inputScript, 0))) :+ ErgoBox(11, outputScript, 0)
    val outputs = (1 to 1000) map (_ => ErgoBox(11, outputScript, 0))

    val tx = createTransaction(outputs)

    val ctx = new ErgoLikeContext(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      dataBoxes = ErgoLikeContext.noBoxes,
      headers = ErgoLikeContext.noHeaders,
      preHeader = ErgoLikeContext.dummyPreHeader,
      boxesToSpend = inputs,
      spendingTransaction = tx,
      self = inputs(0))

    println(s"Timeout: ${Timeout / 1000.0} seconds")

    // check that execution terminated within timeout due to costing exception and cost limit
    val pt0 = System.currentTimeMillis()
    val (res, terminated) = termination(() => prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage))
    val pt = System.currentTimeMillis()
    println(s"Prover time: ${(pt - pt0) / 1000.0} seconds")
    terminated shouldBe true
    assertExceptionThrown(
      res.fold(t => throw t, identity),
      t => {
        rootCause(t).isInstanceOf[CosterException] && t.getMessage.contains("Script cannot be executed")
      }
    )

    // measure time required to execute the script itself and it is more then timeout
    val (_, calcTime) = BenchmarkUtil.measureTime {
      import limitlessProver.IR._
      val Pair(calcF, _) = doCostingEx(emptyEnv, prop, true)
      val calcCtx = ctx.toSigmaContext(limitlessProver.IR, isCost = false)
      limitlessProver.calcResult(calcCtx, calcF)
    }
    println(s"Full time to execute the script: ${calcTime / 1000.0} seconds")
    assert(calcTime > Timeout)
  }

  property("too heavy avl tree lookup") {
    val reg1 = ErgoBox.nonMandatoryRegisters.head
    def genKey(str: String): ADKey = ADKey @@ Blake2b256("key: " + str)
    def genValue(str: String): ADValue = ADValue @@ Blake2b256("val: " + str)

    val prover = new ContextEnrichingTestProvingInterpreter(Long.MaxValue)
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val avlProver = new BatchAVLProver[Digest32, Blake2b256.type](keyLength = 32, None)

    (1 to 100000).foreach {i =>
      avlProver.performOneOperation(Insert(genKey(s"key$i"), genValue(s"value$i")))
    }
    avlProver.generateProof()

    val digest = avlProver.digest

    (1 to 100000).foreach { i =>
      avlProver.performOneOperation(Lookup(genKey(s"key$i")))
    }

    val proof = avlProver.generateProof()

    println("proof size: " + proof.length)

    val treeData = SigmaDsl.avlTree(new AvlTreeData(digest, AvlTreeFlags.ReadOnly, 32, None))

    val key1 = genKey("key1")
    val value1 = genValue("value1")

    val prop = ErgoTree(ErgoTree.DefaultHeader, ErgoTree.EmptyConstants,
      EQ(
        IR.builder.mkMethodCall(
          ExtractRegisterAs[SAvlTree.type](Self, reg1).get,
          SAvlTree.getMethod,
          IndexedSeq(ByteArrayConstant(key1), ByteArrayConstant(proof))).asOption[SByteArray].get,
          ByteArrayConstant(value1)
      ).toSigmaProp
    )

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1)

    val spendingTransaction = createTransaction(newBoxes)

    val s = ErgoBox(20, ErgoScriptPredef.TrueProp, 0, Seq(), Map(reg1 -> AvlTreeConstant(treeData)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
    println("Cost: " + pr.cost)
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).isFailure shouldBe true
  }
}