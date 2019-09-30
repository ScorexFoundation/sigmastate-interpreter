package sigmastate.utxo

import com.google.common.primitives.Bytes
import org.ergoplatform.ErgoBox.R4
import org.ergoplatform._
import org.ergoplatform.validation.ValidationException
import org.scalatest.TryValues._
import scorex.crypto.hash.Blake2b256
import sigmastate.SCollection.SByteArray
import sigmastate.TrivialProp.{FalseProp, TrueProp}
import sigmastate.Values._
import sigmastate._
import sigmastate.eval._
import sigmastate.interpreter.Interpreter._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDHTuple
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter, ErgoLikeTransactionTesting, SigmaTestingCommons}
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.InterpreterException
import sigmastate.serialization.{SerializationSpecification, ValueSerializer}

class ErgoLikeInterpreterSpecification extends SigmaTestingCommons
  with SerializationSpecification {

  implicit lazy val IR: TestingIRContext = new TestingIRContext
  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  property("scripts EQ/NEQ") {
    val prover1 = new ContextEnrichingTestProvingInterpreter
    val prover2 = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val h1 = SigmaPropConstant(prover1.dlogSecrets.head.publicImage)
    val h2 = SigmaPropConstant(prover2.dlogSecrets.head.publicImage)

    val ctx = ErgoLikeContextTesting.dummy(fakeSelf)

    val e = compile(Map("h1" -> h1.treeWithSegregation.bytes, "h2" -> h2.treeWithSegregation.bytes), "h1 == h1")
    val exp = TrueLeaf
    e shouldBe exp

    val res = verifier.reduceToCrypto(ctx, exp).get._1
    res shouldBe TrueProp

    val res2 = verifier.reduceToCrypto(ctx,
      EQ(ByteArrayConstant(h1.treeWithSegregation.bytes),
        ByteArrayConstant(h2.treeWithSegregation.bytes))).get._1
    res2 shouldBe FalseProp
  }

  property("DH tuple") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val fakeProver = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val secret = prover.dhSecrets.head

    val ci = secret.commonInput

    val prop = SigmaPropConstant(ProveDHTuple(ci.g, ci.h, ci.u, ci.v))
    val wrongProp = SigmaPropConstant(ProveDHTuple(ci.g, ci.h, ci.u, ci.u))

    val env = Map("g" -> ci.g, "h" -> ci.h, "u" -> ci.u, "v" -> ci.v, "s" -> secret.publicImage)
    val compiledProp1 = compile(env, "s").asSigmaProp
    val compiledProp2 = compile(env, "proveDHTuple(g, h, u, v)").asSigmaProp
    compiledProp1 shouldBe prop
    compiledProp2 shouldBe prop


    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true

    fakeProver.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
    prover.prove(wrongProp, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("DH tuple - simulation") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhB = proverB.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhB" -> pubdhB)
    val compiledProp = compile(env, """pubkeyA || pubdhB""").asSigmaProp

    val prop = SigmaOr(pubkeyA, pubdhB)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true
  }

  property("DH tuple and DLOG") {
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhA = proverA.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhA" -> pubdhA)
    val compiledProp = compile(env, """pubkeyA && pubdhA""").asSigmaProp

    val prop = SigmaAnd(pubkeyA, pubdhA)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContextTesting(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction = ErgoLikeTransactionTesting.dummy,
      self = fakeSelf)

    val prA = proverA.prove(compiledProp, ctx, fakeMessage).get

    verifier.verify(compiledProp, ctx, prA, fakeMessage).get._1 shouldBe true

    proverB.prove(compiledProp, ctx, fakeMessage).isSuccess shouldBe false
  }

  // related to https://github.com/ScorexFoundation/sigmastate-interpreter/issues/428
  // checks that cost of the folded function doesn't depend on data
  property("mixing scenario w. timeout") {
    val height = 50
    val proverA = new ContextEnrichingTestProvingInterpreter
    val proverB = new ContextEnrichingTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyA2 = proverA.dlogSecrets.head.publicImage

    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyB2 = proverB.dlogSecrets.head.publicImage

    val newBox1 = new ErgoBoxCandidate(10, pubkeyB2, height)
    val newBox2 = new ErgoBoxCandidate(10, pubkeyA2, height)

    val newBoxes = IndexedSeq(newBox1, newBox2)

    val properBytes = Bytes.concat(newBoxes.map(_.bytesWithNoRef): _*)

    val properHash = Blake2b256(properBytes)

    val spendingTransaction = createTransaction(newBoxes)

    def mixingRequestProp(sender: ProveDlog, timeout: Int) = {
      val env = Map("sender" -> sender, "timeout" -> timeout, "properHash" -> properHash)
      val compiledProp = compile(env,
        """{
          |  val notTimePassed = HEIGHT <= timeout
          |  val outBytes = OUTPUTS.map({(box: Box) => box.bytesWithoutRef})
          |  val outSumBytes = outBytes.flatMap({(bytes: Coll[Byte]) => bytes})
          |  val timePassed = HEIGHT > timeout
          |  notTimePassed && blake2b256(outSumBytes) == properHash || timePassed && sender
           }""".stripMargin).asSigmaProp

      compiledProp
    }

    val ctx = ErgoLikeContextTesting(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf)

    //before timeout
    val prA = proverA.prove(
      emptyEnv + (ScriptNameProp -> "before_timeout_prove"),
      mixingRequestProp(pubkeyA, 100), ctx, fakeMessage).get
    verifier.verify(
      emptyEnv + (ScriptNameProp -> "before_timeout_verify1"),
      mixingRequestProp(pubkeyA, 100), ctx, prA, fakeMessage).get._1 shouldBe true
    verifier.verify(
      emptyEnv + (ScriptNameProp -> "before_timeout_verify2"),
      mixingRequestProp(pubkeyB, 100), ctx, prA, fakeMessage).get._1 shouldBe true

    //after timeout
    val prA2 = proverA.prove(
        emptyEnv + (ScriptNameProp -> "after_timeout_prove"),
        mixingRequestProp(pubkeyA, 40), ctx, fakeMessage).get
    verifier.verify(
      emptyEnv + (ScriptNameProp -> "after_timeout_verify1"),
      mixingRequestProp(pubkeyA, 40), ctx, prA2, fakeMessage).get._1 shouldBe true
    verifier.verify(
      emptyEnv + (ScriptNameProp -> "after_timeout_verify2"),
      mixingRequestProp(pubkeyB, 40), ctx, prA2, fakeMessage).map(_._1).getOrElse(false) shouldBe false
  }

  property("map + sum") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env,
      """{
        |  val outValues = OUTPUTS.map({ (box: Box) => box.value })
        |  pubkey && outValues.fold(0L, { (x: Long, y: Long) => x + y }) > 20
         }""".stripMargin).asSigmaProp

    val propExp = BlockValue(
      Vector(
        ValDef(1, MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox))))),
        ValDef(2, Fold.sum[SLong.type](ValUse(1, SCollection.SLongArray), 2))
      ),
      SigmaAnd(
        Seq(
          SigmaPropConstant(pubkey),
          BoolToSigmaProp(GT(ValUse(2, SLong), LongConstant(20)))
        )
      ))
    prop shouldBe propExp

    val newBox1 = ErgoBox(11, pubkey, 0)
    val newBox2 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)
  }

  property("byindex") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val compiledProp = compile(env, """pubkey && OUTPUTS(0).value > 10""").asSigmaProp

    val prop = SigmaAnd(pubkey, BoolToSigmaProp(GT(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(10))))
    compiledProp shouldBe prop

    val newBox1 = ErgoBox(11, pubkey, 0)
    val newBox2 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(compiledProp, ctx, fakeMessage).get
    verifier.verify(compiledProp, ctx, pr, fakeMessage)


    val fProp1 = SigmaAnd(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(11)))
    prover.prove(fProp1, ctx, fakeMessage).isSuccess shouldBe false

    val fProp2 = SigmaAnd(pubkey, GT(ExtractAmount(ByIndex(Outputs, 1)), LongConstant(11)))
    prover.prove(fProp2, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("P2SH") {
    val scriptId = 21.toByte

    val prover0 = new ContextEnrichingTestProvingInterpreter()

    val script = SigmaPropConstant(prover0.dlogSecrets.head.publicImage)
    val scriptBytes = ValueSerializer.serialize(script)
    val scriptHash = Blake2b256(scriptBytes)

    val prover = prover0.withContextExtender(scriptId, ByteArrayConstant(scriptBytes))

    val hashEquals = EQ(CalcBlake2b256(GetVarByteArray(scriptId).get), scriptHash)
    val scriptIsCorrect = DeserializeContext(scriptId, SSigmaProp)
    val prop = SigmaAnd(hashEquals, scriptIsCorrect)

    val recipientProposition = SigmaPropConstant(new ContextEnrichingTestProvingInterpreter().dlogSecrets.head.publicImage)
    val selfBox = ErgoBox(20, ErgoScriptPredef.TrueProp, 0, Seq(), Map())
    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      createTransaction(ErgoBox(1, recipientProposition, 0)),
      self = selfBox)

    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true
  }

  property("Prove keys from registers") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage
    val pubkey3 = prover.dlogSecrets(2).publicImage

    val regPubkey1 = ErgoBox.nonMandatoryRegisters.head
    val regPubkey2 = ErgoBox.nonMandatoryRegisters.tail.head

    val prop = compile(Map(),
      """{
        |  val pubkey1 = SELF.R4[GroupElement].get
        |  val pubkey2 = SELF.R5[GroupElement].get
        |  proveDlog(pubkey1) && proveDlog(pubkey2)
        |}""".stripMargin).asSigmaProp

    val propTree = SigmaAnd(
      CreateProveDlog(ExtractRegisterAs[SGroupElement.type](Self, regPubkey1).get),
      CreateProveDlog(ExtractRegisterAs[SGroupElement.type](Self, regPubkey2).get))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey3, 0)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = createTransaction(newBoxes)

    val s1 = ErgoBox(20, ErgoScriptPredef.TrueProp, 0, Seq(),
      Map(regPubkey1 -> GroupElementConstant(pubkey1.value),
        regPubkey2 -> GroupElementConstant(pubkey2.value)))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s1),
      spendingTransaction,
      self = s1)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).fold(t => throw t, x => x)
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).fold(t => throw t, x => x)._1 shouldBe true


    //make sure that wrong case couldn't be proved
    val s2 = ErgoBox(20, ErgoScriptPredef.TrueProp, 0, Seq(),
      Map(regPubkey1 -> GroupElementConstant(pubkey1.value)))
    val wrongCtx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s2),
      spendingTransaction,
      self = s2)

    prover.prove(prop, wrongCtx, fakeMessage).isFailure shouldBe true
    verifier.verify(prop, wrongCtx, pr, fakeMessage).isFailure shouldBe true
  }

  //p2sh with 160-bit hash function (which is about just cutting first 160 bits from 256-bit hash)
  property("P2SH - 160 bits") {
    val scriptId = 21.toByte

    val bitsCount = 160
    val bytesCount = bitsCount / 8

    val prover0 = new ContextEnrichingTestProvingInterpreter()

    val customScript = prover0.dlogSecrets.head.publicImage.toSigmaProp
    val scriptBytes = ValueSerializer.serialize(customScript)
    val scriptHash = Blake2b256(scriptBytes).take(bytesCount)

    val prover = prover0.withContextExtender(scriptId, ByteArrayConstant(scriptBytes))

    val hashEquals = EQ(
      Slice(CalcBlake2b256(GetVarByteArray(scriptId).get), IntConstant(0), IntConstant(bytesCount)),
      scriptHash)
    val scriptIsCorrect = DeserializeContext(scriptId, SSigmaProp)
    val prop = SigmaAnd(hashEquals.toSigmaProp, scriptIsCorrect)

    val recipientProposition = new ContextEnrichingTestProvingInterpreter().dlogSecrets.head.publicImage
    val selfBox = ErgoBox(20, TrueProp, 0, Seq(), Map())
    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(selfBox),
      createTransaction(ErgoBox(1, recipientProposition, 0)),
      self = selfBox)

    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true
  }


  /**
    * An example script where a box could be spent only along with a box with given id
    * (and no more boxes could be provided as an input of a spending transaction).
    */
  property("Along with a brother (using Box constant in environment)") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage

    val brother = ErgoBox(10, pubkey1, 0)
    val brotherWithWrongId = ErgoBox(10, pubkey1, 0, boxIndex = 120: Short)

    val newBox = ErgoBox(20, pubkey2, 0)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = createTransaction(newBoxes)

    val env = Map("brother" -> brother)
    val prop = compile(env,
      """{
        |  val okInputs = INPUTS.size == 2
        |  val okIds = INPUTS(0).id == brother.id
        |  okInputs && okIds
         }""".stripMargin).asBoolValue.toSigmaProp

    val propExpected = BinAnd(
      EQ(SizeOf(Inputs), IntConstant(2)),
      EQ(ExtractId(ByIndex(Inputs, 0)), ExtractId(BoxConstant(brother)))).toSigmaProp
    prop shouldBe propExpected

    // try a version of the script that matches the white paper
    val altEnv = Map("friend" -> brother)
    val altProp = compile(altEnv, """INPUTS.size == 2 && INPUTS(0).id == friend.id""").asBoolValue.toSigmaProp
    altProp shouldBe prop

    val s = ErgoBox(10, prop, 0, Seq(), Map())

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(brother, s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove_prop"), prop, ctx, fakeMessage).fold(t => throw t, x => x)
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify_prop"), prop, ctx, pr, fakeMessage).fold(t => throw t, x => x)._1 shouldBe true

    val wrongCtx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(brotherWithWrongId, s),
      spendingTransaction,
      self = s)

    prover.prove(prop, wrongCtx, fakeMessage).isFailure shouldBe true
    verifier.verify(prop, wrongCtx, pr, fakeMessage).fold(t => throw t, x => x)._1 shouldBe false

    val prop2 = compile(env,
      """{
        |  val okInputs = INPUTS.size == 3
        |  val okIds = INPUTS(0).id == brother.id
        |  okInputs && okIds
         }""".stripMargin).asBoolValue.toSigmaProp

    prover.prove(emptyEnv + (ScriptNameProp -> "prove_prop2"), prop2, ctx, fakeMessage).isFailure shouldBe true
    verifier
      .verify(emptyEnv + (ScriptNameProp -> "verify_prop2"), prop2, ctx, pr, fakeMessage)
      .fold(t => throw t, x => x)._1 shouldBe false
  }

  /**
    * An example script where an output could be spent only along with an output with given id
    * (and possibly others, too).
    */
  property("Along with a friend and maybe others (!!! Box constant in environment)") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage

    val friend = ErgoBox(10, pubkey1, 0)
    val friendWithWrongId = ErgoBox(10, pubkey1, 0, boxIndex = 120: Short)

    val newBox = ErgoBox(20, pubkey2, 0)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = createTransaction(newBoxes)

    val env = Map("friend" -> friend)
    val prop = compile(env,
      """{
        |
        | def isFriend(inputBox: Box) = inputBox.id == friend.id
        | INPUTS.exists (isFriend)
         }""".stripMargin).asBoolValue.toSigmaProp

    val propExpected = Exists(Inputs,
      FuncValue(
        Vector((1, SBox)),
        EQ(
          ExtractId(ValUse(1, SBox)),
          ExtractId(BoxConstant(friend))
        )
      )
    ).toSigmaProp

    prop shouldBe propExpected

    // same script written differently
    val altProp = compile(env, "INPUTS.exists ({ (inputBox: Box) => inputBox.id == friend.id })").asBoolValue.toSigmaProp
    altProp shouldBe prop

    val s = ErgoBox(10, prop, 0, Seq(), Map())

    val ctx1 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(friend, s),
      spendingTransaction,
      self = s)

    val pr1 = prover.prove(prop, ctx1, fakeMessage).success.value
    verifier.verify(prop, ctx1, pr1, fakeMessage).success.value._1 shouldBe true

    val ctx2 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s, friend),
      spendingTransaction,
      self = s)

    val pr2 = prover.prove(prop, ctx2, fakeMessage).success.value
    verifier.verify(prop, ctx2, pr2, fakeMessage).success.value._1 shouldBe true

    val pr3 = prover.prove(prop, ctx2, fakeMessage).success.value
    verifier.verify(prop, ctx2, pr3, fakeMessage).success.value._1 shouldBe true

    val wrongCtx1 = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(friendWithWrongId, s),
      spendingTransaction,
      self = s)

    prover.prove(prop, wrongCtx1, fakeMessage).isFailure shouldBe true
    verifier.verify(prop, wrongCtx1, pr1, fakeMessage).success.value._1 shouldBe false
  }

  property("If") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val preimageHello = "hello world".getBytes("UTF-8")
    val preimageWrong = "wrong".getBytes("UTF-8")

    val helloHash = Blake2b256.hash(preimageHello)

    val env = Map("helloHash" -> helloHash)
    val prop = compile(env,
      """{
        |  val cond = INPUTS(0).value > 10
        |  val preimage = if (cond)
        |    INPUTS(2).R4[Coll[Byte]].get
        |  else
        |    INPUTS(1).R4[Coll[Byte]].get
        |  helloHash == blake2b256(preimage)
         }""".stripMargin).asBoolValue.toSigmaProp

    val propExpected = EQ(ByteArrayConstant(helloHash),
      CalcBlake2b256(
        If(GT(ExtractAmount(ByIndex(Inputs, 0)), LongConstant(10)),
          ExtractRegisterAs[SByteArray](ByIndex(Inputs, 2), reg1).get,
          ExtractRegisterAs[SByteArray](ByIndex(Inputs, 1), reg1).get))).toSigmaProp
    prop shouldBe propExpected

    val input0 = ErgoBox(10, pubkey, 0, Seq(), Map())
    val input1 = ErgoBox(1, pubkey, 0, Seq(), Map(reg1 -> ByteArrayConstant(preimageHello)))
    val input2 = ErgoBox(1, pubkey, 0, Seq(), Map(reg1 -> ByteArrayConstant(preimageWrong)))
    val input3 = ErgoBox(10, prop, 0, Seq(), Map())

    val output = ErgoBox(22, pubkey, 0, Seq(), Map())

    val spendingTransaction = createTransaction(output)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(input0, input1, input2, input3),
      spendingTransaction,
      self = input3)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true

    //TODO coverage: check failing branches
  }

  property("DeserializeRegister value type mismatch") {
    val prover = new ContextEnrichingTestProvingInterpreter

    val sigmaProp = SigmaPropConstant(prover.dlogSecrets.head.publicImage)
    // put SigmaProp into the register
    val regValue = ByteArrayConstant(ValueSerializer.serialize(sigmaProp))
    val box = ErgoBox(20, TrueProp, 0, Seq(), Map(R4 -> regValue))

    // expect SBoolean in the register
    val prop = DeserializeRegister(R4, SBoolean).toSigmaProp

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(box),
      createTransaction(IndexedSeq(ErgoBox(10, TrueProp, 0))),
      self = box)

    an[RuntimeException] should be thrownBy
      prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).fold(t => throw t, x => x)
  }

  property("DeserializeContext value(script) type mismatch") {
    forAll(logicalExprTreeNodeGen(Seq(AND.apply))) { scriptProp =>
      val verifier = new ErgoLikeTestInterpreter
      val scriptId = 21.toByte
      val prover0 = new ContextEnrichingTestProvingInterpreter()
      // serialize boolean expression
      val prover = prover0.withContextExtender(scriptId, ByteArrayConstant(ValueSerializer.serialize(scriptProp)))
      val prop = OR(
        LogicalNot(DeserializeContext(scriptId, scriptProp.tpe)),
        DeserializeContext(scriptId, scriptProp.tpe)
      ).toSigmaProp

      val box = ErgoBox(20, ErgoScriptPredef.TrueProp, 0, Seq(), Map())
      val ctx = ErgoLikeContextTesting(
        currentHeight = 50,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = IndexedSeq(box),
        createTransaction(IndexedSeq(ErgoBox(10, TrueProp, 0))),
        self = box)

      val pr = prover.prove(prop, ctx, fakeMessage).get
      // make sure verifier will fail on deserializing context with mismatched type
      // try to deserialize it as an expression with integer type
      val prop1 = EQ(DeserializeContext(scriptId, SInt), IntConstant(1)).toSigmaProp
      an[ValidationException] should be thrownBy
        verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop1, ctx, pr, fakeMessage).get
      // make sure prover fails as well on deserializing context with mismatched type
      an[ValidationException] should be thrownBy prover.prove(prop1, ctx, fakeMessage).get
    }
 }

  property("non-const ProveDHT") {
    import sigmastate.interpreter.CryptoConstants.dlogGroup
    compile(Map("gA" -> dlogGroup.generator),
      "proveDHTuple(gA, OUTPUTS(0).R4[GroupElement].get, gA, gA)"
    ).asInstanceOf[BlockValue].result shouldBe a [CreateProveDHTuple]
  }

  property("non-const ProveDlog") {
    compile(Map(), "proveDlog(OUTPUTS(0).R4[GroupElement].get)" ) shouldBe a [CreateProveDlog]
  }
}
