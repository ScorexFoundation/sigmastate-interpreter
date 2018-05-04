package sigmastate.utxo

import com.google.common.primitives.Bytes
import org.scalatest.TryValues._
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import scorex.crypto.hash.Blake2b256
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.ErgoBox._

class ErgoInterpreterSpecification extends SigmaTestingCommons {

  property("scripts EQ/NEQ") {
    val prover1 = new ErgoProvingInterpreter
    val prover2 = new ErgoProvingInterpreter

    val verifier = new ErgoInterpreter

    val h1 = prover1.dlogSecrets.head.publicImage
    val h2 = prover2.dlogSecrets.head.publicImage

    val ctx = ErgoContext.dummy(fakeSelf)

    val e = compile(Map("h1" -> h1.bytes, "h2" -> h2.bytes), "h1 == h1")
    val exp = EQ(ByteArrayConstant(h1.bytes), ByteArrayConstant(h1.bytes))
    e shouldBe exp

    verifier.reduceToCrypto(ctx, exp)
      .get._1.isInstanceOf[TrueLeaf.type] shouldBe true

    verifier.reduceToCrypto(ctx, EQ(ByteArrayConstant(h1.bytes), ByteArrayConstant(h2.bytes)))
      .get._1.isInstanceOf[FalseLeaf.type] shouldBe true
  }

  property("DH tuple") {
    val prover = new ErgoProvingInterpreter
    val fakeProver = new ErgoProvingInterpreter

    val verifier = new ErgoInterpreter

    val secret = prover.dhSecrets.head

    val ci = secret.commonInput

    val prop = ProveDiffieHellmanTuple(ci.g, ci.h, ci.u, ci.v)
    val wrongProp = ProveDiffieHellmanTuple(ci.g, ci.h, ci.u, ci.u)

    val ctx = ErgoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true

    fakeProver.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
    prover.prove(wrongProp, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("DH tuple - simulation") {
    val proverA = new ErgoProvingInterpreter
    val proverB = new ErgoProvingInterpreter

    val verifier = new ErgoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhB = proverB.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhB" -> pubdhB)
    val compiledProp = compile(env, """pubkeyA || pubdhB""")

    val prop = OR(pubkeyA, pubdhB)
    compiledProp shouldBe prop

    val ctx = ErgoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true
  }

  property("DH tuple and DLOG") {
    val proverA = new ErgoProvingInterpreter
    val proverB = new ErgoProvingInterpreter

    val verifier = new ErgoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhA = proverA.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhA" -> pubdhA)
    val compiledProp = compile(env, """pubkeyA && pubdhA""")

    val prop = AND(pubkeyA, pubdhA)
    compiledProp shouldBe prop

    val ctx = ErgoContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true

    proverB.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("mixing scenario w. timeout") {
    val proverA = new ErgoProvingInterpreter
    val proverB = new ErgoProvingInterpreter

    val verifier = new ErgoInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyA2 = proverA.dlogSecrets.head.publicImage

    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyB2 = proverB.dlogSecrets.head.publicImage

    val newBox1 = new ErgoBoxCandidate(10, pubkeyB2)
    val newBox2 = new ErgoBoxCandidate(10, pubkeyA2)

    val newBoxes = IndexedSeq(newBox1, newBox2)

    val properBytes = Bytes.concat(newBoxes.map(_.bytesWithNoRef): _*)

    val properHash = Blake2b256(properBytes)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    def mixingRequestProp(sender: ProveDlog, timeout: Int) = {
      val env = Map("sender" -> sender, "timeout" -> timeout, "properHash" -> properHash)
      val compiledProp = compile(env,
        """{
          |  let notTimePassed = HEIGHT <= timeout
          |  let outBytes = OUTPUTS.map(fun (box: Box) = box.bytesWithNoRef)
          |  let outSumBytes = outBytes.fold(EmptyByteArray, fun (arr1: ByteArray, arr2: ByteArray) = arr2 ++ arr1)
          |  let timePassed = HEIGHT > timeout
          |  notTimePassed && blake2b256(outSumBytes) == properHash || timePassed && sender
           }""".stripMargin).asBoolValue

      val prop = OR(
        AND(LE(Height, IntConstant(timeout)),
          EQ(CalcBlake2b256(Fold.sumBytes(MapCollection(Outputs, 21, ExtractBytesWithNoRef(TaggedBox(21))))),
            ByteArrayConstant(properHash))),
        AND(GT(Height, IntConstant(timeout)), sender)
      )
      compiledProp shouldBe prop
      compiledProp
    }

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    //before timeout
    val prA = proverA.prove(mixingRequestProp(pubkeyA, 100), ctx, fakeMessage).get
    verifier.verify(mixingRequestProp(pubkeyA, 100), ctx, prA, fakeMessage).get._1 shouldBe true
    verifier.verify(mixingRequestProp(pubkeyB, 100), ctx, prA, fakeMessage).get._1 shouldBe true

    //after timeout
    val prA2 = proverA.prove(mixingRequestProp(pubkeyA, 40), ctx, fakeMessage).get
    verifier.verify(mixingRequestProp(pubkeyA, 40), ctx, prA2, fakeMessage).get._1 shouldBe true
    verifier.verify(mixingRequestProp(pubkeyB, 40), ctx, prA2, fakeMessage).isSuccess shouldBe false
  }

  property("map + sum") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env,
      """{
        |  let outValues = OUTPUTS.map(fun (box: Box) = box.value)
        |  pubkey && outValues.fold(0, fun (x: Int, y: Int) = y + x) > 20
         }""".stripMargin).asBoolValue

    val propExp = AND(pubkey, GT(Fold.sum(MapCollection(Outputs, 21, ExtractAmount(TaggedBox(21)))), IntConstant(20)))
    prop shouldBe propExp

    val newBox1 = ErgoBox(11, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)
  }

  property("byindex") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val compiledProp = compile(env, """pubkey && OUTPUTS(0).value > 10""")

    val prop = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), IntConstant(10)))
    compiledProp shouldBe prop

    val newBox1 = ErgoBox(11, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)


    val fProp1 = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), IntConstant(11)))
    prover.prove(fProp1, ctx, fakeMessage).isSuccess shouldBe false

    val fProp2 = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 1)), IntConstant(11)))
    prover.prove(fProp2, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("P2SH") {
    val scriptId = 21.toByte
    val secretId = 22.toByte

    val customScript = EQ(TaggedInt(secretId), IntConstant(12))
    val scriptBytes = ValueSerializer.serialize(customScript)
    val prover = new ErgoProvingInterpreter()
      .withContextExtender(secretId, IntConstant(12))
      .withContextExtender(scriptId, ByteArrayConstant(scriptBytes))
    val scriptHash = Blake2b256(scriptBytes)

    val hashEquals = EQ(CalcBlake2b256(TaggedByteArray(scriptId)), scriptHash)
    val scriptIsCorrect = Deserialize[SBoolean.type](TaggedByteArray(scriptId))
    val prop = AND(hashEquals, scriptIsCorrect)

    val recipientProposition = new ErgoProvingInterpreter().dlogSecrets.head.publicImage
    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      ErgoTransaction(IndexedSeq(), IndexedSeq(ErgoBox(1, recipientProposition))),
      self = ErgoBox(20, TrueLeaf, Map()))

    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true
  }

  property("Prove keys from registers") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage
    val pubkey3 = prover.dlogSecrets(2).publicImage

    val prop = compile(Map(),
      """{
        |  let pubkey1 = SELF.R3[GroupElement].value
        |  let pubkey2 = SELF.R4[GroupElement].value
        |  proveDlog(pubkey1) && proveDlog(pubkey2)
        |}""".stripMargin).asBoolValue

    val propTree = AND(new ProveDlog(ExtractRegisterAs(Self, R3)), new ProveDlog(ExtractRegisterAs(Self, R4)))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey3)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s1 = ErgoBox(20, TrueLeaf, Map(R3 -> pubkey1.value, R4 -> pubkey2.value))

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s1)

    val pr = prover.prove(prop, ctx, fakeMessage).success.value
    verifier.verify(prop, ctx, pr, fakeMessage).success.value._1 shouldBe true


    //make sure that wrong case couldn't be proved
    val s2 = ErgoBox(20, TrueLeaf, Map(R4 -> pubkey2.value, R5 -> pubkey1.value))
    val wrongCtx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s2)

    prover.prove(prop, wrongCtx, fakeMessage).isFailure shouldBe true
    verifier.verify(prop, wrongCtx, pr, fakeMessage).isFailure shouldBe true
  }

  /**
    * An example script where an output could be spent only along with an output with given id
    * (and no more outputs could be provided as an input of a spending transaction).
    */
  property("Along with a brother") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage

    val brother = ErgoBox(10, pubkey1)
    val brotherWithWrongId = ErgoBox(10, pubkey1, boxId = 120: Short)

    val newBox = ErgoBox(20, pubkey2)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val env = Map("brother" -> brother)
    val prop = compile(env,
      """{
        |  let okInputs = INPUTS.size == 2
        |  let okIds = INPUTS(0).id == brother.id
        |  okInputs && okIds
         }""".stripMargin).asBoolValue

    val propExpected = AND(
      EQ(SizeOf(Inputs), IntConstant(2)),
      EQ(ExtractId(ByIndex(Inputs, 0)), ExtractId(BoxConstant(brother))))
    prop shouldBe propExpected

    val s = ErgoBox(10, prop, Map())

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(brother, s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).success.value
    verifier.verify(prop, ctx, pr, fakeMessage).success.value._1 shouldBe true

    val wrongCtx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(brotherWithWrongId, s),
      spendingTransaction,
      self = s)

    prover.prove(prop, wrongCtx, fakeMessage).isFailure shouldBe true
    verifier.verify(prop, wrongCtx, pr, fakeMessage).success.value._1 shouldBe false

    val prop2 = compile(env,
      """{
        |  let okInputs = INPUTS.size == 3
        |  let okIds = INPUTS(0).id == brother.id
        |  okInputs && okIds
         }""".stripMargin).asBoolValue

    prover.prove(prop2, ctx, fakeMessage).isFailure shouldBe true
    verifier.verify(prop2, ctx, pr, fakeMessage).success.value._1 shouldBe false
  }

  property("If") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val preimageHello = "hello world".getBytes("UTF-8")
    val preimageWrong = "wrong".getBytes("UTF-8")

    val helloHash = Blake2b256.hash(preimageHello)

    val env = Map("helloHash" -> helloHash)
    val prop = compile(env,
      """{
        |  let cond = INPUTS(0).value > 10
        |  let preimage = if (cond)
        |    INPUTS(2).R3[ByteArray].value
        |  else
        |    INPUTS(1).R3[ByteArray].value
        |  helloHash == blake2b256(preimage)
         }""".stripMargin).asBoolValue

    val propExpected = EQ(ByteArrayConstant(helloHash),
      CalcBlake2b256(
        If(GT(ExtractAmount(ByIndex(Inputs, 0)), IntConstant(10)),
          ExtractRegisterAs[SByteArray.type](ByIndex(Inputs, 2), R3),
          ExtractRegisterAs[SByteArray.type](ByIndex(Inputs, 1), R3))))
    prop shouldBe propExpected

    val input0 = ErgoBox(10, pubkey, Map())
    val input1 = ErgoBox(1, pubkey, Map(R3 -> ByteArrayConstant(preimageHello)))
    val input2 = ErgoBox(1, pubkey, Map(R3 -> ByteArrayConstant(preimageWrong)))
    val input3 = ErgoBox(10, prop, Map())

    val output = ErgoBox(22, pubkey, Map())

    val spendingTransaction = ErgoTransaction(IndexedSeq(), IndexedSeq(output))

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(input0, input1, input2, input3),
      spendingTransaction,
      self = input3)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true

    //todo: check failing branches
  }
}