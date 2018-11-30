package sigmastate.utxo

import com.google.common.primitives.Bytes
import org.ergoplatform.ErgoLikeContext.Metadata
import org.ergoplatform.ErgoLikeContext.Metadata._
import org.ergoplatform._
import org.scalatest.TryValues._
import scorex.crypto.hash.Blake2b256
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.basics.DLogProtocol.ProveDlog
import sigmastate.basics.ProveDiffieHellmanTuple
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer

class ErgoLikeInterpreterSpecification extends SigmaTestingCommons {

  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  property("scripts EQ/NEQ") {
    val prover1 = new ErgoLikeTestProvingInterpreter
    val prover2 = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val h1 = prover1.dlogSecrets.head.publicImage
    val h2 = prover2.dlogSecrets.head.publicImage

    val ctx = ErgoLikeContext.dummy(fakeSelf)

    val e = compile(Map("h1" -> h1.bytes, "h2" -> h2.bytes), "h1 == h1")
    val exp = EQ(ByteArrayConstant(h1.bytes), ByteArrayConstant(h1.bytes))
    e shouldBe exp

    verifier.reduceToCrypto(ctx, exp)
      .get._1.isInstanceOf[TrueLeaf.type] shouldBe true

    verifier.reduceToCrypto(ctx, EQ(ByteArrayConstant(h1.bytes), ByteArrayConstant(h2.bytes)))
      .get._1.isInstanceOf[FalseLeaf.type] shouldBe true
  }

  property("DH tuple") {
    val prover = new ErgoLikeTestProvingInterpreter
    val fakeProver = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val secret = prover.dhSecrets.head

    val ci = secret.commonInput

    val prop = ProveDiffieHellmanTuple(ci.g, ci.h, ci.u, ci.v)
    val wrongProp = ProveDiffieHellmanTuple(ci.g, ci.h, ci.u, ci.u)

    val env = Map("g" -> ci.g, "h" -> ci.h, "u" -> ci.u, "v" -> ci.v, "s" -> secret.publicImage)
    val compiledProp1 = compile(env, "s").asBoolValue
    val compiledProp2 = compile(env, "proveDHTuple(g, h, u, v)").asBoolValue
    compiledProp1 shouldBe prop
    compiledProp2 shouldBe prop


    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true

    fakeProver.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
    prover.prove(wrongProp, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("DH tuple - simulation") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhB = proverB.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhB" -> pubdhB)
    val compiledProp = compile(env, """pubkeyA || pubdhB""")

    val prop = OR(pubkeyA, pubdhB)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true
  }

  property("DH tuple and DLOG") {
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter

    val verifier = new ErgoLikeTestInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhA = proverA.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhA" -> pubdhA)
    val compiledProp = compile(env, """pubkeyA && pubdhA""")

    val prop = AND(pubkeyA, pubdhA)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get

    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true

    proverB.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("mixing scenario w. timeout") {
    val height = 50
    val proverA = new ErgoLikeTestProvingInterpreter
    val proverB = new ErgoLikeTestProvingInterpreter

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

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    def mixingRequestProp(sender: ProveDlog, timeout: Int) = {
      val env = Map("sender" -> sender, "timeout" -> timeout, "properHash" -> properHash)
      val compiledProp = compile(env,
        """{
          |  val notTimePassed = HEIGHT <= timeout
          |  val outBytes = OUTPUTS.map({(box: Box) => box.bytesWithNoRef})
          |  val outSumBytes = outBytes.fold(Array[Byte](), {(arr1: Array[Byte], arr2: Array[Byte]) => arr1 ++ arr2})
          |  val timePassed = HEIGHT > timeout
          |  notTimePassed && blake2b256(outSumBytes) == properHash || timePassed && sender
           }""".stripMargin).asBoolValue

      val prop = OR(
        AND(LE(Height, LongConstant(timeout)),
          EQ(CalcBlake2b256(
            Fold.concat[SByte.type](
              MapCollection(Outputs, 21, ExtractBytesWithNoRef(TaggedBox(21)))
            ).asByteArray
          ),
            ByteArrayConstant(properHash))),
        AND(GT(Height, LongConstant(timeout)), sender)
      )
      compiledProp shouldBe prop
      compiledProp
    }

    val ctx = ErgoLikeContext(
      currentHeight = height,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
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

    verifier.verify(mixingRequestProp(pubkeyB, 40), ctx, prA2, fakeMessage).map(_._1).getOrElse(false) shouldBe false
  }

  property("map + sum") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env,
      """{
        |  val outValues = OUTPUTS.map({ (box: Box) => box.value })
        |  pubkey && outValues.fold(0L, { (x: Long, y: Long) => x + y }) > 20
         }""".stripMargin).asBoolValue

    val propExp = AND(
      pubkey,
      GT(
        Fold.sum[SLong.type](MapCollection(Outputs, 21, ExtractAmount(TaggedBox(21)))),
        LongConstant(20))
    )
    prop shouldBe propExp

    val newBox1 = ErgoBox(11, pubkey, 0)
    val newBox2 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)
  }

  property("byindex") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val compiledProp = compile(env, """pubkey && OUTPUTS(0).value > 10""")

    val prop = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(10)))
    compiledProp shouldBe prop

    val newBox1 = ErgoBox(11, pubkey, 0)
    val newBox2 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)


    val fProp1 = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(11)))
    prover.prove(fProp1, ctx, fakeMessage).isSuccess shouldBe false

    val fProp2 = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 1)), LongConstant(11)))
    prover.prove(fProp2, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("P2SH") {
    val scriptId = 21.toByte

    val prover0 = new ErgoLikeTestProvingInterpreter()

    val customScript = prover0.dlogSecrets.head.publicImage
    val scriptBytes = ValueSerializer.serialize(customScript)
    val scriptHash = Blake2b256(scriptBytes)

    val prover = prover0.withContextExtender(scriptId, ByteArrayConstant(scriptBytes))

    val hashEquals = EQ(CalcBlake2b256(TaggedByteArray(scriptId)), scriptHash)
    val scriptIsCorrect = DeserializeContext(scriptId, SBoolean)
    val prop = AND(hashEquals, scriptIsCorrect)

    val recipientProposition = new ErgoLikeTestProvingInterpreter().dlogSecrets.head.publicImage
    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(ErgoBox(1, recipientProposition, 0))),
      self = ErgoBox(20, TrueLeaf, 0, Seq(), Map()))

    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true
  }

  property("Prove keys from registers") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage
    val pubkey3 = prover.dlogSecrets(2).publicImage

    val reg1 = ErgoBox.nonMandatoryRegisters.head
    val reg2 = ErgoBox.nonMandatoryRegisters.tail.head

    val prop = compile(Map(),
      """{
        |  val pubkey1 = SELF.R4[GroupElement].get
        |  val pubkey2 = SELF.R5[GroupElement].get
        |  proveDlog(pubkey1) && proveDlog(pubkey2)
        |}""".stripMargin).asBoolValue

    val propTree = AND(
      new ProveDlog(ExtractRegisterAs[SGroupElement.type](Self, reg1).get),
      new ProveDlog(ExtractRegisterAs[SGroupElement.type](Self, reg2).get))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey3, 0)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s1 = ErgoBox(20, TrueLeaf, 0, Seq(),
      Map(reg1 -> pubkey1.value.asInstanceOf[GroupElementConstant],
        reg2 -> pubkey2.value.asInstanceOf[GroupElementConstant]))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s1)

    val pr = prover.prove(prop, ctx, fakeMessage).success.value
    verifier.verify(prop, ctx, pr, fakeMessage).success.value._1 shouldBe true


    //make sure that wrong case couldn't be proved
    val reg3 = ErgoBox.nonMandatoryRegisters(2)
    val reg4 = ErgoBox.nonMandatoryRegisters(3)

    val s2 = ErgoBox(20, TrueLeaf, 0, Seq(), Map(reg3 -> pubkey2.value.asInstanceOf[GroupElementConstant],
      reg4 -> pubkey1.value.asInstanceOf[GroupElementConstant]))
    val wrongCtx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
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

    val prover0 = new ErgoLikeTestProvingInterpreter()

    val customScript = prover0.dlogSecrets.head.publicImage
    val scriptBytes = ValueSerializer.serialize(customScript)
    val scriptHash = Blake2b256(scriptBytes).take(bytesCount)

    val prover = prover0.withContextExtender(scriptId, ByteArrayConstant(scriptBytes))

    val hashEquals = EQ(Slice(CalcBlake2b256(TaggedByteArray(scriptId)), IntConstant(0), IntConstant(bytesCount)),
      scriptHash)
    val scriptIsCorrect = DeserializeContext(scriptId, SBoolean)
    val prop = AND(hashEquals, scriptIsCorrect)

    val recipientProposition = new ErgoLikeTestProvingInterpreter().dlogSecrets.head.publicImage
    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(ErgoBox(1, recipientProposition, 0))),
      self = ErgoBox(20, TrueLeaf, 0, Seq(), Map()))

    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoLikeTestInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true
  }


  /**
    * An example script where an output could be spent only along with an output with given id
    * (and no more outputs could be provided as an input of a spending transaction).
    */
  property("Along with a brother") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage

    val brother = ErgoBox(10, pubkey1, 0)
    val brotherWithWrongId = ErgoBox(10, pubkey1, 0, boxId = 120: Short)

    val newBox = ErgoBox(20, pubkey2, 0)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val env = Map("brother" -> brother)
    val prop = compile(env,
      """{
        |  val okInputs = INPUTS.size == 2
        |  val okIds = INPUTS(0).id == brother.id
        |  okInputs && okIds
         }""".stripMargin).asBoolValue

    val propExpected = AND(
      EQ(SizeOf(Inputs), IntConstant(2)),
      EQ(ExtractId(ByIndex(Inputs, 0)), ExtractId(BoxConstant(brother))))
    prop shouldBe propExpected

    // try a version of the script that matches the white paper
    val altEnv = Map("friend" -> brother)
    val altProp = compile(altEnv, """INPUTS.size == 2 && INPUTS(0).id == friend.id""")
    altProp shouldBe prop


    val s = ErgoBox(10, prop, 0, Seq(), Map())

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(brother, s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).success.value
    verifier.verify(prop, ctx, pr, fakeMessage).success.value._1 shouldBe true

    val wrongCtx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(brotherWithWrongId, s),
      spendingTransaction,
      self = s)

    prover.prove(prop, wrongCtx, fakeMessage).isFailure shouldBe true
    verifier.verify(prop, wrongCtx, pr, fakeMessage).success.value._1 shouldBe false

    val prop2 = compile(env,
      """{
        |  val okInputs = INPUTS.size == 3
        |  val okIds = INPUTS(0).id == brother.id
        |  okInputs && okIds
         }""".stripMargin).asBoolValue

    prover.prove(prop2, ctx, fakeMessage).isFailure shouldBe true
    verifier.verify(prop2, ctx, pr, fakeMessage).success.value._1 shouldBe false
  }

  /**
    * An example script where an output could be spent only along with an output with given id
    * (and possibly others, too).
    */
  property("Along with a friend and maybe others") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage

    val friend = ErgoBox(10, pubkey1, 0)
    val friendWithWrongId = ErgoBox(10, pubkey1, 0, boxId = 120: Short)

    val newBox = ErgoBox(20, pubkey2, 0)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val env = Map("friend" -> friend)
    val prop = compile(env,
      """{
        |
        | val isFriend = { (inputBox: Box) => inputBox.id == friend.id }
        | INPUTS.exists (isFriend)
         }""".stripMargin).asBoolValue

    val propExpected = Exists(Inputs, 21, EQ(ExtractId(TaggedBox(21)), ExtractId(BoxConstant(friend))))
    prop shouldBe propExpected

    // same script written differently
    val altProp = compile(env, "INPUTS.exists ({ (inputBox: Box) => inputBox.id == friend.id })")
    altProp shouldBe prop

    val s = ErgoBox(10, prop, 0, Seq(), Map())

    val ctx1 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(friend, s),
      spendingTransaction,
      self = s)

    val pr1 = prover.prove(prop, ctx1, fakeMessage).success.value
    verifier.verify(prop, ctx1, pr1, fakeMessage).success.value._1 shouldBe true

    val ctx2 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s, friend),
      spendingTransaction,
      self = s)

    val pr2 = prover.prove(prop, ctx2, fakeMessage).success.value
    verifier.verify(prop, ctx2, pr2, fakeMessage).success.value._1 shouldBe true

    val pr3 = prover.prove(prop, ctx2, fakeMessage).success.value
    verifier.verify(prop, ctx2, pr3, fakeMessage).success.value._1 shouldBe true

    val wrongCtx1 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(friendWithWrongId, s),
      spendingTransaction,
      self = s)

    prover.prove(prop, wrongCtx1, fakeMessage).isFailure shouldBe true
    verifier.verify(prop, wrongCtx1, pr1, fakeMessage).success.value._1 shouldBe false
  }


  property("If") {
    val prover = new ErgoLikeTestProvingInterpreter
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
        |    INPUTS(2).R4[Array[Byte]].get
        |  else
        |    INPUTS(1).R4[Array[Byte]].get
        |  helloHash == blake2b256(preimage)
         }""".stripMargin).asBoolValue

    val propExpected = EQ(ByteArrayConstant(helloHash),
      CalcBlake2b256(
        If(GT(ExtractAmount(ByIndex(Inputs, 0)), LongConstant(10)),
          ExtractRegisterAs[SByteArray](ByIndex(Inputs, 2), reg1).get,
          ExtractRegisterAs[SByteArray](ByIndex(Inputs, 1), reg1).get)))
    prop shouldBe propExpected

    val input0 = ErgoBox(10, pubkey, 0, Seq(), Map())
    val input1 = ErgoBox(1, pubkey, 0, Seq(), Map(reg1 -> ByteArrayConstant(preimageHello)))
    val input2 = ErgoBox(1, pubkey, 0, Seq(), Map(reg1 -> ByteArrayConstant(preimageWrong)))
    val input3 = ErgoBox(10, prop, 0, Seq(), Map())

    val output = ErgoBox(22, pubkey, 0, Seq(), Map())

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(output))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(input0, input1, input2, input3),
      spendingTransaction,
      self = input3)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true

    //todo: check failing branches
  }

  property("PK - P2PK address type") {
    implicit val ergoAddressEncoder: ErgoAddressEncoder =
      new ErgoAddressEncoder(TestnetNetworkPrefix)

    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val dk1 = ProveDlog(prover.dlogSecrets.head.publicImage.h)
    val p2pk = P2PKAddress(dk1)
    val encodedP2PK = p2pk.toString

    val prop1 = ErgoAddressToSigmaProp(StringConstant(encodedP2PK)).isValid

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq()),
      self = ErgoBox(20, TrueLeaf, 0, Seq(), Map()))

    val proof1 = prover.prove(prop1, ctx, fakeMessage).get.proof
    verifier.verify(prop1, ctx, proof1, fakeMessage).map(_._1).getOrElse(false) shouldBe true

    val ctxMainnet = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq()),
      self = ErgoBox(20, TrueLeaf, 0, Seq(), Map()),
      metadata = Metadata(MainnetNetworkPrefix))

    verifier.verify(prop1, ctxMainnet, proof1, fakeMessage).map(_._1).getOrElse(false) shouldBe false
  }
}
