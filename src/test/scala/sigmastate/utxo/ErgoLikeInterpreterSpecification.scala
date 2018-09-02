package sigmastate.utxo

import com.google.common.primitives.Bytes
import org.scalatest.TryValues._
import scapi.sigma.DLogProtocol.ProveDlog
import scapi.sigma.ProveDiffieHellmanTuple
import scorex.crypto.hash.Blake2b256
import sigmastate.SCollection.SByteArray
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import sigmastate.serialization.ValueSerializer
import org.ergoplatform._

class ErgoLikeInterpreterSpecification extends SigmaTestingCommons {

  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  property("scripts EQ/NEQ") {
    val prover1 = new ErgoLikeProvingInterpreter
    val prover2 = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

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
    val prover = new ErgoLikeProvingInterpreter
    val fakeProver = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val secret = prover.dhSecrets.head

    val ci = secret.commonInput

    val prop = SigmaPropConstant(ProveDiffieHellmanTuple(ci.g, ci.h, ci.u, ci.v)).isValid
    val wrongProp = SigmaPropConstant(ProveDiffieHellmanTuple(ci.g, ci.h, ci.u, ci.u)).isValid

    val env = Map("g"->ci.g, "h"->ci.h, "u"->ci.u, "v"->ci.v, "s"->secret.publicImage)
    val compiledProp1 = compile(env, "s.isValid").asBoolValue
    val compiledProp2 = compile(env, "proveDHTuple(g, h, u, v).isValid").asBoolValue
    compiledProp1 shouldBe prop
    compiledProp2 shouldBe prop


    val ctx = ErgoLikeContext(
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
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhB = proverB.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhB" -> pubdhB)
    val compiledProp = compile(env, """pubkeyA || pubdhB""")

    val prop = OR(pubkeyA, pubdhB)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction = null,
      self = fakeSelf)

    val prA = proverA.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, prA, fakeMessage).get._1 shouldBe true
  }

  property("DH tuple and DLOG") {
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubdhA = proverA.dhSecrets.head.publicImage

    val env = Map("pubkeyA" -> pubkeyA, "pubdhA" -> pubdhA)
    val compiledProp = compile(env, """pubkeyA && pubdhA""")

    val prop = AND(pubkeyA, pubdhA)
    compiledProp shouldBe prop

    val ctx = ErgoLikeContext(
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
    val proverA = new ErgoLikeProvingInterpreter
    val proverB = new ErgoLikeProvingInterpreter

    val verifier = new ErgoLikeInterpreter

    val pubkeyA = proverA.dlogSecrets.head.publicImage
    val pubkeyA2 = proverA.dlogSecrets.head.publicImage

    val pubkeyB = proverB.dlogSecrets.head.publicImage
    val pubkeyB2 = proverB.dlogSecrets.head.publicImage

    val newBox1 = new ErgoBoxCandidate(10, pubkeyB2)
    val newBox2 = new ErgoBoxCandidate(10, pubkeyA2)

    val newBoxes = IndexedSeq(newBox1, newBox2)

    val properBytes = Bytes.concat(newBoxes.map(_.bytesWithNoRef): _*)

    val properHash = Blake2b256(properBytes)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    def mixingRequestProp(sender: ProveDlog, timeout: Int) = {
      val env = Map("sender" -> sender, "timeout" -> timeout, "properHash" -> properHash)
      val compiledProp = compile(env,
        """{
          |  let notTimePassed = HEIGHT <= timeout
          |  let outBytes = OUTPUTS.map(fun (box: Box) = box.bytesWithNoRef)
          |  let outSumBytes = outBytes.fold(Array[Byte](), fun (arr1: Array[Byte], arr2: Array[Byte]) = arr1 ++ arr2)
          |  let timePassed = HEIGHT > timeout
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

    verifier.verify(mixingRequestProp(pubkeyB, 40), ctx, prA2, fakeMessage).map(_._1).getOrElse(false) shouldBe false
  }

  property("map + sum") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env,
      """{
        |  let outValues = OUTPUTS.map(fun (box: Box) = box.value)
        |  pubkey && outValues.fold(0L, fun (x: Long, y: Long) = x + y) > 20
         }""".stripMargin).asBoolValue

    val propExp = AND(
      pubkey,
      GT(
        Fold.sum[SLong.type](MapCollection(Outputs, 21, ExtractAmount(TaggedBox(21)))),
        LongConstant(20))
    )
    prop shouldBe propExp

    val newBox1 = ErgoBox(11, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)
  }

  property("byindex") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val compiledProp = compile(env, """pubkey && OUTPUTS(0).value > 10""")

    val prop = AND(pubkey, GT(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(10)))
    compiledProp shouldBe prop

    val newBox1 = ErgoBox(11, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
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

    val prover0 = new ErgoLikeProvingInterpreter()

    val customScript = prover0.dlogSecrets.head.publicImage
    val scriptBytes = ValueSerializer.serialize(customScript)
    val scriptHash = Blake2b256(scriptBytes)

    val prover = prover0.withContextExtender(scriptId, ByteArrayConstant(scriptBytes))

    val hashEquals = EQ(CalcBlake2b256(TaggedByteArray(scriptId)), scriptHash)
    val scriptIsCorrect = DeserializeContext(scriptId, SBoolean)
    val prop = AND(hashEquals, scriptIsCorrect)

    val recipientProposition = new ErgoLikeProvingInterpreter().dlogSecrets.head.publicImage
    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      ErgoLikeTransaction(IndexedSeq(), IndexedSeq(ErgoBox(1, recipientProposition))),
      self = ErgoBox(20, TrueLeaf, Seq(), Map()))

    val proof = prover.prove(prop, ctx, fakeMessage).get

    (new ErgoLikeInterpreter).verify(prop, ctx, proof, fakeMessage).get._1 shouldBe true
  }

  property("Prove keys from registers") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage
    val pubkey3 = prover.dlogSecrets(2).publicImage

    val reg1 = ErgoBox.nonMandatoryRegisters.head
    val reg2 = ErgoBox.nonMandatoryRegisters.tail.head

    val prop = compile(Map(),
      """{
        |  let pubkey1 = SELF.R4[GroupElement].value
        |  let pubkey2 = SELF.R5[GroupElement].value
        |  proveDlog(pubkey1) && proveDlog(pubkey2)
        |}""".stripMargin).asBoolValue

    val propTree = AND(new ProveDlog(ExtractRegisterAs(Self, reg1)), new ProveDlog(ExtractRegisterAs(Self, reg2)))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey3)
    val newBoxes = IndexedSeq(newBox1)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s1 = ErgoBox(20, TrueLeaf, Seq(),
      Map(reg1 -> pubkey1.value.asInstanceOf[GroupElementConstant],
          reg2 -> pubkey2.value.asInstanceOf[GroupElementConstant]))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s1)

    val pr = prover.prove(prop, ctx, fakeMessage).success.value
    verifier.verify(prop, ctx, pr, fakeMessage).success.value._1 shouldBe true


    //make sure that wrong case couldn't be proved
    val reg3 = ErgoBox.nonMandatoryRegisters(2)
    val reg4 = ErgoBox.nonMandatoryRegisters(3)

    val s2 = ErgoBox(20, TrueLeaf, Seq(), Map(reg3 -> pubkey2.value.asInstanceOf[GroupElementConstant],
                                              reg4 -> pubkey1.value.asInstanceOf[GroupElementConstant]))
    val wrongCtx = ErgoLikeContext(
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
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage

    val brother = ErgoBox(10, pubkey1)
    val brotherWithWrongId = ErgoBox(10, pubkey1, boxId = 120: Short)

    val newBox = ErgoBox(20, pubkey2)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

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

    // try a version of the script that matches the white paper
    val altEnv = Map("friend" -> brother)
    val altProp = compile(altEnv, """INPUTS.size == 2 && INPUTS(0).id == friend.id""")
    altProp shouldBe prop



    val s = ErgoBox(10, prop, Seq(), Map())

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(brother, s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).success.value
    verifier.verify(prop, ctx, pr, fakeMessage).success.value._1 shouldBe true

    val wrongCtx = ErgoLikeContext(
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

  /**
    * An example script where an output could be spent only along with an output with given id
    * (and possibly others, too).
    */
  property("Along with a friend and maybe others") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey1 = prover.dlogSecrets.head.publicImage
    val pubkey2 = prover.dlogSecrets(1).publicImage

    val friend = ErgoBox(10, pubkey1)
    val friendWithWrongId = ErgoBox(10, pubkey1, boxId = 120: Short)

    val newBox = ErgoBox(20, pubkey2)

    val newBoxes = IndexedSeq(newBox)
    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val env = Map("friend" -> friend)
    val prop = compile(env,
      """{
        |
        | let isFriend = fun (inputBox: Box) = {inputBox.id == friend.id}
        | INPUTS.exists (isFriend)
         }""".stripMargin).asBoolValue

    val propExpected = Exists(Inputs, 21, EQ(ExtractId(TaggedBox(21)), ExtractId(BoxConstant(friend))))
    prop shouldBe propExpected

    // same script written differently
    val altProp = compile(env, "INPUTS.exists (fun (inputBox: Box) = {inputBox.id == friend.id})")
    altProp shouldBe prop

    val s = ErgoBox(10, prop, Seq(), Map())

    val ctx1 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(friend, s),
      spendingTransaction,
      self = s)

    val pr1 = prover.prove(prop, ctx1, fakeMessage).success.value
    verifier.verify(prop, ctx1, pr1, fakeMessage).success.value._1 shouldBe true

    val ctx2 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(s, friend),
      spendingTransaction,
      self = s)

    val pr2 = prover.prove(prop, ctx2, fakeMessage).success.value
    verifier.verify(prop, ctx2, pr2, fakeMessage).success.value._1 shouldBe true

    val ctx3 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(friend, s, friendWithWrongId),
      spendingTransaction,
      self = s)

    val pr3 = prover.prove(prop, ctx2, fakeMessage).success.value
    verifier.verify(prop, ctx2, pr3, fakeMessage).success.value._1 shouldBe true

    val wrongCtx1 = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(friendWithWrongId, s),
      spendingTransaction,
      self = s)

    prover.prove(prop, wrongCtx1, fakeMessage).isFailure shouldBe true
    verifier.verify(prop, wrongCtx1, pr1, fakeMessage).success.value._1 shouldBe false
  }


  property("If") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val preimageHello = "hello world".getBytes("UTF-8")
    val preimageWrong = "wrong".getBytes("UTF-8")

    val helloHash = Blake2b256.hash(preimageHello)

    val env = Map("helloHash" -> helloHash)
    val prop = compile(env,
      """{
        |  let cond = INPUTS(0).value > 10
        |  let preimage = if (cond)
        |    INPUTS(2).R4[Array[Byte]].value
        |  else
        |    INPUTS(1).R4[Array[Byte]].value
        |  helloHash == blake2b256(preimage)
         }""".stripMargin).asBoolValue

    val propExpected = EQ(ByteArrayConstant(helloHash),
      CalcBlake2b256(
        If(GT(ExtractAmount(ByIndex(Inputs, 0)), LongConstant(10)),
          ExtractRegisterAs[SByteArray](ByIndex(Inputs, 2), reg1),
          ExtractRegisterAs[SByteArray](ByIndex(Inputs, 1), reg1))))
    prop shouldBe propExpected

    val input0 = ErgoBox(10, pubkey, Seq(), Map())
    val input1 = ErgoBox(1, pubkey, Seq(), Map(reg1 -> ByteArrayConstant(preimageHello)))
    val input2 = ErgoBox(1, pubkey, Seq(), Map(reg1 -> ByteArrayConstant(preimageWrong)))
    val input3 = ErgoBox(10, prop, Seq(), Map())

    val output = ErgoBox(22, pubkey, Seq(), Map())

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), IndexedSeq(output))

    val ctx = ErgoLikeContext(
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
