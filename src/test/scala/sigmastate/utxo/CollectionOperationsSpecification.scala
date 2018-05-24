package sigmastate.utxo

import org.ergoplatform
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import org.ergoplatform.ErgoBox.R3
import org.ergoplatform._

class CollectionOperationsSpecification extends SigmaTestingCommons {

  private def context(boxesToSpend: IndexedSeq[ErgoBox] = IndexedSeq(),
                      outputs: IndexedSeq[ErgoBox]): ErgoContext =
    ergoplatform.ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = boxesToSpend,
      spendingTransaction = ErgoTransaction(IndexedSeq(), outputs),
      self = fakeSelf)

  private def assertProof(code: String,
                          expectedComp: Value[SType],
                          outputBoxValues: IndexedSeq[Long],
                          boxesToSpendValues: IndexedSeq[Long] = IndexedSeq()) = {
    val (prover, verifier, prop, ctx) = buildEnv(code, expectedComp, outputBoxValues,
      boxesToSpendValues)
    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  private def assertProverFail(code: String,
                               expectedComp: Value[SType],
                               outputBoxValues: IndexedSeq[Long],
                               boxesToSpendValues: IndexedSeq[Long] = IndexedSeq()) = {
    val (prover, _, prop, ctx) = buildEnv(code, expectedComp, outputBoxValues, boxesToSpendValues)
    prover.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  private def buildEnv(code: String, expectedComp: Value[SType], outputBoxValues: IndexedSeq[Long], boxesToSpendValues: IndexedSeq[Long]) = {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage
    val prop = compile(Map(), code).asBoolValue
    prop shouldBe expectedComp
    val ctx = context(boxesToSpendValues.map(ErgoBox(_, pubkey)),
      outputBoxValues.map(ErgoBox(_, pubkey)))
    (prover, verifier, prop, ctx)
  }

  property("exists") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.exists(fun (box: Box) = box.value + 5 > 10)").asBoolValue

    val expProp = Exists(Outputs, 21, GT(Plus(ExtractAmount(TaggedBox(21)), IntConstant(5)), IntConstant(10)))
    prop shouldBe expProp

    val newBox1 = ErgoBox(16, pubkey)
    val newBox2 = ErgoBox(15, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
    //todo: finish
  }

  property("forall") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.forall(fun (box: Box) = box.value == 10)").asBoolValue

    val propTree = ForAll(Outputs, 21, EQ(ExtractAmount(TaggedBox(21)), IntConstant(10)))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
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
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
    //todo: finish
  }


  property("forall - fail") {
    val prover = new ErgoProvingInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.forall(fun (box: Box) = box.value == 10)").asBoolValue
    val propTree = ForAll(Outputs, 21, EQ(ExtractAmount(TaggedBox(21)), IntConstant(10)))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
    val newBox2 = ErgoBox(11, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    prover.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("counter") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(),
      """OUTPUTS.exists(fun (box: Box) = {
        |  box.R3[Int].value == SELF.R3[Int].value + 1
         })""".stripMargin).asBoolValue

    val propTree = Exists(Outputs, 21, EQ(ExtractRegisterAs(TaggedBox(21), R3)(SInt),
      Plus(ExtractRegisterAs(Self, R3)(SInt), IntConstant(1))))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, Map(R3 -> IntConstant(3)))
    val newBox2 = ErgoBox(10, pubkey, Map(R3 -> IntConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Map(R3 -> IntConstant(5)))

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("counter - no register in outputs") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(),
      """OUTPUTS.exists(fun (box: Box) = {
        |  box.R3[Int].valueOrElse(0) == SELF.R3[Int].value + 1
         })""".stripMargin).asBoolValue

    val propTree = Exists(Outputs, 21,
      EQ(ExtractRegisterAs(TaggedBox(21), R3, default = Some(IntConstant(0L))),
        Plus(ExtractRegisterAs(Self, R3), IntConstant(1))))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
    val newBox2 = ErgoBox(10, pubkey, Map(R3 -> IntConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Map(R3 -> IntConstant(5)))

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("sizeof - num of outputs = num of inputs + 1") {
    val prover = new ErgoProvingInterpreter
    val verifier = new ErgoInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env, """pubkey && OUTPUTS.size == INPUTS.size + 1""").asBoolValue
    val propTree = AND(pubkey, EQ(SizeOf(Outputs), Plus(SizeOf(Inputs), IntConstant(1))))
    prop shouldBe propTree

    val newBox1 = ErgoBox(11, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(21, pubkey)

    val ctx = ErgoContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)


    val fProp = AND(pubkey, EQ(SizeOf(Outputs), SizeOf(Inputs)))
    prover.prove(fProp, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("slice") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "OUTPUTS.slice(1, OUTPUTS.size).forall(fun (box: Box) = box.value == 10)"
    val expectedPropTree = ForAll(
      Slice(Outputs, IntConstant(1), SizeOf(Outputs)),
      21,
      EQ(ExtractAmount(TaggedBox(21)), IntConstant(10)))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("slice - fail") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    // starting index out of bounds
    val code = "OUTPUTS.slice(3, OUTPUTS.size).size == 1"
    val expectedPropTree = EQ(
      SizeOf(Slice(Outputs, IntConstant(3), SizeOf(Outputs))),
      IntConstant(1))
    assertProverFail(code, expectedPropTree, outputBoxValues)
  }

  property("append") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "(OUTPUTS ++ OUTPUTS).size == 4"
    val expectedPropTree = EQ(SizeOf(Append(Outputs, Outputs)), IntConstant(4))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("by index") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "OUTPUTS(0).value == 10"
    val expectedPropTree = EQ(ExtractAmount(ByIndex(Outputs, 0)), IntConstant(10))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("by index with evaluation") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "OUTPUTS(OUTPUTS.size - 1).value == 10"
    val expectedPropTree = EQ(
      ExtractAmount(
        ByIndex(Outputs,
          ArithmeticOperations(SizeOf(Outputs), IntConstant(1), 41))),
      IntConstant(10))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("map fold") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """OUTPUTS
        |.map(fun (box: Box) = box.value)
        |.fold(0, fun (acc: Int, val: Int) = acc + val) == 20""".stripMargin
    val expectedPropTree = EQ(
      Fold(
        MapCollection(Outputs, 21, ExtractAmount(TaggedBox(21))),
        21,
        IntConstant(0),
        22,
        Plus(TaggedInt(21), TaggedInt(22))),
      IntConstant(20))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("forall for custom collection") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """{
        |  let indexCollection = Array(0, 1, 2, 3, 4, 5)
        |  fun elementRule(index: Int) = {
        |    let boundaryIndex = if (index == 0) 5 else (index - 1)
        |    boundaryIndex >= 0 && boundaryIndex <= 5
        |  }
        |  indexCollection.forall(elementRule)
         }""".stripMargin

    val indexCollection = new ConcreteCollection((0 until 6).map(i => IntConstant(i)))
    val indexId = 21.toByte
    val index = TaggedInt(indexId)
    val boundaryIndex = If(EQ(index, 0), 5, Minus(index, 1))
    val elementRule = AND(GE(boundaryIndex, 0), LE(boundaryIndex, 5))
    val expectedPropTree = ForAll(indexCollection, indexId, elementRule)
    assertProof(code, expectedPropTree, outputBoxValues)

  }

  property("ByIndex for non-evaluated index") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """{
        |  let string = Array(1, 1, 0, 0, 0, 1)
        |  let indexCollection = Array(0, 1, 2, 3, 4, 5)
        |  fun elementRule(index: Int) = {
        |    let boundedIndex = if (index <= 0) 5 else (index - 1)
        |    let element = string(boundedIndex)
        |    element == 0 || element == 1
        |  }
        |  indexCollection.forall(elementRule)
         }""".stripMargin

    val indexCollection = new ConcreteCollection((0 until 6).map(i => IntConstant(i)))
    val string = new ConcreteCollection(Array(1, 1, 0, 0, 0, 1).map(i => IntConstant(i)))
    val indexId = 21.toByte
    val index = TaggedInt(indexId)
    val element = ByIndex(string, If(LE(index, 0), 5, Minus(index, 1)))
    val elementRule = OR(EQ(element, 0), EQ(element, 1))
    val expectedPropTree = ForAll(indexCollection, indexId, elementRule)
    assertProof(code, expectedPropTree, outputBoxValues)

  }


}
