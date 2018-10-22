package sigmastate.utxo

import org.ergoplatform
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import org.ergoplatform._
import sigmastate.serialization.OpCodes._

class CollectionOperationsSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  private def context(boxesToSpend: IndexedSeq[ErgoBox] = IndexedSeq(),
                      outputs: IndexedSeq[ErgoBox]): ErgoLikeContext =
    ergoplatform.ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = boxesToSpend,
      spendingTransaction = ErgoLikeTransaction(IndexedSeq(), outputs),
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
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage
    val prop = compile(Map(), code).asBoolValue
    prop shouldBe expectedComp
    val ctx = context(boxesToSpendValues.map(ErgoBox(_, pubkey)),
      outputBoxValues.map(ErgoBox(_, pubkey)))
    (prover, verifier, prop, ctx)
  }

  property("exists") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.exists({ (box: Box) => box.value + 5 > 10 })").asBoolValue

    val expProp = Exists(Outputs, 21, GT(Plus(ExtractAmount(TaggedBox(21)), LongConstant(5)), LongConstant(10)))
    prop shouldBe expProp

    val newBox1 = ErgoBox(16, pubkey)
    val newBox2 = ErgoBox(15, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
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
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.forall({ (box: Box) => box.value == 10 })").asBoolValue

    val propTree = ForAll(Outputs, 21, EQ(ExtractAmount(TaggedBox(21)), LongConstant(10)))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
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
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
    //todo: finish
  }


  property("forall - fail") {
    val prover = new ErgoLikeProvingInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.forall({ (box: Box) => box.value == 10 })").asBoolValue
    val propTree = ForAll(Outputs, 21, EQ(ExtractAmount(TaggedBox(21)), LongConstant(10)))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
    val newBox2 = ErgoBox(11, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = fakeSelf)

    prover.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  ignore("counter") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(),
      """OUTPUTS.exists { (box: Box) =>
        |  box.R4[Long].get == SELF.R4[Long].get + 1
         }""".stripMargin).asBoolValue

    val propTree = Exists(Outputs, 21, EQ(ExtractRegisterAs[SLong.type](TaggedBox(21), reg1).get,
      Plus(ExtractRegisterAs[SLong.type](Self, reg1).get, LongConstant(1))))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, Seq(), Map(reg1 -> LongConstant(3)))
    val newBox2 = ErgoBox(10, pubkey, Seq(), Map(reg1 -> LongConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Seq(), Map(reg1 -> LongConstant(5)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  ignore("counter - no register in outputs") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(),
      """OUTPUTS.exists { (box: Box) =>
        |  box.R4[Long].getOrElse(0L) == SELF.R4[Long].get + 1
         }""".stripMargin).asBoolValue

    val propTree = Exists(Outputs, 21,
      EQ(ExtractRegisterAs[SLong.type](TaggedBox(21), reg1).getOrElse(LongConstant(0L)),
        Plus(ExtractRegisterAs[SLong.type](Self, reg1).get, LongConstant(1))))
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey)
    val newBox2 = ErgoBox(10, pubkey, Seq(), Map(reg1 -> LongConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, Seq(), Map(reg1 -> LongConstant(5)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("sizeof - num of outputs = num of inputs + 1") {
    val prover = new ErgoLikeProvingInterpreter
    val verifier = new ErgoLikeInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env, """pubkey && OUTPUTS.size == INPUTS.size + 1""").asBoolValue
    val propTree = BinAnd(pubkey.isValid, EQ(SizeOf(Outputs), Plus(SizeOf(Inputs), IntConstant(1))))
    prop shouldBe propTree

    val newBox1 = ErgoBox(11, pubkey)
    val newBox2 = ErgoBox(10, pubkey)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(21, pubkey)

    val ctx = ErgoLikeContext(
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
    val code = "OUTPUTS.slice(1, OUTPUTS.size).forall({ (box: Box) => box.value == 10 })"
    val expectedPropTree = ForAll(
      Slice(Outputs, IntConstant(1), SizeOf(Outputs)),
      21,
      EQ(ExtractAmount(TaggedBox(21)), LongConstant(10)))
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

  // TODO LHF
  ignore("append") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "(OUTPUTS ++ OUTPUTS).size == 4"
    val expectedPropTree = EQ(SizeOf(Append(Outputs, Outputs)), IntConstant(4))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("by index") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "OUTPUTS(0).value == 10"
    val expectedPropTree = EQ(ExtractAmount(ByIndex(Outputs, 0)), LongConstant(10))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("by index with evaluation") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "OUTPUTS(OUTPUTS.size - 1).value == 10"
    val expectedPropTree = EQ(
      ExtractAmount(
        ByIndex(Outputs,
          ArithOp(SizeOf(Outputs), IntConstant(1), MinusCode))),
      LongConstant(10))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  // TODO LHF
  ignore("by index with default value") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """OUTPUTS
        |.map { (box: Box) => box.value }
        |.getOrElse(3, 0L)== 0""".stripMargin
    val expectedPropTree = EQ(
      ByIndex(
        MapCollection(Outputs,21,ExtractAmount(TaggedBox(21))),
        IntConstant(3),
        Some(LongConstant(0))),
      LongConstant(0))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  // TODO LHF
  ignore("by index with evaluated default value") {
    val outputBoxValues = IndexedSeq(20L, 0L)
    val code = "OUTPUTS.getOrElse(3, OUTPUTS(0)).value == 20"
    val expectedPropTree = EQ(
      ExtractAmount(
        ByIndex(Outputs,
          IntConstant(3),
          Some(ByIndex(Outputs, IntConstant(0))))),
      LongConstant(20))
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  ignore("map fold") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """OUTPUTS
        |.map({ (box: Box) => box.value })
        |.fold(true, { (acc: Boolean, val: Long) => acc && (val < 0) }) == false""".stripMargin
    val expectedPropTree = EQ(
      Fold(
        MapCollection(Outputs, 21, ExtractAmount(TaggedBox(21))),
        22,
        TrueLeaf,
        21,
        BinAnd(TaggedBoolean(21), LT(TaggedLong(22), LongConstant(0)))),
      FalseLeaf)
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  // TODO LHF
  ignore("forall for custom collection") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """{
        |  val indexCollection = Array(0, 1, 2, 3, 4, 5)
        |  val elementRule = {(index: Int) =>
        |    val boundaryIndex = if (index == 0) 5 else (index - 1)
        |    boundaryIndex >= 0 && boundaryIndex <= 5
        |  }
        |  indexCollection.forall(elementRule)
         }""".stripMargin

    val indexCollection = ConcreteCollection((0 until 6).map(i => IntConstant(i)))
    val indexId = 21.toByte
    val index = TaggedInt(indexId)
    val boundaryIndex = If(EQ(index, 0), 5, Minus(index, 1))
    val elementRule = BinAnd(GE(boundaryIndex, 0), LE(boundaryIndex, 5))
    val expectedPropTree = ForAll(indexCollection, indexId, elementRule)
    assertProof(code, expectedPropTree, outputBoxValues)

  }

  ignore("ByIndex for non-evaluated index") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """{
        |  val string = Array(1, 1, 0, 0, 0, 1)
        |  val indexCollection = Array(0, 1, 2, 3, 4, 5)
        |  val elementRule = {(index: Int) =>
        |    val boundedIndex = if (index <= 0) 5 else (index - 1)
        |    val element = string(boundedIndex)
        |    element == 0 || element == 1
        |  }
        |  indexCollection.forall(elementRule)
         }""".stripMargin

    val indexCollection = ConcreteCollection((0 until 6).map(i => IntConstant(i)))
    val string = ConcreteCollection(Array(1, 1, 0, 0, 0, 1).map(i => IntConstant(i)))
    val indexId = 21.toByte
    val index = TaggedInt(indexId)
    val element = ByIndex(string, If(LE(index, 0), 5, Minus(index, 1)))
    val elementRule = BinOr(EQ(element, 0), EQ(element, 1))
    val expectedPropTree = ForAll(indexCollection, indexId, elementRule)
    assertProof(code, expectedPropTree, outputBoxValues)
  }
}
