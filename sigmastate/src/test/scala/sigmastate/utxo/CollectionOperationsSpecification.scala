package sigmastate.utxo

import org.ergoplatform
import org.ergoplatform.ErgoScriptPredef.TrueProp
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ContextEnrichingTestProvingInterpreter, ErgoLikeContextTesting, ErgoLikeTestInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import org.ergoplatform._
import sigmastate.SCollection._
import sigmastate.interpreter.Interpreter.{ScriptNameProp, emptyEnv}
import sigmastate.serialization.OpCodes._
import sigmastate.utils.Helpers._

class CollectionOperationsSpecification extends SigmaTestingCommons {
  implicit lazy val IR: TestingIRContext = new TestingIRContext
  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  private def context(boxesToSpend: IndexedSeq[ErgoBox] = IndexedSeq(),
                      outputs: IndexedSeq[ErgoBox]): ErgoLikeContext =
    {
      val (selfBox, toSpend) = if (boxesToSpend.isEmpty) (fakeSelf, IndexedSeq(fakeSelf)) else (boxesToSpend(0), boxesToSpend)
      ErgoLikeContextTesting(
        currentHeight = 50,
        lastBlockUtxoRoot = AvlTreeData.dummy,
        minerPubkey = ErgoLikeContextTesting.dummyPubkey,
        boxesToSpend = toSpend,
        spendingTransaction = createTransaction(outputs),
        self = selfBox)
    }

  private def assertProof(code: String,
                          expectedComp: SigmaPropValue,
                          outputBoxValues: IndexedSeq[Long],
                          boxesToSpendValues: IndexedSeq[Long] = IndexedSeq()) = {
    val (prover, verifier, prop, ctx) = buildEnv(code, expectedComp, outputBoxValues, boxesToSpendValues)
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).getOrThrow
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).getOrThrow._1 shouldBe true
  }

  private def assertProof(code: String,
                          outputBoxValues: IndexedSeq[Long],
                          boxesToSpendValues: IndexedSeq[Long]) = {
    val (prover, verifier, prop, ctx) = buildEnv(code, None, outputBoxValues, boxesToSpendValues)
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).getOrThrow
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).getOrThrow._1 shouldBe true
  }

  private def assertProverFail(code: String,
                               expectedComp: SigmaPropValue,
                               outputBoxValues: IndexedSeq[Long],
                               boxesToSpendValues: IndexedSeq[Long] = IndexedSeq()) = {
    val (prover, _, prop, ctx) = buildEnv(code, expectedComp, outputBoxValues, boxesToSpendValues)
    prover.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  private def buildEnv(code: String,
                       expectedComp: Value[SType],
                       outputBoxValues: IndexedSeq[Long],
                       boxesToSpendValues: IndexedSeq[Long]):
  (ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaPropValue, ErgoLikeContext) =
    buildEnv(code, Some(expectedComp), outputBoxValues, boxesToSpendValues)

  private def buildEnv(code: String,
                       expectedComp: Option[Value[SType]],
                       outputBoxValues: IndexedSeq[Long],
                       boxesToSpendValues: IndexedSeq[Long]):
  (ContextEnrichingTestProvingInterpreter, ErgoLikeTestInterpreter, SigmaPropValue, ErgoLikeContext) = {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), code).asBoolValue.toSigmaProp

    expectedComp.foreach(prop shouldBe _)
    val ctx = context(boxesToSpendValues.map(ErgoBox(_, pubkey, 0)),
      outputBoxValues.map(ErgoBox(_, pubkey, 0)))
    (prover, verifier, prop, ctx)
  }

  property("exists") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage.toSigmaProp

    val prop = compile(Map(), "OUTPUTS.exists({ (box: Box) => box.value + 5 > 10 })").asBoolValue.toSigmaProp

    val expProp = Exists(Outputs,
      FuncValue(Vector((1, SBox)),
        GT(Plus(ExtractAmount(ValUse(1, SBox)), LongConstant(5)), LongConstant(10)))
    ).toSigmaProp
    prop shouldBe expProp

    val newBox1 = ErgoBox(16, pubkey, 0)
    val newBox2 = ErgoBox(15, pubkey, 0)
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
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true

    //TODO coverage: add negative case for `exists`
  }

  property("forall") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.forall({ (box: Box) => box.value == 10 })").asBoolValue.toSigmaProp

    val propTree = ForAll(Outputs,
        FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(10)))
      ).toSigmaProp
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
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
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }


  property("forall - fail") {
    val prover = new ContextEnrichingTestProvingInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(), "OUTPUTS.forall({ (box: Box) => box.value == 10 })").asBoolValue.toSigmaProp
    val propTree = ForAll(Outputs,
        FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(10)))
      ).toSigmaProp
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBox2 = ErgoBox(11, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf)

    prover.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("counter") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage.toSigmaProp

    val prop = compile(Map(),
      """OUTPUTS.exists { (box: Box) =>
        |  box.R4[Long].get == SELF.R4[Long].get + 1
         }""".stripMargin).asBoolValue.toSigmaProp

    val propTree = Exists(Outputs,
      FuncValue(
        Vector((1, SBox)),
        EQ(
          ExtractRegisterAs[SLong.type](ValUse(1, SBox), reg1).get,
          Plus(ExtractRegisterAs[SLong.type](Self, reg1).get, LongConstant(1)))
      )
    ).toSigmaProp
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0, Seq(), Map(reg1 -> LongConstant(3)))
    val newBox2 = ErgoBox(10, pubkey, 0, Seq(), Map(reg1 -> LongConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val s = ErgoBox(20, TrueProp, 0, Seq(), Map(reg1 -> LongConstant(5)))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("counter - no register in outputs") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compile(Map(),
      """OUTPUTS.exists { (box: Box) =>
        |  box.R4[Long].getOrElse(0L) == SELF.R4[Long].get + 1
         }""".stripMargin).asBoolValue.toSigmaProp

    val propTree = Exists(Outputs,
      FuncValue(
        Vector((1, SBox)),
        EQ(
          ExtractRegisterAs[SLong.type](ValUse(1, SBox), reg1).getOrElse(LongConstant(0)),
          Plus(ExtractRegisterAs[SLong.type](Self, reg1).get, LongConstant(1))
        )
      )
    ).toSigmaProp

    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBox2 = ErgoBox(10, pubkey, 0, Seq(), Map(reg1 -> LongConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val s = ErgoBox(20, TrueProp, 0, Seq(), Map(reg1 -> LongConstant(5)))

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("sizeof - num of outputs = num of inputs + 1") {
    val prover = new ContextEnrichingTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val prop = compile(env, """pubkey && OUTPUTS.size == INPUTS.size + 1""").asSigmaProp
    val propTree = SigmaAnd(pubkey, BoolToSigmaProp(EQ(SizeOf(Outputs), Plus(SizeOf(Inputs), IntConstant(1)))))
    prop shouldBe propTree

    val newBox1 = ErgoBox(11, pubkey, 0)
    val newBox2 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = createTransaction(newBoxes)

    val s = ErgoBox(21, pubkey, 0)

    val ctx = ErgoLikeContextTesting(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContextTesting.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage)


    val fProp = SigmaAnd(pubkey, EQ(SizeOf(Outputs), SizeOf(Inputs)))
    prover.prove(fProp, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("slice") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code = "OUTPUTS.slice(1, OUTPUTS.size).forall({ (box: Box) => box.value == 10 })"
    val expectedPropTree = ForAll(
      Slice(Outputs, IntConstant(1), SizeOf(Outputs)),
      FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(10)))
    ).toSigmaProp
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

  property("by index with default value") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """OUTPUTS
        |.map { (box: Box) => box.value }
        |.getOrElse(3, 0L)== 0""".stripMargin
    val expectedPropTree = BlockValue(
      Vector(ValDef(1, MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))))),
      EQ(
        ByIndex(
          ValUse(1, SLongArray),
          IntConstant(3),
          Some(LongConstant(0))
        ),
        LongConstant(0)
      )).asBoolValue

    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("by index with evaluated default value") {
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

  property("map fold") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """OUTPUTS
        |.map({ (box: Box) => box.value })
        |.fold(true, { (acc: Boolean, val: Long) => acc && (val < 0) }) == false""".stripMargin
    val expectedPropTree = BlockValue(
      Vector(
        ValDef(1, MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox))))),
        ValDef(2, Fold(
          ValUse(1, SLongArray),
          TrueLeaf,
          FuncValue(Vector((2, STuple(SBoolean, SLong))),
            BinAnd(
              SelectField(ValUse(2, STuple(SBoolean, SLong)), 1).asBoolValue,
              LT(SelectField(ValUse(2, STuple(SBoolean, SLong)), 2), LongConstant(0))))))),
      LogicalNot( ValUse(2, SBoolean) )).asBoolValue
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("map") {
    assertProof("OUTPUTS.map({ (out: Box) => out.value })(0) == 1L",
      BlockValue(
        Vector(ValDef(1, MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))))),
        EQ(
          ByIndex( ValUse(1, SLongArray), IntConstant(0) ),
          LongConstant(1)
        )
      ).asBoolValue,
      IndexedSeq(1L, 1L))
  }

  property("forall for custom collection") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """{
        |  val indexCollection = Coll(0, 1, 2, 3, 4, 5)
        |  def elementRule(index: Int) = {
        |    val boundaryIndex = if (index == 0) 5 else (index - 1)
        |    boundaryIndex >= 0 && boundaryIndex <= 5
        |  }
        |  indexCollection.forall(elementRule)
         }""".stripMargin

    val expectedPropTree = ForAll(
      ConcreteCollection(Array(IntConstant(0), IntConstant(1), IntConstant(2), IntConstant(3), IntConstant(4), IntConstant(5)), SInt),
      FuncValue(Array((1, SInt)),
        BlockValue(
          Array(ValDef(3, If(EQ(ValUse(1, SInt), IntConstant(0)), IntConstant(5), Minus(ValUse(1, SInt), IntConstant(1))))),
          BinAnd(GE(ValUse(3, SInt), IntConstant(0)), LE(ValUse(3, SInt), IntConstant(5)))
        )
      )
    )
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("ByIndex for non-evaluated index") {
    val outputBoxValues = IndexedSeq(10L, 10L)
    val code =
      """{
        |  val string = Coll(1, 1, 0, 0, 0, 1)
        |  val indexCollection = Coll(0, 1, 2, 3, 4, 5)
        |  val elementRule = {(index: Int) =>
        |    val boundedIndex = if (index <= 0) 5 else (index - 1)
        |    val element = string(boundedIndex)
        |    element == 0 || element == 1
        |  }
        |  indexCollection.forall(elementRule)
         }""".stripMargin

    val expectedPropTree = ForAll(
      ConcreteCollection(Array(IntConstant(0), IntConstant(1), IntConstant(2), IntConstant(3), IntConstant(4), IntConstant(5)), SInt),
      FuncValue(
        Array((1, SInt)),
        BlockValue(
          Array(
            ValDef(3,
              ByIndex(
                ConcreteCollection(Array(IntConstant(1), IntConstant(1), IntConstant(0), IntConstant(0), IntConstant(0), IntConstant(1)), SInt),
                If(
                  LE(ValUse(1, SInt), IntConstant(0)),
                  IntConstant(5),
                  Minus(ValUse(1, SInt), IntConstant(1))
                ),
                None))),
          BinOr(EQ(ValUse(3, SInt), IntConstant(0)), EQ(ValUse(3, SInt), IntConstant(1)))
        )
      )
    )

    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("flatMap") {
    assertProof("OUTPUTS.flatMap({ (out: Box) => out.propositionBytes })(0) == 0.toByte",
      EQ(
        ByIndex(
          MethodCall(Outputs,
            FlatMapMethod.withConcreteTypes(Map(tIV -> SBox, tOV -> SByte)),
            Vector(FuncValue(1, SBox,
              ExtractScriptBytes(ValUse(1, SBox))
            )),
            Map()
          ).asCollection[SByte.type],
          IntConstant(0)
        ),
        ByteConstant(0)
      ),
      IndexedSeq(1L, 1L))
  }

  property("indexOf") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).indexOf(1L, 0) == 0",
      BlockValue(
        Vector(ValDef(1, MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))))),
        EQ(
          MethodCall(ValUse(1, SLongArray),
            IndexOfMethod.withConcreteTypes(Map(tIV -> SLong)),
            Vector(LongConstant(1), IntConstant(0)),
            Map()),
          IntConstant(0)
        )
      ).asBoolValue,
      IndexedSeq(1L, 1L))
  }

  property("indices") {
    assertProof("OUTPUTS.indices == Coll(0, 1)",
      EQ(MethodCall(Outputs, IndicesMethod.withConcreteTypes(Map(tIV -> SBox)), Vector(), Map()), ConcreteCollection.fromItems(IntConstant(0), IntConstant(1))),
      IndexedSeq(1L, 1L))
  }

  property("zip") {
    assertProof("OUTPUTS.zip(INPUTS).size == 2",
      EQ(
        SizeOf(MethodCall(Outputs,
          SCollection.ZipMethod.withConcreteTypes(Map(SCollection.tIV -> SBox, SCollection.tOV -> SBox)),
          Vector(Inputs),
          Map()).asCollection[STuple]),
        IntConstant(2)),
      IndexedSeq(1L, 2L), IndexedSeq(3L, 4L))
  }

  property("zip (nested)") {
    assertProof(
      """OUTPUTS.zip(INPUTS).zip(OUTPUTS).zip(INPUTS)
        | .map({ (t: (((Box, Box), Box), Box)) =>
        | t._1._2.value + t._2.value
        | }).fold(0L, { (a: Long, v: Long) => a + v }) == 10""".stripMargin,
      IndexedSeq(1L, 2L), IndexedSeq(3L, 4L))
  }

  property("patch") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).patch(0, Coll(3L), 1)(0) == 3L",
      BlockValue(
        Vector(ValDef(1, MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))))),
        EQ(
          ByIndex(
            MethodCall(
              ValUse(1, SLongArray),
              PatchMethod.withConcreteTypes(Map(tIV -> SLong)),
              Vector(IntConstant(0), ConcreteCollection.fromItems(LongConstant(3)), IntConstant(1)),
              Map()).asCollection[SType],
            IntConstant(0)
          ),
          LongConstant(3))
      ).asBoolValue,
      IndexedSeq(1L, 2L))
  }

  property("updated") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).updated(0, 3L)(0) == 3L",
      BlockValue(Vector(ValDef(1, MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))))),
        EQ(
          ByIndex(
            MethodCall(
              ValUse(1, SLongArray),
              UpdatedMethod.withConcreteTypes(Map(tIV -> SLong)),
              Vector(IntConstant(0), LongConstant(3)),
              Map()).asCollection[SType],
            IntConstant(0)
          ),
          LongConstant(3))
      ).asBoolValue,
      IndexedSeq(1L, 2L))
  }

  property("updateMany") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).updateMany(Coll(0), Coll(3L))(0) == 3L",
      BlockValue(
        Vector(ValDef(1, MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))))),
        EQ(
          ByIndex(
            MethodCall(
              ValUse(1, SLongArray),
              UpdateManyMethod.withConcreteTypes(Map(tIV -> SLong)),
              Vector(ConcreteCollection.fromItems(IntConstant(0)), ConcreteCollection.fromItems(LongConstant(3))),
              Map()).asCollection[SType],
            IntConstant(0)
          ),
          LongConstant(3))
      ).asBoolValue,
      IndexedSeq(1L, 2L))
  }
}
