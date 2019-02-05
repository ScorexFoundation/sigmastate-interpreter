package sigmastate.utxo

import org.ergoplatform
import sigmastate.Values._
import sigmastate._
import sigmastate.helpers.{ErgoLikeTestProvingInterpreter, SigmaTestingCommons}
import sigmastate.lang.Terms._
import org.ergoplatform._
import sigmastate.interpreter.Interpreter.{emptyEnv, ScriptNameProp}
import sigmastate.serialization.OpCodes._

class CollectionOperationsSpecification extends SigmaTestingCommons {
  implicit lazy val IR = new TestingIRContext
  private val reg1 = ErgoBox.nonMandatoryRegisters.head

  private def context(boxesToSpend: IndexedSeq[ErgoBox] = IndexedSeq(),
                      outputs: IndexedSeq[ErgoBox]): ErgoLikeContext =
    ergoplatform.ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = boxesToSpend,
      spendingTransaction = ErgoLikeTransaction(IndexedSeq(), outputs),
      self = null)

  private def assertProof(code: String,
                          expectedComp: Value[SType],
                          outputBoxValues: IndexedSeq[Long],
                          boxesToSpendValues: IndexedSeq[Long] = IndexedSeq()) = {
    val (prover, verifier, prop, ctx) = buildEnv(code, expectedComp, outputBoxValues,
      boxesToSpendValues)
    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).fold(t => throw t, x => x)
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  private def assertProverFail(code: String,
                               expectedComp: Value[SType],
                               outputBoxValues: IndexedSeq[Long],
                               boxesToSpendValues: IndexedSeq[Long] = IndexedSeq()) = {
    val (prover, _, prop, ctx) = buildEnv(code, expectedComp, outputBoxValues, boxesToSpendValues)
    prover.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  private def buildEnv(code: String,
                       expectedComp: Value[SType],
                       outputBoxValues: IndexedSeq[Long],
                       boxesToSpendValues: IndexedSeq[Long]) = {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compileWithCosting(Map(), code).asBoolValue

    prop shouldBe expectedComp
    val ctx = context(boxesToSpendValues.map(ErgoBox(_, pubkey, 0)),
      outputBoxValues.map(ErgoBox(_, pubkey, 0)))
    (prover, verifier, prop, ctx)
  }

  property("exists") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage.isProven

    val prop = compileWithCosting(Map(), "OUTPUTS.exists({ (box: Box) => box.value + 5 > 10 })").asBoolValue

    val expProp = Exists(Outputs,
      FuncValue(Vector((1, SBox)),
        GT(Plus(ExtractAmount(ValUse(1, SBox)), LongConstant(5)), LongConstant(10)))
    )
    prop shouldBe expProp

    val newBox1 = ErgoBox(16, pubkey, 0)
    val newBox2 = ErgoBox(15, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
    //todo: finish
  }

  property("forall") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter
    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compileWithCosting(Map(), "OUTPUTS.forall({ (box: Box) => box.value == 10 })").asBoolValue

    val propTree = ForAll(Outputs,
        FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(10)))
      )
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBox2 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
    //todo: finish
  }


  property("forall - fail") {
    val prover = new ErgoLikeTestProvingInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compileWithCosting(Map(), "OUTPUTS.forall({ (box: Box) => box.value == 10 })").asBoolValue
    val propTree = ForAll(Outputs,
        FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(10)))
      )
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBox2 = ErgoBox(11, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(fakeSelf),
      spendingTransaction,
      self = fakeSelf)

    prover.prove(prop, ctx, fakeMessage).isSuccess shouldBe false
  }

  property("counter") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage.isProven

    val prop = compileWithCosting(Map(),
      """OUTPUTS.exists { (box: Box) =>
        |  box.R4[Long].get == SELF.R4[Long].get + 1
         }""".stripMargin).asBoolValue

    val propTree = Exists(Outputs,
      FuncValue(
        Vector((1, SBox)),
        EQ(
          ExtractRegisterAs[SLong.type](ValUse(1, SBox), reg1).get,
          Plus(ExtractRegisterAs[SLong.type](Self, reg1).get, LongConstant(1)))
      )
    )
    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0, Seq(), Map(reg1 -> LongConstant(3)))
    val newBox2 = ErgoBox(10, pubkey, 0, Seq(), Map(reg1 -> LongConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, 0, Seq(), Map(reg1 -> LongConstant(5)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(emptyEnv + (ScriptNameProp -> "prove"), prop, ctx, fakeMessage).get
    verifier.verify(emptyEnv + (ScriptNameProp -> "verify"), prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("counter - no register in outputs") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val prop = compileWithCosting(Map(),
      """OUTPUTS.exists { (box: Box) =>
        |  box.R4[Long].getOrElse(0L) == SELF.R4[Long].get + 1
         }""".stripMargin).asBoolValue

    val propTree = Exists(Outputs,
      FuncValue(
        Vector((1, SBox)),
        EQ(
          ExtractRegisterAs[SLong.type](ValUse(1, SBox), reg1).getOrElse(LongConstant(0)),
          Plus(ExtractRegisterAs[SLong.type](Self, reg1).get, LongConstant(1))
        )
      )
    )

    prop shouldBe propTree

    val newBox1 = ErgoBox(10, pubkey, 0)
    val newBox2 = ErgoBox(10, pubkey, 0, Seq(), Map(reg1 -> LongConstant(6)))
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(20, TrueLeaf, 0, Seq(), Map(reg1 -> LongConstant(5)))

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
      boxesToSpend = IndexedSeq(s),
      spendingTransaction,
      self = s)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("sizeof - num of outputs = num of inputs + 1") {
    val prover = new ErgoLikeTestProvingInterpreter
    val verifier = new ErgoLikeTestInterpreter

    val pubkey = prover.dlogSecrets.head.publicImage

    val env = Map("pubkey" -> pubkey)
    val prop = compileWithCosting(env, """pubkey && OUTPUTS.size == INPUTS.size + 1""").asBoolValue
    val propTree = SigmaAnd(pubkey, BoolToSigmaProp(EQ(SizeOf(Outputs), Plus(SizeOf(Inputs), IntConstant(1)))))
    prop shouldBe propTree

    val newBox1 = ErgoBox(11, pubkey, 0)
    val newBox2 = ErgoBox(10, pubkey, 0)
    val newBoxes = IndexedSeq(newBox1, newBox2)

    val spendingTransaction = ErgoLikeTransaction(IndexedSeq(), newBoxes)

    val s = ErgoBox(21, pubkey, 0)

    val ctx = ErgoLikeContext(
      currentHeight = 50,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      minerPubkey = ErgoLikeContext.dummyPubkey,
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
      FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(10)))
    )
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
    val expectedPropTree = EQ(
      ByIndex(
        MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
        IntConstant(3),
        Some(LongConstant(0))
      ),
      LongConstant(0)
    )

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
    val expectedPropTree = EQ(
      Fold(
        MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
        TrueLeaf,
        FuncValue(Vector((1, STuple(SBoolean, SLong))),
          BinAnd(
            SelectField(ValUse(1, STuple(SBoolean, SLong)), 1).asBoolValue,
            LT(SelectField(ValUse(1, STuple(SBoolean, SLong)), 2), LongConstant(0))))),
      FalseLeaf)
    assertProof(code, expectedPropTree, outputBoxValues)
  }

  property("map") {
    assertProof("OUTPUTS.map({ (out: Box) => out.value })(0) == 1L",
      EQ(
        ByIndex(
          MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
          IntConstant(0)
        ),
        LongConstant(1)
      ),
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
      ConcreteCollection(Vector(IntConstant(0), IntConstant(1), IntConstant(2), IntConstant(3), IntConstant(4), IntConstant(5)), SInt),
      FuncValue(Vector((1, SInt)),
        BlockValue(
          Vector(ValDef(3, If(EQ(ValUse(1, SInt), IntConstant(0)), IntConstant(5), Minus(ValUse(1, SInt), IntConstant(1))))),
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
      ConcreteCollection(Vector(IntConstant(0), IntConstant(1), IntConstant(2), IntConstant(3), IntConstant(4), IntConstant(5)), SInt),
      FuncValue(
        Vector((1, SInt)),
        BlockValue(
          Vector(
            ValDef(3,
              ByIndex(
                ConcreteCollection(Vector(IntConstant(1), IntConstant(1), IntConstant(0), IntConstant(0), IntConstant(0), IntConstant(1)), SInt),
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
            SCollection.FlatMapMethod.withConcreteTypes(Map(SCollection.tIV -> SBox, SCollection.tOV -> SByte)),
            Vector(FuncValue(1, SBox,
              ExtractScriptBytes(ValUse(1, SBox))
            ))
          ).asCollection[SByte.type],
          IntConstant(0)
        ),
        ByteConstant(0)
      ),
      IndexedSeq(1L, 1L))
  }

  property("indexOf") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).indexOf(1L, 0) == 0",
      EQ(
        MethodCall(MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
          SCollection.IndexOfMethod.withConcreteTypes(Map(SCollection.tIV -> SLong)),
          Vector(LongConstant(1), IntConstant(0))
        ),
        IntConstant(0)
      ),
      IndexedSeq(1L, 1L))
  }

  property("indices") {
    assertProof("OUTPUTS.indices == Coll(0)",
      EQ(MethodCall(Outputs, SCollection.IndicesMethod, Vector()), ConcreteCollection(IntConstant(0))),
      IndexedSeq(1L, 1L))
  }

  property("segmentLength") {
    assertProof("OUTPUTS.segmentLength({ (out: Box) => out.value == 1L }, 0) == 1",
      EQ(
        MethodCall(Outputs,
          SCollection.SegmentLengthMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
          Vector(
            FuncValue(Vector((1, SBox)),EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(1))),
            IntConstant(0)
          )
        ),
        IntConstant(1)),
      IndexedSeq(1L, 2L))
  }

  property("indexWhere") {
    assertProof("OUTPUTS.indexWhere({ (out: Box) => out.value == 1L }, 0) == 0",
      EQ(
        MethodCall(Outputs,
          SCollection.IndexWhereMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
          Vector(
            FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(1))),
            IntConstant(0)
          )
        ),
        IntConstant(0)),
      IndexedSeq(1L, 2L))
  }

  property("lastIndexWhere") {
    assertProof("OUTPUTS.lastIndexWhere({ (out: Box) => out.value == 1L }, 1) == 0",
      EQ(
        MethodCall(Outputs,
          SCollection.LastIndexWhereMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
          Vector(
            FuncValue(Vector((1, SBox)), EQ(ExtractAmount(ValUse(1, SBox)), LongConstant(1))),
            IntConstant(1)
          )
        ),
        IntConstant(0)),
      IndexedSeq(1L, 2L))
  }

  property("zip") {
    assertProof("OUTPUTS.zip(Coll(1,2)).size == 2",
      EQ(
        SizeOf(MethodCall(Outputs,
          SCollection.ZipMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
          Vector(
            ConcreteCollection(IntConstant(1), IntConstant(2))
          )
        ).asCollection[STuple]),
        IntConstant(2)),
      IndexedSeq(1L, 2L))
  }

  property("partition") {
    assertProof("OUTPUTS.partition({ (box: Box) => box.value < 2L})._1.size == 1",
      EQ(
        SizeOf(
          SelectField(
            MethodCall(Outputs,
              SCollection.PartitionMethod.withConcreteTypes(Map(SCollection.tIV -> SBox)),
              Vector(
                FuncValue(Vector((1, SBox)), LT(ExtractAmount(ValUse(1, SBox)), LongConstant(2)))
              )
            ).asValue[STuple],
            1
          ).asCollection[SType]
        ),
        IntConstant(1)),
      IndexedSeq(1L, 2L))
  }

  property("patch") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).patch(0, Coll(3L), 1)(0) == 3L",
      EQ(
        ByIndex(
          MethodCall(
            MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
            SCollection.PatchMethod.withConcreteTypes(Map(SCollection.tIV -> SLong)),
            Vector(
              IntConstant(0),
              ConcreteCollection(LongConstant(3)),
              IntConstant(1)
            )
          ).asCollection[SType],
          IntConstant(0)
        ),
        LongConstant(3)),
      IndexedSeq(1L, 2L))
  }

  property("updated") {
    assertProof("OUTPUTS.map({ (b: Box) => b.value }).updated(0, 3L)(0) == 3L",
      EQ(
        ByIndex(
          MethodCall(
            MapCollection(Outputs, FuncValue(Vector((1, SBox)), ExtractAmount(ValUse(1, SBox)))),
            SCollection.UpdatedMethod.withConcreteTypes(Map(SCollection.tIV -> SLong)),
            Vector(
              IntConstant(0),
              LongConstant(3)
            )
          ).asCollection[SType],
          IntConstant(0)
        ),
        LongConstant(3)),
      IndexedSeq(1L, 2L))
  }
}
