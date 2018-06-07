package sigmastate.utxo.examples

import scorex.crypto.hash.Blake2b256
import sigmastate.SCollection.SByteArray
import sigmastate.Values.{LongConstant, FalseLeaf, TrueLeaf, Value, ByteArrayConstant, IntConstant, BooleanConstant, ByteConstant, TaggedInt, ConcreteCollection}
import sigmastate._
import sigmastate.helpers.{ErgoLikeProvingInterpreter, SigmaTestingCommons}
import sigmastate.interpreter.ContextExtension
import sigmastate.lang.Terms._
import org.ergoplatform.ErgoBox.{R6, R3, R4, R5}
import org.ergoplatform._
import sigmastate.utxo._

/**
  * Wolfram's Rule110 implementation
  *
  * A coin holds following data:
  *
  * R4 - index of row
  * R5 - index of column
  * R6 - bit value (represented as a boolean)
  */
class Rule110Specification extends SigmaTestingCommons {

  import BlockchainSimulationSpecification.{Block, ValidationState}

  def calcRule110(left: Boolean, center: Boolean, right: Boolean): Boolean =
    (left, center, right) match {
      case (true, true, true) => false
      case (true, true, false) => true
      case (true, false, true) => true
      case (true, false, false) => false
      case (false, true, true) => true
      case (false, true, false) => true
      case (false, false, true) => true
      case (false, false, false) => false
    }

  property("rule110 - one layer in register") {
    val prover = new ErgoLikeProvingInterpreter {
      override val maxCost: Long = 2000000
    }
    val verifier = new ErgoLikeInterpreter

    val indexId = 21.toByte
    val env = Map("indexId" -> indexId)
    val prop = compile(env,
      """{
        |  let inLayer:Array[Byte] = SELF.R3[Array[Byte]].value
        |  let outLayer:Array[Byte] = OUTPUTS(0).R3[Array[Byte]].value
        |  let indexes: Array[Int] = Array(0, 1, 2, 3, 4, 5)
        |  fun procCell(i: Int): Byte = {
        |    let l = inLayer(if (i == 0) 5 else (i - 1))
        |    let c = inLayer(i)
        |    let r = inLayer((i + 1) % 6)
        |    intToByte((l * c * r + c * r + c + r) % 2)
        |  }
        |  (outLayer == indexes.map(procCell)) //&& (SELF.propositionBytes == OUTPUTS(0).propositionBytes)
         }""".stripMargin).asBoolValue

    val input = ErgoBox(1, prop, Map(R3 -> ByteArrayConstant(Array(0, 0, 0, 0, 1, 0))))
    val output = ErgoBox(1, prop, Map(R3 -> ByteArrayConstant(Array(0, 0, 0, 1, 1, 0))))
    val tx = UnsignedErgoLikeTransaction(IndexedSeq(new UnsignedInput(input.id)), IndexedSeq(output))

    val ctx = ErgoLikeContext(
      currentHeight = 1,
      lastBlockUtxoRoot = AvlTreeData.dummy,
      boxesToSpend = IndexedSeq(),
      tx,
      self = input)

    val pr = prover.prove(prop, ctx, fakeMessage).get
    verifier.verify(prop, ctx, pr, fakeMessage).get._1 shouldBe true
  }

  property("rule110") {
    val prover = new ErgoLikeProvingInterpreter()

    val RowReg = R3
    val ColumnReg = R4
    val ValueReg = R5
    val bitsInString = 31
    val lastBitIndex = bitsInString - 1

    val midBitColumn = EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg),
      ExtractRegisterAs[SLong.type](ByIndex(Inputs, 1), ColumnReg))

    val leftBitColumn =
      OR(
        AND(
          EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg), LongConstant(0)),
          EQ(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), ColumnReg), LongConstant(lastBitIndex))
        ),
        EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg),
          Plus(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), ColumnReg), LongConstant(1)))
      )

    val rightBitColumn =
      OR(
        AND(
          EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg), LongConstant(lastBitIndex)),
          EQ(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 2), ColumnReg), LongConstant(0))
        ),
        EQ(Plus(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), ColumnReg), LongConstant(1)),
          ExtractRegisterAs[SLong.type](ByIndex(Inputs, 2), ColumnReg))
      )

    val row0 = EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), RowReg),
      Plus(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 0), RowReg), LongConstant(1)))

    val row1 = EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), RowReg),
      Plus(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 1), RowReg), LongConstant(1)))

    val row2 = EQ(ExtractRegisterAs[SLong.type](ByIndex(Outputs, 0), RowReg),
      Plus(ExtractRegisterAs[SLong.type](ByIndex(Inputs, 2), RowReg), LongConstant(1)))

    val input0 = ExtractRegisterAs[SBoolean.type](ByIndex(Inputs, 0), ValueReg)
    val input1 = ExtractRegisterAs[SBoolean.type](ByIndex(Inputs, 1), ValueReg)
    val input2 = ExtractRegisterAs[SBoolean.type](ByIndex(Inputs, 2), ValueReg)

    val output = ExtractRegisterAs[SBoolean.type](ByIndex(Outputs, 0), ValueReg)

    val t = TrueLeaf
    val f = FalseLeaf

    val rule110 = OR(Seq(
      AND(EQ(input0, t), EQ(input1, t), EQ(input2, t), EQ(output, f)),
      AND(EQ(input0, t), EQ(input1, t), EQ(input2, f), EQ(output, t)),
      AND(EQ(input0, t), EQ(input1, f), EQ(input2, t), EQ(output, t)),
      AND(EQ(input0, t), EQ(input1, f), EQ(input2, f), EQ(output, f)),
      AND(EQ(input0, f), EQ(input1, t), EQ(input2, t), EQ(output, t)),
      AND(EQ(input0, f), EQ(input1, t), EQ(input2, f), EQ(output, t)),
      AND(EQ(input0, f), EQ(input1, f), EQ(input2, t), EQ(output, t)),
      AND(EQ(input0, f), EQ(input1, f), EQ(input2, f), EQ(output, f))
    ))

    val prop = AND(Seq(
      EQ(SizeOf(Inputs), IntConstant(3)),
      EQ(SizeOf(Outputs), IntConstant(4)),

      //We're checking that the outputs are indeed contain the same script
      EQ(ExtractScriptBytes(Self), ExtractScriptBytes(ByIndex(Outputs, 0))),
      EQ(ExtractScriptBytes(Self), ExtractScriptBytes(ByIndex(Outputs, 1))),
      EQ(ExtractScriptBytes(Self), ExtractScriptBytes(ByIndex(Outputs, 2))),
      EQ(ExtractScriptBytes(Self), ExtractScriptBytes(ByIndex(Outputs, 2))),

      EQ(ExtractBytesWithNoRef(ByIndex(Inputs, 0)), ExtractBytesWithNoRef(ByIndex(Outputs, 1))),
      EQ(ExtractBytesWithNoRef(ByIndex(Inputs, 1)), ExtractBytesWithNoRef(ByIndex(Outputs, 2))),
      EQ(ExtractBytesWithNoRef(ByIndex(Inputs, 2)), ExtractBytesWithNoRef(ByIndex(Outputs, 3))),

      midBitColumn,
      leftBitColumn,
      rightBitColumn,
      row0,
      row1,
      row2,
      rule110
    ))


    val hash = Blake2b256
    val txId = hash.hash(scala.util.Random.nextString(12).getBytes)

    // further we fill the genesis row as in the first example at http://mathworld.wolfram.com/Rule110.html,
    // and check that the first row (after the genesis one) is satisfying the example

    val coins = (0 until bitsInString).map { col =>
      val row = RowReg -> LongConstant(0)
      val column = ColumnReg -> LongConstant(col)
      val value = if (col == 15) ValueReg -> TrueLeaf else ValueReg -> FalseLeaf
      ErgoBox(0L, prop, Map(row, column, value), txId, col.toShort)
    }

    val initBlock = BlockchainSimulationSpecification.Block {
      IndexedSeq(ErgoLikeTransaction(IndexedSeq(), coins))
    }

    val genesisState = ValidationState.initialState(initBlock)

    def byPos(state: ValidationState, row: Int, pos: Int) =
      state.boxesReader.byTwoInts(RowReg, row, ColumnReg, pos).get

    def generateTransactionsForRow(state: ValidationState, row: Int): IndexedSeq[ErgoLikeTransaction] = {
      require(row >= 1)

      (0 until bitsInString).map { col =>
        val leftCol = if (col == 0) lastBitIndex else col - 1
        val centerCol = col
        val rightCol = if (col == lastBitIndex) 0 else col + 1

        val left = byPos(state, row - 1, leftCol)
        val center = byPos(state, row - 1, centerCol)
        val right = byPos(state, row - 1, rightCol)

        val lv = left.get(ValueReg).get.asInstanceOf[BooleanConstant].value
        val cv = center.get(ValueReg).get.asInstanceOf[BooleanConstant].value
        val rv = right.get(ValueReg).get.asInstanceOf[BooleanConstant].value

        val value = ValueReg -> BooleanConstant.fromBoolean(calcRule110(lv, cv, rv))

        val c = new ErgoBoxCandidate(0L, prop, Map(RowReg -> LongConstant(row), ColumnReg -> LongConstant(col), value))

        val ut = UnsignedErgoLikeTransaction(
          IndexedSeq(new UnsignedInput(left.id), new UnsignedInput(center.id), new UnsignedInput(right.id)),
          IndexedSeq(c, left.toCandidate, center.toCandidate, right.toCandidate)
        )

        val contextLeft = ErgoLikeContext(row,
          state.state.lastBlockUtxoRoot,
          IndexedSeq(left, center, right),
          ut,
          left,
          ContextExtension.empty)
        val proverResultLeft = prover.prove(left.proposition, contextLeft, ut.messageToSign).get

        val contextCenter = ErgoLikeContext(row,
          state.state.lastBlockUtxoRoot,
          IndexedSeq(left, center, right),
          ut,
          center,
          ContextExtension.empty)
        val proverResultCenter = prover.prove(center.proposition, contextCenter, ut.messageToSign).get

        val contextRight = ErgoLikeContext(row,
          state.state.lastBlockUtxoRoot,
          IndexedSeq(left, center, right),
          ut,
          right,
          ContextExtension.empty)
        val proverResultRight = prover.prove(right.proposition, contextRight, ut.messageToSign).get
        ut.toSigned(IndexedSeq(proverResultLeft, proverResultCenter, proverResultRight))
      }
    }

    val firstRowBlock = Block(generateTransactionsForRow(genesisState, 1))

    val t0 = System.currentTimeMillis()
    val firstRowState = genesisState.applyBlock(firstRowBlock, 10000000).get
    val t1 = System.currentTimeMillis()

    println(s"First row time ${t1 - t0} ms.")

    firstRowState.boxesReader.byTwoInts(RowReg, 1, ColumnReg, 13).get.get(ValueReg).get.asInstanceOf[BooleanConstant].value shouldBe false
    firstRowState.boxesReader.byTwoInts(RowReg, 1, ColumnReg, 14).get.get(ValueReg).get.asInstanceOf[BooleanConstant].value shouldBe true
    firstRowState.boxesReader.byTwoInts(RowReg, 1, ColumnReg, 15).get.get(ValueReg).get.asInstanceOf[BooleanConstant].value shouldBe true
    firstRowState.boxesReader.byTwoInts(RowReg, 1, ColumnReg, 16).get.get(ValueReg).get.asInstanceOf[BooleanConstant].value shouldBe false
  }
}