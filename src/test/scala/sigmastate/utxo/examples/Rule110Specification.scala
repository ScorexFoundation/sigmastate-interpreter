package sigmastate.utxo.examples

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.hash.Blake2b256
import sigmastate.Values.{BooleanConstant, FalseLeaf, IntConstant, TrueLeaf}
import sigmastate._
import sigmastate.helpers.ErgoProvingInterpreter
import sigmastate.interpreter.ContextExtension
import sigmastate.utxo.ErgoBox.{R3, R4, R5, R6}
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
class Rule110Specification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers {

  import BlockchainSimulationSpecification.{ValidationState, Block}

  def calcRule110(left: Boolean, center: Boolean, right: Boolean):Boolean =
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

  property("rule110") {
    val prover = new ErgoProvingInterpreter()

    val bitsInString = 31
    val lastBitIndex = bitsInString - 1

    val midBitColumn = EQ(ExtractRegisterAs[SInt.type](ByIndex(Outputs, 0), R5),
      ExtractRegisterAs[SInt.type](ByIndex(Inputs, 1), R5))

    val leftBitColumn =
      OR(
        AND(
          EQ(ExtractRegisterAs[SInt.type](ByIndex(Outputs, 0), R5), IntConstant(0)),
          EQ(ExtractRegisterAs[SInt.type](ByIndex(Inputs, 0), R5), IntConstant(lastBitIndex))
        ),
        EQ(ExtractRegisterAs[SInt.type](ByIndex(Outputs, 0), R5),
          Plus(ExtractRegisterAs[SInt.type](ByIndex(Inputs, 0), R5), IntConstant(1)))
      )

    val rightBitColumn =
      OR(
        AND(
          EQ(ExtractRegisterAs[SInt.type](ByIndex(Outputs, 0), R5), IntConstant(lastBitIndex)),
          EQ(ExtractRegisterAs[SInt.type](ByIndex(Inputs, 2), R5), IntConstant(0))
        ),
        EQ(Plus(ExtractRegisterAs[SInt.type](ByIndex(Outputs, 0), R5), IntConstant(1)),
          ExtractRegisterAs[SInt.type](ByIndex(Inputs, 2), R5))
      )

    val row0 = EQ(ExtractRegisterAs[SInt.type](ByIndex(Outputs, 0), R4),
      Plus(ExtractRegisterAs[SInt.type](ByIndex(Inputs, 0), R4), IntConstant(1)))

    val row1 = EQ(ExtractRegisterAs[SInt.type](ByIndex(Outputs, 0), R4),
      Plus(ExtractRegisterAs[SInt.type](ByIndex(Inputs, 1), R4), IntConstant(1)))

    val row2 = EQ(ExtractRegisterAs[SInt.type](ByIndex(Outputs, 0), R4),
      Plus(ExtractRegisterAs[SInt.type](ByIndex(Inputs, 2), R4), IntConstant(1)))

    val input0 = ExtractRegisterAs[SBoolean.type](ByIndex(Inputs, 0), R6)
    val input1 = ExtractRegisterAs[SBoolean.type](ByIndex(Inputs, 1), R6)
    val input2 = ExtractRegisterAs[SBoolean.type](ByIndex(Inputs, 2), R6)

    val output = ExtractRegisterAs[SBoolean.type](ByIndex(Outputs, 0), R6)

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
      val row = R4 -> IntConstant(0)
      val column = R5 -> IntConstant(col)
      val value = if(col == 15) R6 -> TrueLeaf else R6 -> FalseLeaf
      ErgoBox(0L, prop, Map(row, column, value), txId, col.toShort)
    }

    val initBlock = BlockchainSimulationSpecification.Block {
      IndexedSeq(ErgoTransaction(IndexedSeq(), coins))
    }

    val genesisState = ValidationState.initialState(initBlock)

    def byPos(state: ValidationState, row: Int, pos:Int) =
      state.boxesReader.byTwoInts(R4, row, R5, pos).get

    def generateTransactionsForRow(state: ValidationState, row: Int): IndexedSeq[ErgoTransaction] = {
      require(row >=1)

      (0 until bitsInString).map { col =>
        val leftCol = if (col == 0) lastBitIndex else col - 1
        val centerCol = col
        val rightCol = if (col == lastBitIndex) 0 else col + 1

        val left = byPos(state, row-1, leftCol)
        val center = byPos(state, row-1, centerCol)
        val right = byPos(state, row-1, rightCol)

        val lv = left.get(R6).get.asInstanceOf[BooleanConstant].value
        val cv = center.get(R6).get.asInstanceOf[BooleanConstant].value
        val rv = right.get(R6).get.asInstanceOf[BooleanConstant].value

        val value = R6 -> BooleanConstant.fromBoolean(calcRule110(lv, cv, rv))

        val c = new ErgoBoxCandidate(0L, prop, Map(R4 -> IntConstant(row), R5 -> IntConstant(col), value))

        val ut = UnsignedErgoTransaction(
          IndexedSeq(UnsignedInput(left.id),UnsignedInput(center.id),UnsignedInput(right.id)),
          IndexedSeq(c, left.toCandidate, center.toCandidate, right.toCandidate)
        )

        val contextLeft = ErgoContext(row,
          state.state.lastBlockUtxoRoot,
          IndexedSeq(left, center, right),
          ut,
          left,
          ContextExtension.empty)
        val proverResultLeft = prover.prove(left.proposition, contextLeft, ut.messageToSign).get

        val contextCenter = ErgoContext(row,
          state.state.lastBlockUtxoRoot,
          IndexedSeq(left, center, right),
          ut,
          center,
          ContextExtension.empty)
        val proverResultCenter = prover.prove(center.proposition, contextCenter, ut.messageToSign).get

        val contextRight = ErgoContext(row,
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
    val firstRowState = genesisState.applyBlock(firstRowBlock).get
    val t1 = System.currentTimeMillis()

    println(s"Script cost: ${prop.cost}")
    println(s"First row time ${t1-t0} ms.")

    firstRowState.boxesReader.byTwoInts(R4, 1, R5, 13).get.get(R6).get.asInstanceOf[BooleanConstant].value shouldBe false
    firstRowState.boxesReader.byTwoInts(R4, 1, R5, 14).get.get(R6).get.asInstanceOf[BooleanConstant].value shouldBe true
    firstRowState.boxesReader.byTwoInts(R4, 1, R5, 15).get.get(R6).get.asInstanceOf[BooleanConstant].value shouldBe true
    firstRowState.boxesReader.byTwoInts(R4, 1, R5, 16).get.get(R6).get.asInstanceOf[BooleanConstant].value shouldBe false
  }
}