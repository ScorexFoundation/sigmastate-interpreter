package org.ergoplatform

import org.ergoplatform.ErgoLikeContext.Metadata._
import org.ergoplatform.ErgoLikeContext.{Height, Metadata}
import sigmastate.Values._
import sigmastate._
import sigmastate.eval.{CostingAvlTree, CostingDataContext, Evaluation, CostingBox}
import sigmastate.interpreter.{ContextExtension, Context}
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utxo.CostTable.Cost
import special.collection.Col
import special.sigma
import special.sigma.{AnyValue, TestValue, Box}
import scala.util.Try

case class BlockchainState(currentHeight: Height, lastBlockUtxoRoot: AvlTreeData)

// todo: write description
class ErgoLikeContext(val currentHeight: Height,
                      val lastBlockUtxoRoot: AvlTreeData,
                      val boxesToSpend: IndexedSeq[ErgoBox],
                      val spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                      val self: ErgoBox,
                      val metadata: Metadata,
                      override val extension: ContextExtension = ContextExtension(Map())
                 ) extends Context[ErgoLikeContext] {
  override def withExtension(newExtension: ContextExtension): ErgoLikeContext =
    ErgoLikeContext(currentHeight, lastBlockUtxoRoot, boxesToSpend, spendingTransaction, self, metadata, newExtension)

  def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoLikeContext =
    ErgoLikeContext(currentHeight, lastBlockUtxoRoot, boxesToSpend, newSpendingTransaction, self, metadata, extension)

  import ErgoLikeContext._

  override def toSigmaContext(IR: Evaluation, isCost: Boolean): sigma.Context = {
    implicit val IRForBox = IR
    val inputs = boxesToSpend.toArray.map(_.toTestBox(isCost))
    val outputs =
      if (spendingTransaction == null) noOutputs
      else spendingTransaction.outputs.toArray.map(_.toTestBox(isCost))
    val vars = contextVars(extension.values)
    val noBytes = IR.sigmaDslBuilderValue.Cols.fromArray[Byte](Array[Byte]())
    val avlTree = CostingAvlTree(IR, lastBlockUtxoRoot)
    new CostingDataContext(IR, inputs, outputs, currentHeight, self.toTestBox(isCost), avlTree, vars.arr, isCost)
  }


}

object ErgoLikeContext {
  type Height = Long

  case class Metadata(networkPrefix: NetworkPrefix)

  object Metadata {
    type NetworkPrefix = Byte
    val MainnetNetworkPrefix: NetworkPrefix = 0.toByte
    val TestnetNetworkPrefix: NetworkPrefix = 16.toByte
  }

  def apply(currentHeight: Height,
            lastBlockUtxoRoot: AvlTreeData,
            boxesToSpend: IndexedSeq[ErgoBox],
            spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
            self: ErgoBox,
            metadata: Metadata = Metadata(TestnetNetworkPrefix),
            extension: ContextExtension = ContextExtension(Map())) =
    new ErgoLikeContext(currentHeight, lastBlockUtxoRoot, boxesToSpend, spendingTransaction, self, metadata, extension)


  def dummy(selfDesc: ErgoBox) = ErgoLikeContext(currentHeight = 0,
    lastBlockUtxoRoot = AvlTreeData.dummy, boxesToSpend = IndexedSeq(),
    spendingTransaction = null, self = selfDesc, metadata = Metadata(networkPrefix = TestnetNetworkPrefix))

  def fromTransaction(tx: ErgoLikeTransaction,
                      blockchainState: BlockchainState,
                      boxesReader: ErgoBoxReader,
                      inputIndex: Int,
                      metadata: Metadata): Try[ErgoLikeContext] = Try {

    val boxes = tx.inputs.map(_.boxId).map(id => boxesReader.byId(id).get)

    val proverExtension = tx.inputs(inputIndex).spendingProof.extension

    ErgoLikeContext(blockchainState.currentHeight,
      blockchainState.lastBlockUtxoRoot,
      boxes,
      tx,
      boxes(inputIndex),
      metadata,
      proverExtension)
  }

  val noInputs = Array[Box]()
  val noOutputs = Array[Box]()

  def toTestData(value: Any, tpe: SType)(implicit IR: Evaluation): Any = (value, tpe) match {
    case (arr: Array[a], SCollectionType(elemType)) =>
      elemType match {
        case SCollectionType(_) | STuple(_) =>
          val testArr = arr.map(x => toTestData(x, elemType))
          IR.sigmaDslBuilderValue.Cols.fromArray(testArr)
        case _ =>
          IR.sigmaDslBuilderValue.Cols.fromArray(arr)
      }
    case (arr: Array[a], STuple(items)) =>
      val res = arr.zip(items).map { case (x, t) => toTestData(x, t)}
      IR.sigmaDslBuilderValue.Cols.fromArray(res)
    case (t: AvlTreeData, SAvlTree) => CostingAvlTree(IR, t)
    case (x, _) => x
  }

  def regs(m: Map[Byte, Any])(implicit IR: Evaluation): Col[AnyValue] = {
    val res = new Array[AnyValue](10)
    for ((id, v) <- m) {
      assert(res(id) == null, s"register $id is defined more then once")
      res(id) = new TestValue(v)
    }
    IR.sigmaDslBuilderValue.Cols.fromArray(res)
  }

  def contextVars(m: Map[Byte, Any])(implicit IR: Evaluation): Col[AnyValue] = {
    val maxKey = if (m.keys.isEmpty) 0 else m.keys.max
    val res = new Array[AnyValue](maxKey + 1)
    for ((id, v) <- m) {
      assert(res(id) == null, s"register $id is defined more then once")
      res(id) = new TestValue(v)
    }
    IR.sigmaDslBuilderValue.Cols.fromArray(res)
  }

  implicit class ErgoBoxOps(ebox: ErgoBox) {
    def toTestBox(isCost: Boolean)(implicit IR: Evaluation): Box = {
      val rs = regs(ebox.additionalRegisters.map({ case (k, v) => k.number -> v }))
      new CostingBox(IR,
        IR.sigmaDslBuilderValue.Cols.fromArray(ebox.id),
        ebox.value,
        IR.sigmaDslBuilderValue.Cols.fromArray(ebox.bytes),
        IR.sigmaDslBuilderValue.Cols.fromArray(ebox.bytesWithNoRef),
        IR.sigmaDslBuilderValue.Cols.fromArray(ebox.propositionBytes),
        rs, isCost)
    }
  }
}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object Height extends NotReadyValueLong {
  override val opCode: OpCode = OpCodes.HeightCode

  override def cost[C <: Context[C]](context: C): Long = 2 * Cost.IntConstantDeclaration

  def opType = SFunc(SContext, SInt)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.boxesToSpend */
case object Inputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.InputsCode

  override def cost[C <: Context[C]](context: C) =
    context.asInstanceOf[ErgoLikeContext].boxesToSpend.map(_.cost).sum + Cost.ConcreteCollectionDeclaration

  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.spendingTransaction.outputs */
case object Outputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.OutputsCode

  override def cost[C <: Context[C]](context: C) =
    context.asInstanceOf[ErgoLikeContext].spendingTransaction.outputs.map(_.cost).sum + Cost.ConcreteCollectionDeclaration

  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a AvlTreeConstant built from Context.lastBlockUtxoRoot */
case object LastBlockUtxoRootHash extends NotReadyValueAvlTree {
  override val opCode: OpCode = OpCodes.LastBlockUtxoRootHashCode

  override def cost[C <: Context[C]](context: C) = Cost.AvlTreeConstantDeclaration + 1
  def opType = SFunc(SContext, tpe)
}


/** When interpreted evaluates to a BoxConstant built from Context.self */
case object Self extends NotReadyValueBox {
  override val opCode: OpCode = OpCodes.SelfCode

  override def cost[C <: Context[C]](context: C) = context.asInstanceOf[ErgoLikeContext].self.cost

  def opType = SFunc(SContext, SBox)
}
