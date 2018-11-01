package org.ergoplatform

import com.google.common.primitives.Shorts
import org.ergoplatform.ErgoBox.ReferenceRegId
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.ErgoLikeContext.Metadata.NetworkPrefix
import sigmastate.Values._
import sigmastate._
import sigmastate.eval.{CostingAvlTree, CostingBox, CostingDataContext, Evaluation}
import sigmastate.interpreter.{Context, ContextExtension}
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import sigmastate.utxo.CostTable.Cost
import special.collection.Col
import special.sigma
import special.sigma.{AnyValue, Box, TestValue}

import scala.util.Try

case class BlockchainState(currentHeight: Height, lastBlockUtxoRoot: AvlTreeData)

// todo: write description
class ErgoLikeContext(val currentHeight: Height,
                      val lastBlockUtxoRoot: AvlTreeData,
                      val minerPubkey: Array[Byte],
                      val boxesToSpend: IndexedSeq[ErgoBox],
                      val spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                      val self: ErgoBox,
                      override val extension: ContextExtension = ContextExtension(Map())
                 ) extends Context {
  override def withExtension(newExtension: ContextExtension): ErgoLikeContext =
    ErgoLikeContext(currentHeight, lastBlockUtxoRoot, minerPubkey, boxesToSpend, spendingTransaction, self, newExtension)

  def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoLikeContext =
    ErgoLikeContext(currentHeight, lastBlockUtxoRoot, minerPubkey, boxesToSpend, newSpendingTransaction, self, extension)

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

  val dummyPubkey: Array[Byte] = Array.fill(32)(0: Byte)

  case class Metadata(networkPrefix: NetworkPrefix)

  object Metadata {
    type NetworkPrefix = Byte
    val MainnetNetworkPrefix: NetworkPrefix = 0.toByte
    val TestnetNetworkPrefix: NetworkPrefix = 16.toByte
  }

  def apply(currentHeight: Height,
            lastBlockUtxoRoot: AvlTreeData,
            minerPubkey: Array[Byte],
            boxesToSpend: IndexedSeq[ErgoBox],
            spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
            self: ErgoBox,
            extension: ContextExtension = ContextExtension(Map())) =
    new ErgoLikeContext(currentHeight, lastBlockUtxoRoot, minerPubkey, boxesToSpend, spendingTransaction, self, extension)


  def dummy(selfDesc: ErgoBox) = ErgoLikeContext(currentHeight = 0,
    lastBlockUtxoRoot = AvlTreeData.dummy, dummyPubkey, boxesToSpend = IndexedSeq(),
    spendingTransaction = null, self = selfDesc)

  def fromTransaction(tx: ErgoLikeTransaction,
                      blockchainState: BlockchainState,
                      boxesReader: ErgoBoxReader,
                      inputIndex: Int): Try[ErgoLikeContext] = Try {

    val boxes = tx.inputs.map(_.boxId).map(id => boxesReader.byId(id).get)

    val proverExtension = tx.inputs(inputIndex).spendingProof.extension

    ErgoLikeContext(blockchainState.currentHeight,
      blockchainState.lastBlockUtxoRoot,
      dummyPubkey,
      boxes,
      tx,
      boxes(inputIndex),
      proverExtension)
  }

  val noInputs = Array[Box]()
  val noOutputs = Array[Box]()

  def toTestData(value: Any, tpe: SType, isCost: Boolean)(implicit IR: Evaluation): Any = (value, tpe) match {
    case (arr: Array[a], SCollectionType(elemType)) =>
      elemType match {
        case SCollectionType(_) | STuple(_) =>
          val testArr = arr.map(x => toTestData(x, elemType, isCost))
          IR.sigmaDslBuilderValue.Cols.fromArray(testArr)
        case _ =>
          IR.sigmaDslBuilderValue.Cols.fromArray(arr)
      }
    case (arr: Array[a], STuple(items)) =>
      val res = arr.zip(items).map { case (x, t) => toTestData(x, t, isCost)}
      IR.sigmaDslBuilderValue.Cols.fromArray(res)
    case (b: ErgoBox, SBox) => b.toTestBox(isCost)
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
      if (ebox == null) return null
      val givenRegs = ebox.additionalRegisters ++ ErgoBox.mandatoryRegisters.map(id => (id, ebox.get(id).get))
      val rs = regs(givenRegs.map({ case (k, v) => k.number -> v }))
      new CostingBox(IR,
        IR.sigmaDslBuilderValue.Cols.fromArray(ebox.id),
        ebox.value,
        IR.sigmaDslBuilderValue.Cols.fromArray(ebox.bytes),
        IR.sigmaDslBuilderValue.Cols.fromArray(ebox.bytesWithNoRef),
        IR.sigmaDslBuilderValue.Cols.fromArray(ebox.propositionBytes),
        (ebox.creationHeight,
          IR.sigmaDslBuilderValue.Cols.fromArray(ebox.transactionId.toBytes ++ Shorts.toByteArray(ebox.index))),
        rs, isCost)
    }
  }
}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object MinerPubkey extends NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.MinerPubkeyCode

  override def cost[C <: Context](context: C): Long = Cost.ByteArrayDeclaration // todo: ???
  def opType = SFunc(SContext, SCollection.SByteArray)
}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object Height extends NotReadyValueLong {
  override val opCode: OpCode = OpCodes.HeightCode

  override def cost[C <: Context](context: C): Long = 2 * Cost.IntConstantDeclaration

  def opType = SFunc(SContext, SInt)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.boxesToSpend */
case object Inputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.InputsCode

  override def cost[C <: Context](context: C) =
    context.asInstanceOf[ErgoLikeContext].boxesToSpend.map(_.cost).sum + Cost.ConcreteCollectionDeclaration

  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.spendingTransaction.outputs */
case object Outputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.OutputsCode

  override def cost[C <: Context](context: C) =
    context.asInstanceOf[ErgoLikeContext].spendingTransaction.outputs.map(_.cost).sum + Cost.ConcreteCollectionDeclaration

  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a AvlTreeConstant built from Context.lastBlockUtxoRoot */
case object LastBlockUtxoRootHash extends NotReadyValueAvlTree {
  override val opCode: OpCode = OpCodes.LastBlockUtxoRootHashCode

  override def cost[C <: Context](context: C) = Cost.AvlTreeConstantDeclaration + 1
  def opType = SFunc(SContext, tpe)
}


/** When interpreted evaluates to a BoxConstant built from Context.self */
case object Self extends NotReadyValueBox {
  override val opCode: OpCode = OpCodes.SelfCode

  override def cost[C <: Context](context: C) = context.asInstanceOf[ErgoLikeContext].self.cost

  def opType = SFunc(SContext, SBox)
}
