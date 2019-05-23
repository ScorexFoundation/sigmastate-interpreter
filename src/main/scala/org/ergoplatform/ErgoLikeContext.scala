package org.ergoplatform

import org.ergoplatform.ErgoLikeContext.Height
import scalan.RType
import sigmastate.Values._
import sigmastate._
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigmastate.interpreter.{ContextExtension, InterpreterContext}
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import special.collection.Coll
import special.sigma
import special.sigma.{AnyValue, Box, PreHeader, Header}
import sigmastate.SType._
import scalan.RType._
import sigmastate.constants.SigmaConstants
import special.sigma.{Header, Box, AnyValue, PreHeader}
import SType._
import RType._
import special.sigma.Extensions._

import scala.util.Try

case class BlockchainState(currentHeight: Height, lastBlockUtxoRoot: AvlTreeData)

/**
  * TODO currentHeight and minerPubkey should be calculated from PreHeader
  * TODO lastBlockUtxoRoot should be calculated from headers if it is nonEmpty
  *
  * @param self - box that contains the script we're evaluating
  * @param currentHeight - height of a block with the current `spendingTransaction`
  * @param lastBlockUtxoRoot - state root before current block application
  * @param minerPubkey - public key of a miner of the block with the current `spendingTransaction`
  * @param headers - fixed number of last block headers in descending order (first header is the newest one)
  * @param preHeader - fields of block header with the current `spendingTransaction`, that can be predicted
  *                  by a miner before it's formation
  * @param dataBoxes -  boxes, that corresponds to id's of `spendingTransaction.dataInputs`
  * @param boxesToSpend - boxes, that corresponds to id's of `spendingTransaction.inputs`
  * @param spendingTransaction - transaction that contains `self` box
  * @param extension - prover-defined key-value pairs, that may be used inside a script
  */
class ErgoLikeContext(val currentHeight: Height,
                      val lastBlockUtxoRoot: AvlTreeData,
                      val minerPubkey: Array[Byte],
                      val headers: Coll[Header],
                      val preHeader: PreHeader,
                      val dataBoxes: IndexedSeq[ErgoBox],
                      val boxesToSpend: IndexedSeq[ErgoBox],
                      val spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                      val self: ErgoBox,
                      override val extension: ContextExtension = ContextExtension(Map()),
                      val validationSettings: ValidationSettings = ValidationRules.currentSettings
                 ) extends InterpreterContext {

  assert(self == null || boxesToSpend.exists(box => box.id == self.id), s"Self box if defined should be among boxesToSpend")
  assert(preHeader == null || preHeader.height == currentHeight, "Incorrect preHeader height")
  assert(preHeader == null || java.util.Arrays.equals(minerPubkey, preHeader.minerPk.getEncoded.toArray), "Incorrect preHeader minerPubkey")
  assert(headers.toArray.headOption.forall(h => java.util.Arrays.equals(h.stateRoot.digest.toArray, lastBlockUtxoRoot.digest)), "Incorrect lastBlockUtxoRoot")
  headers.toArray.indices.foreach { i =>
    if (i > 0) assert(headers(i - 1).parentId == headers(i).id, s"Incorrect chain: ${headers(i - 1).parentId},${headers(i).id}")
  }
  assert(preHeader == null || headers.toArray.headOption.forall(_.id == preHeader.parentId), s"preHeader.parentId should be id of the best header")

  checkCorrectness()
  checkExtensionValidity(extension.values)

  override def withExtension(newExtension: ContextExtension): ErgoLikeContext =
    new ErgoLikeContext(
      currentHeight, lastBlockUtxoRoot, minerPubkey, headers, preHeader,
      dataBoxes, boxesToSpend, spendingTransaction, self, newExtension, validationSettings)

  def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoLikeContext =
    new ErgoLikeContext(
      currentHeight, lastBlockUtxoRoot, minerPubkey, headers, preHeader,
      dataBoxes, boxesToSpend, newSpendingTransaction, self, extension, validationSettings)

  import ErgoLikeContext._
  import Evaluation._

  override def toSigmaContext(IR: Evaluation, isCost: Boolean, extensions: Map[Byte, AnyValue] = Map()): sigma.Context = {
    implicit val IRForBox: Evaluation = IR
    val dataInputs = this.dataBoxes.toArray.map(_.toTestBox(isCost)).toColl
    val inputs = boxesToSpend.toArray.map(_.toTestBox(isCost)).toColl
    val outputs = if (spendingTransaction == null)
        noOutputs.toColl
      else
        spendingTransaction.outputs.toArray.map(_.toTestBox(isCost)).toColl
    val varMap = extension.values.mapValues { case v: EvaluatedValue[_] =>
      val tVal = stypeToRType[SType](v.tpe)
      toAnyValue(v.value.asWrappedType)(tVal)
    }
    val vars = contextVars(varMap ++ extensions)
    val avlTree = CAvlTree(lastBlockUtxoRoot)
    new CostingDataContext(
      dataInputs, headers, preHeader, inputs, outputs, currentHeight, self.toTestBox(isCost), avlTree,
      minerPubkey.toColl,
      vars,
      isCost)
  }

  def checkCorrectness(): Boolean = {
    if (currentHeight < 0)
      return false
    if (headers.length > SigmaConstants.MaxHeaders.value)
      return false
    for (box <- boxesToSpend) if (box.dataSize > SigmaConstants.MaxBoxSize.value) return false
    for (box <- dataBoxes) if (box.dataSize > SigmaConstants.MaxBoxSize.value) return false

    true
  }

  def checkExtensionValidity(extension: Map[Byte, AnyValue]): Boolean = {
    for ((id, item) <- extension) {
      item match {
        case (cl: ConcreteCollection[t]) =>
          if (cl.bytes.length > SigmaConstants.MaxCollectionSize.value)
            return false
        case (tup: Tuple) =>
          if (tup.asTuple.length > SigmaConstants.MaxTupleLength.value)
            return false
        case _ => {
          println(item)
          return false
        }
      }
    }
    true
  }

}

object ErgoLikeContext {
  type Height = Int

  val dummyPubkey: Array[Byte] = Array.fill(32)(0: Byte)

  val noBoxes = IndexedSeq.empty[ErgoBox]
  val noHeaders = CostingSigmaDslBuilder.Colls.emptyColl[Header]
  val dummyPreHeader: PreHeader = null

  /** Maximimum number of headers in `headers` collection of the context. */
  val MaxHeaders = SigmaConstants.MaxHeaders.value

  def apply(currentHeight: Height,
            lastBlockUtxoRoot: AvlTreeData,
            minerPubkey: Array[Byte],
            boxesToSpend: IndexedSeq[ErgoBox],
            spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
            self: ErgoBox,
            extension: ContextExtension = ContextExtension(Map()),
            vs: ValidationSettings = ValidationRules.currentSettings) =
    new ErgoLikeContext(currentHeight, lastBlockUtxoRoot, minerPubkey,
      noHeaders,
      dummyPreHeader,
      noBoxes,
      boxesToSpend, spendingTransaction, self, extension, vs)

  def apply(currentHeight: Height,
            lastBlockUtxoRoot: AvlTreeData,
            minerPubkey: Array[Byte],
            dataBoxes: IndexedSeq[ErgoBox],
            boxesToSpend: IndexedSeq[ErgoBox],
            spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
            self: ErgoBox) =
    new ErgoLikeContext(currentHeight, lastBlockUtxoRoot, minerPubkey,
      noHeaders,
      dummyPreHeader,
      dataBoxes, boxesToSpend, spendingTransaction, self, ContextExtension(Map()))


  def dummy(selfDesc: ErgoBox) = ErgoLikeContext(currentHeight = 0,
    lastBlockUtxoRoot = AvlTreeData.dummy, dummyPubkey, boxesToSpend = IndexedSeq(selfDesc),
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

  val noInputs: Array[Box] = Array[Box]()
  val noOutputs: Array[Box] = Array[Box]()

  import special.sigma._

  def contextVars(m: Map[Byte, AnyValue])(implicit IR: Evaluation): Coll[AnyValue] = {
    val maxKey = if (m.keys.isEmpty) 0 else m.keys.max
    val res = new Array[AnyValue](maxKey + 1)
    for ((id, v) <- m) {
      assert(res(id) == null, s"register $id is defined more then once")
      res(id) = v
    }
    IR.sigmaDslBuilderValue.Colls.fromArray(res)
  }

  implicit class ErgoBoxOps(val ebox: ErgoBox) extends AnyVal {
    def toTestBox(isCost: Boolean): Box = {
      if (ebox == null) return null
      new CostingBox(isCost, ebox)
    }
  }
}

/** When interpreted evaluates to a ByteArrayConstant built from Context.minerPubkey */
case object MinerPubkey extends NotReadyValueByteArray {
  override val opCode: OpCode = OpCodes.MinerPubkeyCode
  def opType = SFunc(SContext, SCollection.SByteArray)
}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object Height extends NotReadyValueInt {
  override val opCode: OpCode = OpCodes.HeightCode
  def opType = SFunc(SContext, SInt)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.boxesToSpend */
case object Inputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.InputsCode
  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.spendingTransaction.outputs */
case object Outputs extends LazyCollection[SBox.type] {
  override val opCode: OpCode = OpCodes.OutputsCode
  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a AvlTreeConstant built from Context.lastBlockUtxoRoot */
case object LastBlockUtxoRootHash extends NotReadyValueAvlTree {
  override val opCode: OpCode = OpCodes.LastBlockUtxoRootHashCode
  def opType = SFunc(SContext, tpe)
}


/** When interpreted evaluates to a BoxConstant built from Context.self */
case object Self extends NotReadyValueBox {
  override val opCode: OpCode = OpCodes.SelfCode
  def opType = SFunc(SContext, SBox)
}

case object Context extends NotReadyValue[SContext.type] {
  override val opCode: OpCode = OpCodes.ContextCode
  override def tpe: SContext.type = SContext
  override def opType: SFunc = SFunc(SUnit, SContext)
}

case object Global extends NotReadyValue[SGlobal.type] {
  override val opCode: OpCode = OpCodes.GlobalCode
  override def tpe: SGlobal.type = SGlobal
  override def opType: SFunc = SFunc(SUnit, SGlobal)
}
