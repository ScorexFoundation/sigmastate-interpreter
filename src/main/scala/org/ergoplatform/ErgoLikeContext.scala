package org.ergoplatform

import java.math.BigInteger

import org.bouncycastle.math.ec.ECPoint
import org.ergoplatform.ErgoLikeContext.Height
import scalan.RType
import scalan.RType.{TupleType, PairType}
import sigmastate.Values._
import sigmastate._
import sigmastate.eval._
import sigmastate.interpreter.{ContextExtension, Context}
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import special.collection.{Coll, CollType}
import special.sigma
import special.sigma.{AnyValue, TestValue, Box, WrapperType}
import RType._

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
  assert(self == null || boxesToSpend.exists(box => box.id == self.id), s"Self box if defined should be among boxesToSpend")
  override def withExtension(newExtension: ContextExtension): ErgoLikeContext =
    ErgoLikeContext(currentHeight, lastBlockUtxoRoot, minerPubkey, boxesToSpend, spendingTransaction, self, newExtension)

  def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoLikeContext =
    ErgoLikeContext(currentHeight, lastBlockUtxoRoot, minerPubkey, boxesToSpend, newSpendingTransaction, self, extension)

  import ErgoLikeContext._

  override def toSigmaContext(IR: Evaluation, isCost: Boolean): sigma.Context = {
    implicit val IRForBox: Evaluation = IR
    val inputs = boxesToSpend.toArray.map(_.toTestBox(isCost))
    val outputs =
      if (spendingTransaction == null) noOutputs
      else spendingTransaction.outputs.toArray.map(_.toTestBox(isCost))
    val vars = contextVars(extension.values)
    val avlTree = CostingAvlTree(lastBlockUtxoRoot)
    new CostingDataContext(IR, inputs, outputs, currentHeight, self.toTestBox(isCost), avlTree, minerPubkey, vars.toArray, isCost)
  }

}

object ErgoLikeContext {
  type Height = Int

  val dummyPubkey: Array[Byte] = Array.fill(32)(0: Byte)

  def apply(currentHeight: Height,
            lastBlockUtxoRoot: AvlTreeData,
            minerPubkey: Array[Byte],
            boxesToSpend: IndexedSeq[ErgoBox],
            spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
            self: ErgoBox,
            extension: ContextExtension = ContextExtension(Map())) =
    new ErgoLikeContext(currentHeight, lastBlockUtxoRoot, minerPubkey, boxesToSpend, spendingTransaction, self, extension)


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

  def fromEvalData(value: Any, tpe: SType)(implicit IR: Evaluation): Any = {
    val dsl = IR.sigmaDslBuilderValue
    value match {
      case w: WrapperOf[_] => w.wrappedValue
      case coll: Coll[a] =>
        val elemTpe = tpe.asCollection[SType].elemType
        coll.toArray.map(x => fromEvalData(x, elemTpe))
      case _ => value
    }
  }

  def toEvalData(value: Any, tpe: SType, isCost: Boolean)(implicit IR: Evaluation): Any = {
    val dsl = IR.sigmaDslBuilderValue
    (value, tpe) match {
      case (c: Constant[_], tpe) => toEvalData(c.value, c.tpe, isCost)
      case (_, STuple(Seq(tpeA, tpeB))) =>
        value match {
          case tup: Tuple2[_,_] =>
            val valA = toEvalData(tup._1, tpeA, isCost)
            val valB = toEvalData(tup._2, tpeB, isCost)
            (valA, valB)
          case arr: Array[Any] =>
            val valA = toEvalData(arr(0), tpeA, isCost)
            val valB = toEvalData(arr(1), tpeB, isCost)
            (valA, valB)
        }
      case (arr: Array[a], SCollectionType(elemType)) =>
        implicit val elemRType: RType[SType#WrappedType] = Evaluation.stypeToRType(elemType)
        elemRType.asInstanceOf[RType[_]] match {
          case _: CollType[_] | _: TupleType | _: PairType[_,_] | _: WrapperType[_] =>
            val testArr = arr.map(x => toEvalData(x, elemType, isCost))
            dsl.Colls.fromArray(testArr.asInstanceOf[Array[SType#WrappedType]])
          case _ =>
            dsl.Colls.fromArray(arr.asInstanceOf[Array[SType#WrappedType]])
        }
      case (arr: Array[a], STuple(items)) =>
        val res = arr.zip(items).map { case (x, t) => toEvalData(x, t, isCost)}
        dsl.Colls.fromArray(res)(RType.AnyType)
      case (b: ErgoBox, SBox) => b.toTestBox(isCost)
      case (n: BigInteger, SBigInt) =>
        dsl.BigInt(n)
      case (p: ECPoint, SGroupElement) => dsl.GroupElement(p)
      case (t: SigmaBoolean, SSigmaProp) => dsl.SigmaProp(t)
      case (t: AvlTreeData, SAvlTree) => CostingAvlTree(t)
      case (x, _) => x
    }
  }

  def contextVars(m: Map[Byte, Any])(implicit IR: Evaluation): Coll[AnyValue] = {
    val maxKey = if (m.keys.isEmpty) 0 else m.keys.max
    val res = new Array[AnyValue](maxKey + 1)
    for ((id, v) <- m) {
      assert(res(id) == null, s"register $id is defined more then once")
      res(id) = new TestValue(v)
    }
    IR.sigmaDslBuilderValue.Colls.fromArray(res)
  }

  implicit class ErgoBoxOps(val ebox: ErgoBox) extends AnyVal {
    def toTestBox(isCost: Boolean)(implicit IR: Evaluation): Box = {
      if (ebox == null) return null
      new CostingBox(IR, isCost, ebox)
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
