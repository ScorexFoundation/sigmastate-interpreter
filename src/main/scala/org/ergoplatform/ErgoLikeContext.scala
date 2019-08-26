package org.ergoplatform

import java.util

import org.ergoplatform.validation.SigmaValidationSettings
import sigmastate.SType._
import sigmastate.Values._
import sigmastate._
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.{ContextExtension, InterpreterContext}
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import special.collection.Coll
import special.sigma
import special.sigma.{AnyValue, Header, PreHeader}
import spire.syntax.all.cfor

/**
  * TODO lastBlockUtxoRoot should be calculated from headers if it is nonEmpty
  *
  * @param selfIndex           - index of the box in `boxesToSpend` that contains the script we're evaluating
  * @param lastBlockUtxoRoot   - state root before current block application
  * @param headers             - fixed number of last block headers in descending order (first header is the newest one)
  * @param preHeader           - fields of block header with the current `spendingTransaction`, that can be predicted
  *                            by a miner before it's formation
  * @param dataBoxes           -  boxes, that corresponds to id's of `spendingTransaction.dataInputs`
  * @param boxesToSpend        - boxes, that corresponds to id's of `spendingTransaction.inputs`
  * @param spendingTransaction - transaction that contains `self` box
  * @param extension           - prover-defined key-value pairs, that may be used inside a script
  * @param validationSettings  validataion parameters passed to Interpreter.verify to detect soft-fork conditions
  * @param costLimit           hard limit on accumulated execution cost, if exceeded lead to CostLimitException to be thrown
  * @param initCost            initial value of execution cost already accumulated before Interpreter.verify is called
  */
class ErgoLikeContext(val lastBlockUtxoRoot: AvlTreeData,
                      val headers: Coll[Header],
                      val preHeader: PreHeader,
                      val dataBoxes: IndexedSeq[ErgoBox],
                      val boxesToSpend: IndexedSeq[ErgoBox],
                      val spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                      val selfIndex: Int,
                      val extension: ContextExtension,
                      val validationSettings: SigmaValidationSettings,
                      val costLimit: Long,
                      val initCost: Long
                 ) extends InterpreterContext {

  /* NOHF PROOF:
  Added: assert(preHeader != null)
  Motivation: to fail early, rather then on evaluation on `CONTEXT.preHeader` access
  Safety: According to ergo design PreHeader should always exist.
  Examined ergo code: all that leads to ErgoLikeContext creation.
  Fixed some cases in ergo where PreHeader might be null.
   */
  assert(preHeader != null, "preHeader cannot be null")
  /* NOHF PROOF:
  Added: assert(spendingTransaction != null)
  Motivation: to fail early
  Safety: According to ergo design spendingTransaction should always exist.
  Examined ergo code: all that leads to ErgoLikeContext creation.
   */
  assert(spendingTransaction != null, "spendingTransaction cannot be null")
  /* NOHF PROOF:
  Added: assert that box with `selfIndex` exist in boxesToSpend
  Motivation: to fail early, rather than when going into evaluation
  Safety: ergo itself uses index to identify the box
  Examined ergo code: all that leads to ErgoLikeContext creation.
 */
  assert(boxesToSpend.isDefinedAt(selfIndex), s"Self box if defined should be among boxesToSpend")
  assert(headers.toArray.headOption.forall(h => java.util.Arrays.equals(h.stateRoot.digest.toArray, lastBlockUtxoRoot.digest)), "Incorrect lastBlockUtxoRoot")
  cfor(0)(_ < headers.length, _ + 1) { i =>
    if (i > 0) assert(headers(i - 1).parentId == headers(i).id, s"Incorrect chain: ${headers(i - 1).parentId},${headers(i).id}")
  }
  assert(headers.toArray.headOption.forall(_.id == preHeader.parentId), s"preHeader.parentId should be id of the best header")
  /* NOHF PROOF:
  Added: assert that dataBoxes corresponds to spendingTransaction.dataInputs
  Motivation: to fail early, rather than when going into evaluation
  Safety: dataBoxes and spendingTransaction are supplied separately in ergo. No checks in ergo.
  Examined ergo code: all that leads to ErgoLikeContext creation.
 */
  assert(spendingTransaction.dataInputs.length == dataBoxes.length &&
    spendingTransaction.dataInputs.forall(dataInput => dataBoxes.exists(b => util.Arrays.equals(b.id, dataInput.boxId))),
    "dataBoxes do not correspond to spendingTransaction.dataInputs")

  // TODO assert boxesToSpend correspond to spendingTransaction.inputs

  /* NOHF PROOF:
  Changed: make `self` a property returning box from `boxesToSpend`.
  Motivation: avoid DRY and avoid user error when trying to get the box from `boxesToSpend` supplying wrong index.
  Safety: index of the box and not the box itself are used internally in ergo.
  Examined ergo code: all that leads to ErgoLikeContext creation.
  */
  val self: ErgoBox = boxesToSpend(selfIndex)

  override def withCostLimit(newCostLimit: Long): ErgoLikeContext =
    new ErgoLikeContext(
      lastBlockUtxoRoot, headers, preHeader,
      dataBoxes, boxesToSpend, spendingTransaction, selfIndex, extension, validationSettings, newCostLimit, initCost)

  override def withInitCost(newCost: Long): ErgoLikeContext =
    new ErgoLikeContext(
      lastBlockUtxoRoot, headers, preHeader,
      dataBoxes, boxesToSpend, spendingTransaction, selfIndex, extension, validationSettings, costLimit, newCost)

  override def withValidationSettings(newVs: SigmaValidationSettings): ErgoLikeContext =
    new ErgoLikeContext(
      lastBlockUtxoRoot, headers, preHeader,
      dataBoxes, boxesToSpend, spendingTransaction, selfIndex, extension, newVs, costLimit, initCost)

  override def withExtension(newExtension: ContextExtension): ErgoLikeContext =
    new ErgoLikeContext(
      lastBlockUtxoRoot, headers, preHeader,
      dataBoxes, boxesToSpend, spendingTransaction, selfIndex, newExtension, validationSettings, costLimit, initCost)

  def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoLikeContext =
    new ErgoLikeContext(
      lastBlockUtxoRoot, headers, preHeader,
      dataBoxes, boxesToSpend, newSpendingTransaction, selfIndex, extension, validationSettings, costLimit, initCost)


  override def toSigmaContext(IR: Evaluation, isCost: Boolean, extensions: Map[Byte, AnyValue] = Map()): sigma.Context = {
    implicit val IRForBox: Evaluation = IR
    import Evaluation._

    def contextVars(m: Map[Byte, AnyValue])(implicit IR: Evaluation): Coll[AnyValue] = {
      val maxKey = if (m.keys.isEmpty) 0 else m.keys.max
      val res = new Array[AnyValue](maxKey + 1)
      for ((id, v) <- m) {
        res(id) = v
      }
      IR.sigmaDslBuilderValue.Colls.fromArray(res)
    }

    val dataInputs = this.dataBoxes.toArray.map(_.toTestBox(isCost)).toColl
    val inputs = boxesToSpend.toArray.map(_.toTestBox(isCost)).toColl
    /* NOHF PROOF:
    Changed: removed check for spendingTransaction == null
    Motivation: spendingTransaction cannot be null
    Safety: in ergo spendingTransaction cannot be null
    Examined ergo code: all that leads to ErgoLikeContext creation.
    */
    val outputs = spendingTransaction.outputs.toArray.map(_.toTestBox(isCost)).toColl
    val varMap = extension.values.mapValues { case v: EvaluatedValue[_] =>
      val tVal = stypeToRType[SType](v.tpe)
      toAnyValue(v.value.asWrappedType)(tVal)
    }
    val vars = contextVars(varMap ++ extensions)
    val avlTree = CAvlTree(lastBlockUtxoRoot)
    CostingDataContext(
      dataInputs, headers, preHeader, inputs, outputs, preHeader.height, boxesToSpend(selfIndex).toTestBox(isCost), avlTree,
      preHeader.minerPk.getEncoded, vars, isCost)
  }


  override def equals(other: Any): Boolean = other match {
    case that: ErgoLikeContext =>
      (that canEqual this) &&
        lastBlockUtxoRoot == that.lastBlockUtxoRoot &&
        headers == that.headers &&
        preHeader == that.preHeader &&
        dataBoxes == that.dataBoxes &&
        boxesToSpend == that.boxesToSpend &&
        spendingTransaction == that.spendingTransaction &&
        selfIndex == that.selfIndex &&
        extension == that.extension &&
        validationSettings == that.validationSettings &&
        costLimit == that.costLimit &&
        initCost == that.initCost
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ErgoLikeContext]

  override def hashCode(): Int = {
    val state = Array(lastBlockUtxoRoot, headers, preHeader, dataBoxes, boxesToSpend, spendingTransaction, selfIndex, extension, validationSettings, costLimit, initCost)
    var hashCode = 0
    cfor(0)(_ < state.length, _ + 1) { i =>
      hashCode = 31 * hashCode + state(i).hashCode
    }
    hashCode
  }

  override def toString = s"ErgoLikeContext(lastBlockUtxoRoot=$lastBlockUtxoRoot, headers=$headers, preHeader=$preHeader, dataBoxes=$dataBoxes, boxesToSpend=$boxesToSpend, spendingTransaction=$spendingTransaction, selfIndex=$selfIndex, extension=$extension, validationSettings=$validationSettings, costLimit=$costLimit, initCost=$initCost)"
}

object ErgoLikeContext {

  type Height = Int

  /** Maximimum number of headers in `headers` collection of the context. */
  val MaxHeaders = SigmaConstants.MaxHeaders.value
}

/** When interpreted evaluates to a ByteArrayConstant built from Context.minerPubkey */
case object MinerPubkey extends NotReadyValueByteArray with ValueCompanion {
  override def opCode: OpCode = OpCodes.MinerPubkeyCode
  def opType = SFunc(SContext, SCollection.SByteArray)
  override def companion = this
}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object Height extends NotReadyValueInt with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.HeightCode
  def opType = SFunc(SContext, SInt)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.boxesToSpend */
case object Inputs extends LazyCollection[SBox.type] with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.InputsCode
  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.spendingTransaction.outputs */
case object Outputs extends LazyCollection[SBox.type] with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.OutputsCode
  val tpe = SCollection(SBox)
  def opType = SFunc(SContext, tpe)
}

/** When interpreted evaluates to a AvlTreeConstant built from Context.lastBlockUtxoRoot */
case object LastBlockUtxoRootHash extends NotReadyValueAvlTree with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.LastBlockUtxoRootHashCode
  def opType = SFunc(SContext, tpe)
}


/** When interpreted evaluates to a BoxConstant built from context.boxesToSpend(context.selfIndex) */
case object Self extends NotReadyValueBox with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.SelfCode
  def opType = SFunc(SContext, SBox)
}

case object Context extends NotReadyValue[SContext.type] with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.ContextCode
  override def tpe: SContext.type = SContext
  override def opType: SFunc = SFunc(SUnit, SContext)
}

case object Global extends NotReadyValue[SGlobal.type] with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.GlobalCode
  override def tpe: SGlobal.type = SGlobal
  override def opType: SFunc = SFunc(SUnit, SGlobal)
}
