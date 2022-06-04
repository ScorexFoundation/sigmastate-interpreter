package sigmastate.helpers

import scorex.crypto.hash.Digest32
import special.collection.{Coll, CollOverArray, PairOfCols}
import scorex.util.ModifierId
import org.ergoplatform.{ErgoLikeTransactionTemplate, ErgoLikeTransaction, ErgoLikeContext, UnsignedInput, Input, ErgoBox, DataInput, ErgoBoxCandidate}
import sigmastate.Values.ErgoTree
import org.ergoplatform.ErgoBox.{AdditionalRegisters, allZerosModifierId, TokenId}
import org.ergoplatform.validation.SigmaValidationSettings
import sigmastate.AvlTreeData
import sigmastate.eval.CostingSigmaDslBuilder
import sigmastate.eval._
import sigmastate.interpreter.ContextExtension
import special.sigma.{PreHeader, Header}

import scala.collection.compat.immutable.ArraySeq

// TODO refactor: unification is required between two hierarchies of tests
//  and as part of it, more methods can be moved to TestingHelpers

/** A collection of helper methods which can be used across test suites. */
object TestingHelpers {

  def testBox(value: Long,
              ergoTree: ErgoTree,
              creationHeight: Int,
              additionalTokens: Seq[(TokenId, Long)] = ArraySeq.empty,
              additionalRegisters: AdditionalRegisters = Map.empty,
              transactionId: ModifierId = allZerosModifierId,
              boxIndex: Short = 0): ErgoBox =
    new ErgoBox(value, ergoTree,
      CostingSigmaDslBuilder.Colls.fromArray(additionalTokens.toArray[(TokenId, Long)]),
      additionalRegisters,
      transactionId, boxIndex, creationHeight)

  def createBox(value: Long,
                proposition: ErgoTree,
                additionalTokens: Seq[(Digest32, Long)] = ArraySeq.empty,
                additionalRegisters: AdditionalRegisters = Map.empty)
  = testBox(value, proposition, 0, additionalTokens, additionalRegisters)

  /** Creates a new test box with the given parameters. */
  def createBox(value: Long,
                proposition: ErgoTree,
                creationHeight: Int)
  = testBox(value, proposition, creationHeight, ArraySeq.empty, Map.empty, ErgoBox.allZerosModifierId)

  /** Creates a clone instance of the given collection by recursively cloning all the underlying
    * sub-collections.
    */
  def cloneColl[A](c: Coll[A]): Coll[A] = (c match {
    case c: CollOverArray[_] =>
      new CollOverArray(c.toArray.clone(), SigmaDsl.Colls)(c.tItem)
    case ps: PairOfCols[_,_] =>
      new PairOfCols(cloneColl(ps.ls), cloneColl(ps.rs))
  }).asInstanceOf[Coll[A]]

  /** Copies the given box allowing also to update fields. */
  def copyBox(box: ErgoBox)(
      value: Long = box.value,
      ergoTree: ErgoTree = box.ergoTree,
      additionalTokens: Coll[(TokenId, Long)] = box.additionalTokens,
      additionalRegisters: AdditionalRegisters = box.additionalRegisters,
      transactionId: ModifierId = box.transactionId,
      index: Short = box.index,
      creationHeight: Int = box.creationHeight): ErgoBox = {
    new ErgoBox(value, ergoTree, additionalTokens, additionalRegisters, transactionId, index, creationHeight)
  }

  /** Copies the given transaction allowing also to update fields.
    * NOTE: it can be used ONLY for instances of ErgoLikeTransaction.
    * @tparam T used here to limit use of this method to only ErgoLikeTransaction instances
    * @return a new instance of [[ErgoLikeTransaction]]. */
  def copyTransaction[T >: ErgoLikeTransaction <: ErgoLikeTransaction](tx: T)(
      inputs: IndexedSeq[Input] = tx.inputs,
      dataInputs: IndexedSeq[DataInput] = tx.dataInputs,
      outputCandidates: IndexedSeq[ErgoBoxCandidate] = tx.outputCandidates) = {
    new ErgoLikeTransaction(inputs, dataInputs, outputCandidates)
  }

  /** Copies the given context allowing also to update fields. */
  def copyContext(ctx: ErgoLikeContext)(
      lastBlockUtxoRoot: AvlTreeData = ctx.lastBlockUtxoRoot,
      headers: Coll[Header] = ctx.headers,
      preHeader: PreHeader = ctx.preHeader,
      dataBoxes: IndexedSeq[ErgoBox] = ctx.dataBoxes,
      boxesToSpend: IndexedSeq[ErgoBox] = ctx.boxesToSpend,
      spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput] = ctx.spendingTransaction,
      selfIndex: Int = ctx.selfIndex,
      extension: ContextExtension = ctx.extension,
      validationSettings: SigmaValidationSettings = ctx.validationSettings,
      costLimit: Long = ctx.costLimit,
      initCost: Long = ctx.initCost,
      activatedScriptVersion: Byte = ctx.activatedScriptVersion): ErgoLikeContext = {
    new ErgoLikeContext(
      lastBlockUtxoRoot, headers, preHeader, dataBoxes, boxesToSpend,
      spendingTransaction, selfIndex, extension, validationSettings,
      costLimit, initCost, activatedScriptVersion)
  }

  /** Creates a new box by updating some of the additional registers with the given new bindings.
    * @param newBindings a map of the registers to be updated with new values
    */
  def updatedRegisters(box: ErgoBox, newBindings: AdditionalRegisters): ErgoBox = {
    copyBox(box)(additionalRegisters = box.additionalRegisters ++ newBindings)
  }

  /**
    * Create fake transaction with provided outputCandidates, but without inputs and data inputs.
    * Normally, this transaction will be invalid as far as it will break rule that sum of
    * coins in inputs should not be less then sum of coins in outputs, but we're not checking it
    * in our test cases
    */
  def createTransaction(outputCandidates: IndexedSeq[ErgoBoxCandidate]): ErgoLikeTransaction = {
    new ErgoLikeTransaction(ArraySeq.empty, ArraySeq.empty, outputCandidates)
  }

  def createTransaction(box: ErgoBoxCandidate): ErgoLikeTransaction = createTransaction(Array(box))

  def createTransaction(dataInputs: IndexedSeq[ErgoBox],
                        outputCandidates: IndexedSeq[ErgoBoxCandidate]): ErgoLikeTransaction =
    new ErgoLikeTransaction(ArraySeq.empty, dataInputs.map(b => DataInput(b.id)), outputCandidates)

}
