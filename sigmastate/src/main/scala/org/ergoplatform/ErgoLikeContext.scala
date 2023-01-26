package org.ergoplatform

import org.ergoplatform.validation.SigmaValidationSettings
import sigmastate.SType._
import sigmastate.Values._
import sigmastate._
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.ErgoTreeEvaluator.DataEnv
import sigmastate.interpreter.{Interpreter, InterpreterContext, ErgoTreeEvaluator, ContextExtension}
import sigmastate.lang.exceptions.InterpreterException
import sigmastate.serialization.OpCodes
import sigmastate.serialization.OpCodes.OpCode
import special.collection.Coll
import special.sigma
import special.sigma.{AnyValue, Header, PreHeader}
import debox.cfor

/** Represents a script evaluation context to be passed to a prover and a verifier to execute and
  * validate guarding proposition of input boxes of a transaction.
  *
  * @param selfIndex              - index of the box in `boxesToSpend` that contains the script we're evaluating
  * @param lastBlockUtxoRoot      - state root before current block application
  * @param headers                - fixed number of last block headers in descending order (first header is the newest one)
  * @param preHeader              - fields of block header with the current `spendingTransaction`, that can be predicted
  *                               by a miner before it's formation
  * @param dataBoxes              -  boxes, that corresponds to id's of `spendingTransaction.dataInputs`
  * @param boxesToSpend           - boxes, that corresponds to id's of `spendingTransaction.inputs`
  * @param spendingTransaction    - transaction that contains `self` box
  * @param extension              - prover-defined key-value pairs, that may be used inside a script
  * @param validationSettings     validation parameters passed to Interpreter.verify to detect soft-fork conditions
  * @param costLimit              hard limit on accumulated execution cost, if exceeded lead to CostLimitException to be thrown
  * @param initCost               initial value of execution cost already accumulated before Interpreter.verify is called
  * @param activatedScriptVersion Maximum version of ErgoTree currently activated on the network.
  *                               The activation is performed via miners voting.
  *                               For verification of *mined* blocks this parameter should  be passed according
  *                               to the latest voted (activated) script version on the network.
  *                               However this is not the case for *candidate* blocks.
  *                               When `activatedScriptVersion > Interpreter.MaxSupportedScriptVersion`
  *                               then the interpreter accept script without verification which is not
  *                               what should happen for *candidate* blocks.
  *                               This means Ergo node should always pass Interpreter.MaxSupportedScriptVersion
  *                               as a value of ErgoLikeContext.activatedScriptVersion during
  *                               verification of candidate blocks (which is a default).
  *                               The following values are used for current and upcoming forks:
  *                               - version 3.x this value must be 0
  *                               - in v4.0 must be 1
  *                               - in v5.x must be 2
  *                               etc.
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
                      val initCost: Long,
                      val activatedScriptVersion: Byte
                 ) extends InterpreterContext {
  // TODO lastBlockUtxoRoot should be calculated from headers if it is nonEmpty

  /* NOHF PROOF:
  Added: assert(preHeader != null)
  Motivation: to fail early, rather then on evaluation on `CONTEXT.preHeader` access
  Safety: According to ergo design PreHeader should always exist.
  Examined ergo code: all that leads to ErgoLikeContext creation.
  Fixed some cases in ergo where PreHeader might be null.
   */
  require(preHeader != null, "preHeader cannot be null")
  /* NOHF PROOF:
  Added: assert(spendingTransaction != null)
  Motivation: to fail early
  Safety: According to ergo design spendingTransaction should always exist.
  Examined ergo code: all that leads to ErgoLikeContext creation.
   */
  require(spendingTransaction != null, "spendingTransaction cannot be null")
  /* NOHF PROOF:
  Added: assert that box with `selfIndex` exist in boxesToSpend
  Motivation: to fail early, rather than when going into evaluation
  Safety: ergo itself uses index to identify the box
  Examined ergo code: all that leads to ErgoLikeContext creation.
 */
  require(boxesToSpend.isDefinedAt(selfIndex), s"Self box if defined should be among boxesToSpend")
  require(headers.toArray.headOption.forall(h => java.util.Arrays.equals(h.stateRoot.digest.toArray, lastBlockUtxoRoot.digest)), "Incorrect lastBlockUtxoRoot")
  cfor(0)(_ < headers.length, _ + 1) { i =>
    if (i > 0) require(headers(i - 1).parentId == headers(i).id, s"Incorrect chain: ${headers(i - 1).parentId},${headers(i).id}")
  }
  require(headers.toArray.headOption.forall(_.id == preHeader.parentId), s"preHeader.parentId should be id of the best header")
  /* NOHF PROOF:
  Added: assert that dataBoxes corresponds to spendingTransaction.dataInputs
  Motivation: to fail early, rather than when going into evaluation
  Safety: dataBoxes and spendingTransaction are supplied separately in ergo. No checks in ergo.
  Examined ergo code: all that leads to ErgoLikeContext creation.
 */
  require(spendingTransaction.dataInputs.length == dataBoxes.length &&
    spendingTransaction.dataInputs.forall(dataInput => dataBoxes.exists(b => java.util.Arrays.equals(b.id, dataInput.boxId))),
    "dataBoxes do not correspond to spendingTransaction.dataInputs")

  // TODO assert boxesToSpend correspond to spendingTransaction.inputs

  /* NOHF PROOF:
  Changed: make `self` a property returning box from `boxesToSpend`.
  Motivation: avoid DRY and avoid user error when trying to get the box from `boxesToSpend` supplying wrong index.
  Safety: index of the box and not the box itself are used internally in ergo.
  Examined ergo code: all that leads to ErgoLikeContext creation.
  */
  val self: ErgoBox = boxesToSpend(selfIndex)

  /** Current version of the ErgoTree executed by the interpreter.
    * This property is used to implement version dependent operations and passed to
    * interpreter via [[special.sigma.Context]].
    * The value cannot be assigned on [[ErgoLikeContext]] construction and must be
    * attached using [[withErgoTreeVersion()]] method.
    * When the value is None, the [[InterpreterException]] is thrown by the interpreter.
    */
  val currentErgoTreeVersion: Option[Byte] = None

  override def withErgoTreeVersion(newVersion: Byte): ErgoLikeContext =
    ErgoLikeContext.copy(this)(currErgoTreeVersion = Some(newVersion))

  override def withCostLimit(newCostLimit: Long): ErgoLikeContext =
    ErgoLikeContext.copy(this)(costLimit = newCostLimit)

  override def withInitCost(newInitCost: Long): ErgoLikeContext =
    ErgoLikeContext.copy(this)(initCost = newInitCost)

  override def withValidationSettings(newVs: SigmaValidationSettings): ErgoLikeContext =
    ErgoLikeContext.copy(this)(validationSettings = newVs)

  override def withExtension(newExtension: ContextExtension): ErgoLikeContext =
    ErgoLikeContext.copy(this)(extension = newExtension)

  def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoLikeContext =
    ErgoLikeContext.copy(this)(spendingTransaction = newSpendingTransaction)

  override def toSigmaContext(extensions: Map[Byte, AnyValue] = Map()): sigma.Context = {
    import Evaluation._

    def contextVars(m: Map[Byte, AnyValue]): Coll[AnyValue] = {
      val maxKey = if (m.keys.isEmpty) 0 else m.keys.max  // TODO optimize: max takes 90% of this method
      val res = new Array[AnyValue](maxKey + 1)
      for ((id, v) <- m) {
        res(id) = v
      }
      CostingSigmaDslBuilder.Colls.fromArray(res)
    }

    val dataInputs = this.dataBoxes.toArray.map(_.toTestBox).toColl
    val inputs = boxesToSpend.toArray.map(_.toTestBox).toColl
    /* NOHF PROOF:
    Changed: removed check for spendingTransaction == null
    Motivation: spendingTransaction cannot be null
    Safety: in ergo spendingTransaction cannot be null
    Examined ergo code: all that leads to ErgoLikeContext creation.
    */
    val outputs = spendingTransaction.outputs.toArray.map(_.toTestBox).toColl
    val varMap = extension.values.map { case (k, v: EvaluatedValue[_]) =>
      val tVal = stypeToRType[SType](v.tpe)
      k -> toAnyValue(v.value.asWrappedType)(tVal)
    }.toMap
    val vars = contextVars(varMap ++ extensions)
    val avlTree = CAvlTree(lastBlockUtxoRoot)
    // so selfBox is never one of the `inputs` instances
    // as result selfBoxIndex is always (erroneously) returns -1 in ErgoTree v0, v1
    val selfBox = boxesToSpend(selfIndex).toTestBox
    val ergoTreeVersion = currentErgoTreeVersion.getOrElse(
        Interpreter.error(s"Undefined context property: currentErgoTreeVersion"))
    CostingDataContext(
      dataInputs, headers, preHeader, inputs, outputs, preHeader.height, selfBox, selfIndex, avlTree,
      preHeader.minerPk.getEncoded, vars, activatedScriptVersion, ergoTreeVersion)
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
        initCost == that.initCost &&
        activatedScriptVersion == that.activatedScriptVersion
    case _ => false
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ErgoLikeContext]

  override def hashCode(): Int = {
    val state = Array(
      lastBlockUtxoRoot, headers, preHeader, dataBoxes, boxesToSpend, spendingTransaction,
      selfIndex, extension, validationSettings, costLimit, initCost,
      activatedScriptVersion)
    var hashCode = 0
    cfor(0)(_ < state.length, _ + 1) { i =>
      hashCode = 31 * hashCode + state(i).hashCode
    }
    hashCode
  }

  override def toString = s"ErgoLikeContext(lastBlockUtxoRoot=$lastBlockUtxoRoot, headers=$headers, preHeader=$preHeader, dataBoxes=$dataBoxes, boxesToSpend=$boxesToSpend, spendingTransaction=$spendingTransaction, selfIndex=$selfIndex, extension=$extension, validationSettings=$validationSettings, costLimit=$costLimit, initCost=$initCost, activatedScriptVersion=$activatedScriptVersion)"
}

object ErgoLikeContext {

  type Height = Int

  /** Maximimum number of headers in `headers` collection of the context. */
  val MaxHeaders = SigmaConstants.MaxHeaders.value

  /** Copies the given context allowing also to update fields.
    * NOTE: it can be used ONLY for instances of ErgoLikeContext.
    * @tparam T used here to limit use of this method to only ErgoLikeContext instances
    * @return a new instance of [[ErgoLikeContext]]. */
  @inline def copy[T >: ErgoLikeContext <: ErgoLikeContext](ctx: T)(
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
      activatedScriptVersion: Byte = ctx.activatedScriptVersion,
      currErgoTreeVersion: Option[Byte] = ctx.currentErgoTreeVersion): ErgoLikeContext = {
    new ErgoLikeContext(
      lastBlockUtxoRoot, headers, preHeader, dataBoxes, boxesToSpend,
      spendingTransaction, selfIndex, extension, validationSettings, costLimit, initCost,
      activatedScriptVersion) {
      override val currentErgoTreeVersion: Option[TypeCode] = currErgoTreeVersion
    }
  }
}

/** When interpreted evaluates to a ByteArrayConstant built from Context.minerPubkey */
case object MinerPubkey extends NotReadyValueByteArray with ValueCompanion {
  override def opCode: OpCode = OpCodes.MinerPubkeyCode
  /** Cost of calling Context.minerPubkey Scala method. */
  override val costKind = FixedCost(JitCost(20))
  override val opType = SFunc(SContext, SCollection.SByteArray)
  override def companion = this
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.minerPubKey
  }
}

/** When interpreted evaluates to a IntConstant built from Context.currentHeight */
case object Height extends NotReadyValueInt with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.HeightCode
  /** Cost of: 1) Calling Context.HEIGHT Scala method. */
  override val costKind = FixedCost(JitCost(26))
  override val opType = SFunc(SContext, SInt)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.HEIGHT
  }
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.boxesToSpend */
case object Inputs extends LazyCollection[SBox.type] with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.InputsCode
  /** Cost of: 1) Calling Context.INPUTS Scala method. */
  override val costKind = FixedCost(JitCost(10))
  override def tpe = SCollection.SBoxArray
  override val opType = SFunc(SContext, tpe)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.INPUTS
  }
}

/** When interpreted evaluates to a collection of BoxConstant built from Context.spendingTransaction.outputs */
case object Outputs extends LazyCollection[SBox.type] with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.OutputsCode
  /** Cost of: 1) Calling Context.OUTPUTS Scala method. */
  override val costKind = FixedCost(JitCost(10))
  override def tpe = SCollection.SBoxArray
  override val opType = SFunc(SContext, tpe)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.OUTPUTS
  }
}

/** When interpreted evaluates to a AvlTreeConstant built from Context.lastBlockUtxoRoot */
case object LastBlockUtxoRootHash extends NotReadyValueAvlTree with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.LastBlockUtxoRootHashCode

  /** Cost of: 1) Calling Context.LastBlockUtxoRootHash Scala method. */
  override val costKind = FixedCost(JitCost(15))

  override val opType = SFunc(SContext, tpe)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.LastBlockUtxoRootHash
  }
}


/** When interpreted evaluates to a BoxConstant built from context.boxesToSpend(context.selfIndex) */
case object Self extends NotReadyValueBox with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.SelfCode
  /** Cost of: 1) Calling Context.SELF Scala method. */
  override val costKind = FixedCost(JitCost(10))
  override val opType = SFunc(SContext, SBox)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context.SELF
  }
}

/** When interpreted evaluates to the singleton instance of [[special.sigma.Context]].
  * Corresponds to `CONTEXT` variable in ErgoScript which can be used like `CONTEXT.headers`.
  */
case object Context extends NotReadyValue[SContext.type] with ValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.ContextCode

  /** Cost of: 1) accessing global Context instance. */
  override val costKind = FixedCost(JitCost(1))

  override def tpe: SContext.type = SContext
  override val opType: SFunc = SFunc(SUnit, SContext)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    E.context
  }
}

/** When interpreted evaluates to the singleton instance of [[special.sigma.SigmaDslBuilder]].
  * Corresponds to `Global` variable in ErgoScript which can be used like `Global.groupGenerator`.
  */
case object Global extends NotReadyValue[SGlobal.type] with FixedCostValueCompanion {
  override def companion = this
  override def opCode: OpCode = OpCodes.GlobalCode
  /** Cost of: 1) accessing Global instance. */
  override val costKind = FixedCost(JitCost(5))
  override def tpe: SGlobal.type = SGlobal
  override val opType: SFunc = SFunc(SUnit, SGlobal)
  protected final override def eval(env: DataEnv)(implicit E: ErgoTreeEvaluator): Any = {
    addCost(this.costKind)
    CostingSigmaDslBuilder
  }
}
