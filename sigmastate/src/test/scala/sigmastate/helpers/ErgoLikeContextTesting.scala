package sigmastate.helpers

import org.ergoplatform.SigmaConstants.ScriptCostLimit
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform._
import org.ergoplatform.validation.{ValidationRules, SigmaValidationSettings}
import sigmastate.AvlTreeData
import sigmastate.eval._
import sigmastate.interpreter.{ContextExtension, CryptoConstants, Interpreter}
import sigmastate.serialization.{SigmaSerializer, GroupElementSerializer}
import special.collection.Coll
import special.sigma.{Box, PreHeader, Header}

import scala.util.Try

object ErgoLikeContextTesting {
  /* NO HF PROOF:
  Changed: val dummyPubkey from `Array[Byte] = Array.fill(32)(0: Byte)` to `GroupElementSerializer.toBytes(CryptoConstants.dlogGroup.generator)`
  Motivation: to avoid exception on deserialization(wrong size, needs to be 33 bytes) and later in GroupElement.toString (infinity was not handled) and to provide more practical value in tests.
  Safety:
  Used only in tests and not used in ergo.
  Examined ergo code: all (with IDE's "find usages" action).
*/
  val dummyPubkey: Array[Byte] = GroupElementSerializer.toBytes(CryptoConstants.dlogGroup.generator)

  val noBoxes: IndexedSeq[ErgoBox] = IndexedSeq.empty[ErgoBox]
  val noHeaders: Coll[Header] = CostingSigmaDslBuilder.Colls.emptyColl[Header]

  def dummyPreHeader(currentHeight: Height, minerPk: Array[Byte]): PreHeader = CPreHeader(0,
    parentId = Colls.emptyColl[Byte],
    timestamp = 3,
    nBits = 0,
    height = currentHeight,
    minerPk = GroupElementSerializer.parse(SigmaSerializer.startReader(minerPk)),
    votes = Colls.emptyColl[Byte]
  )

  def apply(currentHeight: Height,
            lastBlockUtxoRoot: AvlTreeData,
            minerPubkey: Array[Byte],
            boxesToSpend: IndexedSeq[ErgoBox],
            spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
            self: ErgoBox,
            activatedVersion: Byte,
            extension: ContextExtension = ContextExtension.empty,
            vs: SigmaValidationSettings = ValidationRules.currentSettings): ErgoLikeContext =
    new ErgoLikeContext(
      lastBlockUtxoRoot, noHeaders, dummyPreHeader(currentHeight, minerPubkey), noBoxes,
      boxesToSpend, spendingTransaction, boxesToSpend.indexOf(self), extension, vs,
      ScriptCostLimit.value, initCost = 0L, activatedVersion)

  def apply(currentHeight: Height,
            lastBlockUtxoRoot: AvlTreeData,
            minerPubkey: Array[Byte],
            dataBoxes: IndexedSeq[ErgoBox],
            boxesToSpend: IndexedSeq[ErgoBox],
            spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
            selfIndex: Int,
            activatedVersion: Byte) =
    new ErgoLikeContext(
      lastBlockUtxoRoot, noHeaders, dummyPreHeader(currentHeight, minerPubkey),
      dataBoxes, boxesToSpend, spendingTransaction, selfIndex, ContextExtension.empty,
      ValidationRules.currentSettings, ScriptCostLimit.value,
      initCost = 0L, activatedVersion)


  def dummy(selfDesc: ErgoBox, activatedVersion: Byte): ErgoLikeContext =
    ErgoLikeContextTesting(currentHeight = 0,
      lastBlockUtxoRoot = AvlTreeData.dummy, dummyPubkey, boxesToSpend = IndexedSeq(selfDesc),
      spendingTransaction = ErgoLikeTransaction(IndexedSeq(), IndexedSeq()), self = selfDesc,
      activatedVersion = activatedVersion)

  val noInputs: Array[Box] = Array[Box]()
  val noOutputs: Array[Box] = Array[Box]()
}

case class BlockchainState(currentHeight: Height, lastBlockUtxoRoot: AvlTreeData)

