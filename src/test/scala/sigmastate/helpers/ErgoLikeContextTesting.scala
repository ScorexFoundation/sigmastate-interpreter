package sigmastate.helpers

import org.ergoplatform.ErgoConstants.ScriptCostLimit
import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.{ErgoBox, ErgoBoxReader, ErgoLikeContext, ErgoLikeTransaction, ErgoLikeTransactionTemplate, UnsignedInput}
import org.ergoplatform.validation.{SigmaValidationSettings, ValidationRules}
import sigmastate.AvlTreeData
import sigmastate.eval._
import sigmastate.interpreter.{ContextExtension, CryptoConstants}
import sigmastate.serialization.{GroupElementSerializer, SigmaSerializer}
import special.collection.Coll
import special.sigma.{Box, Header, PreHeader}

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
            extension: ContextExtension = ContextExtension.empty,
            vs: SigmaValidationSettings = ValidationRules.currentSettings) =
    new ErgoLikeContext(lastBlockUtxoRoot, noHeaders, dummyPreHeader(currentHeight, minerPubkey), noBoxes,
      boxesToSpend, spendingTransaction, boxesToSpend.indexOf(self), extension, vs, ScriptCostLimit.value, 0L)

  def apply(currentHeight: Height,
            lastBlockUtxoRoot: AvlTreeData,
            minerPubkey: Array[Byte],
            dataBoxes: IndexedSeq[ErgoBox],
            boxesToSpend: IndexedSeq[ErgoBox],
            spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
            selfIndex: Int) =
    new ErgoLikeContext(lastBlockUtxoRoot, noHeaders, dummyPreHeader(currentHeight, minerPubkey),
      dataBoxes, boxesToSpend, spendingTransaction, selfIndex, ContextExtension.empty, ValidationRules.currentSettings, ScriptCostLimit.value, 0L)


  def dummy(selfDesc: ErgoBox) = ErgoLikeContextTesting(currentHeight = 0,
    lastBlockUtxoRoot = AvlTreeData.dummy, dummyPubkey, boxesToSpend = IndexedSeq(selfDesc),
    spendingTransaction = ErgoLikeTransaction(IndexedSeq(), IndexedSeq()), self = selfDesc)

  def fromTransaction(tx: ErgoLikeTransaction,
                      blockchainState: BlockchainState,
                      boxesReader: ErgoBoxReader,
                      inputIndex: Int): Try[ErgoLikeContext] = Try {

    val boxes = tx.inputs.map(_.boxId).map(id => boxesReader.byId(id).get)

    val proverExtension = tx.inputs(inputIndex).spendingProof.extension

    ErgoLikeContextTesting(blockchainState.currentHeight,
      blockchainState.lastBlockUtxoRoot,
      dummyPubkey,
      boxes,
      tx,
      boxes(inputIndex),
      proverExtension)
  }

  val noInputs: Array[Box] = Array[Box]()
  val noOutputs: Array[Box] = Array[Box]()
}

case class BlockchainState(currentHeight: Height, lastBlockUtxoRoot: AvlTreeData)

