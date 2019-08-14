package sigmastate.verified

import sigmastate.verified.CrowdFundingContractVerification.SigmaProp
import stainless.annotation._
import stainless.collection._
import stainless.lang._
import stainless.math._
import stainless.proof._

trait Box {
  def value: Long
  def id: List[Byte]
  def R4[T]: Option[T]
  def R5[T]: Option[T]
  def tokens: List[(List[Byte], Long)]
  def propositionBytes: List[Byte]
}

trait AvlTree {
  def digest: List[Byte]
  def keyLength: Int
  def valueLengthOpt: Option[Int]
  def enabledOperations: Byte
  def getMany(keys: List[List[Byte]], proof: List[Byte]): List[Option[List[Byte]]]
  def insert(toAdd: List[(List[Byte], List[Byte])], proof: List[Byte]): Option[AvlTree]
  def remove(operations: List[List[Byte]], proof: List[Byte]): Option[AvlTree]
}

// TODO remove after getOrElse are removed
case object AvlTree extends AvlTree {
  def apply(): AvlTree = ???
  override def digest: List[Byte] = ???
  override def keyLength: Int = ???
  override def valueLengthOpt: Option[Int] = ???
  override def enabledOperations: Byte = ???
  override def getMany(keys: List[List[Byte]], proof: List[Byte]): List[Option[List[Byte]]] = ???
  override def insert(toAdd: List[(List[Byte], List[Byte])], proof: List[Byte]): Option[AvlTree] = ???
  override def remove(operations: List[List[Byte]], proof: List[Byte]): Option[AvlTree] = ???
}

trait Context {
  def OUTPUTS: List[Box]
  def INPUTS: List[Box]
  def SELF: Box
  def HEIGHT: Int

  def getVar[T](id: Byte): Option[T]
  def longToByteArray(l: Long): List[Byte]
  def byteArrayToLong(bytes: List[Byte]): Long
  def blake2b256(bytes: List[Byte]): List[Byte]
}

trait FundingContext extends Context {
  def nextStageScriptHash: List[Byte]
  def feeBytes: List[Byte]
}

trait IssuanceContext extends Context {
  def nextStageScriptHash: List[Byte]
  def projectPubKey: SigmaProp
}

trait WithdrawalContext extends Context {
}

trait ICOContractVerification {

  def ICOFundingContract(ctx: FundingContext): Boolean = {
    import ctx._

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty)

    val selfIndexIsZero = INPUTS(0).id == SELF.id

    // TODO: proper option handling
    val proof = getVar[List[Byte]](1).getOrElse(List())

    val inputsCount = INPUTS.size

    val toAdd: List[(List[Byte], List[Byte])] = INPUTS.slice(1, inputsCount).map({ (b: Box) =>
      // TODO: proper option handling
      val pk = b.R4[List[Byte]].getOrElse(List())
      val value = longToByteArray(b.value)
      (pk, value)
    })

    // TODO: proper option handling
    val modifiedTree = SELF.R5[AvlTree].getOrElse(AvlTree()).insert(toAdd, proof).getOrElse(AvlTree())

    // TODO: proper option handling
    val expectedTree = OUTPUTS(0).R5[AvlTree].getOrElse(AvlTree())

    val properTreeModification = modifiedTree == expectedTree

    val outputsCount = OUTPUTS.size == 2

    val selfOutputCorrect = if (HEIGHT < 2000) {
      OUTPUTS(0).propositionBytes == SELF.propositionBytes
    } else {
      blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash
    }

    val feeOutputCorrect = outputsCount &&
      (OUTPUTS(1).value <= 1) &&
      (OUTPUTS(1).propositionBytes == feeBytes)

    val outputsCorrect = outputsCount && feeOutputCorrect && selfOutputCorrect

    selfIndexIsZero && outputsCorrect && properTreeModification
  }

  def ICOIssuanceContract(ctx: IssuanceContext): Boolean = {
    import ctx._

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty)

    // TODO: proper option handling
    val openTree = SELF.R5[AvlTree].getOrElse(AvlTree())

    // TODO: proper option handling
    val closedTree = OUTPUTS(0).R5[AvlTree].getOrElse(AvlTree())

    val digestPreserved = openTree.digest == closedTree.digest
    val keyLengthPreserved = openTree.keyLength == closedTree.keyLength
    val valueLengthPreserved = openTree.valueLengthOpt == closedTree.valueLengthOpt
    val treeIsClosed = closedTree.enabledOperations == 4

    val tokenId: List[Byte] = INPUTS(0).id

    val outputsCountCorrect = OUTPUTS.size == 3
    val secondOutputNoTokens = outputsCountCorrect &&
      OUTPUTS(0).tokens.size == 1 &&
      OUTPUTS(1).tokens.size == 0 &&
      OUTPUTS(2).tokens.size == 0

    val correctTokensIssued = OUTPUTS(0).tokens.size == 1 && SELF.value == OUTPUTS(0).tokens(0)._2

    val correctTokenId = outputsCountCorrect &&
    // TODO: proper option handling
    OUTPUTS(0).R4[List[Byte]].getOrElse(List()) == tokenId &&
      OUTPUTS(0).tokens.size == 1 &&
      OUTPUTS(0).tokens(0)._1 == tokenId

    val valuePreserved = outputsCountCorrect && secondOutputNoTokens && correctTokensIssued && correctTokenId
    val stateChanged = blake2b256(OUTPUTS(0).propositionBytes) == nextStageScriptHash

    val treeIsCorrect = digestPreserved && valueLengthPreserved && keyLengthPreserved && treeIsClosed

    projectPubKey.isValid && treeIsCorrect && valuePreserved && stateChanged
  }

  def ICOWithdrawalContract(ctx: WithdrawalContext): Boolean = {
    import ctx._

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty)

    // TODO: make verifier green
    // TODO: proper option handling
    val removeProof = getVar[List[Byte]](2).getOrElse(List())
    // TODO: proper option handling
    val lookupProof = getVar[List[Byte]](3).getOrElse(List())
    // TODO: proper option handling
    val withdrawIndexes = getVar[List[BigInt]](4).getOrElse(List())

    val out0 = OUTPUTS(0)

    // TODO: proper option handling
    val tokenId: List[Byte] = SELF.R4[List[Byte]].getOrElse(List())

    val withdrawals: List[(List[Byte], Long)] = withdrawIndexes.flatMap({ (idx: BigInt) =>
      if (idx >= 0 && idx < OUTPUTS.length) {
        val b = OUTPUTS(idx)
        if (b.tokens.nonEmpty && b.tokens(0)._1 == tokenId) {
          List((blake2b256(b.propositionBytes), b.tokens(0)._2))
        } else {
          List((blake2b256(b.propositionBytes), 0L))
        }
      } else {
        List()
      }
    })

    //val withdrawals = OUTPUTS.slice(1, OUTPUTS.size-1).map(...)

    val withdrawValues = withdrawals.map({ (t: (List[Byte], Long)) => t._2 })

    val withdrawTotal = withdrawValues.foldLeft(0L) { (l1: Long, l2: Long) => l1 + l2 }

    val toRemove = withdrawals.map({ (t: (List[Byte], Long)) => t._1 })

    // TODO: proper option handling
    val initialTree = SELF.R5[AvlTree].getOrElse(AvlTree())

    // TODO: proper option handling
    val removedValues = initialTree.getMany(toRemove, lookupProof).map({ (o: Option[List[Byte]]) => byteArrayToLong(o.getOrElse(List())) })
    val valuesCorrect = removedValues == withdrawValues

    // TODO: proper option handling
    val modifiedTree = initialTree.remove(toRemove, removeProof).getOrElse(AvlTree())

    // TODO: proper option handling
    val expectedTree = out0.R5[AvlTree].getOrElse(AvlTree())

    val selfTokensCorrect = SELF.tokens.nonEmpty && SELF.tokens(0)._1 == tokenId
    val selfOutTokensAmount = if (selfTokensCorrect) SELF.tokens(0)._2 else 0
    val soutTokensCorrect = out0.tokens.nonEmpty && out0.tokens(0)._1 == tokenId
    val soutTokensAmount = if (soutTokensCorrect) out0.tokens(0)._2 else 0

    val tokensPreserved = selfTokensCorrect && soutTokensCorrect && (soutTokensAmount + withdrawTotal == selfOutTokensAmount)

    val properTreeModification = modifiedTree == expectedTree

    val selfOutputCorrect = out0.propositionBytes == SELF.propositionBytes

     properTreeModification && valuesCorrect && selfOutputCorrect && tokensPreserved
  }

}
