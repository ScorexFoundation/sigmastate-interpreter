package sigmastate.verified

import sigmastate.verified.CrowdFundingContractVerification.SigmaProp
import stainless.annotation._
import stainless.collection._
import stainless.lang._
import stainless.math._
import stainless.proof._

object ICOContractVerification {

  @extern @pure
  def arrayEquals(arr1: Array[Byte], arr2: Array[Byte]): Boolean = ???

  case class Box(value: Long) {

    def id: List[Byte] = ???

    @extern @pure
    def R4[T]: Option[T] = ???

    @extern @pure
    def R5[T]: Option[T] = ???

    def tokens: List[(List[Byte], Long)] = ???

    def propositionBytes: List[Byte] = ???
  }

  case class AvlTree() {

    def digest: List[Byte] = ???

    def keyLength: Int = ???

    def valueLengthOpt: Option[Int] = ???

    def enabledOperations: Byte = ???

    def getMany(keys: List[List[Byte]], proof: List[Byte]): List[Option[List[Byte]]] = ???

    def insert(toAdd: List[(List[Byte], List[Byte])], proof: List[Byte]): Option[AvlTree] = ???

    def remove(operations: List[List[Byte]], proof: List[Byte]): Option[AvlTree] = ???
  }


  case class FundingContext(OUTPUTS: List[Box],
                            INPUTS: List[Box],
                            SELF: Box,
                            HEIGHT: Int,
                            nextStageScriptHash: List[Byte],
                            feeBytes: List[Byte]) {

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty)

  }

  @extern @pure
  def getVar[T](id: Byte): Option[T] = ???
  def longToByteArray(l: Long): List[Byte] = ???
  def byteArrayToLong(bytes: List[Byte]): Long = ???
  def blake2b256(bytes: List[Byte]): List[Byte] = ???

  def ICOFundingContract(ctx: FundingContext): Boolean = {

    val selfIndexIsZero = ctx.INPUTS(0).id == ctx.SELF.id

    // TODO: proper option handling
    val proof = getVar[List[Byte]](1).getOrElse(List())

    val inputsCount = ctx.INPUTS.size

    val toAdd: List[(List[Byte], List[Byte])] = ctx.INPUTS.slice(1, inputsCount).map({ (b: Box) =>
      // TODO: proper option handling
      val pk = b.R4[List[Byte]].getOrElse(List())
      val value = longToByteArray(b.value)
      (pk, value)
    })

    // TODO: proper option handling
    val modifiedTree = ctx.SELF.R5[AvlTree].getOrElse(AvlTree()).insert(toAdd, proof).getOrElse(AvlTree())

    // TODO: proper option handling
    val expectedTree = ctx.OUTPUTS(0).R5[AvlTree].getOrElse(AvlTree())

    val properTreeModification = modifiedTree == expectedTree

    val outputsCount = ctx.OUTPUTS.size == 2

    val selfOutputCorrect = if (ctx.HEIGHT < 2000) {
      ctx.OUTPUTS(0).propositionBytes == ctx.SELF.propositionBytes
    } else {
      blake2b256(ctx.OUTPUTS(0).propositionBytes) == ctx.nextStageScriptHash
    }

    val feeOutputCorrect = outputsCount &&
      (ctx.OUTPUTS(1).value <= 1) &&
      (ctx.OUTPUTS(1).propositionBytes == ctx.feeBytes)

    val outputsCorrect = outputsCount && feeOutputCorrect && selfOutputCorrect

    selfIndexIsZero && outputsCorrect && properTreeModification
  }

  case class IssuanceContext(OUTPUTS: List[Box],
                     INPUTS: List[Box],
                     SELF: Box,
                     HEIGHT: Int,
                     nextStageScriptHash: List[Byte],
                     projectPubKey: SigmaProp) {

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty)
  }

  def ICOIssuanceContract(ctx: IssuanceContext): Boolean = {
    // TODO: proper option handling
    val openTree = ctx.SELF.R5[AvlTree].getOrElse(AvlTree())

    // TODO: proper option handling
    val closedTree = ctx.OUTPUTS(0).R5[AvlTree].getOrElse(AvlTree())

    val digestPreserved = openTree.digest == closedTree.digest
    val keyLengthPreserved = openTree.keyLength == closedTree.keyLength
    val valueLengthPreserved = openTree.valueLengthOpt == closedTree.valueLengthOpt
    val treeIsClosed = closedTree.enabledOperations == 4

    val tokenId: List[Byte] = ctx.INPUTS(0).id

    val outputsCountCorrect = ctx.OUTPUTS.size == 3
    val secondOutputNoTokens = outputsCountCorrect &&
      ctx.OUTPUTS(0).tokens.size == 1 &&
      ctx.OUTPUTS(1).tokens.size == 0 &&
      ctx.OUTPUTS(2).tokens.size == 0

    val correctTokensIssued = ctx.OUTPUTS(0).tokens.size == 1 && ctx.SELF.value == ctx.OUTPUTS(0).tokens(0)._2

    val correctTokenId = outputsCountCorrect &&
    // TODO: proper option handling
    ctx.OUTPUTS(0).R4[List[Byte]].getOrElse(List()) == tokenId &&
      ctx.OUTPUTS(0).tokens.size == 1 &&
      ctx.OUTPUTS(0).tokens(0)._1 == tokenId

    val valuePreserved = outputsCountCorrect && secondOutputNoTokens && correctTokensIssued && correctTokenId
    val stateChanged = blake2b256(ctx.OUTPUTS(0).propositionBytes) == ctx.nextStageScriptHash

    val treeIsCorrect = digestPreserved && valueLengthPreserved && keyLengthPreserved && treeIsClosed

    ctx.projectPubKey.isValid && treeIsCorrect && valuePreserved && stateChanged
  }

  case class WithdrawalContext(OUTPUTS: List[Box],
                               INPUTS: List[Box],
                               SELF: Box,
                               HEIGHT: Int) {

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty)
  }

  def ICOWithdrawalContract(ctx: WithdrawalContext): Boolean = {
    // TODO: make verifier green
    // TODO: proper option handling
    val removeProof = getVar[List[Byte]](2).getOrElse(List())
    // TODO: proper option handling
    val lookupProof = getVar[List[Byte]](3).getOrElse(List())
    // TODO: proper option handling
    val withdrawIndexes = getVar[List[BigInt]](4).getOrElse(List())

    val out0 = ctx.OUTPUTS(0)

    // TODO: proper option handling
    val tokenId: List[Byte] = ctx.SELF.R4[List[Byte]].getOrElse(List())

    val withdrawals: List[(List[Byte], Long)] = withdrawIndexes.flatMap({ (idx: BigInt) =>
      if (idx >= 0 && idx < ctx.OUTPUTS.length) {
        val b = ctx.OUTPUTS(idx)
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
    val initialTree = ctx.SELF.R5[AvlTree].getOrElse(AvlTree())

    // TODO: proper option handling
    val removedValues = initialTree.getMany(toRemove, lookupProof).map({ (o: Option[List[Byte]]) => byteArrayToLong(o.getOrElse(List())) })
    val valuesCorrect = removedValues == withdrawValues

    // TODO: proper option handling
    val modifiedTree = initialTree.remove(toRemove, removeProof).getOrElse(AvlTree())

    // TODO: proper option handling
    val expectedTree = out0.R5[AvlTree].getOrElse(AvlTree())

    val selfTokensCorrect = ctx.SELF.tokens.nonEmpty && ctx.SELF.tokens(0)._1 == tokenId
    val selfOutTokensAmount = if (selfTokensCorrect) ctx.SELF.tokens(0)._2 else 0
    val soutTokensCorrect = out0.tokens.nonEmpty && out0.tokens(0)._1 == tokenId
    val soutTokensAmount = if (soutTokensCorrect) out0.tokens(0)._2 else 0

    val tokensPreserved = selfTokensCorrect && soutTokensCorrect && (soutTokensAmount + withdrawTotal == selfOutTokensAmount)

    val properTreeModification = modifiedTree == expectedTree

    val selfOutputCorrect = out0.propositionBytes == ctx.SELF.propositionBytes

     properTreeModification && valuesCorrect && selfOutputCorrect && tokensPreserved
  }

}
