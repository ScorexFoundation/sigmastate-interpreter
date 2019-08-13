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

    @extern @pure
    def insert(toAdd: List[(List[Byte], List[Byte])], proof: List[Byte]): Option[AvlTree] = ???
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

}
