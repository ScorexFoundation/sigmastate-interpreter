package sigmastate.verified

import stainless.annotation._
import stainless.collection._
import stainless.lang._
import stainless.math._
import stainless.proof._

object ICOContractVerification {

  def arrayEquals(arr1: Array[Byte], arr2: Array[Byte]): Boolean = ???

  case class Box(value: Long) {

    def id: Array[Byte] = ???

    @extern @pure
    def R4[T]: Option[T] = ???

    @extern @pure
    def R5[T]: Option[T] = ???

    def propositionBytes: Array[Byte] = ???
  }

  case class AvlTree() {

    @extern @pure
    def insert(toAdd: List[(List[Byte], List[Byte])], proof: List[Byte]): Option[AvlTree] = ???
  }


  case class Context(OUTPUTS: List[Box],
                     INPUTS: List[Box],
                     SELF: Box,
                     HEIGHT: Int,
                     nextStageScriptHash: Array[Byte],
                     feeBytes: Array[Byte]) {

    require(HEIGHT > 0 &&
      OUTPUTS.nonEmpty &&
      INPUTS.nonEmpty)

  }

  @extern @pure
  def getVar[T](id: Byte): Option[T] = ???
  def longToByteArray(l: Long): List[Byte] = ???
  def blake2b256(bytes: Array[Byte]): Array[Byte] = ???

  // TODO avoid Option.get?

  def ICOFundingContract(ctx: Context): Boolean = {

    val selfIndexIsZero = arrayEquals(ctx.INPUTS(0).id, ctx.SELF.id)

    val proof = getVar[List[Byte]](1).get

    val inputsCount = ctx.INPUTS.size

    val toAdd: List[(List[Byte], List[Byte])] = ctx.INPUTS.slice(1, inputsCount).map({ (b: Box) =>
      val pk = b.R4[List[Byte]].get
      val value = longToByteArray(b.value)
      (pk, value)
    })

    val modifiedTree = ctx.SELF.R5[AvlTree].get.insert(toAdd, proof).get

    val expectedTree = ctx.OUTPUTS(0).R5[AvlTree].get

    val properTreeModification = modifiedTree == expectedTree

    val outputsCount = ctx.OUTPUTS.size == 2

    val selfOutputCorrect = if (ctx.HEIGHT < 2000) {
      arrayEquals(ctx.OUTPUTS(0).propositionBytes, ctx.SELF.propositionBytes)
    } else {
      arrayEquals(blake2b256(ctx.OUTPUTS(0).propositionBytes), ctx.nextStageScriptHash)
    }

    val feeOutputCorrect = (ctx.OUTPUTS(1).value <= 1) && (arrayEquals(ctx.OUTPUTS(1).propositionBytes, ctx.feeBytes))

    val outputsCorrect = outputsCount && feeOutputCorrect && selfOutputCorrect

    selfIndexIsZero && outputsCorrect && properTreeModification
  }

}
