package org.ergoplatform

import scalan.util.CollectionUtil._
import sigmastate.eval.Sized

case class SizeConstant[T: Numeric](value: T, id: Short, description: String)

/** Constants facade that provide access to the values used in sigma's logic and checks.
  * All the constants are collected in a sequence.
  * Each constant has id, value and description.
  * The constant ids are stable and never change.
  * Some constant values may be defined in terms of other more fundamental values.
  * In the future versions of sigma, the values may change,
  * but due to versioned execution, all versions of the values should be
  * available simultaneously.
  */
object ErgoConstants {

  object MaxBoxSize extends SizeConstant[Int](4 * 1024, 1,
    "Box size should not be greater than provided value") {
  }

  object MaxTreeDepth extends SizeConstant[Int](110, 2,
    "Max tree depth should not be greater then provided value") {
  }

  object MaxTokens extends SizeConstant[Byte](4, 3,
    "Tokens count should not be greater than provided value") {
  }

  object MaxRegisters extends SizeConstant[Int](10, 4,
    "Registers count should not be greater than provided value") {
  }

  // TODO set to 1500 and make tests pass by using this constant there
  object MaxPropositionBytes extends SizeConstant[Int](Sized.SizePropositionBytesMax.dataSize.toInt, 5,
    "Max length of Box.propositionBytes collection") {
  }

  object MaxBoxSizeWithoutRefs extends SizeConstant[Int](Sized.SizeBoxBytesWithoutRefsMax.dataSize.toInt, 6,
    "Box size should not be greater than provided value") {
  }

  object MaxBigIntSizeInBytes extends SizeConstant[Long](32L, 7,
    "BigInt size in bytes should not be greater than provided value") {
  }

  object MaxSigmaPropSizeInBytes extends SizeConstant[Long](1024L, 8,
    "SigmaProp size in bytes should not be greater than provided value") {
  }

  object MaxTupleLength extends SizeConstant[Int](255, 9,
    "Tuple length should not be greater than provided value") {
  }

  object MaxHeaders extends SizeConstant[Int](10, 10,
    "Headers count should not be greater than provided value") {
  }

  object MaxChildrenCountForAtLeastOp extends SizeConstant[Int](255, 11,
    "Max children count should not be greater than provided value") {
  }

  object ScriptCostLimit extends SizeConstant[Int](1000000, 12,
    "Maximum execution cost of a script") {
  }

  object MaxLoopLevelInCostFunction extends SizeConstant[Int](1, 13,
    "Maximum allowed loop level in a cost function") {
  }

  val ConstTable: Seq[SizeConstant[_]] = {
    val rows = Seq(
      MaxBoxSize,
      MaxTreeDepth,
      MaxTokens,
      MaxRegisters,
      MaxPropositionBytes,
      MaxBoxSizeWithoutRefs,
      MaxBigIntSizeInBytes,
      MaxSigmaPropSizeInBytes,
      MaxTupleLength,
      MaxHeaders,
      MaxChildrenCountForAtLeastOp,
      ScriptCostLimit,
      MaxLoopLevelInCostFunction
    )
    require(rows.length == rows.distinctBy(_.id).length, s"Duplicate constant id in $rows")
    rows
  }
}
