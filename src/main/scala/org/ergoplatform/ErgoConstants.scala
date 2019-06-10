package org.ergoplatform

import sigmastate.interpreter.CryptoConstants
import scalan.util.CollectionUtil._

case class SizeConstant[T: Numeric](value: T, id: Short, description: String) {
  def get: T = value
}

/**
  * Fundamental constants that are used in sigma's logic and checks
  */
object ErgoConstants {

  object MaxInputSize extends SizeConstant[Int](4 * 1024, 1,
    "Input size should not be greater then provided value") {
  }

  object MaxTreeDepth extends SizeConstant[Int](110, 2,
    "Max tree depth should not be greater then provided value") {
  }

  object MaxPropositionBytes extends SizeConstant[Int](4 * 1024, 3,
    "Max length of Box.propositionBytes collection") {
  }

  object MaxTokens extends SizeConstant[Byte](4, 6,
    "Tokens count should not be greater than provided value") {
  }

  object MaxRegisters extends SizeConstant[Int](10, 7,
    "Registers count should not be greater than provided value") {
  }

  object MaxBoxSize extends SizeConstant[Int](64 * 1024, 8,
    "Box size should not be greater than provided value") {
  }

  object MaxBoxSizeWithoutRefs extends SizeConstant[Int](MaxBoxSize.value - (CryptoConstants.hashLength + 4), 9,
    "Box size should not be greater than provided value") {
  }

  object MaxBigIntSizeInBytes extends SizeConstant[Long](32L, 10,
    "BigInt size in bytes should not be greater than provided value") {
  }

  object MaxTupleLength extends SizeConstant[Int](255, 11,
    "Tuple length should not be greater than provided value") {
  }

  object MaxHeaders extends SizeConstant[Int](10, 12,
    "Headers count should not be greater than provided value") {
  }

  object MaxChildrenCountForAtLeastOp extends SizeConstant[Int](255, 13,
    "Max children count should not be greater than provided value") {
  }

  val ConstTable: Seq[SizeConstant[_]] = {
    val rows = Seq(
      MaxInputSize,
      MaxTreeDepth,
      MaxPropositionBytes,
      MaxTokens,
      MaxRegisters,
      MaxBoxSize,
      MaxBigIntSizeInBytes,
      MaxTupleLength,
      MaxHeaders,
      MaxChildrenCountForAtLeastOp
    )
    require(rows.length == rows.distinctBy(_.id).length, s"Duplicate constant id in $rows")
    rows
  }
}
