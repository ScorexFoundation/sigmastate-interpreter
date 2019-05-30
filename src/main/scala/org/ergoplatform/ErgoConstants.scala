package org.ergoplatform

import org.ergoplatform.ErgoLikeContext
import sigmastate.{AtLeast, SBigInt, SPrimType}

case class SizeConstant[T: Numeric](value: T, id: Short, description: String) {
  def get: T = value
}

/**
  * Fundamental constants that are used in sigma's logic and checks
  */
object ErgoConstants {

  object MaxInputSize extends SizeConstant[Int](64 * 1024 * 1, 1,
    "Input size should not be greater then provided value") {
  }

  object MaxTreeDepth extends SizeConstant[Int](64, 2,
    "Max tree depth should not be greater then provided value") {
  }

  object MaxByteArrayLength extends SizeConstant[Int](10000, 3,
    "Max bytearray length") {
  }

  object MaxBoxAdditionalTokens extends SizeConstant[Byte](4, 6,
    "Tokens count in the box should not be greater than provided value") {
  }

  object MaxRegisters extends SizeConstant[Int](10, 7,
    "Registers count should not be greater than provided value") {
  }

  object MaxBoxSize extends SizeConstant[Int](64 * 1024, 8,
    "Box size should not be greater than provided value") {
  }

  object MaxBigIntSizeInBytes extends SizeConstant[Long](32L, 9,
    "BigInt size in bytes should not be greater than provided value") {
  }

  object MaxTupleLength extends SizeConstant[Int](255, 10,
    "Tuple length should not be greater than provided value") {
  }

  object MaxHeaders extends SizeConstant[Int](10, 11,
    "Headers count should not be greater than provided value") {
  }

  object MaxChildrenCountForAtLeastOp extends SizeConstant[Int](255, 12,
    "Max children count should not be greater than provided value") {
  }

  val ConstTable: Seq[SizeConstant[_]] = Seq(
    MaxInputSize,
    MaxTreeDepth,
    MaxByteArrayLength,
    MaxBoxAdditionalTokens,
    MaxRegisters,
    MaxBoxSize,
    MaxBigIntSizeInBytes,
    MaxTupleLength,
    MaxHeaders,
    MaxChildrenCountForAtLeastOp
  )
}
