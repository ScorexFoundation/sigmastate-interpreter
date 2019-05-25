package org.ergoplatform

import org.ergoplatform.ErgoLikeContext
import sigmastate.{AtLeast, SBigInt, SPrimType}

case class SizeConstant[T: Numeric](value: T, id: Short, description: String) {
  def get: T = value
}

object ErgoConstants {

  object MaxInputSize extends SizeConstant[Int](1024 * 1024 * 1, 1,
    "Input size should not be greater then provided value") {
  }

  object MaxTreeDepth extends SizeConstant[Int](110, 2,
    "Max tree depth should not be greater then provided value") {
  }

  object MaxByteArrayLength extends SizeConstant[Int](10000, 3,
    "Max bytearray length") {
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

  object MaxCollectionSize extends SizeConstant[Long](1024, 13,
    "Max collection size")

  val ConstTable: Seq[SizeConstant[_]] = Seq(
    MaxInputSize,
    MaxTreeDepth,
    MaxByteArrayLength,
    MaxTokens,
    MaxRegisters,
    MaxBoxSize,
    MaxBigIntSizeInBytes,
    MaxTupleLength,
    MaxHeaders,
    MaxChildrenCountForAtLeastOp
  )
}
