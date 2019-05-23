package sigmastate.constants

import org.ergoplatform.ErgoLikeContext
import sigmastate.{AtLeast, SBigInt, SPrimType}

/*
  THIS FILE IS OBSOLETE AND TO BE REMOVED IN FUTURE
 */
case class SizeConstant[T: Numeric](value: T, id: Short, description: String) {
}

object SigmaConstants {

  object MaxInputSize extends SizeConstant[Int](1024 * 1024 * 1, 1,
    "Input size should not be greater then provided value") {
  }

  object MaxTreeDepth extends SizeConstant[Int](110, 2,
    "Max depth should not be greater then provided value") {
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

  object MaxSizeInBytes extends SizeConstant[Long](32L, 9,
    "BigInt size in bytes should not be greater than provided value") {
  }

  object MaxTupleLength extends SizeConstant[Int](255, 10,
    "Tuple length should not be greater than provided value") {
  }

  object MaxHeaders extends SizeConstant[Int](10, 11,
    "Headers count should not be greater than provided value") {
  }

  object MaxChildrenCount extends SizeConstant[Int](255, 12,
    "Max children count should not be greater than provided value") {
  }

  object MaxCollectionSize extends SizeConstant[Long](1024, 13,
  "Max collection size")

  val constSpecs: Seq[SizeConstant[_]] = Seq(
    MaxInputSize,
    MaxTreeDepth,
    MaxByteArrayLength,
    MaxTokens,
    MaxRegisters,
    MaxBoxSize,
    MaxSizeInBytes,
    MaxTupleLength,
    MaxHeaders,
    MaxChildrenCount
  )
}
