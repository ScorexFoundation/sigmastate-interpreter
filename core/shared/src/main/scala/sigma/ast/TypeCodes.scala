package sigma.ast

import supertagged.TaggedType

/** Encoding of types for serialization. */
object TypeCodes {
  object TypeCode extends TaggedType[Byte]
  type TypeCode = TypeCode.Type

  /** Decoding of types depends on the first byte and in general is a recursive procedure
    * consuming some number of bytes from Reader.
    * All data types are recognized by the first byte falling in the region [FirstDataType .. LastDataType] */
  val FirstDataType: TypeCode = TypeCode @@ 1.toByte

  val LastDataType : TypeCode = TypeCode @@ 111.toByte

  /** SFunc types occupy remaining space of byte values [FirstFuncType .. 255] */
  val FirstFuncType: TypeCode = TypeCode @@ (LastDataType + 1).toByte

  val LastFuncType : TypeCode = TypeCode @@ 255.toByte
}
