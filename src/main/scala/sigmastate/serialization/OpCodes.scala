package sigmastate.serialization

/** Encoding of types for serialization. */
trait TypeCodes {
  /** Decoding of types depends on the first byte and in general is a recursive procedure
    * consuming some number of bytes from ByteReader.
    * All data types are recognized by the first byte falling in the region [FirstDataType .. LastDataType] */
  val FirstDataType: Byte = 1.toByte
  val LastDataType: Byte = 111.toByte

  /** SFunc types occupy remaining space of byte values [FirstFuncType .. 255]*/
  val FirstFuncType: Byte = (LastDataType + 1).toByte
  val LastFuncType: Byte = 255.toByte
}

/** Encoding of values for serialization. */
trait ValueCodes extends TypeCodes {
  /** We use optimized encoding of constant values to save space in serialization.
    * Since Box registers are stored as Constant nodes we save 1 byte for each register.
    * This is due to convention that Value.opCode falling in [1..LastDataType] region is a constant.
    * Thus, we can just decode an instance of SType and then decode data using DataSerializer.
    *
    * Decoding of constants depends on the first byte and in general is a recursive procedure
    * consuming some number of bytes from ByteReader.
    * */
  val ConstantCode: Byte = 0
  
  /** The last constant code is equal to FirstFuncType which represent generic function type.
    * We use this single code to represent all functional constants, since we don't have enough space in single byte.
    * Subsequent bytes have to be read from ByteReader in order to decode the type of the function and the corresponding data. */
  val LastConstantCode: Byte = (LastDataType + 1).toByte
}

object OpCodes extends ValueCodes {

  type OpCode = Byte

  // serialization is not required
  val Undefined: OpCode = 0: Byte

  // variable
  val TaggedVariableCode: OpCode = (LastConstantCode + 1).toByte

  // EvaluatedValue descendants
  val ByteConstantCode:         OpCode = (LastConstantCode + 10).toByte
  val IntConstantCode:          OpCode = (LastConstantCode + 11).toByte
  val TrueCode:                 OpCode = (LastConstantCode + 12).toByte
  val FalseCode:                OpCode = (LastConstantCode + 13).toByte
  val UnitConstantCode:         OpCode = (LastConstantCode + 14).toByte
  val BigIntConstantCode:       OpCode = (LastConstantCode + 15).toByte
  val CollectionConstantCode:   OpCode = (LastConstantCode + 16).toByte
  val GroupElementConstantCode: OpCode = (LastConstantCode + 17).toByte
  val GroupGeneratorCode      : OpCode = (LastConstantCode + 18).toByte
  val BoxConstantCode         : OpCode = (LastConstantCode + 19).toByte
  val ConcreteCollectionCode  : OpCode = (LastConstantCode + 20).toByte
  val TupleCode               : OpCode = (LastConstantCode + 21).toByte
  val SomeValueCode           : OpCode = (LastConstantCode + 22).toByte
  val NoneValueCode           : OpCode = (LastConstantCode + 23).toByte
  val AvlTreeConstantCode     : OpCode = (LastConstantCode + 24).toByte

  // Relation descendants
  val LtCode : OpCode = (LastConstantCode + 31).toByte
  val LeCode : OpCode = (LastConstantCode + 32).toByte
  val GtCode : OpCode = (LastConstantCode + 33).toByte
  val GeCode : OpCode = (LastConstantCode + 34).toByte
  val EqCode : OpCode = (LastConstantCode + 35).toByte
  val NeqCode: OpCode = (LastConstantCode + 36).toByte
  val IfCode : OpCode = (LastConstantCode + 37).toByte
  val AndCode: OpCode = (LastConstantCode + 38).toByte
  val OrCode : OpCode = (LastConstantCode + 39).toByte

  // Arithmetic codes
  val MinusCode        : OpCode = (LastConstantCode + 41).toByte
  val PlusCode         : OpCode = (LastConstantCode + 42).toByte
  val XorCode          : OpCode = (LastConstantCode + 43).toByte
  val MultiplyCode     : OpCode = (LastConstantCode + 44).toByte
  val DivisionCode     : OpCode = (LastConstantCode + 45).toByte
  val ModuloCode       : OpCode = (LastConstantCode + 46).toByte
  val ExponentiateCode : OpCode = (LastConstantCode + 47).toByte
  val MultiplyGroupCode: OpCode = (LastConstantCode + 48).toByte

  // Environment codes
  val HeightCode               : OpCode = (LastConstantCode + 51).toByte
  val InputsCode               : OpCode = (LastConstantCode + 52).toByte
  val OutputsCode              : OpCode = (LastConstantCode + 53).toByte
  val LastBlockUtxoRootHashCode: OpCode = (LastConstantCode + 54).toByte
  val SelfCode                 : OpCode = (LastConstantCode + 55).toByte

  // Collection and tree operations codes
  val MapCollectionCode: OpCode = (LastConstantCode + 61).toByte
  val ExistsCode       : OpCode = (LastConstantCode + 62).toByte
  val ForAllCode       : OpCode = (LastConstantCode + 63).toByte
  val FoldCode         : OpCode = (LastConstantCode + 64).toByte
  val SizeOfCode       : OpCode = (LastConstantCode + 65).toByte
  val ByIndexCode      : OpCode = (LastConstantCode + 66).toByte
  val AppendCode       : OpCode = (LastConstantCode + 67).toByte
//  val AppendBytesCode  : OpCode = (LastConstantCode + 68).toByte  // TODO remove
  val SliceCode        : OpCode = (LastConstantCode + 69).toByte
  val WhereCode        : OpCode = (LastConstantCode + 70).toByte
  val IsMemberCode     : OpCode = (LastConstantCode + 71).toByte

  // Type casts codes
  val ExtractAmountCode        : OpCode = (LastConstantCode + 81).toByte
  val ExtractScriptBytesCode   : OpCode = (LastConstantCode + 82).toByte
  val ExtractBytesCode         : OpCode = (LastConstantCode + 83).toByte
  val ExtractBytesWithNoRefCode: OpCode = (LastConstantCode + 84).toByte
  val ExtractIdCode            : OpCode = (LastConstantCode + 85).toByte
  val ExtractRegisterAs        : OpCode = (LastConstantCode + 86).toByte
  val IntToByteArrayCode       : OpCode = (LastConstantCode + 87).toByte
  val ByteArrayToBigIntCode    : OpCode = (LastConstantCode + 88).toByte

  // Cryptographic operations codes
  val CalcBlake2b256Code         : OpCode = (LastConstantCode + 91).toByte
  val CalcSha256Code             : OpCode = (LastConstantCode + 92).toByte
  val ProveDlogCode              : OpCode = (LastConstantCode + 93).toByte
  val ProveDiffieHellmanTupleCode: OpCode = (LastConstantCode + 94).toByte

  // Deserialization codes
  val DeserializeContextCode : OpCode = (LastConstantCode + 100).toByte
  val DeserializeRegisterCode: OpCode = (LastConstantCode + 101).toByte

}
