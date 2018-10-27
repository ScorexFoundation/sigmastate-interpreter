package sigmastate.serialization

/** Encoding of types for serialization. */
trait TypeCodes {
  /** Decoding of types depends on the first byte and in general is a recursive procedure
    * consuming some number of bytes from ByteReader.
    * All data types are recognized by the first byte falling in the region [FirstDataType .. LastDataType] */
  val FirstDataType: Byte = 1.toByte
  val LastDataType : Byte = 111.toByte

  /** SFunc types occupy remaining space of byte values [FirstFuncType .. 255] */
  val FirstFuncType: Byte = (LastDataType + 1).toByte
  val LastFuncType : Byte = 255.toByte
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

  // variables
  val TaggedVariableCode: OpCode = (LastConstantCode + 1).toByte
  val ValUseCode: OpCode = (LastConstantCode + 2).toByte   // reserved 3 - 9

  val LongToByteArrayCode  : OpCode = (LastConstantCode + 10).toByte
  val ByteArrayToBigIntCode: OpCode = (LastConstantCode + 11).toByte
  val DowncastCode         : OpCode = (LastConstantCode + 12).toByte
  val UpcastCode           : OpCode = (LastConstantCode + 13).toByte // reserved 14

  // EvaluatedValue descendants
  val TrueCode              : OpCode = (LastConstantCode + 15).toByte
  val FalseCode             : OpCode = (LastConstantCode + 16).toByte
  val UnitConstantCode      : OpCode = (LastConstantCode + 17).toByte
  val GroupGeneratorCode    : OpCode = (LastConstantCode + 18).toByte
  val ConcreteCollectionCode: OpCode = (LastConstantCode + 19).toByte // reserved 20
  val ConcreteCollectionBooleanConstantCode: OpCode = (LastConstantCode + 21).toByte

  val TupleCode      : OpCode = (LastConstantCode + 22).toByte
  val Select1Code    : OpCode = (LastConstantCode + 23).toByte
  val Select2Code    : OpCode = (LastConstantCode + 24).toByte
  val Select3Code    : OpCode = (LastConstantCode + 25).toByte
  val Select4Code    : OpCode = (LastConstantCode + 26).toByte
  val Select5Code    : OpCode = (LastConstantCode + 27).toByte
  val SelectFieldCode: OpCode = (LastConstantCode + 28).toByte // reserved 29

  // Relation descendants
  val LtCode     : OpCode = (LastConstantCode + 31).toByte
  val LeCode     : OpCode = (LastConstantCode + 32).toByte
  val GtCode     : OpCode = (LastConstantCode + 33).toByte
  val GeCode     : OpCode = (LastConstantCode + 34).toByte
  val EqCode     : OpCode = (LastConstantCode + 35).toByte
  val NeqCode    : OpCode = (LastConstantCode + 36).toByte
  val IfCode     : OpCode = (LastConstantCode + 37).toByte
  val AndCode    : OpCode = (LastConstantCode + 38).toByte
  val OrCode     : OpCode = (LastConstantCode + 39).toByte
  val AtLeastCode: OpCode = (LastConstantCode + 40).toByte

  // Arithmetic codes
  val MinusCode        : OpCode = (LastConstantCode + 41).toByte
  val PlusCode         : OpCode = (LastConstantCode + 42).toByte
  val XorCode          : OpCode = (LastConstantCode + 43).toByte
  val MultiplyCode     : OpCode = (LastConstantCode + 44).toByte
  val DivisionCode     : OpCode = (LastConstantCode + 45).toByte
  val ModuloCode       : OpCode = (LastConstantCode + 46).toByte
  val ExponentiateCode : OpCode = (LastConstantCode + 47).toByte
  val MultiplyGroupCode: OpCode = (LastConstantCode + 48).toByte
  val MinCode          : OpCode = (LastConstantCode + 49).toByte
  val MaxCode          : OpCode = (LastConstantCode + 50).toByte

  // Environment codes
  val HeightCode               : OpCode = (LastConstantCode + 51).toByte
  val InputsCode               : OpCode = (LastConstantCode + 52).toByte
  val OutputsCode              : OpCode = (LastConstantCode + 53).toByte
  val LastBlockUtxoRootHashCode: OpCode = (LastConstantCode + 54).toByte
  val SelfCode                 : OpCode = (LastConstantCode + 55).toByte
  val SigmaAndCode             : OpCode = (LastConstantCode + 56).toByte
  val SigmaOrCode              : OpCode = (LastConstantCode + 57).toByte
  val BinOrCode                : OpCode = (LastConstantCode + 58).toByte
  val BinAndCode               : OpCode = (LastConstantCode + 59).toByte // reserved 60

  // Collection and tree operations codes
  val MapCollectionCode    : OpCode = (LastConstantCode + 61).toByte
  val ExistsCode           : OpCode = (LastConstantCode + 62).toByte
  val ForAllCode           : OpCode = (LastConstantCode + 63).toByte
  val FoldCode             : OpCode = (LastConstantCode + 64).toByte
  val SizeOfCode           : OpCode = (LastConstantCode + 65).toByte
  val ByIndexCode          : OpCode = (LastConstantCode + 66).toByte
  val AppendCode           : OpCode = (LastConstantCode + 67).toByte
  val SliceCode            : OpCode = (LastConstantCode + 68).toByte
  val WhereCode            : OpCode = (LastConstantCode + 69).toByte
  val TreeLookupCode       : OpCode = (LastConstantCode + 70).toByte
  val StringConcatCode     : OpCode = (LastConstantCode + 71).toByte
  val TreeModificationsCode: OpCode = (LastConstantCode + 72).toByte
  // reserved 73 - 80

  // Type casts codes
  val ExtractAmountCode        : OpCode = (LastConstantCode + 81).toByte
  val ExtractScriptBytesCode   : OpCode = (LastConstantCode + 82).toByte
  val ExtractBytesCode         : OpCode = (LastConstantCode + 83).toByte
  val ExtractBytesWithNoRefCode: OpCode = (LastConstantCode + 84).toByte
  val ExtractIdCode            : OpCode = (LastConstantCode + 85).toByte
  val ExtractRegisterAs        : OpCode = (LastConstantCode + 86).toByte // reserved 87 - 90

  // Cryptographic operations codes
  val CalcBlake2b256Code         : OpCode = (LastConstantCode + 91).toByte
  val CalcSha256Code             : OpCode = (LastConstantCode + 92).toByte
  val ProveDlogCode              : OpCode = (LastConstantCode + 93).toByte
  val ProveDiffieHellmanTupleCode: OpCode = (LastConstantCode + 94).toByte
  val SigmaPropIsValidCode       : OpCode = (LastConstantCode + 95).toByte
  val SigmaPropBytesCode         : OpCode = (LastConstantCode + 96).toByte
  val BoolToSigmaPropCode        : OpCode = (LastConstantCode + 97 ).toByte
  val TrivialProofCode           : OpCode = (LastConstantCode + 98).toByte  // reserved 99

  // Deserialization codes
  val DeserializeContextCode : OpCode = (LastConstantCode + 100).toByte
  val DeserializeRegisterCode: OpCode = (LastConstantCode + 101).toByte  // Block codes
  val ValDefCode: OpCode = (LastConstantCode + 102).toByte
  val FunDefCode: OpCode = (LastConstantCode + 103).toByte
  val BlockValueCode: OpCode = (LastConstantCode + 104).toByte
  val FuncValueCode: OpCode = (LastConstantCode + 105).toByte // reserved 106 - 109

  val SomeValueCode: OpCode = (LastConstantCode + 110).toByte
  val NoneValueCode: OpCode = (LastConstantCode + 111).toByte

  val GetVarCode         : OpCode = (LastConstantCode + 115).toByte
  val OptionGetCode      : OpCode = (LastConstantCode + 116).toByte
  val OptionGetOrElseCode: OpCode = (LastConstantCode + 117).toByte
  val OptionIsDefinedCode: OpCode = (LastConstantCode + 118).toByte
}
