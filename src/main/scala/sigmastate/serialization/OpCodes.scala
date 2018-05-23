package sigmastate.serialization

/** Encoding of types for serialization. */
trait TypeCodes {
  val FirstDataType = 1.toByte
  val LastDataType = 111.toByte
  val FirstFuncType = LastDataType + 1.toByte
  val LastFuncType = 255.toByte
}

/** Encoding of values for serialization. */
trait ValueCodes extends TypeCodes {
  val ConstantCode: Byte = (LastFuncType - LastDataType + 1).toByte
}

object OpCodes extends ValueCodes {

  type OpCode = Byte

  // serialization is not required
  val Undefined: OpCode = 0: Byte

  // variable
  val TaggedVariableCode: OpCode = 1: Byte

  // EvaluatedValue descendants
  val ByteConstantCode:          OpCode = 10: Byte
  val IntConstantCode:          OpCode = 11: Byte
  val TrueCode:                 OpCode = 12: Byte
  val FalseCode:                OpCode = 13: Byte
  val UnitConstantCode:         OpCode = 14: Byte
  val BigIntConstantCode:       OpCode = 15: Byte
  val CollectionConstantCode:   OpCode = 16: Byte
  val GroupElementConstantCode: OpCode = 17: Byte
  val GroupGeneratorCode      : OpCode = 18: Byte
  val BoxConstantCode         : OpCode = 19: Byte
  val ConcreteCollectionCode  : OpCode = 20: Byte
  val TupleCode               : OpCode = 21: Byte
  val SomeValueCode           : OpCode = 22: Byte
  val NoneValueCode           : OpCode = 23: Byte
  val AvlTreeConstantCode     : OpCode = 24: Byte

  // Relation descendants
  val LtCode : OpCode = 31: Byte
  val LeCode : OpCode = 32: Byte
  val GtCode : OpCode = 33: Byte
  val GeCode : OpCode = 34: Byte
  val EqCode : OpCode = 35: Byte
  val NeqCode: OpCode = 36: Byte
  val IfCode : OpCode = 37: Byte
  val AndCode: OpCode = 38: Byte
  val OrCode : OpCode = 39: Byte

  // Arithmetic codes
  val MinusCode        : OpCode = 41: Byte
  val PlusCode         : OpCode = 42: Byte
  val XorCode          : OpCode = 43: Byte
  val MultiplyCode     : OpCode = 44: Byte
  val DivisionCode     : OpCode = 45: Byte
  val ModuloCode       : OpCode = 46: Byte
  val ExponentiateCode : OpCode = 47: Byte
  val MultiplyGroupCode: OpCode = 48: Byte

  // Environment codes
  val HeightCode               : OpCode = 51: Byte
  val InputsCode               : OpCode = 52: Byte
  val OutputsCode              : OpCode = 53: Byte
  val LastBlockUtxoRootHashCode: OpCode = 54: Byte
  val SelfCode                 : OpCode = 55: Byte

  // Collection and tree operations codes
  val MapCollectionCode: OpCode = 61: Byte
  val ExistsCode       : OpCode = 62: Byte
  val ForAllCode       : OpCode = 63: Byte
  val FoldCode         : OpCode = 64: Byte
  val SizeOfCode       : OpCode = 65: Byte
  val ByIndexCode      : OpCode = 66: Byte
  val AppendCode       : OpCode = 67: Byte
  val AppendBytesCode  : OpCode = 68: Byte
  val SliceCode        : OpCode = 69: Byte
  val WhereCode        : OpCode = 70: Byte
  val IsMemberCode     : OpCode = 71: Byte

  // Type casts codes
  val ExtractAmountCode        : OpCode = 81: Byte
  val ExtractScriptBytesCode   : OpCode = 82: Byte
  val ExtractBytesCode         : OpCode = 83: Byte
  val ExtractBytesWithNoRefCode: OpCode = 84: Byte
  val ExtractIdCode            : OpCode = 85: Byte
  val ExtractRegisterAs        : OpCode = 86: Byte
  val IntToByteArrayCode       : OpCode = 87: Byte
  val ByteArrayToBigIntCode    : OpCode = 88: Byte

  // Cryptographic operations codes
  val CalcBlake2b256Code         : OpCode = 91: Byte
  val CalcSha256Code             : OpCode = 92: Byte
  val ProveDlogCode              : OpCode = 93: Byte
  val ProveDiffieHellmanTupleCode: OpCode = 94: Byte

  // Deserialization codes
  val DeserializeContextCode : OpCode = 100: Byte
  val DeserializeRegisterCode: OpCode = 101: Byte

}
