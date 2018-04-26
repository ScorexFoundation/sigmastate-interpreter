package sigmastate.serialization

object OpCodes {

  type OpCode = Byte

  val TaggedVariableCode:   OpCode = 1: Byte

  // EvaluatedValue descendants

  val IntConstantCode:          OpCode = 11: Byte
  val TrueCode:                 OpCode = 12: Byte
  val FalseCode:                OpCode = 13: Byte
  val UnitConstantCode:         OpCode = 14: Byte
  val BigIntConstantCode:       OpCode = 15: Byte
  val ByteArrayConstantCode:    OpCode = 16: Byte
  val GroupElementConstantCode: OpCode = 17: Byte


  // Relation descendants

  val LtCode:     OpCode = 21: Byte
  val LeCode:     OpCode = 22: Byte
  val GtCode:     OpCode = 23: Byte
  val GeCode:     OpCode = 24: Byte
  val EqCode:     OpCode = 25: Byte
  val NeqCode:    OpCode = 26: Byte

  val ConcreteCollectionCode: OpCode = 35: Byte

  val TupleCode:  OpCode = 36: Byte
  val AndCode:    OpCode = 37: Byte
  val OrCode:     OpCode = 38: Byte
  val NotCode:    OpCode = 39: Byte

  // TwoArgumentsOperation descendants

  val MinusCode:          OpCode = 40: Byte
  val PlusCode:           OpCode = 41: Byte
  val XorCode:            OpCode = 42: Byte
  val AppendBytesCode:    OpCode = 43: Byte
  val ExponentiateCode:   OpCode = 44: Byte
  val MultiplyGroupCode:  OpCode = 45: Byte

  val SomeValueCode: OpCode = 50: Byte
  val NoneValueCode: OpCode = 51: Byte

  val ProveDlogCode:   OpCode = 60: Byte
  val HeightCode:      OpCode = 61: Byte

  val MapCollectionCode:         OpCode = 70: Byte
  val ExistsCode:                OpCode = 71: Byte
  val ForAllCode:                OpCode = 72: Byte
  val FoldCode:                  OpCode = 73: Byte
  val SizeOfCode:                OpCode = 74: Byte
  val ExtractAmountCode:         OpCode = 75: Byte
  val ExtractScriptBytesCode:    OpCode = 76: Byte
  val ExtractBytesCode:          OpCode = 77: Byte
  val ExtractBytesWithNoRefCode: OpCode = 78: Byte
  val ExtractIdCode:             OpCode = 79: Byte
  val ExtractRegisterAs:         OpCode = 80: Byte


}
