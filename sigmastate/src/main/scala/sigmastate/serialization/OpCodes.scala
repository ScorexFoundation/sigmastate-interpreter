package sigmastate.serialization

import scalan.util.Extensions.ByteOps
import sigmastate.serialization.OpCodes.OpCode
import supertagged.TaggedType

/** Encoding of types for serialization. */
trait TypeCodes {
  /** Decoding of types depends on the first byte and in general is a recursive procedure
    * consuming some number of bytes from Reader.
    * All data types are recognized by the first byte falling in the region [FirstDataType .. LastDataType] */
  val FirstDataType: OpCode = OpCode @@ 1.toByte
  val LastDataType : OpCode = OpCode @@ 111.toByte

  /** SFunc types occupy remaining space of byte values [FirstFuncType .. 255] */
  val FirstFuncType: OpCode = OpCode @@ (LastDataType + 1).toByte
  val LastFuncType : OpCode = OpCode @@ 255.toByte
}

/** Encoding of values for serialization. */
trait ValueCodes extends TypeCodes {
  /** We use optimized encoding of constant values to save space in serialization.
    * Since Box registers are stored as Constant nodes we save 1 byte for each register.
    * This is due to convention that Value.opCode falling in [1..LastDataType] region is a constant.
    * Thus, we can just decode an instance of SType and then decode data using DataSerializer.
    *
    * Decoding of constants depends on the first byte and in general is a recursive procedure
    * consuming some number of bytes from Reader.
    * */
  val ConstantCode: OpCode = OpCode @@ 0.toByte

  /** The last constant code is equal to FirstFuncType which represent generic function type.
    * We use this single code to represent all functional constants, since we don't have enough space in single byte.
    * Subsequent bytes have to be read from Reader in order to decode the type of the function and the corresponding data. */
  val LastConstantCode: OpCode = OpCode @@ (LastDataType + 1).toByte
}

/** The set of all possible IR graph nodes can be split in two subsets:
  * 1) operations which may appear in ErgoTree (these are defined by `OpCodes` below)
  * 2) operations which are not valid to be in ErgoTree, but serve special purposes. (these are defined by `OpCodesExtra`)
  * We can assume they are both Byte-sized codes, and store as a single byte, but as long as we can differentiate them
  * from context (and where we cannot, we should use special encoding).
  *
  * The general extended encoding is like the following:
  * 0-255 - range of OpCodes
  * 256-511 - range of OpCodesExtra
  * Thus, any code in an extended code range of 0-511 can be saved using `putUShort`.
  * We use Byte to represent OpCodes and OpCodesExtra.
  * We use Short to represent any op code from extended code range.
  * And we use VLQ to serialize Short values of extended codes.
  *
  * Examples:
  * 1) For validation rule CheckValidOpCode we use OpCodes range, so we use single byte encoding.
  * 2) For CheckCostFuncOperation we use 1-511 range and extended encoding (see docs)
  */
object OpCodes extends ValueCodes {

  object OpCode extends TaggedType[Byte]
  type OpCode = OpCode.Type

  object OpCodeExtra extends TaggedType[Short]
  type OpCodeExtra = OpCodeExtra.Type

  def toExtra(oc: OpCode): OpCodeExtra = OpCodeExtra @@ oc.toUByte.toShort
  private def newOpCode(shift: Short): OpCode = OpCode @@ (LastConstantCode + shift).toByte

  // serialization is not required
  val Undefined: OpCode = OpCode @@ 0.toByte

  // variables
  val TaggedVariableCode: OpCode = newOpCode(1)
  val ValUseCode: OpCode = newOpCode(2)
  val ConstantPlaceholderCode: OpCode = newOpCode(3)
  val SubstConstantsCode: OpCode = newOpCode(4) // reserved 5 - 9 (5)

  val LongToByteArrayCode  : OpCode = newOpCode(10)
  val ByteArrayToBigIntCode: OpCode = newOpCode(11)
  val ByteArrayToLongCode  : OpCode = newOpCode(12)
  val DowncastCode         : OpCode = newOpCode(13)
  val UpcastCode           : OpCode = newOpCode(14)

  // EvaluatedValue descendants
  val TrueCode              : OpCode = newOpCode(15)
  val FalseCode             : OpCode = newOpCode(16)
  val UnitConstantCode      : OpCode = newOpCode(17)
  val GroupGeneratorCode    : OpCode = newOpCode(18)
  val ConcreteCollectionCode: OpCode = newOpCode(19) // reserved 20 (1)
  val ConcreteCollectionBooleanConstantCode: OpCode = newOpCode(21)

  val TupleCode      : OpCode = newOpCode(22)
  val Select1Code    : OpCode = newOpCode(23)
  val Select2Code    : OpCode = newOpCode(24)
  val Select3Code    : OpCode = newOpCode(25)
  val Select4Code    : OpCode = newOpCode(26)
  val Select5Code    : OpCode = newOpCode(27)
  val SelectFieldCode: OpCode = newOpCode(28) // reserved 29-30 (2)

  // Relation descendants
  val LtCode     : OpCode = newOpCode(31)
  val LeCode     : OpCode = newOpCode(32)
  val GtCode     : OpCode = newOpCode(33)
  val GeCode     : OpCode = newOpCode(34)
  val EqCode     : OpCode = newOpCode(35)
  val NeqCode    : OpCode = newOpCode(36)
  val IfCode     : OpCode = newOpCode(37)
  val AndCode    : OpCode = newOpCode(38)
  val OrCode     : OpCode = newOpCode(39)
  val AtLeastCode: OpCode = newOpCode(40)

  // Arithmetic codes
  val MinusCode        : OpCode = newOpCode(41)
  val PlusCode         : OpCode = newOpCode(42)
  val XorCode          : OpCode = newOpCode(43)
  val MultiplyCode     : OpCode = newOpCode(44)
  val DivisionCode     : OpCode = newOpCode(45)
  val ModuloCode       : OpCode = newOpCode(46)
  val ExponentiateCode : OpCode = newOpCode(47)
  val MultiplyGroupCode: OpCode = newOpCode(48)
  val MinCode          : OpCode = newOpCode(49)
  val MaxCode          : OpCode = newOpCode(50)

  // Environment codes
  val HeightCode               : OpCode = newOpCode(51)
  val InputsCode               : OpCode = newOpCode(52)
  val OutputsCode              : OpCode = newOpCode(53)
  val LastBlockUtxoRootHashCode: OpCode = newOpCode(54)
  val SelfCode                 : OpCode = newOpCode(55) // reserved 56 - 59 (4)

  val MinerPubkeyCode          : OpCode = newOpCode(60)

  // Collection and tree operations codes
  val MapCollectionCode    : OpCode = newOpCode(61)
  val ExistsCode           : OpCode = newOpCode(62)
  val ForAllCode           : OpCode = newOpCode(63)
  val FoldCode             : OpCode = newOpCode(64)
  val SizeOfCode           : OpCode = newOpCode(65)
  val ByIndexCode          : OpCode = newOpCode(66)
  val AppendCode           : OpCode = newOpCode(67)
  val SliceCode            : OpCode = newOpCode(68)
  val FilterCode           : OpCode = newOpCode(69)
  val AvlTreeCode          : OpCode = newOpCode(70)
  val AvlTreeGetCode       : OpCode = newOpCode(71)
  val FlatMapCollectionCode: OpCode = newOpCode(72) // reserved 73 - 80 (8)

  // Type casts codes
  val ExtractAmountCode        : OpCode = newOpCode(81)
  val ExtractScriptBytesCode   : OpCode = newOpCode(82)
  val ExtractBytesCode         : OpCode = newOpCode(83)
  val ExtractBytesWithNoRefCode: OpCode = newOpCode(84)
  val ExtractIdCode            : OpCode = newOpCode(85)
  val ExtractRegisterAs        : OpCode = newOpCode(86)
  val ExtractCreationInfoCode  : OpCode = newOpCode(87) // reserved 88 - 90 (3)

  // Cryptographic operations codes
  val CalcBlake2b256Code         : OpCode = newOpCode(91)
  val CalcSha256Code             : OpCode = newOpCode(92)
  val ProveDlogCode              : OpCode = newOpCode(93)
  val ProveDiffieHellmanTupleCode: OpCode = newOpCode(94)
  // 95 reserved for deprecated SigmaPropIsProvenCode
  val SigmaPropBytesCode         : OpCode = newOpCode(96)
  val BoolToSigmaPropCode        : OpCode = newOpCode(97)
  // we don't rely on this yet but it's nice to have TrivialPropFalseCode.toUByte < TrivialPropTrueCode.toUByte
  val TrivialPropFalseCode       : OpCode = newOpCode(98)
  val TrivialPropTrueCode        : OpCode = newOpCode(99)

  // Deserialization codes
  val DeserializeContextCode : OpCode = newOpCode(100)
  val DeserializeRegisterCode: OpCode = newOpCode(101) // Block codes
  val ValDefCode: OpCode = newOpCode(102)
  val FunDefCode: OpCode = newOpCode(103)
  val BlockValueCode: OpCode = newOpCode(104)
  val FuncValueCode: OpCode = newOpCode(105)
  val FuncApplyCode: OpCode = newOpCode(106)
  val PropertyCallCode: OpCode = newOpCode(107)
  val MethodCallCode: OpCode = newOpCode(108)
  val GlobalCode    : OpCode = newOpCode(109)

  val SomeValueCode: OpCode = newOpCode(110)
  val NoneValueCode: OpCode = newOpCode(111) // reserved 112 - 114 (3)

  val GetVarCode         : OpCode = newOpCode(115)
  val OptionGetCode      : OpCode = newOpCode(116)
  val OptionGetOrElseCode: OpCode = newOpCode(117)
  val OptionIsDefinedCode: OpCode = newOpCode(118)

  // Modular arithmetic operations codes
  val ModQCode     : OpCode = newOpCode(119)
  val PlusModQCode : OpCode = newOpCode(120)
  val MinusModQCode: OpCode = newOpCode(121)

  val SigmaAndCode : OpCode = newOpCode(122)
  val SigmaOrCode  : OpCode = newOpCode(123)
  val BinOrCode    : OpCode = newOpCode(124)
  val BinAndCode   : OpCode = newOpCode(125)

  val DecodePointCode: OpCode = newOpCode(126)

  val LogicalNotCode : OpCode = newOpCode(127)
  val NegationCode   : OpCode = newOpCode(128)
  val BitInversionCode   : OpCode = newOpCode(129)
  val BitOrCode      : OpCode = newOpCode(130)
  val BitAndCode     : OpCode = newOpCode(131)

  val BinXorCode     : OpCode = newOpCode(132)

  val BitXorCode     : OpCode = newOpCode(133)
  val BitShiftRightCode    : OpCode = newOpCode(134)
  val BitShiftLeftCode     : OpCode = newOpCode(135)
  val BitShiftRightZeroedCode     : OpCode = newOpCode(136)

  val CollShiftRightCode    : OpCode = newOpCode(137)
  val CollShiftLeftCode     : OpCode = newOpCode(138)
  val CollShiftRightZeroedCode     : OpCode = newOpCode(139)

  val CollRotateLeftCode     : OpCode = newOpCode(140)
  val CollRotateRightCode     : OpCode = newOpCode(141)

  val ContextCode             : OpCode = newOpCode(142)
  val XorOfCode               : OpCode = newOpCode(143) // equals to 255

  // OpCodesExtra (range of 256-511)
  private def newOpCodeExtra(shift: Short): OpCodeExtra = OpCodeExtra @@ (256 + shift).toShort

  val OpCostCode: OpCodeExtra         = newOpCodeExtra(1)
  val PerKbCostOfCode: OpCodeExtra    = newOpCodeExtra(2)
  val CastCode: OpCodeExtra           = newOpCodeExtra(3)
  val IntPlusMonoidCode: OpCodeExtra  = newOpCodeExtra(4)
  val ThunkDefCode: OpCodeExtra       = newOpCodeExtra(5)
  val SCMInputsCode: OpCodeExtra      = newOpCodeExtra(6)
  val SCMOutputsCode: OpCodeExtra     = newOpCodeExtra(7)
  val SCMDataInputsCode: OpCodeExtra  = newOpCodeExtra(8)
  val SCMSelfBoxCode: OpCodeExtra     = newOpCodeExtra(9)
  val SCMLastBlockUtxoRootHashCode: OpCodeExtra = newOpCodeExtra(10)
  val SCMHeadersCode: OpCodeExtra     = newOpCodeExtra(11)
  val SCMPreHeaderCode: OpCodeExtra   = newOpCodeExtra(12)
  val SCMGetVarCode: OpCodeExtra      = newOpCodeExtra(13)
  val SBMPropositionBytesCode: OpCodeExtra = newOpCodeExtra(14)
  val SBMBytesCode: OpCodeExtra       = newOpCodeExtra(15)
  val SBMBytesWithoutRefCode: OpCodeExtra = newOpCodeExtra(16)
  val SBMRegistersCode: OpCodeExtra   = newOpCodeExtra(17)
  val SBMGetRegCode: OpCodeExtra      = newOpCodeExtra(18)
  val SBMTokensCode: OpCodeExtra      = newOpCodeExtra(19)
  val SSPMPropBytesCode: OpCodeExtra  = newOpCodeExtra(20)
  val SAVMTValCode: OpCodeExtra       = newOpCodeExtra(21)
  val SAVMValueSizeCode: OpCodeExtra  = newOpCodeExtra(22)
  val SizeMDataSizeCode: OpCodeExtra  = newOpCodeExtra(23)
  val SPairLCode: OpCodeExtra         = newOpCodeExtra(24)
  val SPairRCode: OpCodeExtra         = newOpCodeExtra(25)
  val SCollMSizesCode: OpCodeExtra    = newOpCodeExtra(26)
  val SOptMSizeOptCode: OpCodeExtra   = newOpCodeExtra(27)
  val SFuncMSizeEnvCode: OpCodeExtra  = newOpCodeExtra(28)
  val CSizePairCtorCode: OpCodeExtra  = newOpCodeExtra(29)
  val CSizeFuncCtorCode: OpCodeExtra  = newOpCodeExtra(30)
  val CSizeOptionCtorCode: OpCodeExtra = newOpCodeExtra(31)
  val CSizeCollCtorCode: OpCodeExtra  = newOpCodeExtra(32)
  val CSizeBoxCtorCode: OpCodeExtra   = newOpCodeExtra(33)
  val CSizeContextCtorCode: OpCodeExtra = newOpCodeExtra(34)
  val CSizeAnyValueCtorCode: OpCodeExtra = newOpCodeExtra(35)
  val CReplCollCtorCode: OpCodeExtra  = newOpCodeExtra(36)
  val CollMSumCode: OpCodeExtra       = newOpCodeExtra(37)
  val PairOfColsCtorCode: OpCodeExtra = newOpCodeExtra(37)
  val CBMReplicateCode: OpCodeExtra   = newOpCodeExtra(38)
  val CBMFromItemsCode: OpCodeExtra   = newOpCodeExtra(39)
  val CostOfCode: OpCodeExtra         = newOpCodeExtra(40)
  val UOSizeOfCode: OpCodeExtra       = newOpCodeExtra(41)
  val SPCMSomeCode: OpCodeExtra       = newOpCodeExtra(42)
  val ThunkForceCode: OpCodeExtra     = newOpCodeExtra(43)
}
