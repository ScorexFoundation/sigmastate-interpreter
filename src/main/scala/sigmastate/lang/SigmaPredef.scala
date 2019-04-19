package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scalan.Nullable
import scorex.util.encode.{Base64, Base58}
import sigmastate.SCollection.{SIntArray, SByteArray}
import sigmastate.SOption._
import sigmastate.Values.{StringConstant, Constant, EvaluatedValue, SValue, IntValue, SigmaPropConstant, BoolValue, Value, ByteArrayConstant, SigmaPropValue, ValueCompanion}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.InvalidArguments
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.{GetVar, DeserializeContext, SelectField}

object SigmaPredef {

  type IrBuilderFunc = PartialFunction[(SValue, Seq[SValue]), SValue]

  /** Metadata for predefined function.
    * @param irBuilder Builder of SigmaIR node which is equivalent to function application
    *                  Rule: Apply(f, args) -->  irBuilder(f, args)
    * @param opDesc    ErgoTree node descriptor
    */
  case class PredefFuncInfo(irBuilder: IrBuilderFunc)

  case class PredefinedFunc(
    /** A name which is used in scripts */
    name: String,
    /** Function declaration without body */
    declaration: Lambda,
    /** Metadata for this function */
    irInfo: PredefFuncInfo,
    docInfo: OperationInfo) {

    val sym: Ident = Ident(name, declaration.tpe)
    val symNoType: Ident = Ident(name, NoType)

    def argInfo(argName: String): ArgInfo =
      docInfo.args.find(_.name == argName).get
  }

  class PredefinedFuncRegistry(builder: SigmaBuilder) {

    import builder._

    /** Type variable used in the signatures of global functions below. */
    private val tT = STypeIdent("T")
    private val tK = STypeIdent("K")
    private val tL = STypeIdent("L")
    private val tR = STypeIdent("R")
    private val tO = STypeIdent("O")

    private val undefined: IrBuilderFunc =
      PartialFunction.empty[(SValue, Seq[SValue]), SValue]

    val AllOfFunc = PredefinedFunc("allOf",
      Lambda(IndexedSeq("conditions" -> SCollection(SBoolean)), SBoolean, None),
      PredefFuncInfo({ case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkAND(col) }),
      OperationInfo(AND, "Returns true if \\emph{all} the elements in collection are \\lst{true}.",
       Seq(ArgInfo("conditions", "a collection of conditions")))
    )

    val AnyOfFunc = PredefinedFunc("anyOf",
      Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
      PredefFuncInfo( { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkOR(col) }),
      OperationInfo(OR, "Returns true if \\emph{any} the elements in collection are \\lst{true}.",
        Seq(ArgInfo("conditions", "a collection of conditions")))
    )

    val XorOfFunc = PredefinedFunc("xorOf",
      Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
      PredefFuncInfo({ case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkXorOf(col) }),
      OperationInfo(XorOf, "Similar to \\lst{allOf}, but performing logical XOR operation between all conditions instead of \\lst{&&}",
          Seq(ArgInfo("conditions", "a collection of conditions")))
    )

    val AtLeastFunc = PredefinedFunc("atLeast",
      Lambda(Vector("k" -> SInt, "conditions" -> SCollection(SSigmaProp)), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(bound: IntValue@unchecked, arr: Value[SCollection[SSigmaProp.type]]@unchecked)) =>
          mkAtLeast(bound, arr)
        }),
      OperationInfo(AtLeast,
        """ Logical threshold.
         | AtLeast has two inputs: integer \\lst{bound\ and \\lst{children} same as in AND/OR.
         | The result is true if at least \\lst{bound} children are proven.
        """.stripMargin, Seq(
          ArgInfo("bound", "required minimum of proven children"),
          ArgInfo("children", "proposition to be proven/validated")))
    )

    val OuterJoinFunc = PredefinedFunc(
      "outerJoin",
      Lambda(
        Seq(STypeParam(tK), STypeParam(tL), STypeParam(tR), STypeParam(tO)),
        Vector(
          "left" -> SCollection(STuple(tK, tL)),
          "right" -> SCollection(STuple(tK, tR)),
          "l" -> SFunc(IndexedSeq(tK, tL), tO),
          "r" -> SFunc(IndexedSeq(tK, tR), tO),
          "inner" -> SFunc(IndexedSeq(tK, tL, tR), tO),
        ),
        SCollection(STuple(tK, tO)), None),
      PredefFuncInfo(undefined),
      OperationInfo(MethodCall, "",
          Seq(ArgInfo("", "")))
    )

    val ZKProofFunc = PredefinedFunc("ZKProof",
      Lambda(Vector("block" -> SSigmaProp), SBoolean, None),
      PredefFuncInfo({ case (_, Seq(block: SigmaPropValue@unchecked)) => mkZKProofBlock(block) }),
      OperationInfo(ZKProofBlock, "",
          Seq(ArgInfo("", "")))
    )

    val SigmaPropFunc = PredefinedFunc("sigmaProp",
      Lambda(Vector("condition" -> SBoolean), SSigmaProp, None),
      PredefFuncInfo({ case (_, Seq(b: BoolValue@unchecked)) => mkBoolToSigmaProp(b) }),
      OperationInfo(BoolToSigmaProp,
        """Embedding of \\lst{Boolean} values to \\lst{SigmaProp} values.
         | As an example, this operation allows boolean experessions
         | to be used as arguments of \\lst{atLeast(..., sigmaProp(boolExpr), ...)} operation.
         | During execution results to either \\lst{TrueProp} or \\lst{FalseProp} values of \\lst{SigmaProp} type.
        """.stripMargin,
          Seq(ArgInfo("condition", "boolean value to embed in SigmaProp value")))
    )

    val GetVarFunc = PredefinedFunc("getVar",
      Lambda(Seq(STypeParam(tT)), Vector("varId" -> SByte), SOption(tT), None),
      PredefFuncInfo(
        { case (Ident(_, SFunc(_, SOption(rtpe), _)), Seq(id: Constant[SNumericType]@unchecked)) =>
          mkGetVar(SByte.downcast(id.value.asInstanceOf[AnyVal]), rtpe)
        }),
      OperationInfo(GetVar,
        "Get context variable with given \\lst{varId} and type.",
        Seq(ArgInfo("varId", "\\lst{Byte} identifier of context variable")))
    )

    def PKFunc(networkPrefix: NetworkPrefix) = PredefinedFunc("PK",
      Lambda(Vector("input" -> SString), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ErgoAddressEncoder(networkPrefix).fromString(arg.value).get match {
            case a: P2PKAddress => SigmaPropConstant(a.pubkey)
            case a@_ => sys.error(s"unsupported address $a")
          }
        }),
      OperationInfo(Constant, "",
          Seq(ArgInfo("", "")))
    )

    val DeserializeFunc = PredefinedFunc("deserialize",
      Lambda(Seq(STypeParam(tT)), Vector("str" -> SString), tT, None),
      PredefFuncInfo(
      { case (Ident(_, SFunc(_, tpe, _)), args) =>
        if (args.length != 1)
          throw new InvalidArguments(s"Wrong number of arguments in $args: expected one argument")
        val str = args.head match {
          case StringConstant(s) => s
          case _ =>
            throw new InvalidArguments(s"invalid argument in $args: expected a string constant")
        }
        val bytes = Base58.decode(str).get
        val res = ValueSerializer.deserialize(bytes)
        if (res.tpe != tpe)
          throw new InvalidArguments(s"Wrong type after deserialization, expected $tpe, got ${res.tpe}")
        res
      }),
      OperationInfo(Constant, "",
          Seq(ArgInfo("", "")))
    )

    val FromBase58Func = PredefinedFunc("fromBase58",
      Lambda(Vector("input" -> SString), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ByteArrayConstant(Base58.decode(arg.value).get)
        }),
      OperationInfo(Constant, "",
          Seq(ArgInfo("", "")))
    )

    val FromBase64Func = PredefinedFunc("fromBase64",
      Lambda(Vector("input" -> SString), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ByteArrayConstant(Base64.decode(arg.value).get)
        }),
      OperationInfo(Constant, "",
          Seq(ArgInfo("", "")))
    )

    val Blake2b256Func = PredefinedFunc("blake2b256",
      Lambda(Vector("input" -> SByteArray), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkCalcBlake2b256(arg)
        }),
      OperationInfo(CalcBlake2b256, "Calculate Blake2b hash from \\lst{input} bytes.",
          Seq(ArgInfo("input", "collection of bytes")))
    )

    val Sha256Func = PredefinedFunc("sha256",
      Lambda(Vector("input" -> SByteArray), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkCalcSha256(arg)
        }),
      OperationInfo(CalcSha256, "Calculate Sha256 hash from \\lst{input} bytes.",
          Seq(ArgInfo("input", "collection of bytes")))
    )

    val ByteArrayToBigIntFunc = PredefinedFunc("byteArrayToBigInt",
      Lambda(Vector("input" -> SByteArray), SBigInt, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkByteArrayToBigInt(arg)
        }),
      OperationInfo(ByteArrayToBigInt,
        "Convert big-endian bytes representation (Coll[Byte]) to BigInt value.",
        Seq(ArgInfo("input", "collection of bytes in big-endian format")))
    )

    val ByteArrayToLongFunc = PredefinedFunc("byteArrayToLong",
      Lambda(Vector("input" -> SByteArray), SLong, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkByteArrayToLong(arg)
        }),
      OperationInfo(ByteArrayToLong, "Convert big-endian bytes representation (Coll[Byte]) to Long value.",
          Seq(ArgInfo("input", "collection of bytes in big-endian format")))
    )

    val DecodePointFunc = PredefinedFunc("decodePoint",
      Lambda(Vector("input" -> SByteArray), SGroupElement, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkDecodePoint(arg)
        }),
      OperationInfo(DecodePoint,
        "Convert \\lst{Coll[Byte]} to \\lst{GroupElement} using \\lst{GroupElementSerializer}",
          Seq(ArgInfo("input", "serialized bytes of some \\lst{GroupElement} value")))
    )

    val LongToByteArrayFunc = PredefinedFunc("longToByteArray",
      Lambda(Vector("input" -> SLong), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SLong.type]@unchecked)) =>
          mkLongToByteArray(arg)
        }),
      OperationInfo(LongToByteArray,
        "Converts \\lst{Long} value to big-endian bytes representation.",
        Seq(ArgInfo("input", "value to convert")))
    )

    val ProveDHTupleFunc = PredefinedFunc("proveDHTuple",
      Lambda(Vector("g" -> SGroupElement, "h" -> SGroupElement, "u" -> SGroupElement, "v" -> SGroupElement), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(g, h, u, v)) =>
            mkCreateProveDHTuple(g.asGroupElement, h.asGroupElement, u.asGroupElement, v.asGroupElement)
        }),
      OperationInfo(CreateProveDHTuple,
        """ ErgoTree operation to create a new SigmaProp value representing public key
         | of Diffie Hellman signature protocol.
         | Common input: (g,h,u,v)
        """.stripMargin,
          Seq(ArgInfo("g", ""),ArgInfo("h", ""),ArgInfo("u", ""),ArgInfo("v", "")))
    )

    val ProveDlogFunc = PredefinedFunc("proveDlog",
      Lambda(Vector("value" -> SGroupElement), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SGroupElement.type]@unchecked)) =>
          mkCreateProveDlog(arg)
        }),
      OperationInfo(CreateProveDlog,
        """ErgoTree operation to create a new \\lst{SigmaProp} value representing public key
         | of discrete logarithm signature protocol.
        """.stripMargin,
          Seq(ArgInfo("value", "element of elliptic curve group")))
    )

    val AvlTreeFunc = PredefinedFunc("avlTree",
      Lambda(Vector("operationFlags" -> SByte, "digest" -> SByteArray, "keyLength" -> SInt, "valueLengthOpt" -> SIntOption), SAvlTree, None),
      PredefFuncInfo(
        { case (_, Seq(flags, digest, keyLength, valueLength)) =>
          mkCreateAvlTree(flags.asByteValue, digest.asByteArray, keyLength.asIntValue, valueLength.asOption[SInt.type])
        }),
      OperationInfo(CreateAvlTree,
        "Construct a new authenticated dictionary with given parameters and tree root digest.",
        Seq(
          ArgInfo("operationFlags", "flags of available operations"),
          ArgInfo("digest", "hash of merkle tree root"),
          ArgInfo("keyLength", "length of dictionary keys in bytes"),
          ArgInfo("valueLengthOpt", "optional width of dictionary values in bytes")))
    )

    val SubstConstantsFunc = PredefinedFunc("substConstants",
      Lambda(
        Seq(STypeParam(tT)),
        Vector("scriptBytes" -> SByteArray, "positions" -> SIntArray, "newValues" -> SCollection(tT)),
        SByteArray, None
      ),
      PredefFuncInfo( undefined),
      OperationInfo(SubstConstants,
        """Transforms serialized bytes of ErgoTree with segregated constants by replacing constants
         | at given positions with new values. This operation allow to use serialized scripts as
         | pre-defined templates.
         | The typical usage is "check that output box have proposition equal to given script bytes,
         | where minerPk (constants(0)) is replaced with currentMinerPk".
         | Each constant in original scriptBytes have SType serialized before actual data (see ConstantSerializer).
         | During substitution each value from newValues is checked to be an instance of the corresponding type.
         | This means, the constants during substitution cannot change their types.
         |
         | Returns original scriptBytes array where only specified constants are replaced and all other bytes remain exactly the same.
        """.stripMargin, Seq(
        ArgInfo("scriptBytes", "serialized ErgoTree with ConstantSegregationFlag set to 1."),
        ArgInfo("positions", "zero based indexes in ErgoTree.constants array which should be replaced with new values"),
        ArgInfo("newValues", "new values to be injected into the corresponding positions in ErgoTree.constants array")))
    )

    val ExecuteFromVarFunc = PredefinedFunc("executeFromVar",
      Lambda(
        Seq(STypeParam(tT)),
        Vector("id" -> SByte),
        tT, None
      ),
      PredefFuncInfo(undefined),
      OperationInfo(DeserializeContext,
        """Extracts context variable as \\lst{Coll[Byte]}, deserializes it to script
         | and then executes this script in the current context.
         | The original \\lst{Coll[Byte]} of the script is available as \\lst{getVar[Coll[Byte]](id)}.
         | Type parameter \\lst{V} result type of the deserialized script.
         | Throws an exception if the actual script type doesn't conform to T.
         | Returns a result of the script execution in the current context
        """.stripMargin,
        Seq(ArgInfo("id", "identifier of the context variable")))
    )

    val globalFuncs: Map[String, PredefinedFunc] = Seq(
      AllOfFunc,
      AnyOfFunc,
      AtLeastFunc,
      OuterJoinFunc,
      ZKProofFunc,
      SigmaPropFunc,
      GetVarFunc,
      DeserializeFunc,
      FromBase64Func,
      FromBase58Func,
      Blake2b256Func,
      Sha256Func,
      ByteArrayToBigIntFunc,
      ByteArrayToLongFunc,
      DecodePointFunc,
      LongToByteArrayFunc,
      ProveDHTupleFunc,
      ProveDlogFunc,
      AvlTreeFunc,
      XorOfFunc,
      SubstConstantsFunc,
      ExecuteFromVarFunc,
    ).map(f => f.name -> f).toMap

    def binop(symbolName: String, opDesc: ValueCompanion, desc: String, args: Seq[ArgInfo]) = {
      PredefinedFunc(symbolName,
        Lambda(Seq(STypeParam(tT)), Vector("left" -> tT, "right" -> tT), SBoolean, None),
        PredefFuncInfo(undefined),
        OperationInfo(opDesc, desc, args)
      )
    }
    def logical(symbolName: String, opDesc: ValueCompanion, desc: String, args: Seq[ArgInfo]) = {
      PredefinedFunc(symbolName,
        Lambda(Vector("left" -> SBoolean, "right" -> SBoolean), SBoolean, None),
        PredefFuncInfo(undefined),
        OperationInfo(opDesc, desc, args)
      )
    }

    val infixFuncs: Map[String, PredefinedFunc] = Seq(
      binop("==", EQ, "Compare equality of \\lst{left} and \\lst{right} arguments",
          Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binop("!=", NEQ, "Compare inequality of \\lst{left} and \\lst{right} arguments",
          Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),

      binop("<", LT, "",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binop("<=", LE, "",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binop(">", GT, "",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binop(">=", GE, "",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      binop("^", BinXor, "",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),

      logical("||", BinOr, "",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
      logical("&&", BinAnd, "",
        Seq(ArgInfo("left", "left operand"), ArgInfo("right", "right operand"))),
    ).map(f => f.name -> f).toMap

    val funcs: Map[String, PredefinedFunc] = globalFuncs ++ infixFuncs

    /** WARNING: This operations are not used in frontend, and should be be used.
      * They are used in SpecGen only the source of metadata for the corresponding ErgoTree nodes.
      */
    val specialFuncs: Map[String, PredefinedFunc] = Seq(
      PredefinedFunc("selectField",
        Lambda(Seq(STypeParam(tT), STypeParam(tR)), Vector("input" -> tT, "fieldIndex" -> SByte), tR, None),
        PredefFuncInfo(undefined),
        OperationInfo(SelectField,
          "Select tuple field by its 1-based index. E.g. input._1 is transformed to SelectField(input, 1)",
          Seq(ArgInfo("input", "tuple of items"), ArgInfo("fieldIndex", "index of an item to select")))
      ),
      PredefinedFunc("treeLookup",
        Lambda(Vector("tree" -> SAvlTree, "key" -> SByteArray, "proof" -> SByteArray), SOption(SByteArray), None),
        PredefFuncInfo(undefined),
        OperationInfo(TreeLookup,
          "",
          Seq(ArgInfo("tree", "tree to lookup the key"),
          ArgInfo("key", "a key of an item in the \\lst{tree} to lookup"),
          ArgInfo("proof", "proof to perform verification of the operation")))
      ),
      PredefinedFunc("if",
        Lambda(Seq(STypeParam(tT)), Vector("condition" -> SBoolean, "trueBranch" -> tT, "falseBranch" -> tT), tT, None),
        PredefFuncInfo(undefined),
        OperationInfo(If,
          "Compute condition, if true then compute trueBranch else compute falseBranch",
          Seq(ArgInfo("condition", "condition expression"),
          ArgInfo("trueBranch", "expression to execute when \\lst{condition == true}"),
          ArgInfo("falseBranch", "expression to execute when \\lst{condition == false}")))
      )
    ).map(f => f.name -> f).toMap

    private val funcNameToIrBuilderMap: Map[String, PredefinedFunc] =
      funcs.filter { case (n, f) => f.irInfo.irBuilder != undefined }

    def irBuilderForFunc(name: String): Option[IrBuilderFunc] = funcNameToIrBuilderMap.get(name).map(_.irInfo.irBuilder)
  }

  object PredefinedFuncApply {
    def unapply(apply: Apply)(implicit registry: PredefinedFuncRegistry): Nullable[SValue] = apply.func match {
      case Ident(name, _) =>
        registry.irBuilderForFunc(name)
          .filter(_.isDefinedAt(apply.func, apply.args))
          .map(b => Nullable(b(apply.func, apply.args))).getOrElse(Nullable.None)
      case _ => Nullable.None
    }
  }
}
