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
import sigmastate.utxo.{GetVar, DeserializeContext}

object SigmaPredef {

  type IrBuilderFunc = PartialFunction[(SValue, Seq[SValue]), SValue]

  /** Metadata for predefined function.
    * @param irBuilder Builder of SigmaIR node which is equivalent to function application
    *                  Rule: Apply(f, args) -->  irBuilder(f, args)
    * @param opDesc    ErgoTree node descriptor
    */
  case class PredefFuncInfo(irBuilder: IrBuilderFunc, opDesc: ValueCompanion)

  case class PredefinedFunc(
    /** A name which is used in scripts */
    name: String,
    /** Function declaration without body */
    declaration: Lambda,
    /** Metadata for this function */
    info: PredefFuncInfo) {

    val sym: Ident = Ident(name, declaration.tpe)
    val symNoType: Ident = Ident(name, NoType)
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
      PredefFuncInfo(
        { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkAND(col) },
        AND)
    )

    val AnyOfFunc = PredefinedFunc("anyOf",
      Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
      PredefFuncInfo(
        { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkOR(col) },
        OR)
    )

    val XorOfFunc = PredefinedFunc("xorOf",
      Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
      PredefFuncInfo(
        { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkXorOf(col) },
        XorOf)
    )

    val AtLeastFunc = PredefinedFunc("atLeast",
      Lambda(Vector("k" -> SInt, "conditions" -> SCollection(SSigmaProp)), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(bound: IntValue@unchecked, arr: Value[SCollection[SSigmaProp.type]]@unchecked)) =>
          mkAtLeast(bound, arr)
        },
        AtLeast)
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
      PredefFuncInfo(undefined, MethodCall)
    )

    val ZKProofFunc = PredefinedFunc("ZKProof",
      Lambda(Vector("block" -> SSigmaProp), SBoolean, None),
      PredefFuncInfo(
        { case (_, Seq(block: SigmaPropValue@unchecked)) => mkZKProofBlock(block) },
        ZKProofBlock)
    )

    val SigmaPropFunc = PredefinedFunc("sigmaProp",
      Lambda(Vector("condition" -> SBoolean), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(b: BoolValue@unchecked)) => mkBoolToSigmaProp(b) },
        BoolToSigmaProp)
    )

    val GetVarFunc = PredefinedFunc("getVar",
      Lambda(Seq(STypeParam(tT)), Vector("varId" -> SByte), SOption(tT), None),
      PredefFuncInfo(
        { case (Ident(_, SFunc(_, SOption(rtpe), _)), Seq(id: Constant[SNumericType]@unchecked)) =>
          mkGetVar(SByte.downcast(id.value.asInstanceOf[AnyVal]), rtpe)
        },
        GetVar)
    )

    def PKFunc(networkPrefix: NetworkPrefix) = PredefinedFunc("PK",
      Lambda(Vector("input" -> SString), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ErgoAddressEncoder(networkPrefix).fromString(arg.value).get match {
            case a: P2PKAddress => SigmaPropConstant(a.pubkey)
            case a@_ => sys.error(s"unsupported address $a")
          }
        },
        Constant)
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
      },
      Constant)
    )

    val FromBase58Func = PredefinedFunc("fromBase58",
      Lambda(Vector("input" -> SString), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ByteArrayConstant(Base58.decode(arg.value).get)
        },
      Constant)
    )

    val FromBase64Func = PredefinedFunc("fromBase64",
      Lambda(Vector("input" -> SString), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
          ByteArrayConstant(Base64.decode(arg.value).get)
        },
        Constant)
    )

    val Blake2b256Func = PredefinedFunc("blake2b256",
      Lambda(Vector("input" -> SByteArray), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkCalcBlake2b256(arg)
        },
        CalcBlake2b256)
    )

    val Sha256Func = PredefinedFunc("sha256",
      Lambda(Vector("input" -> SByteArray), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkCalcSha256(arg)
        },
        CalcSha256)
    )

    val ByteArrayToBigIntFunc = PredefinedFunc("byteArrayToBigInt",
      Lambda(Vector("input" -> SByteArray), SBigInt, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkByteArrayToBigInt(arg)
        }, ByteArrayToBigInt)
    )

    val ByteArrayToLongFunc = PredefinedFunc("byteArrayToLong",
      Lambda(Vector("input" -> SByteArray), SLong, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkByteArrayToLong(arg)
        },
        ByteArrayToLong)
    )

    val DecodePointFunc = PredefinedFunc("decodePoint",
      Lambda(Vector("input" -> SByteArray), SGroupElement, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
          mkDecodePoint(arg)
        },
        DecodePoint)
    )

    val LongToByteArrayFunc = PredefinedFunc("longToByteArray",
      Lambda(Vector("input" -> SLong), SByteArray, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SLong.type]@unchecked)) =>
          mkLongToByteArray(arg)
        },
        LongToByteArray)
    )

    val ProveDHTupleFunc = PredefinedFunc("proveDHTuple",
      Lambda(Vector("g" -> SGroupElement, "h" -> SGroupElement, "u" -> SGroupElement, "v" -> SGroupElement), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(g, h, u, v)) =>
            mkCreateProveDHTuple(g.asGroupElement, h.asGroupElement, u.asGroupElement, v.asGroupElement)
        },
        CreateProveDHTuple)
    )

    val ProveDlogFunc = PredefinedFunc("proveDlog",
      Lambda(Vector("value" -> SGroupElement), SSigmaProp, None),
      PredefFuncInfo(
        { case (_, Seq(arg: Value[SGroupElement.type]@unchecked)) =>
          mkCreateProveDlog(arg)
        },
        CreateProveDlog)
    )

    val AvlTreeFunc = PredefinedFunc("avlTree",
      Lambda(Vector("operationFlags" -> SByte, "digest" -> SByteArray, "keyLength" -> SInt, "valueLengthOpt" -> SIntOption), SAvlTree, None),
      PredefFuncInfo(
        { case (_, Seq(flags, digest, keyLength, valueLength)) =>
          mkCreateAvlTree(flags.asByteValue, digest.asByteArray, keyLength.asIntValue, valueLength.asOption[SInt.type])
        },
        CreateAvlTree)
    )

    val SubstConstantsFunc = PredefinedFunc("substConstants",
      Lambda(
        Seq(STypeParam(tT)),
        Vector("scriptBytes" -> SByteArray, "positions" -> SIntArray, "newValues" -> SCollection(tT)),
        SByteArray, None
      ),
      PredefFuncInfo( undefined, SubstConstants)
    )

    val ExecuteFromVarFunc = PredefinedFunc("executeFromVar",
      Lambda(
        Seq(STypeParam(tT)),
        Vector("id" -> SByte),
        tT, None
      ),
      PredefFuncInfo(undefined, DeserializeContext)
    )

    val funcs: Seq[PredefinedFunc] = Seq(
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
    )

    private val funcNameToIrBuilderMap: Map[String, IrBuilderFunc] =
      funcs.filter(_.info.irBuilder != undefined)
        .map(f => f.name -> f.info.irBuilder)
        .toMap

    def irBuilderForFunc(name: String): Option[IrBuilderFunc] = funcNameToIrBuilderMap.get(name)
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
