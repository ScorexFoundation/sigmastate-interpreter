package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress, Global}
import scalan.Nullable
import scorex.util.encode.{Base64, Base58}
import sigmastate.SCollection.{SIntArray, SByteArray}
import sigmastate.SOption._
import sigmastate.Values.{StringConstant, Constant, EvaluatedValue, SValue, IntValue, SigmaPropConstant, GroupGenerator, BoolValue, Value, ByteArrayConstant, SigmaPropValue}
import sigmastate._
import sigmastate.lang.SigmaTyper.emptySubst
import sigmastate.lang.Terms._
import sigmastate.lang.exceptions.InvalidArguments
import sigmastate.serialization.ValueSerializer

object SigmaPredef {

  type IrBuilderFunc = PartialFunction[(SValue, Seq[SValue]), SValue]

  case class PredefinedFunc(
    /** A name which is used in scripts */
    name: String,
    /** Function declaration without body */
    declaration: Lambda,
    /** Builder of SigmaIR node which is equivalent to function application
      * Rule: Apply(f, args) -->  irBuilder(f, args) */
    irBuilder: IrBuilderFunc) {

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
      { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkAND(col) }
    )

    val AnyOfFunc = PredefinedFunc("anyOf",
      Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
      { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkOR(col) }
    )

    val XorOfFunc = PredefinedFunc("xorOf",
      Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
      { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkXorOf(col) }
    )

    val AtLeastFunc = PredefinedFunc("atLeast",
      Lambda(Vector("k" -> SInt, "conditions" -> SCollection(SSigmaProp)), SSigmaProp, None),
      { case (_, Seq(bound: IntValue@unchecked, arr: Value[SCollection[SSigmaProp.type]]@unchecked)) =>
        mkAtLeast(bound, arr)
      }
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
      undefined
    )

    val ZKProofFunc = PredefinedFunc("ZKProof",
      Lambda(Vector("block" -> SSigmaProp), SBoolean, None),
      { case (_, Seq(block: SigmaPropValue@unchecked)) => mkZKProofBlock(block) }
    )

    val SigmaPropFunc = PredefinedFunc("sigmaProp",
      Lambda(Vector("condition" -> SBoolean), SSigmaProp, None),
      { case (_, Seq(b: BoolValue@unchecked)) => mkBoolToSigmaProp(b) }
    )

    val GetVarFunc = PredefinedFunc("getVar",
      Lambda(Seq(STypeParam(tT)), Vector("varId" -> SByte), SOption(tT), None),
      { case (Ident(_, SFunc(_, SOption(rtpe), _)), Seq(id: Constant[SNumericType]@unchecked)) =>
        mkGetVar(SByte.downcast(id.value.asInstanceOf[AnyVal]), rtpe)
      }
    )

    def PKFunc(networkPrefix: NetworkPrefix) = PredefinedFunc("PK",
      Lambda(Vector("input" -> SString), SSigmaProp, None),
      { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
        ErgoAddressEncoder(networkPrefix).fromString(arg.value).get match {
          case a: P2PKAddress => mkConstant[SSigmaProp.type](a.pubkey, SSigmaProp)
          case a@_ => sys.error(s"unsupported address $a")
        }
      }
    )

    val DeserializeFunc = PredefinedFunc("deserialize",
      Lambda(Seq(STypeParam(tT)), Vector("str" -> SString), tT, None),
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
      }
    )

    val FromBase58Func = PredefinedFunc("fromBase58",
      Lambda(Vector("input" -> SString), SByteArray, None),
      { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
        ByteArrayConstant(Base58.decode(arg.value).get)
      }
    )

    val FromBase64Func = PredefinedFunc("fromBase64",
      Lambda(Vector("input" -> SString), SByteArray, None),
      { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
        ByteArrayConstant(Base64.decode(arg.value).get)
      }
    )

    val Blake2b256Func = PredefinedFunc("blake2b256",
      Lambda(Vector("input" -> SByteArray), SByteArray, None),
      { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
        mkCalcBlake2b256(arg)
      }
    )

    val Sha256Func = PredefinedFunc("sha256",
      Lambda(Vector("input" -> SByteArray), SByteArray, None),
      { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
        mkCalcSha256(arg)
      }
    )

    val ByteArrayToBigIntFunc = PredefinedFunc("byteArrayToBigInt",
      Lambda(Vector("input" -> SByteArray), SBigInt, None),
      { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
        mkByteArrayToBigInt(arg)
      }
    )

    val ByteArrayToLongFunc = PredefinedFunc("byteArrayToLong",
      Lambda(Vector("input" -> SByteArray), SLong, None),
      { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
        mkByteArrayToLong(arg)
      }
    )

    val DecodePointFunc = PredefinedFunc("decodePoint",
      Lambda(Vector("input" -> SByteArray), SGroupElement, None),
      { case (_, Seq(arg: Value[SByteArray]@unchecked)) =>
        mkDecodePoint(arg)
      }
    )

    val GroupGeneratorFunc = PredefinedFunc("groupGenerator",
      Lambda(Vector(), SGroupElement, None),
      { case (_, Seq()) => GroupGenerator }
    )

    val LongToByteArrayFunc = PredefinedFunc("longToByteArray",
      Lambda(Vector("input" -> SLong), SByteArray, None),
      { case (_, Seq(arg: Value[SLong.type]@unchecked)) =>
        mkLongToByteArray(arg)
      }
    )

    val ProveDHTupleFunc = PredefinedFunc("proveDHTuple",
      Lambda(Vector("g" -> SGroupElement, "h" -> SGroupElement, "u" -> SGroupElement, "v" -> SGroupElement), SSigmaProp, None),
      { case (_, Seq(g, h, u, v)) =>
          mkCreateProveDHTuple(g.asGroupElement, h.asGroupElement, u.asGroupElement, v.asGroupElement)
      }
    )

    val ProveDlogFunc = PredefinedFunc("proveDlog",
      Lambda(Vector("value" -> SGroupElement), SSigmaProp, None),
      { case (_, Seq(arg: Value[SGroupElement.type]@unchecked)) =>
        mkCreateProveDlog(arg)
      }
    )
    val AvlTreeFunc = PredefinedFunc("avlTree",
      Lambda(Vector("operationFlags" -> SByte, "digest" -> SByteArray, "keyLength" -> SInt, "valueLengthOpt" -> SIntOption), SAvlTree, None),
      { case (_, Seq(flags, digest, keyLength, valueLength)) =>
        mkCreateAvlTree(flags.asByteValue, digest.asByteArray, keyLength.asIntValue, valueLength.asOption[SInt.type])
      }
    )

    val SubstConstantsFunc = PredefinedFunc("substConstants",
      Lambda(
        Seq(STypeParam(tT)),
        Vector("scriptBytes" -> SByteArray, "positions" -> SIntArray, "newValues" -> SCollection(tT)),
        SByteArray, None
      ),
      undefined
    )

    val ExecuteFromVarFunc = PredefinedFunc("executeFromVar",
      Lambda(
        Seq(STypeParam(tT)),
        Vector("id" -> SByte),
        tT, None
      ),
      undefined
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
      GroupGeneratorFunc,
      LongToByteArrayFunc,
      ProveDHTupleFunc,
      ProveDlogFunc,
      AvlTreeFunc,
      XorOfFunc,
      SubstConstantsFunc,
      ExecuteFromVarFunc,
    )

    private val funcNameToIrBuilderMap: Map[String, IrBuilderFunc] =
      funcs.filter(_.irBuilder != undefined)
        .map(f => f.name -> f.irBuilder)
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
