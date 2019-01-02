package sigmastate.lang

import org.ergoplatform.ErgoAddressEncoder.NetworkPrefix
import org.ergoplatform.{ErgoAddressEncoder, P2PKAddress}
import scorex.util.encode.Base58
import sigmastate.SCollection.{SByteArray, SIntArray}
import sigmastate.Values.{BoolValue, Constant, EvaluatedValue, IntConstant, IntValue, SValue, SigmaPropValue, StringConstant, Value}
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.lang.TransformingSigmaBuilder._
import sigmastate.lang.exceptions.InvalidArguments
import sigmastate.serialization.ValueSerializer

object SigmaPredef {

  case class PredefinedFunc(
    /** A name which is used in scripts */
    name: String,
    /** Function declaration without body */
    declaration: Lambda,
    /** Builder of SigmaIR node which is equivalent to function application
      * Rule: Apply(f, args) -->  irBuilder(f, args) */
    irBuilder: PartialFunction[(SValue, Seq[SValue]), SValue]) {

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

    val AllOfFunc = PredefinedFunc("allOf",
      Lambda(IndexedSeq("conditions" -> SCollection(SBoolean)), SBoolean, None),
      { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkAND(col) }
    )

    val AnyOfFunc = PredefinedFunc("anyOf",
      Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
      { case (_, Seq(col: Value[SCollection[SBoolean.type]]@unchecked)) => mkOR(col) }
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
      { case (_, args) => IntConstant(1) }
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

    def PKFunc(networkPrefix: Option[NetworkPrefix]) = PredefinedFunc("PK",
      Lambda(Vector("input" -> SString), SSigmaProp, None),
      { case (_, Seq(arg: EvaluatedValue[SString.type]@unchecked)) =>
        val np = networkPrefix match {
          case Some(value) => value
          case None => sys.error("Expected network prefix to decode address")
        }
        ErgoAddressEncoder(np).fromString(arg.value).get match {
          case a: P2PKAddress => mkConstant[SSigmaProp.type](a.pubkey, SSigmaProp)
          case a@_ => sys.error(s"unsupported address $a")
        }
      }
    )

    val DeserializeFunc = PredefinedFunc("deserialize",
      Lambda(Seq(STypeParam(tT)), Vector("str" -> SString), SOption(tT), None),
      { case (Ident(_, tpe), args) =>
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

    val funcs: Seq[PredefinedFunc] = Seq(
      AllOfFunc,
      AnyOfFunc,
      AtLeastFunc,
      OuterJoinFunc,
      ZKProofFunc,
      SigmaPropFunc,
      GetVarFunc,
      DeserializeFunc,
    )
  }

  object PredefinedFuncApply {
    def unapply(apply: Apply)(implicit registry: PredefinedFuncRegistry): Option[SValue] = apply.func match {
      case Ident(name, _) => registry.funcs
        .find(_.name == name)
        .map(f => f.irBuilder(apply.func, apply.args))
      case _ => None
    }
  }

  private val tT = STypeIdent("T")

  val predefinedEnv: Map[String, SValue] = Seq(

    "blake2b256" -> mkLambda(Vector("input" -> SByteArray), SByteArray, None),
    "sha256" -> mkLambda(Vector("input" -> SByteArray), SByteArray, None),
    "byteArrayToBigInt" -> mkLambda(Vector("input" -> SByteArray), SBigInt, None),
    "byteArrayToLong" -> mkLambda(Vector("input" -> SByteArray), SLong, None),
    "decodePoint" -> mkLambda(Vector("input" -> SByteArray), SGroupElement, None),
    "longToByteArray" -> mkLambda(Vector("input" -> SLong), SByteArray, None),


    "proveDHTuple" -> mkLambda(Vector("g" -> SGroupElement, "h" -> SGroupElement, "u" -> SGroupElement, "v" -> SGroupElement), SSigmaProp, None),
    "proveDlog" -> mkLambda(Vector("value" -> SGroupElement), SSigmaProp, None),

    "isMember" -> mkLambda(Vector("tree" -> SAvlTree, "key" -> SByteArray, "proof" -> SByteArray), SBoolean, None),
    "treeLookup" -> mkLambda(Vector("tree" -> SAvlTree, "key" -> SByteArray, "proof" -> SByteArray), SOption[SByteArray], None),
    "treeModifications" -> mkLambda(Vector("tree" -> SAvlTree, "ops" -> SByteArray, "proof" -> SByteArray), SOption[SByteArray], None),

    "fromBase58" -> mkLambda(Vector("input" -> SString), SByteArray, None),
    "fromBase64" -> mkLambda(Vector("input" -> SString), SByteArray, None),
    "substConstants" -> mkGenLambda(
      Seq(STypeParam(tT)),
      Vector("scriptBytes" -> SByteArray, "positions" -> SIntArray, "newValues" -> SCollection(tT)),
      SByteArray, None),
    "xorOf" -> mkLambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
  ).toMap

  def PredefIdent(name: String): Value[SType] = {
    val v = predefinedEnv(name)
    mkIdent(name, v.tpe)
  }

  val Blake2b256Sym = PredefIdent("blake2b256")
  val Sha256Sym = PredefIdent("sha256")
  val IsMemberSym = PredefIdent("isMember")
  val TreeLookupSym = PredefIdent("treeLookup")
  val TreeModificationsSym = PredefIdent("treeModifications")
  val ProveDlogSym = PredefIdent("proveDlog")
  val ProveDHTupleSym = PredefIdent("proveDHTuple")

  val LongToByteArraySym = PredefIdent("longToByteArray")
  val ByteArrayToBigIntSym = PredefIdent("byteArrayToBigInt")
  val ByteArrayToLongSym = PredefIdent("byteArrayToLong")  // mutually inverse to longToByteArray

  /** Implemented as CryptoConstants.dlogGroup.curve.decodePoint(bytes)*/
  val DecodePointSym = PredefIdent("decodePoint")

  val FromBase58Sym = PredefIdent("fromBase58")
  val FromBase64Sym = PredefIdent("fromBase64")

  val XorOf = PredefIdent("xorOf")
}
