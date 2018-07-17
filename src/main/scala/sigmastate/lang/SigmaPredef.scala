package sigmastate.lang

import sigmastate.Values.SValue
import sigmastate._
import sigmastate.SCollection.SByteArray
import sigmastate.lang.Terms.{Lambda, Ident}
import sigmastate.lang.TransformingSigmaBuilder._

object SigmaPredef {

  case class PredefinedFunc(
    /** A name which is used in scripts */
    name: String,
    /** Function declaration without body */
    declaration: Lambda,
    /** Builder of SigmaIR node which is equivalent to function application
      * Rule: Apply(f, args) -->  irBuilder(f, args) */
    irBuilder: (SValue, Seq[SValue]) => SValue
  )

  /** Type variable used in the signatures of global functions below.*/
  private val tT = STypeIdent("T")

  val predefinedEnv: Map[String, SValue] = Seq(
    "allOf" -> mkLambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "anyOf" -> mkLambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "blake2b256" -> mkLambda(Vector("input" -> SByteArray), SByteArray, None),
    "sha256" -> mkLambda(Vector("input" -> SByteArray), SByteArray, None),
    "byteArrayToBigInt" -> mkLambda(Vector("input" -> SByteArray), SBigInt, None),
    "longToByteArray" -> mkLambda(Vector("input" -> SLong), SByteArray, None),
//    "intToBigInt" -> mkLambda(Vector("input" -> SLong), SBigInt, None),
    "intToByte" -> mkLambda(Vector("input" -> SInt), SByte, None),

    "getVar" -> mkLambda(Vector("varId" -> SByte), tT, None),

    "proveDHTuple" -> mkLambda(Vector(
      "g" -> SGroupElement, "h" -> SGroupElement, "u" -> SGroupElement, "v" -> SGroupElement), SBoolean, None),
    "proveDlog" -> mkLambda(Vector("value" -> SGroupElement), SBoolean, None),
    "isMember" -> mkLambda(Vector(
       "tree" -> SAvlTree, "key" -> SByteArray, "proof" -> SByteArray), SBoolean, None),
  ).toMap

  def PredefIdent(name: String): Ident = {
    val v = predefinedEnv(name)
    mkIdent(name, v.tpe).asInstanceOf[Ident]
  }

  val AllSym = PredefIdent("allOf")
  val AnySym = PredefIdent("anyOf")

  val GetVarSym = PredefIdent("getVar")

  val Blake2b256Sym = PredefIdent("blake2b256")
  val Sha256Sym = PredefIdent("sha256")
  val IsMemberSym = PredefIdent("isMember")
  val ProveDlogSym = PredefIdent("proveDlog")
  val ProveDHTupleSym = PredefIdent("proveDHTuple")

//  val IntToBigSym = PredefIdent("intToBigInt")
  val IntToByteSym = PredefIdent("intToByte")
  val LongToByteArraySym = PredefIdent("longToByteArray")
}
