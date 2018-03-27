package sigmastate.lang

import sigmastate.Values.SValue
import sigmastate._
import sigmastate.lang.Terms.{Lambda, Ident}

object SigmaPredef {

  case class PredefinedFunc(
    /** A name which is used in scripts */
    name: String,
    /** Function declaration without body */
    declaration: Lambda,
    /** Builder of SigmaIR node which is equivalent to function application
      * Rule: Apply(f, args) -->  irBuilder(args) */
    irBuilder: Seq[SValue] => SValue
  )
  
  val predefinedEnv: Map[String, SValue] = Seq(
    "allOf" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "anyOf" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "sigmaAll" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "sigmaAny" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "blake2b256" -> Lambda(Vector("input" -> SByteArray), SByteArray, None),
    "byteArrayToBigInt" -> Lambda(Vector("input" -> SByteArray), SBigInt, None),
    "intToByteArray" -> Lambda(Vector("input" -> SInt), SByteArray, None),

    "taggedByteArray" -> Lambda(Vector("input" -> SInt), SByteArray, None),
    "taggedInt" -> Lambda(Vector("input" -> SInt), SInt, None),
    "taggedBigInt" -> Lambda(Vector("input" -> SInt), SBigInt, None),
    "taggedBox" -> Lambda(Vector("input" -> SInt), SBox, None),
    "taggedGroupElement" -> Lambda(Vector("input" -> SInt), SGroupElement, None),
    "taggedAvlTree" -> Lambda(Vector("input" -> SInt), SAvlTree, None),
    "taggedBoolean" -> Lambda(Vector("input" -> SInt), SBoolean, None),

    "ProveDiffieHellmanTuple" -> Lambda(Vector(
      "g" -> SGroupElement, "h" -> SGroupElement, "u" -> SGroupElement, "v" -> SGroupElement), SBoolean, None),
    "ProveDlog" -> Lambda(Vector("value" -> SGroupElement), SBoolean, None),
    "isMember" -> Lambda(Vector(
       "tree" -> SAvlTree, "key" -> SByteArray, "proof" -> SByteArray), SBoolean, None),
  ).toMap

  def PredefIdent(name: String) = {
    val v = predefinedEnv(name)
    Ident(name, v.tpe)
  }

  val AllSym = PredefIdent("allOf")
  val AnySym = PredefIdent("anyOf")

  val TaggedByteArraySym = PredefIdent("taggedByteArray")
  val TaggedIntSym = PredefIdent("taggedInt")
  val TaggedBigIntSym = PredefIdent("taggedBigInt")
  val TaggedBoxSym = PredefIdent("taggedBox")
  val TaggedGroupElementSym = PredefIdent("taggedGroupElement")
  val TaggedAvlTreeSym = PredefIdent("taggedAvlTree")
  val TaggedBooleanSym = PredefIdent("taggedBoolean")

  val Blake2b256Sym = PredefIdent("blake2b256")
  val IsMemberSym = PredefIdent("isMember")
}
