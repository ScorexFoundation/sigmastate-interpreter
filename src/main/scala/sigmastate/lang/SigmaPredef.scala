package sigmastate.lang

import sigmastate.Values.SValue
import sigmastate._
import sigmastate.lang.Terms.{Lambda, Ident}

object SigmaPredef {
  val predefinedEnv: Map[String, SValue] = Seq(
    "allOf" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "anyOf" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "sigmaAll" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "sigmaAny" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "Blake2b256" -> Lambda(Vector("input" -> SByteArray), SByteArray, None),
    "ByteArrayToBigInt" -> Lambda(Vector("input" -> SByteArray), SBigInt, None),
    "IntToByteArray" -> Lambda(Vector("input" -> SInt), SByteArray, None),
    "ProveDiffieHellmanTuple" -> Lambda(Vector(
      "g" -> SGroupElement, "h" -> SGroupElement, "u" -> SGroupElement, "v" -> SGroupElement), SBoolean, None),
    "ProveDlog" -> Lambda(Vector("value" -> SGroupElement), SBoolean, None),
    "isMember" -> Lambda(Vector(
       "tree" -> SAvlTree, "key" -> SByteArray, "proof" -> SByteArray), SBoolean, None),
    "exists" -> Lambda(
      Vector("input" -> SCollection(NoType), "pred" -> SFunc(Vector(NoType), SBoolean)),
      SBoolean, None),
  ).toMap

  def PredefIdent(name: String) = {
    val v = predefinedEnv(name)
    Ident(name, v.tpe)
  }

  val AllSym = PredefIdent("allOf")
  val ExistsSym = PredefIdent("exists")
}
