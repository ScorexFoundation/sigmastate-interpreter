package sigmastate.lang

import sigmastate.Values.SValue
import sigmastate.{SCollection, NoType, SBoolean, SFunc}
import sigmastate.lang.Terms.{Lambda, Ident}

object SigmaPredef {
  val predefinedEnv: Map[String, SValue] = Seq(
    "all" -> Lambda(Vector("conditions" -> SCollection(SBoolean)), SBoolean, None),
    "exists" -> Lambda(
      Vector("input" -> SCollection(NoType), "pred" -> SFunc(Vector(NoType), SBoolean)),
      SBoolean, None),
  ).toMap

  def PredefIdent(name: String) = {
    val v = predefinedEnv(name)
    Ident(name, v.tpe)
  }

  val AllSym = PredefIdent("all")
  val ExistsSym = PredefIdent("exists")
}
