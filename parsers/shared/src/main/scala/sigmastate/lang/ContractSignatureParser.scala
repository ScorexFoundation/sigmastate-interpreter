package sigmastate.lang

import fastparse._
import fastparse.NoWhitespace._
import sigmastate.SType

case class ContractParam(name: String, tpe: SType, defaultValue: Option[SType#WrappedType])

case class ContractSignature(name: String, params: Seq[ContractParam])

object ContractSignatureParser {
  import SigmaParser._

  def parse(source: String): Parsed[ContractSignature] = {
    fastparse.parse(source, parse(_))
  }

  def parse[_: P]: P[ContractSignature] = P(annotation ~ " ".rep.? ~ `def` ~ " ".rep.? ~ Id.! ~ params).map(s => ContractSignature(s._1, s._2.getOrElse(Seq())))

  private def annotation[_: P] = P("@contract")

  private def paramDefault[_: P] = P(" ".rep.? ~ `=` ~ " ".rep.? ~ ExprLiteral).map(s => s.asWrappedType)

  private def param[_: P] = P(" ".rep.? ~ Id.! ~ ":" ~ Type ~ paramDefault.?).map(s => ContractParam(s._1, s._2, s._3))

  private def params[_: P] = P("(" ~ param.rep(1, ",").? ~ ")")
}
