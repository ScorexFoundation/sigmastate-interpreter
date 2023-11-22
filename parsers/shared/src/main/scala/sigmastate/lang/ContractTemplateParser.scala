package sigmastate.lang

import fastparse._
import fastparse.NoWhitespace._

case class ParsedContractTemplate(docs: ContractDoc, signature: ContractSignature)

object ContractTemplateParser {
  def parse(source: String): Parsed[ParsedContractTemplate] = fastparse.parse(source, parse(_))

  def parse[_: P]: P[ParsedContractTemplate] = P(ContractDocParser.parse ~ "\n" ~ ContractSignatureParser.parse).map(s => ParsedContractTemplate(s._1, s._2))
}
