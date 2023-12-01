package sigmastate.lang

import sigmastate.SType
import fastparse._
import fastparse.NoWhitespace._

/**
 * Represents a docstring line.
 */
case class DocumentationToken(kind: DocumentationToken.Kind, name: Option[String], body: Option[String])

/**
 * Companion object containing the classes required for describing an docstring token.
 */
object DocumentationToken {
  def apply(kind: Kind): DocumentationToken = new DocumentationToken(kind, None, None)

  def apply(kind: Kind, body: String): DocumentationToken = new DocumentationToken(kind, None, Option(body))

  def apply(kind: TagKind, name: String, body: String): DocumentationToken =
    new DocumentationToken(kind, Option(name), Option(body))

  /**
   * Represents a documentation remark.
   */
  sealed abstract class Kind

  /**
   * Documents an untagged doc description, this is simply a line of text anywhere in the docstring.
   */
  case object Description extends Kind

  /**
   * Represents an empty doc line.
   */
  case object EmptyLine extends Kind

  /**
   * Represents a line starting with a tag (@) but is not supported.
   */
  case object UnsupportedTag extends Kind

  /**
   * Represents a labeled documentation remark.
   */
  sealed abstract class TagKind(val label: String) extends Kind

  /**
   * Documents a specific value parameter of the contract template.
   */
  case object Param extends TagKind("@param")

  /**
   * Documents the return value of the contract template.
   */
  case object Return extends TagKind("@returns")
}

/**
 * Holds values extracted from a `@param` tag line in docstrings.
 *
 * @param name        Name of the parameter.
 * @param description Description of the parameter.
 */
case class ParameterDoc(name: String, description: String)

/**
 * Contract template documentation extracted from the preceding docstring.
 *
 * @param description Top level contract template description.
 * @param params      Contract template parameters as defined in the docstring.
 */
case class ContractDoc(description: String, params: Seq[ParameterDoc])

object ContractDoc {
  def apply(tokens: Seq[DocumentationToken]): ContractDoc = {
    val nonEmptyTokens = tokens.dropWhile(_.kind == DocumentationToken.EmptyLine)

    val (description, paramTokens) = nonEmptyTokens.span(_.kind == DocumentationToken.Description)
    val descriptionText = description.flatMap(_.body).mkString(" ")

    def extractParamDocs(tokens: Seq[DocumentationToken]): Seq[ParameterDoc] = {
      tokens match {
        case DocumentationToken(kind: DocumentationToken.TagKind, Some(name), Some(body)) +: tail if kind == DocumentationToken.Param =>
          // grab the succeeding description tokens if there are any, this is multiline description of params
          val (descTokens, remainingTokens) = tail.span(_.kind == DocumentationToken.Description)
          val fullDescription = (body +: descTokens.flatMap(_.body)).mkString(" ")
          ParameterDoc(name, fullDescription) +: extractParamDocs(remainingTokens)
        case _ +: tail => extractParamDocs(tail)
        case Seq() => Seq()
      }
    }

    ContractDoc(descriptionText, extractParamDocs(paramTokens))
  }
}

case class ContractParam(name: String, tpe: SType, defaultValue: Option[SType#WrappedType])

case class ContractSignature(name: String, params: Seq[ContractParam])

case class ParsedContractTemplate(docs: ContractDoc, signature: ContractSignature)

object ContractParser {
  def parse(source: String): Parsed[ParsedContractTemplate] = fastparse.parse(source, parse(_))

  def parse[_: P]: P[ParsedContractTemplate] = P(Docs.parse ~ "\n" ~ Signature.parse ~ " ".rep.? ~ "=" ~ " ".rep.? ~ &("{")).map(s => ParsedContractTemplate(s._1, s._2))

  object Docs {

    import DocumentationToken._

    def parse(source: String): Parsed[ContractDoc] = {
      fastparse.parse(source, parse(_))
    }

    def parse[_: P]: P[ContractDoc] = P(" ".rep.? ~ "/*" ~ docLine.rep ~ " ".rep.? ~ "*/").map(ContractDoc.apply)

    def linePrefix[_: P] = P(" ".rep.? ~ "*" ~ " ".rep.? ~ !"/")

    def word[_: P] = CharsWhile(c => c != ' ')

    def charUntilNewLine[_: P] = CharsWhile(c => c != '\n')

    def unsupportedTag[_: P] = P("@" ~ charUntilNewLine.?).map(_ => DocumentationToken(UnsupportedTag))

    def returnTag[_: P] = P("@returns").map(_ => DocumentationToken(Return))

    def paramTag[_: P] = P("@param" ~ " ".rep ~ word.! ~ " ".rep ~ charUntilNewLine.!).map(s => DocumentationToken(Param, s._1, s._2))

    def tag[_: P] = P(returnTag | paramTag | unsupportedTag)

    def emptyLine[_: P] = P(("" | " ".rep.?) ~ &("\n")).map(_ => DocumentationToken(EmptyLine))

    def description[_: P] = P(!"@" ~ charUntilNewLine.!).map(s => DocumentationToken(Description, s))

    def docLine[_: P] = P(linePrefix ~ (emptyLine | description | tag) ~ "\n")
  }

  object Signature {

    import SigmaParser._

    def parse(source: String): Parsed[ContractSignature] = {
      fastparse.parse(source, parse(_))
    }

    def parse[_: P]: P[ContractSignature] = P(annotation ~ " ".rep.? ~ `def` ~ " ".rep.? ~ Id.! ~ params).map(s => ContractSignature(s._1, s._2.getOrElse(Seq())))

    def annotation[_: P] = P("@contract")

    def paramDefault[_: P] = P(" ".rep.? ~ `=` ~ " ".rep.? ~ ExprLiteral).map(s => s.asWrappedType)

    def param[_: P] = P(" ".rep.? ~ Id.! ~ ":" ~ Type ~ paramDefault.?).map(s => ContractParam(s._1, s._2, s._3))

    def params[_: P] = P("(" ~ param.rep(1, ",").? ~ ")")
  }
}
