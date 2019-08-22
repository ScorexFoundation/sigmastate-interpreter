package sigmastate.lang.syntax

import fastparse.CharPredicates.{isDigit, isLetter}
import fastparse.all._
import sigmastate.lang.syntax.Basic._

//noinspection ForwardReference
object Identifiers {
  case class NamedFunction(f: Char => Boolean)
      (implicit name: sourcecode.Name) extends (Char => Boolean){
    def apply(t: Char) = f(t)
    override def toString: String = name.value
  }
  val OpCharNotSlash = NamedFunction(x => isOpChar(x) && x != '/')
  val NotBackTick = NamedFunction(_ != '`')

  val Operator: Parser[Unit] = P(
    !Keywords ~ (!("/*" | "//") ~ (CharsWhile(OpCharNotSlash) | "/")).rep(1)
  )

  val VarId: Parser[Unit] = VarId0(true)

  def VarId0(dollar: Boolean): Parser[Unit] = P( !Keywords ~ Lower ~ IdRest(dollar) )
  val PlainId: Parser[Unit] = P( !Keywords ~ Upper ~ IdRest(true) | VarId | Operator ~ (!OpChar | &("/*" | "//")) )
  val PlainIdNoDollar: Parser[Unit] = P( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
  val BacktickId: Parser[Unit] = P( "`" ~ CharsWhile(NotBackTick) ~ "`" )
  val Id: P0 = P( BacktickId | PlainId )

  def IdRest(allowDollar: Boolean): Parser[Unit] = {

    val IdCharacter =
      if(allowDollar) NamedFunction(c => c == '$' || isLetter(c) || isDigit(c))
      else NamedFunction(c => isLetter(c) || isDigit(c))

    val IdUnderscoreChunk = P( CharsWhileIn("_", min = 0) ~ CharsWhile(IdCharacter) )
    P( IdUnderscoreChunk.rep ~ (CharsWhileIn("_") ~ CharsWhile(isOpChar, min = 0)).? )
  }

  val alphaKeywords = Seq(
    "case", "else", "false", "function", "if", "match", "return", "then", "true"
  )
  val AlphabetKeywords: Parser[Unit] = P {
    StringIn(alphaKeywords:_*) ~ !Letter
  }

  val symbolKeywords = Seq(
    ":", ";", "=>", "=", "#", "@"
  )
  val SymbolicKeywords: Parser[Unit] = P{
    StringIn(symbolKeywords:_*) ~ !OpChar
  }

  val keywords: Seq[String] = alphaKeywords ++ symbolKeywords

  val Keywords: Parser[Unit] = P( AlphabetKeywords | SymbolicKeywords )
}
