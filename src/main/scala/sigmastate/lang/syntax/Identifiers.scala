package sigmastate.lang.syntax

import fastparse.CharPredicates.{isDigit, isLetter}
import fastparse.all._
import sigmastate.lang.syntax.Basic._

object Identifiers {
  case class NamedFunction(f: Char => Boolean)
      (implicit name: sourcecode.Name) extends (Char => Boolean){
    def apply(t: Char) = f(t)
    override def toString() = name.value

  }
  val OpCharNotSlash = NamedFunction(x => isOpChar(x) && x != '/')
  val NotBackTick = NamedFunction(_ != '`')

  val Operator = P(
    !Keywords ~ (!("/*" | "//") ~ (CharsWhile(OpCharNotSlash) | "/")).rep(1)
  )

  val VarId = VarId0(true)

  def VarId0(dollar: Boolean) = P( !Keywords ~ Lower ~ IdRest(dollar) )
  val PlainId = P( !Keywords ~ Upper ~ IdRest(true) | VarId | Operator ~ (!OpChar | &("/*" | "//")) )
  val PlainIdNoDollar = P( !Keywords ~ Upper ~ IdRest(false) | VarId0(false) | Operator )
  val BacktickId = P( "`" ~ CharsWhile(NotBackTick) ~ "`" )
  val Id: P0 = P( BacktickId | PlainId )

  def IdRest(allowDollar: Boolean) = {

    val IdCharacter =
      if(allowDollar) NamedFunction(c => c == '$' || isLetter(c) || isDigit(c))
      else NamedFunction(c => isLetter(c) || isDigit(c))

    val IdUnderscoreChunk = P( CharsWhileIn("_", min = 0) ~ CharsWhile(IdCharacter) )
    P( IdUnderscoreChunk.rep ~ (CharsWhileIn("_") ~ CharsWhile(isOpChar, min = 0)).? )
  }

  val alphaKeywords = Seq(
    "case", "else", "false", "function", "if", "match", "return", "then", "true"
  )
  val AlphabetKeywords = P {
    StringIn(alphaKeywords:_*) ~ !Letter
  }

  val symbolKeywords = Seq(
    ":", ";", "=>", "=", "#", "@"
  )
  val SymbolicKeywords = P{
    StringIn(symbolKeywords:_*) ~ !OpChar
  }

  val keywords = alphaKeywords ++ symbolKeywords

  val Keywords = P( AlphabetKeywords | SymbolicKeywords )
}
