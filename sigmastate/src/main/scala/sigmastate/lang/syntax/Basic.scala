package sigmastate.lang.syntax

import fastparse.all._
import fastparse.CharPredicates._
import scalan.Nullable
import sigmastate.lang.SourceContext
import sigmastate.lang.exceptions.SigmaException
import sigma.util.Extensions._

object Basic {
  val digits = "0123456789"
  val Digit: Parser[Unit] = P( CharIn(digits) )
  val hexDigits: String = digits + "abcdefABCDEF"
  val HexDigit: Parser[Unit] = P( CharIn(hexDigits) )
  val UnicodeEscape: Parser[Unit] = P( "u" ~ HexDigit ~ HexDigit ~ HexDigit ~ HexDigit )

  //Numbers and digits


  val HexNum: Parser[Unit] = P( "0x" ~ CharsWhileIn(hexDigits) )
  val DecNum: Parser[Unit] = P( CharsWhileIn(digits) )
  val Exp: Parser[Unit] = P( CharIn("Ee") ~ CharIn("+-").? ~ DecNum )
  val FloatType: Parser[Unit] = P( CharIn("fFdD") )

  val WSChars: Parser[Unit] = P( CharsWhileIn("\u0020\u0009") )
  val Newline: Parser[Unit] = P( StringIn("\r\n", "\n") )
  val Semi: Parser[Unit] = P( ";" | Newline.rep(1) )
  val OpChar: Parser[Unit] = P ( CharPred(isOpChar) )

  def isOpChar(c: Char): Boolean = c match{
    case '!' | '#' | '%' | '&' | '*' | '+' | '-' | '/' |
         ':' | '<' | '=' | '>' | '?' | '@' | '\\' | '^' | '|' | '~' => true
    case _ => isOtherSymbol(c) || isMathSymbol(c)
  }
  val Letter: Parser[Unit] = P( CharPred(c => isLetter(c) | isDigit(c) | c == '$' | c == '_' ) )
  val LetterDigitDollarUnderscore: Parser[Unit] =  P(
    CharPred(c => isLetter(c) | isDigit(c) | c == '$' | c == '_' )
  )
  val Lower: Parser[Unit] = P( CharPred(c => isLower(c) || c == '$' | c == '_') )
  val Upper: Parser[Unit] = P( CharPred(isUpper) )

  def error(msg: String, srcCtx: Option[SourceContext]) = throw new ParserException(msg, srcCtx)
  def error(msg: String, srcCtx: Nullable[SourceContext]) = throw new ParserException(msg, srcCtx.toOption)
}

class ParserException(message: String, source: Option[SourceContext])
  extends SigmaException(message, source)

/**
  * Most keywords don't just require the correct characters to match,
  * they have to ensure that subsequent characters *don't* match in
  * order for it to be a keyword. This enforces that rule for key-words
  * (W) and key-operators (O) which have different non-match criteria.
  */
object Key {
  def W(s: String) = P( s ~ !Basic.LetterDigitDollarUnderscore )(sourcecode.Name(s"`$s`"))
  // If the operator is followed by a comment, stop early so we can parse the comment
  def O(s: String) = P( s ~ (!Basic.OpChar | &("/*" | "//")) )(sourcecode.Name(s"`$s`"))
}
