package sigmastate.lang.syntax

import fastparse.all._
import Identifiers._
import sigmastate._
import Values._
import fastparse.{all, core}
import java.lang.Long.parseLong
import java.lang.Integer.parseInt

import sigmastate.lang.{SigmaBuilder, StdSigmaBuilder}

trait Literals { l =>
  var builder: SigmaBuilder = StdSigmaBuilder
  def Block: P[Value[SType]]
  def Pattern: P0

  implicit class ParserOps[+T](p: Parser[T]) {
    def ignore: core.Parser[Unit, Char, String] = p.map(_ => ())
  }
  
  /**
    * Parses all whitespace, excluding newlines. This is only
    * really useful in e.g. {} blocks, where we want to avoid
    * capturing newlines so semicolon-inference would work
    */
  val WS: Parser[Unit] = P( NoCut(NoTrace((Basic.WSChars | Literals.Comment).rep)) )

  /**
    * Parses whitespace, including newlines.
    * This is the default for most things
    */
  val WL0: Parser[Unit] = P( NoTrace((Basic.WSChars | Literals.Comment | Basic.Newline).rep) )(sourcecode.Name("WL"))
  val WL: Parser[Unit] = P( NoCut(WL0) )

  val Semi: Parser[Unit] = P( WS ~ Basic.Semi )
  val Semis: Parser[Unit] = P( Semi.rep(1) ~ WS )
  val Newline: Parser[Unit] = P( WL ~ Basic.Newline )

  val NotNewline: P0 = P( &( WS ~ !Basic.Newline ) )
  val OneNLMax: P0 = {
    val ConsumeComments = P( (Basic.WSChars.? ~ Literals.Comment ~ Basic.WSChars.? ~ Basic.Newline).rep )
    P( NoCut( WS ~ Basic.Newline.? ~ ConsumeComments ~ NotNewline) )
  }
  val TrailingComma: P0 = P( ("," ~ WS ~ Basic.Newline).? )

  //noinspection ForwardReference
  object Literals{
    import Basic._
    val Float: Parser[Unit] = {
      def Thing = P( DecNum ~ Exp.? ~ FloatType.? )
      def Thing2 = P( "." ~ Thing | Exp ~ FloatType.? | Exp.? ~ FloatType )
      P( "." ~ Thing | DecNum ~ Thing2 )
    }

    val Int: Parser[Unit] = P( (HexNum | DecNum) ~ CharIn("Ll").? )

    val Bool: Parser[BooleanConstant] = P( Key.W("true").map(_ => TrueLeaf) | Key.W("false").map(_ => FalseLeaf)  )

    // Comments cannot have cuts in them, because they appear before every
    // terminal node. That means that a comment before any terminal will
    // prevent any backtracking from working, which is not what we want!
    val CommentChunk: Parser[Unit] = P( CharsWhile(c => c != '/' && c != '*') | MultilineComment | !"*/" ~ AnyChar )
    val MultilineComment: P0 = P( "/*" ~/ CommentChunk.rep ~ "*/" )
    val SameLineCharChunks: Parser[Unit] = P( CharsWhile(c => c != '\n' && c != '\r')  | !Basic.Newline ~ AnyChar )
    val LineComment: Parser[Unit] = P( "//" ~ SameLineCharChunks.rep ~ &(Basic.Newline | End) )
    val Comment: P0 = P( MultilineComment | LineComment )

    val Null: Parser[Unit] = Key.W("null")

    val OctalEscape: Parser[Unit] = P( Digit ~ Digit.? ~ Digit.? )
    val Escape: Parser[Unit] = P( "\\" ~/ (CharIn("""btnfr'\"]""") | OctalEscape | UnicodeEscape ) )

    // Note that symbols can take on the same values as keywords!
    val Symbol: Parser[Unit] = P( Identifiers.PlainId | Identifiers.Keywords )

    val Char: Parser[Unit] = {
      // scalac 2.10 crashes if PrintableChar below is substituted by its body
      def PrintableChar = CharPred(CharPredicates.isPrintableChar)

      P( (Escape | PrintableChar) ~ "'" )
    }

    class InterpCtx(interp: Option[P0]){
      //noinspection TypeAnnotation
      val Literal = P(
        ("-".!.? ~ /*Float |*/ Int.!).map {
            case (signOpt, lit) =>
              val sign = if (signOpt.isDefined) -1 else 1
              val suffix = lit.charAt(lit.length - 1)
              val (digits, radix) = if (lit.startsWith("0x")) (lit.substring(2), 16) else (lit, 10)
              if (suffix == 'L' || suffix == 'l')
                builder.mkConstant[SLong.type](sign * parseLong(digits.substring(0, digits.length - 1), radix), SLong)
              else
                builder.mkConstant[SInt.type](sign * parseInt(digits, radix), SInt)
          }
        | Bool
        /*| String | "'" ~/ (Char | Symbol) | Null*/ )

      val Interp: Parser[Unit] = interp match{
        case None => P ( Fail )
        case Some(p) => P( "$" ~ Identifiers.PlainIdNoDollar | ("${" ~ p ~ WL ~ "}") | "$$" )
      }


      val TQ: Parser[Unit] = P( "\"\"\"" )
      /**
        * Helper to quickly gobble up large chunks of un-interesting
        * characters. We break out conservatively, even if we don't know
        * it's a "real" escape sequence: worst come to worst it turns out
        * to be a dud and we go back into a CharsChunk next rep
        */
      val StringChars: Parser[Unit] = P( CharsWhile(c => c != '\n' && c != '"' && c != '\\' && c != '$') )
      val NonTripleQuoteChar: Parser[Unit] = P( "\"" ~ "\"".? ~ !"\"" | CharIn("\\$\n") )
      val TripleChars: Parser[Unit] = P( (StringChars | Interp | NonTripleQuoteChar).rep )
      val TripleTail: Parser[Unit] = P( TQ ~ "\"".rep )
      def SingleChars(allowSlash: Boolean): Parser[Unit] = {
        val LiteralSlash = P( if(allowSlash) "\\" else Fail )
        val NonStringEnd = P( !CharIn("\n\"") ~ AnyChar )
        P( (StringChars | Interp | LiteralSlash | Escape | NonStringEnd ).rep )
      }
      val String: Parser[Unit] = {
        P {
          (Id ~ TQ ~/ TripleChars ~ TripleTail) |
              (Id ~ "\"" ~/ SingleChars(true)  ~ "\"") |
              (TQ ~/ NoInterp.TripleChars ~ TripleTail) |
              ("\"" ~/ NoInterp.SingleChars(false) ~ "\"")
        }
      }

    }
    object NoInterp extends InterpCtx(None)
    object Pat extends InterpCtx(Some(l.Pattern))
    object Expr extends InterpCtx(Some(Block.ignore))
  }
}

