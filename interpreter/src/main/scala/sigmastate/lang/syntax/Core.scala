package sigmastate.lang.syntax

import scala.language.implicitConversions
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.syntax

trait Core extends syntax.Literals {
  import fastparse._
  import ScalaWhitespace._

  def mkUnaryOp(opName: String, arg: Value[SType]): Value[SType]
  def mkBinaryOp(l: Value[SType], opName: String, r: Value[SType]): Value[SType]

  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.

  import Key._

  // Keywords that match themselves and nothing else
  def `=>`[_:P] = O("=>") | O("⇒")
//  val `<-`[_:P] = O("<-") | O("←")
  def `:`[_:P] = O(":")
  def `=`[_:P] = O("=")
  def `@`[_:P] = O("@")
  def `_`[_:P] = W("_")
  def `type`[_:P] = W("type")
  def `val`[_:P] = W("val")
  def `def`[_:P] = W("def")
  def `case`[_:P] = W("case")
  def `then`[_:P] = W("then")
  def `else`[_:P] = W("else")
  def `#`[_:P] = O("#")
  def `return`[_:P] = W("return")
  def `if`[_:P] = W("if")
  def `match`[_:P] = W("match")
  def `this`[_:P] = W("this")
  def `super`[_:P] = W("super")
  //  val `var`[_:P] = W("var")
  //  val `def`[_:P] = W("def")
    def `with`[_:P] = W("with")
  //  val `package`[_:P] = W("package")
  //  val `object`[_:P] = W("object")
  //  val `class`[_:P] = W("class")
  //  val `trait`[_:P] = W("trait")
    def `extends`[_:P] = W("extends")
    def `implicit`[_:P] = W("implicit")
  //  val `try`[_:P] = W("try")
    def `new`[_:P] = W("new")
  //  val `macro`[_:P] = W("macro")
  //  val `import`[_:P] = W("import")
//  val `catch`[_:P] = W("catch")
//  val `finally`[_:P] = W("finally")
//  val `do`[_:P] = W("do")
//  val `yield`[_:P] = W("yield")
//  val `while`[_:P] = W("while")
//  val `<%`[_:P] = O("<%")
//  val `override`[_:P] = W("override")
//  val `forSome`[_:P] = W("forSome")
//  val `for`[_:P] = W("for")
//  val `abstract`[_:P] = W("abstract")
//  val `throw`[_:P] = W("throw")
  def `lazy`[_:P] = W("lazy")
  def `>:`[_:P] = O(">:")
  def `<:`[_:P] = O("<:")
//  val `final` =  W("final")
//  val `sealed`[_:P] = W("sealed")
//  val `private`[_:P] = W("private")
//  val `protected`[_:P] = W("protected")


  // kinda-sorta keywords that are common patterns even if not
  // really-truly keywords
  def `*`[_:P] = O("*")
  def `_*`[_:P] = P( `_` ~ `*` )
  def `}`[_:P] = P( Semis.? ~ "}" )
  def `{`[_:P] = P( "{" ~ Semis.? )

  def Id[_:P] = P( WL ~ Identifiers.Id )
  def VarId[_:P] = P( WL ~ Identifiers.VarId )
  def BacktickId[_:P] = P( WL ~ Identifiers.BacktickId )
  def ExprLiteral[_:P] = P( WL ~ Literals.Expr.Literal )
  def PatLiteral[_:P] = P( WL ~ Literals.Pat.Literal )

  def QualId[_:P] = P( WL ~ Id.rep(1, sep = ".") )
  def Ids[_:P] = P( Id.rep(1, sep = ",") )

  /**
   * Sketchy way to whitelist a few suffixes that come after a . select;
   * apart from these and IDs, everything else is illegal
   */
  def PostDotCheck[_:P]: P0 = P( WL ~ !(`super` | `this` | "{" |  `_` | `type`) )
  def StableId[_:P] = {
//    val ClassQualifier[_:P] = P( "[" ~ Id ~ "]" )
//    val ThisSuper[_:P] = P( `this` | `super` ~ ClassQualifier.? )
//    val ThisPath: P0[_:P] = P( ThisSuper ~ ("." ~ PostDotCheck ~/ Id).rep )
    def IdPath = P( Index ~ Id.! ~ ("." ~ PostDotCheck ~/ Index ~ (`this`.! | Id.!)).rep /*~ ("." ~ ThisPath).?*/ ).map {
      case (hi, hs, t) => t.foldLeft[SValue](atSrcPos(hi){builder.mkIdent(hs, NoType)}){
        case (obj, (i, s)) => atSrcPos(i) { builder.mkSelect(obj, s) }
      }
    }
    P( /*ThisPath |*/ IdPath )
  }
}
