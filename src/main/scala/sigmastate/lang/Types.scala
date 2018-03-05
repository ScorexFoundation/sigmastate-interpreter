package sigmastate.lang

import fastparse.noApi._

trait Types extends Core {
  import WhitespaceApi._
  def TypeExpr: P0
  def ValVarDef: P0
  def FunDef: P0

  val Dcl: P0 = {
    P( `let` ~/ ValVarDef | `fun` ~/ FunDef )
  }

  val PostfixType = P( InfixType ~ (`=>` ~/ Type ).? )
  val Type: P0 = P( `=>`.? ~~ PostfixType ~ TypeBounds ~ `*`.? )


  // Can't cut after `Id` because it may be a `*`, in which case
  // we may need to backtrack and settle for the `*`-postfix rather than
  // an infix type
  val InfixType = P( CompoundType ~~ (NotNewline ~ Id ~~ OneNLMax ~ CompoundType).repX )

  val CompoundType = {
    val Refinement = P( OneNLMax ~ `{` ~/ Dcl.repX(sep=Semis) ~ `}` )
    val NamedType = P( (Pass ~ AnnotType).rep(1, `with`.~/) )
    P( NamedType ~~ Refinement.? | Refinement )
  }
  val NLAnnot = P( NotNewline ~ Annot )
  val AnnotType = P(SimpleType ~~ NLAnnot.repX )

  val TypeId = P( StableId )
  val SimpleType: P0 = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    val TupleType = P( "(" ~/ Type.repTC() ~ ")" )
    val BasicType = P( TupleType | Literals.Expr.Literal | TypeId ~ ("." ~ `type`).?  | `_` )
    P( BasicType ~ (TypeArgs | `#` ~/ Id).rep )
  }

  val TypeArgs = P( "[" ~/ Type.repTC() ~ "]" )


  val FunSig: P0 = {
    val FunArg = P( Annot.rep ~ Id ~ (`:` ~/ Type).? ~ (`=` ~/ TypeExpr).? )
    val Args = P( FunArg.repTC(1) )
    val FunArgs = P( OneNLMax ~ "(" ~/ Args.? ~ ")" )
    val FunTypeArgs = P( "[" ~/ (Annot.rep ~ TypeArg).repTC(1) ~ "]" )
    P( (Id | `this`) ~ (FunTypeArgs).? ~~ FunArgs.rep )
  }

  val TypeBounds: P0 = P( (`>:` ~/ Type).? ~ (`<:` ~/ Type).? )
  val TypeArg: P0 = {
    val CtxBounds = P((`:` ~/ Type).rep)
    P((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds)
  }

  val Annot: P0 = P( `@` ~/ SimpleType ~  ("(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ TrailingComma ~ ")").rep )

  val TypeArgList: P0 = {
    val Variant: P0 = P( Annot.rep ~ CharIn("+-").? ~ TypeArg )
    P( "[" ~/ Variant.repTC(1) ~ "]" )
  }
  val Exprs: P0 = P( TypeExpr.rep(1, ",") )
}
