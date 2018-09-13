package sigmastate.lang

import fastparse.core.Logger
import fastparse.core
import sigmastate._
import Values._
import sigmastate.lang.Terms._
import sigmastate.SCollection.SByteArray
import sigmastate.lang.syntax.Basic._
import sigmastate.lang.syntax.{Core, Exprs}

import scala.collection.mutable

object SigmaParser extends Exprs with Types with Core {
  import fastparse.noApi._
  import WhitespaceApi._

  val TmplBody = {
    val Prelude = P( (Annot ~ OneNLMax).rep )
    val TmplStat = P( Prelude ~ BlockDef | StatCtx.Expr )
    P( "{" ~/ BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.? ~ `}` )
  }

//  val FunDef = {
//    P( (Id | `this`).! ~ LambdaDef ).map { case (name, lam) => builder.mkVal(name, NoType, lam) }
//  }

  val ValVarDef = P( BindPattern/*.rep(1, ",".~/)*/ ~ (`:` ~/ Type).? ~ (`=` ~/ FreeCtx.Expr) ).map {
    case (Ident(n,_), t, body) => builder.mkVal(n, t.getOrElse(NoType), body)
    case (pat,_,_) => error(s"Only single name patterns supported but was $pat")
  }

  val BlockDef = P( Dcl )

  val Constr = P( AnnotType ~~ (NotNewline ~ ParenArgList ).repX )
  val Constrs = P( (WL ~ Constr).rep(1, `with`.~/) )
  val EarlyDefTmpl = P( TmplBody ~ (`with` ~/ Constr).rep ~ TmplBody.? )
  val NamedTmpl = P( Constrs ~ TmplBody.? )

  val AnonTmpl = P( EarlyDefTmpl | NamedTmpl | TmplBody ).ignore
  val DefTmpl = P( (`extends` | `<:`) ~ AnonTmpl | TmplBody )


  val logged = mutable.Buffer.empty[String]
  implicit val logger = Logger(m => this.synchronized { logged.append(m) })

  def mkUnaryOp(opName: String, arg: Value[SType]) = opName match {
    case _ => error(s"Unknown prefix operation $opName")
  }

  val parseAsMethods = Set("*", "++", "||", "&&", "+")

  def mkBinaryOp(l: Value[SType], opName: String, r: Value[SType]): Value[SType] = opName match {
    case "==" => EQ(l, r)
    case "!=" => NEQ(l, r)
    case ">=" => GE(l, r)
    case ">"  => GT(l, r)
    case "<=" => LE(l, r)
    case "<"  => LT(l, r)
    case "-"  => builder.mkMinus(l.asValue[SLong.type], r.asValue[SLong.type])
    case "|"  => builder.mkXor(l.asValue[SByteArray], r.asValue[SByteArray])
    case "^"  => builder.mkExponentiate(l.asValue[SGroupElement.type], r.asValue[SBigInt.type])
    case _ if parseAsMethods.contains(opName) =>
      MethodCall(l, opName, IndexedSeq(r))
    case "/"  => builder.mkDivide(l.asValue[SLong.type], r.asValue[SLong.type])
    case "%"  => builder.mkModulo(l.asValue[SLong.type], r.asValue[SLong.type])
    case _ => error(s"Unknown binary operation $opName")
  }

  def apply(str: String, sigmaBuilder: SigmaBuilder): core.Parsed[Value[_ <: SType], Char, String] = {
    builder = sigmaBuilder
    (StatCtx.Expr ~ End).parse(str)
  }

  def parseType(str: String): core.Parsed[SType, Char, String] = (Type ~ End).parse(str)
}
