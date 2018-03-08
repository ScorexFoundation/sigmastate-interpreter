package sigmastate.lang

import fastparse.core.Logger
import fastparse.{WhitespaceApi, core}
import sigmastate._
import sigmastate.lang.Terms._
import scorex.crypto.encode.Base58
import sigmastate.lang.syntax.Basic._
import sigmastate.utxo.SizeOf

import scala.collection.mutable

object SigmaParser extends Exprs with Types with Core {
  import fastparse.noApi._
  import WhitespaceApi._

  val TmplBody = {
    val Prelude = P( (Annot ~ OneNLMax).rep )
    val TmplStat = P( Prelude ~ BlockDef | StatCtx.Expr )
    P( "{" ~/ BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.? ~ `}` )
  }

  val FunDef = {
    P( (Id | `this`).! ~ LambdaDef ).map { case (name, lam) => Let(name, lam) }
  }

  val ValVarDef = P( BindPattern/*.rep(1, ",".~/)*/ ~ (`:` ~/ Type).? ~ (`=` ~/ FreeCtx.Expr) ).map {
    case (Ident(IndexedSeq(n),_), t, body) => Let(n, t, body)
    case (pat,_,_) => error(s"Only single name patterns supported but was $pat")
  }

  val BlockDef = P( Dcl )

  val Constrs = P( (WL ~ Constr).rep(1, `with`.~/) )
  val EarlyDefTmpl = P( TmplBody ~ (`with` ~/ Constr).rep ~ TmplBody.? )
  val NamedTmpl = P( Constrs ~ TmplBody.? )

  val DefTmpl = P( (`extends` | `<:`) ~ AnonTmpl | TmplBody )
  val AnonTmpl = P( EarlyDefTmpl | NamedTmpl | TmplBody ).ignore
  val Constr = P( AnnotType ~~ (NotNewline ~ ParenArgList ).repX )

  val logged = mutable.Buffer.empty[String]
  implicit val logger = Logger(logged.append(_))

  private val Base58Chars = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  private def byteVectorP: P[ByteArrayConstant] =
    P("base58'" ~ CharsWhileIn(Base58Chars).! ~ "'")
        .map(x => ByteArrayConstant(Base58.decode(x).get))

  def mkUnaryOp(opName: String, arg: Value[SType]) = opName match {
    case "!" => Not(arg.asValue[SBoolean.type])
    case _ => error(s"Unknown prefix operation $opName")
  }

  def mkBinaryOp(opName: String, l: Value[SType], r: Value[SType]): Value[SType] = opName match {
    case "||" => typed[SBoolean.type, SBoolean.type](l, r)(OR.apply)
    case "&&" => typed[SBoolean.type, SBoolean.type](l, r)(AND.apply)
    case "==" => EQ(l, r)
    case ">=" => typed[SInt.type, SInt.type](l, r)(GE)
    case ">"  => typed[SInt.type, SInt.type](l, r)(GT)
    case "<=" => typed[SInt.type, SInt.type](l, r)(LE)
    case "<"  => typed[SInt.type, SInt.type](l, r)(LT)
    case "+"  => typed[SInt.type, SInt.type](l, r)(Plus)
    case "-"  => typed[SInt.type, SInt.type](l, r)(Minus)
    case "," => Comma(l, r)
    case _ => error(s"Unknown binary operation $opName")
  }

  def apply(str: String): core.Parsed[Value[_ <: SType], Char, String] = (StatCtx.Expr ~ End).log().parse(str)
}
