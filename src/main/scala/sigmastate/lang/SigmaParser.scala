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

class SigmaParser(str: String,
                  override val builder: SigmaBuilder) extends Exprs with Types with Core {
  import fastparse.noApi._
  import WhitespaceApi._

  override def srcCtx(parserIndex: Int): SourceContext = SourceContext(parserIndex, str)

  val TmplBody = {
    val Prelude = P( (Annot ~ OneNLMax).rep )
    val TmplStat = P( Prelude ~ BlockDef | StatCtx.Expr )
    P( "{" ~/ BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.? ~ `}` )
  }

//  val FunDef = {
//    P( (Id | `this`).! ~ LambdaDef ).map { case (name, lam) => builder.mkVal(name, NoType, lam) }
//  }

  val ValVarDef = P( Index ~ BindPattern/*.rep(1, ",".~/)*/ ~ (`:` ~/ Type).? ~ (`=` ~/ FreeCtx.Expr) ).map {
    case (index, Ident(n,_), t, body) => builder.mkVal(n, t.getOrElse(NoType), body, srcCtx(index))
    case (_, pat,_,_) => error(s"Only single name patterns supported but was $pat")
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
    case "-" if arg.isInstanceOf[Constant[_]] && arg.tpe.isNumType =>
      arg match {
        case IntConstant(value) =>
          builder.mkConstant[SInt.type](-value, SInt)
        case LongConstant(value) =>
          builder.mkConstant[SLong.type](-value, SLong)
        case _ => error(s"cannot prefix $arg with op $opName")
      }
    case "!" => builder.mkLogicalNot(arg.asBoolValue)
    case "-" => builder.mkNegation(arg.asNumValue)
    case "~" => builder.mkBitInversion(arg.asNumValue)
    case _ => error(s"Unknown prefix operation $opName for $arg")
  }

  val parseAsMethods = Set("*", "++", "||", "&&", "+", "^", "<<", ">>", ">>>")

  def mkBinaryOp(l: Value[SType], opName: String, r: Value[SType]): Value[SType] = opName match {
    case "==" => EQ(l, r)
    case "!=" => NEQ(l, r)
    case ">=" => GE(l, r)
    case ">"  => GT(l, r)
    case "<=" => LE(l, r)
    case "<"  => LT(l, r)
    case "-"  => builder.mkMinus(l.asValue[SLong.type], r.asValue[SLong.type])
    case "|"  => builder.mkBitOr(l.asNumValue, r.asNumValue)
    case "&"  => builder.mkBitAnd(l.asNumValue, r.asNumValue)
    case _ if parseAsMethods.contains(opName) =>
      MethodCallLike(l, opName, IndexedSeq(r))
    case "/"  => builder.mkDivide(l.asValue[SLong.type], r.asValue[SLong.type])
    case "%"  => builder.mkModulo(l.asValue[SLong.type], r.asValue[SLong.type])
    case _ => error(s"Unknown binary operation $opName")
  }

  def parse: core.Parsed[Value[_ <: SType], Char, String] = (StatCtx.Expr ~ End).parse(str)

  def parsedType: core.Parsed[SType, Char, String] = (Type ~ End).parse(str)

  def parseType: SType = {
    val res = parsedType.get.value
    res
  }

}

object SigmaParser {

  def apply(str: String, sigmaBuilder: SigmaBuilder): SigmaParser =
    new SigmaParser(str, sigmaBuilder)

  def parsedType(str: String): core.Parsed[SType, Char, String] =
    new SigmaParser(str, StdSigmaBuilder).parsedType

  def parseType(x: String): SType = new SigmaParser(x, StdSigmaBuilder).parseType
}
