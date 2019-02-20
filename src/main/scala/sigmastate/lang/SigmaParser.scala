package sigmastate.lang

import fastparse.core.Logger
import fastparse.core
import sigmastate._
import Values._
import scalan.Nullable
import sigmastate.lang.Terms._
import sigmastate.SCollection.SByteArray
import sigmastate.lang.syntax.Basic._
import sigmastate.lang.syntax.{Core, Exprs}

import scala.collection.mutable
import scala.util.DynamicVariable

object SigmaParser extends Exprs with Types with Core {
  import fastparse.noApi._
  import WhitespaceApi._
  import builder._

  val currentInput = new DynamicVariable[String]("")

  override def atSrcPos[A](parserIndex: Int)(thunk: => A): A =
    builder.currentSrcCtx.withValue(Nullable(srcCtx(parserIndex))) { thunk }

  override def srcCtx(parserIndex: Int): SourceContext =
    SourceContext.fromParserIndex(parserIndex, currentInput.value)

  val TmplBody = {
    val Prelude = P( (Annot ~ OneNLMax).rep )
    val TmplStat = P( Prelude ~ BlockDef | StatCtx.Expr )
    P( "{" ~/ BlockLambda.? ~ Semis.? ~ TmplStat.repX(sep = Semis) ~ Semis.? ~ `}` )
  }

//  val FunDef = {
//    P( (Id | `this`).! ~ LambdaDef ).map { case (name, lam) => builder.mkVal(name, NoType, lam) }
//  }

  val ValVarDef = P( Index ~ BindPattern/*.rep(1, ",".~/)*/ ~ (`:` ~/ Type).? ~ (`=` ~/ FreeCtx.Expr) ).map {
    case (index, Ident(n,_), t, body) =>
      atSrcPos(index) {
        mkVal(n, t.getOrElse(NoType), body)
      }
    case (index, pat,_,_) => error(s"Only single name patterns supported but was $pat", Some(srcCtx(index)))
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

  def mkUnaryOp(opName: String, arg: Value[SType]) =
    builder.currentSrcCtx.withValue(arg.sourceContext) {
      opName match {
        case "-" if arg.isInstanceOf[Constant[_]] && arg.tpe.isNumType =>
          arg match {
            case IntConstant(value) =>
              mkConstant[SInt.type](-value, SInt)
            case LongConstant(value) =>
              mkConstant[SLong.type](-value, SLong)
            case _ => error(s"cannot prefix $arg with op $opName", arg.sourceContext)
          }
        case "!" => mkLogicalNot(arg.asBoolValue)
        case "-" => mkNegation(arg.asNumValue)
        case "~" => mkBitInversion(arg.asNumValue)
        case _ => error(s"Unknown prefix operation $opName for $arg", arg.sourceContext)
      }
    }

  val parseAsMethods = Set("*", "++", "||", "&&", "+", "^", "<<", ">>", ">>>")

  def mkBinaryOp(l: Value[SType], opName: String, r: Value[SType]): Value[SType] =
    builder.currentSrcCtx.withValue(l.sourceContext) {
      opName match {
        case "==" => mkEQ(l, r)
        case "!=" => mkNEQ(l, r)
        case ">=" => mkGE(l, r)
        case ">" => mkGT(l, r)
        case "<=" => mkLE(l, r)
        case "<" => mkLT(l, r)
        case "-" => mkMinus(l.asValue[SLong.type], r.asValue[SLong.type])
        case "|" => mkBitOr(l.asNumValue, r.asNumValue)
        case "&" => mkBitAnd(l.asNumValue, r.asNumValue)
        case _ if parseAsMethods.contains(opName) => mkMethodCallLike(l, opName, IndexedSeq(r))
        case "/" => mkDivide(l.asValue[SLong.type], r.asValue[SLong.type])
        case "%" => mkModulo(l.asValue[SLong.type], r.asValue[SLong.type])
        case _ => error(s"Unknown binary operation $opName", l.sourceContext)
      }
    }

  def parsedType(str: String): core.Parsed[SType, Char, String] = (Type ~ End).parse(str)

  def parseType(str: String): SType = {
    val res = parsedType(str).get.value
    res
  }

  def apply(script: String, sigmaBuilder: SigmaBuilder): core.Parsed[Value[_ <: SType], Char, String] =
    currentInput.withValue(script) {
      (StatCtx.Expr ~ End).parse(script)
    }
}
