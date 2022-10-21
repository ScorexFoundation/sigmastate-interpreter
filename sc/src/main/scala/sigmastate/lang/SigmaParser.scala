package sigmastate.lang

import fastparse.internal.Logger
import sigmastate._
import Values._
import scalan.Nullable
import sigmastate.lang.Terms._
import sigmastate.lang.syntax.Basic._
import sigmastate.lang.syntax.{Core, Exprs}

import scala.collection.mutable
import scala.util.DynamicVariable

object SigmaParser extends Exprs with Types with Core {
  import fastparse._; import ScalaWhitespace._
  import builder._

  val currentInput = new DynamicVariable[String]("")

  override def atSrcPos[A](parserIndex: Int)(thunk: => A): A =
    builder.currentSrcCtx.withValue(Nullable(srcCtx(parserIndex))) { thunk }

  override def srcCtx(parserIndex: Int): SourceContext =
    SourceContext.fromParserIndex(parserIndex, currentInput.value)

  def TmplBodyPrelude[_:P] = P( (Annot ~ OneNLMax).rep )
  def TmplBodyStat[_:P] = P( TmplBodyPrelude ~ BlockDef | StatCtx.Expr )

  def TmplBody[_:P] = {
    P( "{" ~/ BlockLambda.? ~ Semis.? ~ TmplBodyStat.repX(sep = Semis) ~ Semis.? ~ `}` )
  }

//  val FunDef = {
//    P( (Id | `this`).! ~ LambdaDef ).map { case (name, lam) => builder.mkVal(name, NoType, lam) }
//  }

  def ValVarDef[_:P] = P( Index ~ BindPattern/*.rep(1, ",".~/)*/ ~ (`:` ~/ Type).? ~ (`=` ~/ FreeCtx.Expr) ).map {
    case (index, Ident(n,_), t, body) =>
      atSrcPos(index) {
        mkVal(n, t.getOrElse(NoType), body)
      }
    case (index, pat,_,_) => error(s"Only single name patterns supported but was $pat", Some(srcCtx(index)))
  }

  def BlockDef[_:P] = P( Dcl )

  def Constr[_:P] = P( AnnotType ~~ (NotNewline ~ ParenArgList ).repX )
  def Constrs[_:P] = P( (WL ~ Constr).rep(1, `with`) )  //fix  `with`.~/
  def EarlyDefTmpl[_:P] = P( TmplBody ~ (`with` ~/ Constr).rep ~ TmplBody.? )
  def NamedTmpl[_:P] = P( Constrs ~ TmplBody.? )

  def AnonTmpl[_:P] = P( EarlyDefTmpl | NamedTmpl | TmplBody ).ignore
  def DefTmpl[_:P] = P( (`extends` | `<:`) ~ AnonTmpl | TmplBody )


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

        case "-" =>
          if (arg.tpe.isNumTypeOrNoType)
            mkNegation(arg.asNumValue)
          else
            error(s"Numeric argument expected for '$opName' operation: $arg", arg.sourceContext)

        case "~" =>
          if (arg.tpe.isNumTypeOrNoType)
            mkBitInversion(arg.asNumValue)
          else
            error(s"Numeric argument expected for '$opName' operation: $arg", arg.sourceContext)

        case _ =>
          error(s"Unknown prefix operation $opName for $arg", arg.sourceContext)
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
        case "-" => mkMinus(l.asNumValue, r.asNumValue)

        case "|" =>
          if (l.tpe.isNumTypeOrNoType && r.tpe.isNumTypeOrNoType)
            mkBitOr(l.asNumValue, r.asNumValue)
          else
            error(s"Numeric arguments expected for '$opName' operation: ($l, $r)", l.sourceContext)

        case "&" =>
          if (l.tpe.isNumTypeOrNoType && r.tpe.isNumTypeOrNoType)
            mkBitAnd(l.asNumValue, r.asNumValue)
          else
            error(s"Numeric arguments expected for '$opName' operation: ($l, $r)", l.sourceContext)

        case _ if parseAsMethods.contains(opName) => mkMethodCallLike(l, opName, IndexedSeq(r))
        case "/" => mkDivide(l.asNumValue, r.asNumValue)
        case "%" => mkModulo(l.asNumValue, r.asNumValue)
        case _ => error(s"Unknown binary operation $opName", l.sourceContext)
      }
    }

  def parsedType(str: String): Parsed[SType] = parse(str, implicit p => Type ~ End)

  def parseType(str: String): SType = {
    val res = parsedType(str).get.value
    res
  }

  def apply(script: String, sigmaBuilder: SigmaBuilder): Parsed[Value[_ <: SType]] =
    currentInput.withValue(script) {
      parse(script, implicit p => (StatCtx.Expr ~ End))
    }
}
