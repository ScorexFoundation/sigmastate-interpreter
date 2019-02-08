package sigmastate.lang.syntax

import fastparse.noApi._
import scalan.Nullable
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms.{Apply, ApplyTypes, Ident, Lambda, MethodCallLike, Select, Val, ValueOps}
import sigmastate.lang._
import sigmastate.lang.SigmaPredef._
import sigmastate.lang.syntax.Basic._

import scala.annotation.tailrec

//noinspection ForwardReference,TypeAnnotation
trait Exprs extends Core with Types {

  import WhitespaceApi._
  def AnonTmpl: P0
  def BlockDef: P[Value[SType]]

  // Depending on where an expression is located, subtle behavior around
  // semicolon inference and arrow-type-ascriptions like i: a => b
  // varies.

  // Expressions used as statements, directly within a {block}
  object StatCtx extends WsCtx(semiInference=true, arrowTypeAscriptions=false)
  // Expressions nested within other expressions
  object ExprCtx extends WsCtx(semiInference=false, arrowTypeAscriptions=true)
  // Expressions directly within a `val x = ...` or `def x = ...`
  object FreeCtx extends WsCtx(semiInference=true, arrowTypeAscriptions=true)

  val TypeExpr = ExprCtx.Expr

  private val predefFuncRegistry = new PredefinedFuncRegistry(builder)
  import predefFuncRegistry._

  //noinspection TypeAnnotation,ForwardReference
  class WsCtx(semiInference: Boolean, arrowTypeAscriptions: Boolean){

    val OneSemiMax = if (semiInference) OneNLMax else Pass
    val NoSemis = if (semiInference) NotNewline else Pass

    val Expr: P[Value[SType]] = {
      val If = {
        val Else = P( Semi.? ~ `else` ~/ Expr )
        P( `if` ~/ "(" ~ ExprCtx.Expr ~ ")" ~ Expr ~ Else ).map {
          case (c, t, e) => builder.mkIf(c.asValue[SBoolean.type], t, e)
        }
      }
      val Fun = P(`def` ~ FunDef)

      val LambdaRhs = if (semiInference) P( BlockChunk.map {
        case (_ , b)  => mkBlock(b)
      } )
      else P( Expr )
//      val ParenedLambda = P( Parened ~~ (WL ~ `=>` ~ LambdaRhs.? /*| ExprSuffix ~~ PostfixSuffix ~ SuperPostfixSuffix*/) ).map {
//        case (args, None) => mkLambda(args, UnitConstant)
//        case (args, Some(body)) => mkLambda(args, body)
//      }
      val PostfixLambda = P( PostfixExpr ~ (`=>` ~ LambdaRhs.? | SuperPostfixSuffix).? ).map {
        case (e, None) => e
        case (e, Some(None)) => e
        case (Tuple(args), Some(Some(body))) => mkLambda(args, body)
        case (e, Some(body)) => error(s"Invalid declaration of lambda $e => $body")
      }
      val SmallerExprOrLambda = P( /*ParenedLambda |*/ PostfixLambda )
//      val Arg = (Id.! ~ `:` ~/ Type).map { case (n, t) => Ident(IndexedSeq(n), t)}
      P( If | Fun | SmallerExprOrLambda )
    }

    val SuperPostfixSuffix = P( (`=` ~/ Expr).? /*~ MatchAscriptionSuffix.?*/ )
    val AscriptionType = (if (arrowTypeAscriptions) P( Type ) else P( InfixType )).ignore
    val Ascription = P( `:` ~/ (`_*` |  AscriptionType | Annot.rep(1)) )
    val MatchAscriptionSuffix = P(`match` ~/ "{" ~ CaseClauses | Ascription)
    val ExprPrefix = P( WL ~ CharIn("-+!~").! ~~ !syntax.Basic.OpChar ~ WS)
    val ExprSuffix = P(
      (WL ~ "." ~/ Id.!.map(Ident(_))
      | WL ~ TypeArgs.map(items => STypeApply("", items.toIndexedSeq))
      | NoSemis ~ ArgList ).repX /* ~~ (NoSemis  ~ `_`).? */
    )

    val PrefixExpr = P( ExprPrefix.? ~ SimpleExpr ).map {
      case (Some(op), e) => mkUnaryOp(op, e)
      case (None, e) => e
    }

    // Intermediate `WL` needs to always be non-cutting, because you need to
    // backtrack out of `InfixSuffix` into `PostFixSuffix` if it doesn't work out
    val InfixSuffix = P( NoSemis ~~ WL ~~ Id.! /*~ TypeArgs.?*/ ~~ OneSemiMax ~ PrefixExpr ~~ ExprSuffix).map {
      case (op, f, args) =>
        val rhs = applySuffix(f, args)
        (op, rhs)
    }
    val PostFix = P( NoSemis ~~ WL ~~ Id.! ~ Newline.? ).map(Ident(_))

    val PostfixSuffix = P( InfixSuffix.repX ~~ PostFix.?)

    val PostfixExpr = P( PrefixExpr ~~ ExprSuffix ~~ PostfixSuffix ).map {
      case (prefix, suffix, (infixOps, postfix)) =>
        val lhs = applySuffix(prefix, suffix)
        val obj = mkInfixTree(lhs, infixOps)
        postfix.fold(obj) {
          case Ident(name, _) =>
            builder.mkMethodCallLike(obj, name, IndexedSeq.empty)
        }
    }

    val Parened = P ( "(" ~/ TypeExpr.repTC() ~ ")" )
    val SimpleExpr = {
//      val New = P( `new` ~/ AnonTmpl )

      P( /*New | */ BlockExpr
        | ExprLiteral
        | StableId //.map { case Ident(ps, t) => mkIdent(ps, t) }
        | `_`.!.map(Ident(_))
        | Parened.map(items =>
            if (items.isEmpty) UnitConstant
            else if (items.lengthCompare(1) == 0) items.head
            else builder.mkTuple(items)) )
    }
    val Guard : P0 = P( `if` ~/ PostfixExpr ).ignore
  }

  protected def mkIdent(nameParts: String, tpe: SType = NoType): SValue = {
    builder.mkIdent(nameParts, tpe)
  }

  protected def mkLambda(args: Seq[Value[SType]], body: Value[SType]): Value[SType] = {
    val names = args.map { case Ident(n, t) => (n, t) }
    builder.mkLambda(names.toIndexedSeq, NoType, Some(body))
  }

  protected def mkApply(func: Value[SType], args: IndexedSeq[Value[SType]]): Value[SType] = (func, args) match {
    case _ => builder.mkApply(func, args)
  }

  def mkApplyTypes(input: Value[SType], targs: IndexedSeq[SType]): Value[SType] = {
//    val subst = targs.zipWithIndex.map { case (t, i) => (STypeIdent(s"_${i + 1}") -> t) }.toMap
    builder.mkApplyTypes(input, targs)
  }

  /** The precedence of an infix operator is determined by the operator's first character.
    * Characters are listed below in increasing order of precedence, with characters on the same line
    * having the same precedence. */
  val priorityList = Seq(
    // all letters have lowerst precedence 0
    Seq('|'),
    Seq('^'),
    Seq('&'),
    Seq('=', '!'),
    Seq('<', '>'),
    Seq(':', '>'),
    Seq('+', '-'),
    Seq('*', '/', '%')
  )
  
  val priorityMap = (for { 
    (xs, p) <- priorityList.zipWithIndex.map { case (xs, i) => (xs, i + 1) }
    x <- xs
  } yield (x, p)).toMap

  @inline def precedenceOf(ch: Char): Int = if (priorityMap.contains(ch)) priorityMap(ch) else 0
  @inline def precedenceOf(op: String): Int = precedenceOf(op(0))

  protected[lang] def mkInfixTree(lhs: SValue, rhss: Seq[(String, SValue)]): SValue = {
    @tailrec def build(wait: List[(SValue, String)], x: SValue, rest: List[(String, SValue)]): SValue = (wait, rest) match {
      case ((l, op1) :: stack, (op2, r) :: tail) =>
        if (precedenceOf(op1) >= precedenceOf(op2)) {
          val n = mkBinaryOp(l, op1, x)
          build(stack, n, rest)
        }
        else {
          build((x, op2) :: wait, r, tail)
        }

      case (Nil, Nil) => x
      case (Nil, (op, r):: Nil) => mkBinaryOp(x, op, r)
      case ((l, op) :: Nil, Nil) => mkBinaryOp(l, op, x)

      case (Nil, (op, r) :: tail) =>
        build((x, op) :: Nil, r, tail)
      case ((l, op) :: stack, Nil) =>
        val n = mkBinaryOp(l, op, x)
        build(stack, n, Nil)
    }
    build(Nil, lhs, rhss.toList)
  }



  protected def applySuffix(f: Value[SType], args: Seq[SigmaNode]): Value[SType] = {
    val rhs = args.foldLeft(f)((acc, arg) => arg match {
      case Ident(name, _) => builder.mkSelect(acc, name)
      case UnitConstant => mkApply(acc, IndexedSeq.empty)
      case Tuple(xs) => mkApply(acc, xs)
      case STypeApply("", targs) => mkApplyTypes(acc, targs)
      case arg: SValue => acc match {
        case Ident(name, _) if name == "ZKProof" => arg match {
          case Terms.Block(_, body) => Apply(ZKProofFunc.sym, IndexedSeq(body))
          case nonBlock => error(s"expected block parameter for ZKProof, got $nonBlock")
        }
        case _ => mkApply(acc, IndexedSeq(arg))
      }
      case _ => error(s"Error after expression $f: invalid suffixes $args")
    })
    rhs
  }

  val FunDef = {
    val Body = P( WL ~ `=` ~/ FreeCtx.Expr )
    P(Index ~ DottyExtMethodSubj.? ~ Id.! ~ FunSig ~ (`:` ~/ Type).? ~~ Body ).map {
      case (index, None, n, args, resType, body) =>
        val lambda = builder.mkLambda(args.headOption.getOrElse(Seq()).toIndexedSeq, resType.getOrElse(NoType), Some(body))
        builder.currentSrcCtx.withValue(Nullable(srcCtx(index))) {
          builder.mkVal(n, resType.getOrElse(NoType), lambda)
        }
      case (index, Some(dottyExtSubj), n, args, resType, body) if args.length <= 1 =>
        val combinedArgs = Seq(dottyExtSubj) ++ args.headOption.getOrElse(Seq())
        val lambda = builder.mkLambda(combinedArgs.toIndexedSeq, resType.getOrElse(NoType), Some(body))
        builder.currentSrcCtx.withValue(Nullable(srcCtx(index))) {
          builder.mkVal(n, resType.getOrElse(NoType), lambda)
        }
      case (index, dottyExt, n, secs, resType, body) =>
        error(s"Function can only have single argument list: def ${dottyExt.getOrElse("")} $n($secs): ${resType.getOrElse(NoType)} = $body")
    }
  }

  val SimplePattern = {
    val TupleEx = P( "(" ~/ Pattern.repTC() ~ ")" )
    val Extractor = P( StableId /* ~ TypeArgs.?*/ ~ TupleEx.? )
//    val Thingy = P( `_` ~ (`:` ~/ TypePat).? ~ !("*" ~~ !syntax.Basic.OpChar) )
    P( /*Thingy | PatLiteral |*/ TupleEx | Extractor | VarId.!.map(Ident(_)))
  }

  val BlockExpr = P( "{" ~/ (/*CaseClauses |*/ Block ~ "}") )

  val BlockLambdaHead = {
    val Arg = P( Annot.rep ~ Id.! ~ (`:` ~/ Type).? ).map {
      case (n, Some(t)) => (n, t)
      case (n, None) => (n, NoType)
    }
    val Args = P( Arg.repTC(1) )
    P( OneNLMax ~ "(" ~ Args.? ~ ")" ).map(_.toSeq.flatten)
  }

  val BlockLambda = P( BlockLambdaHead ~ `=>` )

  val BlockChunk = {
    val Prelude = P( Annot.rep ~ `lazy`.? )
    val BlockStat = P( Prelude ~ BlockDef | StatCtx.Expr )
    P( BlockLambda.rep ~ BlockStat.rep(sep = Semis) )
  }

  def extractBlockStats(stats: Seq[SValue]): (Seq[Val], SValue) = {
    if (stats.nonEmpty) {
      val lets = stats.iterator.take(stats.size - 1).map {
        case l: Val => l
        case _ => error(s"Block should contain a list of Val bindings and one expression: but was $stats")
      }
      (lets.toList, stats.last)
    }
    else
      (Seq(), UnitConstant)
  }

  protected def mkBlock(stats: Seq[SValue]): SValue = {
    val (lets, body) = extractBlockStats(stats)
    builder.mkBlock(lets, body)
  }

  def BaseBlock(end: P0)(implicit name: sourcecode.Name): P[Value[SType]] = {
    val BlockEnd = P( Semis.? ~ &(end) )
    val Body = P( BlockChunk.repX(sep = Semis) )
    P( Semis.? ~ BlockLambda.? ~ Body ~/ BlockEnd ).map {
      case (Some(args), Seq((Seq(), Seq(b)))) =>
        builder.mkLambda(args.toIndexedSeq, NoType, Some(b))
      case (Some(args), bodyItems) =>
        val block = mkBlock(bodyItems.flatMap {
          case (Seq(), exprs) => exprs
        })
        builder.mkLambda(args.toIndexedSeq, NoType, Some(block))
      case (None, bodyItems) =>
        mkBlock(bodyItems.flatMap {
          case (Seq(), exprs) => exprs
        })
    }
  }
  val Block = BaseBlock("}")
  val CaseBlock = BaseBlock("}" | `case`)

  val Patterns: P0 = P( Pattern.rep(1, sep = ",".~/) )
  val Pattern: P0 = P( (WL ~ TypeOrBindPattern).rep(1, sep = "|".~/) )
  val TypePattern = P( (`_` | BacktickId | VarId) ~ `:` ~ TypePat )
  val TypeOrBindPattern: P0 = P( TypePattern | BindPattern ).ignore
  val BindPattern = {
    val InfixPattern = P( SimplePattern /*~ (Id ~/ SimplePattern).rep | `_*`*/ )
//    val Binding = P( (Id | `_`) ~ `@` )
    P( /*Binding ~ InfixPattern | */ InfixPattern /*| VarId*/ )
  }

  val TypePat = P( CompoundType )
  val ParenArgList = P( "(" ~/ Exprs /*~ (`:` ~/ `_*`).?*/.? ~ TrailingComma ~ ")" ).map {
    case Some(exprs) => builder.mkTuple(exprs)
    case None => UnitConstant
  }
  val ArgList = P( ParenArgList | OneNLMax ~ BlockExpr )

  val CaseClauses: P0 = {
    // Need to lookahead for `class` and `object` because
    // the block { case object X } is not a case clause!
    val CaseClause: P0 = P( `case` ~/ Pattern ~ ExprCtx.Guard.? ~ `=>` ~ CaseBlock  ).ignore
    P( CaseClause.rep(1) ~ "}"  )
  }
}
