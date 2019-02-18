package sigmastate.lang

import fastparse.noApi._
import sigmastate._
import sigmastate.SCollection.SByteArray
import Values._
import sigmastate.lang.Terms.Ident
import sigmastate.lang.syntax.Core
import syntax.Basic.error

//noinspection ForwardReference
trait Types extends Core {
  import WhitespaceApi._
  def TypeExpr: P[Value[SType]]
  def ValVarDef: P[Value[SType]]
//  def FunDef: P[Value[SType]]

  val Dcl = {
    P( `val` ~/ ValVarDef /*| /* `fun` ~/ */ FunDef */ )
  }

  /** This map should be in sync with SType.allPredefTypes*/
  val predefTypes = Map(
    "Boolean" -> SBoolean, "Byte" -> SByte, "Short" -> SShort, "Int" -> SInt,"Long" -> SLong, "BigInt" -> SBigInt,
    "ByteArray" -> SByteArray,
    "AvlTree" -> SAvlTree, "Context" -> SContext, "GroupElement" -> SGroupElement, "SigmaProp" -> SSigmaProp,
    "String" -> SString,
    "Box" -> SBox, "Unit" -> SUnit, "Any" -> SAny
  )

  def typeFromName(tn: String): Option[SType] = predefTypes.get(tn)

  val PostfixType = P( InfixType ~ (`=>` ~/ Type ).? ).map {
    case (t, None) => t
    case (d, Some(r)) => d match {
      case STuple(items) =>
        SFunc(items, r)
      case _ =>
        SFunc(IndexedSeq(d), r)
    }
  }
  val Type: P[SType] = P( `=>`.? ~~ PostfixType ~ TypeBounds ~ `*`.? )


  // Can't cut after `Id` because it may be a `*`, in which case
  // we may need to backtrack and settle for the `*`-postfix rather than
  // an infix type
  // See http://www.scala-lang.org/files/archive/spec/2.12/03-types.html
  val InfixType = {
    val RightAssoc = 1; val LeftAssoc = -1
    /** All operators op1,…,opn must have the same associativity */
    def checkAssoc(ops: Seq[String], index: Int): Int = {
      val right = ops.forall(_.endsWith(":"))
      if (right) RightAssoc
      else {
        val left = ops.forall(!_.endsWith(":"))
        if (left) LeftAssoc
        else
          error(s"All operators $ops must have the same associativity.", Some(srcCtx(index)))
      }
    }
    def buildInfix(head: SType, tail: Seq[(String, SType)], index: Int): SType = {
      val associativity = checkAssoc(tail.map(_._1), index)
      if (associativity == RightAssoc) {
        tail.foldRight(head) { case ((op, t), acc) => STypeApply(op, IndexedSeq(t, acc)) }
      }
      else {
        tail.foldLeft(head) { case (acc, (op, t)) => STypeApply(op, IndexedSeq(acc, t)) }
      }
    }
    P( Index ~ CompoundType ~~ (NotNewline ~ Id.! ~~ OneNLMax ~ CompoundType).repX ).map {
      case (index, t, h) => buildInfix(t, h, index)
    }
  }

  val CompoundType = {
//    val Refinement = P( OneNLMax ~ `{` ~/ Dcl.repX(sep=Semis) ~ `}` )
    val NamedType = P( (Pass ~ AnnotType).rep(1, `with`.~/) )
    P( Index ~ NamedType /*~~ Refinement.? | Refinement*/ ).map {
      case (_, Seq(t)) => t
      case (index, ts) => error(s"Compound types are not supported: $ts", Some(srcCtx(index)))
    }
  }
  val NLAnnot = P( NotNewline ~ Annot )
  val AnnotType = P(SimpleType ~~ NLAnnot.repX )

  val TypeId = P( StableId ).map {
    case Ident(tn, _) =>
      typeFromName(tn) match {
        case Some(t) => t
        case None => STypeApply(tn)
      }
    case path => error(s"Path types are not supported: $path", path.sourceContext)
  }

  val TypeArgs = P( "[" ~/ Type.repTC() ~ "]" )

  val SimpleType = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    val TupleType = P( "(" ~/ Type.repTC() ~ ")" ).map(items => STuple(items.toIndexedSeq))
    val BasicType = P( TupleType | TypeId )
    P( Index ~ BasicType ~ TypeArgs.rep ).map {
      case (_, t: STuple, Seq()) => t
      case (_, STypeApply("Coll", IndexedSeq()), Seq(Seq(t))) => SCollection(t)
      case (_, STypeApply("Option", IndexedSeq()), Seq(Seq(t))) => SOption(t)
      case (_, SPrimType(t), Seq()) => t
      case (_, STypeApply(tn, IndexedSeq()), args) if args.isEmpty => STypeIdent(tn)
      case (index, t, typeArgs) =>
        error(s"Unsupported type $t[$typeArgs]", Some(srcCtx(index)))
    }
  }

  val FunSig = {
    val FunArg = P( Annot.rep ~ Id.! ~ (`:` ~/ Type).? ).map {
      case (n, Some(t)) => (n, t)
      case (n, None) => (n, NoType)
    }
    val Args = P( FunArg.repTC(1) )
    val FunArgs = P( OneNLMax ~ "(" ~/ Args.? ~ ")" ).map(_.toSeq.flatten)
    val FunTypeArgs = P( "[" ~/ (Annot.rep ~ TypeArg).repTC(1) ~ "]" )
    P( FunTypeArgs.? ~~ FunArgs.rep )
  }

  // extension method subject (type that being extended)
  // see dotty extension method http://dotty.epfl.ch/blog/2019/01/21/12th-dotty-milestone-release.html
  val DottyExtMethodSubj = P( "(" ~/ Id.! ~  `:` ~/ Type ~ ")" )

  val TypeBounds: P0 = P( (`>:` ~/ Type).? ~ (`<:` ~/ Type).? ).ignore
  val TypeArg: P0 = {
    val CtxBounds = P((`:` ~/ Type).rep)
    P((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds).ignore
  }

  val Annot: P0 = P( `@` ~/ SimpleType ~  ("(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ TrailingComma ~ ")").rep ).ignore

  val TypeArgList: P0 = {
    val Variant: P0 = P( Annot.rep ~ CharIn("+-").? ~ TypeArg )
    P( "[" ~/ Variant.repTC(1) ~ "]" )
  }
  val Exprs = P( TypeExpr.rep(1, ",") )
}
