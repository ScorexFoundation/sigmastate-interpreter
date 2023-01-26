package sigmastate.lang

import fastparse._
import ScalaWhitespace._
import sigmastate._
import sigmastate.SCollection.SByteArray
import Values._
import sigmastate.lang.Terms.Ident
import sigmastate.lang.syntax.Core
import syntax.Basic.error

//noinspection ForwardReference
trait Types extends Core {
  def TypeExpr[_:P]: P[Value[SType]]
  def ValVarDef[_:P]: P[Value[SType]]
//  def FunDef: P[Value[SType]]

  def Dcl[_:P] = {
    P( `val` ~/ ValVarDef /*| /* `fun` ~/ */ FunDef */ )
  }

  /** This map should be in sync with SType.allPredefTypes*/
  val predefTypes = Map(
    "Boolean" -> SBoolean, "Byte" -> SByte, "Short" -> SShort, "Int" -> SInt,"Long" -> SLong, "BigInt" -> SBigInt,
    "ByteArray" -> SByteArray,
    "AvlTree" -> SAvlTree, "Context" -> SContext, "GroupElement" -> SGroupElement, "SigmaProp" -> SSigmaProp,
    "SigmaDslBuilder" -> SGlobal,
    "Header" -> SHeader,
    "PreHeader" -> SPreHeader,
    "String" -> SString,
    "Box" -> SBox, "Unit" -> SUnit, "Any" -> SAny
  )

  def typeFromName(tn: String): Option[SType] = predefTypes.get(tn)

  def PostfixType[_:P] = P( InfixType ~ (`=>` ~/ Type ).? ).map {
    case (t, None) => t
    case (d, Some(r)) => d match {
      case STuple(items) =>
        SFunc(items, r)
      case _ =>
        SFunc(Array(d), r)
    }
  }
  def Type[_:P]: P[SType] = P( `=>`.? ~~ PostfixType ~ TypeBounds ~ `*`.? )


  // Can't cut after `Id` because it may be a `*`, in which case
  // we may need to backtrack and settle for the `*`-postfix rather than
  // an infix type
  // See http://www.scala-lang.org/files/archive/spec/2.12/03-types.html
  def InfixType[_:P] = {
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

  def CompoundType[_:P] = {
//    val Refinement[_:P] = P( OneNLMax ~ `{` ~/ Dcl.repX(sep=Semis) ~ `}` )
    def NamedType = P( (Pass ~ AnnotType).rep(1, `with`./) )
    P( Index ~ NamedType /*~~ Refinement.? | Refinement*/ ).map {
      case (_, Seq(t)) => t
      case (index, ts) => error(s"Compound types are not supported: $ts", Some(srcCtx(index)))
    }
  }
  def NLAnnot[_:P] = P( NotNewline ~ Annot )
  def AnnotType[_:P] = P(SimpleType ~~ NLAnnot.repX )

  def TypeId[_:P] = P( StableId ).map {
    case Ident(tn, _) =>
      typeFromName(tn) match {
        case Some(t) => t
        case None => STypeApply(tn)
      }
    case path => error(s"Path types are not supported: $path", path.sourceContext)
  }

  def TypeArgs[_:P] = P( "[" ~/ Type.rep(0, ",") ~ TrailingComma ~ "]" )

  def SimpleType[_:P] = {
    // Can't `cut` after the opening paren, because we might be trying to parse `()`
    // or `() => T`! only cut after parsing one type
    def TupleType = P( "(" ~/ Type.rep(0, ",") ~ TrailingComma ~ ")" ).map(items => STuple(items.toArray))
    def BasicType = P( TupleType | TypeId )
    P( Index ~ BasicType ~ TypeArgs.rep ).map {
      case (_, t: STuple, Seq()) => t
      case (_, STypeApply("Coll", IndexedSeq()), Seq(Seq(t))) => SCollection(t)
      case (_, STypeApply("Option", IndexedSeq()), Seq(Seq(t))) => SOption(t)
      case (_, SPrimType(t), Seq()) => t
      case (_, STypeApply(tn, IndexedSeq()), args) if args.isEmpty => STypeVar(tn)
      case (index, t, typeArgs) =>
        error(s"Unsupported type $t[$typeArgs]", Some(srcCtx(index)))
    }
  }

  def FunSig[_:P] = {
    def FunArg = P( Annot.rep ~ Id.! ~ (`:` ~/ Type).? ).map {
      case (n, Some(t)) => (n, t)
      case (n, None) => (n, NoType)
    }
    def Args = P( FunArg.rep(1, ",") ~ TrailingComma )
    def FunArgs = P( OneNLMax ~ "(" ~/ Args.? ~ ")" ).map(_.toSeq.flatten)
    def FunTypeArgs = P( "[" ~/ (Annot.rep ~ TypeArg).rep(1, ",") ~ TrailingComma ~ "]" )
    P( FunTypeArgs.? ~~ FunArgs.rep )
  }

  // extension method subject (type that being extended)
  // see dotty extension method http://dotty.epfl.ch/blog/2019/01/21/12th-dotty-milestone-release.html
  def DottyExtMethodSubj[_:P] = P( "(" ~/ Id.! ~  `:` ~/ Type ~ ")" )

  def TypeBounds[_:P]: P0 = P( (`>:` ~/ Type).? ~ (`<:` ~/ Type).? ).ignore
  def TypeArg[_:P]: P0 = {
    def CtxBounds = P((`:` ~/ Type).rep)
    P((Id | `_`) ~ TypeArgList.? ~ TypeBounds ~ CtxBounds).ignore
  }

  def Annot[_:P]: P0 = P( `@` ~/ SimpleType ~  ("(" ~/ (Exprs ~ (`:` ~/ `_*`).?).? ~ TrailingComma ~ ")").rep ).ignore

  def TypeArgVariant[_:P]: P0 = P( Annot.rep ~ ("+" | "-").? ~ TypeArg )

  def TypeArgList[_:P]: P0 = {
    P( "[" ~/ TypeArgVariant.rep(1, ",") ~ TrailingComma ~ "]" )  // fix
  }
  def Exprs[_:P] = P( TypeExpr.rep(1, ",") )
}
