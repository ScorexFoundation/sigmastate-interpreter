package sigmastate.lang

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._
import sigmastate.utxo.{SizeOf, Inputs}

/**
  * Analyses for typed lambda calculus expressions.  A simple free variable
  * analysis plus name and type analysis.  There are two versions of the
  * latter here: one (tipe) that constructs an explicit environment separate
  * from the AST, and one (tipe2) that represents names by references to the
  * nodes of their binding lambda expressions.
  */
class Analyser(globalEnv: Map[String, Any], tree : SigmaTree) extends Attribution {

  import PrettyPrinter.formattedLayout
  import org.bitbucket.inkytonik.kiama.util.Messaging.{check, collectMessages, noMessages, message, Messages}

  /** The semantic error messages for the tree. */
  lazy val errors : Messages =
    collectMessages(tree) {
      case e : SValue =>
//        checkType(e, tipe) ++
            check(e) {
              case Apply(e1, e2) =>
                check(tipe(e1)) {
                  case _: SFunc => noMessages
                  case _ =>
                    message(e1, "application of non-function")
                }
              case Ident(x, _) =>
                message(e, s"unknown name '$x'", tipe(e) == NoType)
            }
    }

  /**
    * The variables that are free in the given expression.
    */
  val fv : SValue => Set[Idn] =
    attr {
      case IntConstant(_)            => Set()
      case Ident(v, _)            => Set(v)
      case Lambda(args, _, Some(e))      => fv(e) -- args.map(_._1).toSet
      case Apply(e1, args)       => args.foldLeft(fv(e1))((acc, e) => acc ++ fv(e))
      case Let(i, t, e) => fv(e)
      case Block(bs, e) => {
        val fbv = bs.map(_.value).map(fv).flatten.toSet
        val bvars = bs.map(_.name).toSet
        fbv ++ (fv(e) -- bvars)
      }
    }

  /**
    * The environment of an expression is the list of variable names that
    * are visible in that expression and their types.
    */
  val env : SValue => List[(Idn, SType)] =
    attr {

      // Inside a lambda expression the bound variable is now visible
      // in addition to everything that is visible from above. Note
      // that an inner declaration of a var hides an outer declaration
      // of the same var since we add inner bindings at the beginning
      // of the env and we search the env list below in tipe from
      // beginning to end
      case tree.parent(p @ Lambda(args, t, _)) =>
        (args ++ env(p)).toList

      // Other expressions do not bind new identifiers so they just
      // get their environment from their parent
      case tree.parent(p : SValue) =>
        env(p)

      // global level contains all predefined names
      case _ =>
        val predef = SigmaPredef.predefinedEnv.mapValues(_.tpe).toList
        predef ++ globalEnv.mapValues(SType.typeOfData).toList
    }

  /**
    * Check that the type of `e` is its expected type or unknown. If not,
    * return an error. `tipe` is used to obtain the type.
    */
  def checkType(e : SValue, tipe : SValue => SType) : Messages = {
    val expectedType = exptipe(e)
    message(e, s"expected ${formattedLayout(expectedType)}, found ${formattedLayout(tipe(e))}",
      tipe(e) != NoType && expectedType != NoType && tipe(e) != expectedType)
  }

  /**
    * The type of an expression.  Checks constituent names and types.  Uses
    * the env attribute to get the bound variables and their types.
    */
  val tipe : SValue => SType =
    attr {
      // A number is always of integer type
      case v: EvaluatedValue[_] => v.tpe
      case v: NotReadyValueInt => v.tpe
      case Inputs => Inputs.tpe
      
      // An operation must be applied to two arguments of the same type
      case GT(e1, e2) => binOpTipe(e1, e2)(SInt, SBoolean)
      case LT(e1, e2) => binOpTipe(e1, e2)(SInt, SBoolean)
      case GE(e1, e2) => binOpTipe(e1, e2)(SInt, SBoolean)
      case LE(e1, e2) => binOpTipe(e1, e2)(SInt, SBoolean)
      case EQ(e1, e2) => binOpTipe(e1, e2)(tipe(e1), SBoolean)
      case NEQ(e1, e2) => binOpTipe(e1, e2)(tipe(e1), SBoolean)

      case AND(xs) =>
        if (xs.tpe.elemType == SBoolean) SBoolean else NoType
      case OR(xs) =>
        if (xs.tpe.elemType == SBoolean) SBoolean else NoType

      // An identifier is looked up in the environement of the current
      // expression.  If we find it, then we use the type that we find.
      // Otherwise it's an error.
      case e @ Ident(x,_) =>
        env(e).collectFirst {
          case (y, t) if x == y => t
        }.getOrElse {
          NoType
        }

      // A lambda expression is a function from the type of its argument
      // to the type of the body expression
      case Lambda(args, t, body) =>
        if (t == NoType)
          SFunc(args.map(_._2), body.fold(NoType: SType)(tipe))
        else
          SFunc(args.map(_._2), t)

      // For an application we first determine the type of the expression
      // being applied.  If it's a function then the application has type
      // of that function's return type. If it's not a function then any
      // type is allowed. We check separately that only functions are
      // applied.
      case Apply(e1, e2) =>
        tipe(e1) match {
          case SFunc(t1, t2) =>
            if (e2.map(tipe) == t1)
              t2
            else
              NoType
          case _ =>
            NoType
        }


      // A parallel returns the type of the body expression
      case Block(bs, e) =>
        tipe(e)
      case e => sys.error(s"Don't know how to compute type for $e")
    }

  def binOpTipe(e1: SValue, e2: SValue)(arg: SType, res: SType): SType =
    if ((tipe(e1) == arg) && (tipe(e2) == arg))
      res
    else
      NoType

  /** The expected type of an expression. */
  val exptipe : SValue => SType =
    attr {

      // An applied expression is allowed to be anything. We check
      // elsewhere that it's a function.
      case e @ tree.parent(Apply(e1, _)) if e eq e1 => SAny

      // An argument is expected to be of the function's input type
      case e @ tree.parent(Apply(e1, e2)) if e eq e2 => SAny
//        tipe(e1) match {
//          case FunType(t1, _) =>
//            t1
//          case _ =>
//            NoType()
//        }

      // The type of let body must match the declared type
      case e @ tree.parent(p @ Let(_, Some(t), body)) if e eq body => t

      // The operands of an operation should be integers
      case tree.parent(Plus(_, _)) => SInt
      case tree.parent(Minus(_, _)) => SInt

      // Other expressions are allowed to be anything
      case _ => SAny
    }

//  /**
//    * The declaration (if any) of an identifier use.
//    */
//  def decl : Var => Option[Lam] =
//    attr {
//      case e @ Var(x) => lookup(x)(e)
//    }
//
}

