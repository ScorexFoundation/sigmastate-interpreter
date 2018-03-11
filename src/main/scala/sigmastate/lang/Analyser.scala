package sigmastate.lang

import org.bitbucket.inkytonik.kiama.attribution.Attribution
import sigmastate._
import sigmastate.Values._
import sigmastate.lang.Terms._

/**
  * Analyses for typed lambda calculus expressions.  A simple free variable
  * analysis plus name and type analysis.  There are two versions of the
  * latter here: one (tipe) that constructs an explicit environment separate
  * from the AST, and one (tipe2) that represents names by references to the
  * nodes of their binding lambda expressions.
  */
class Analyser(tree : SigmaTree) extends Attribution {

  import PrettyPrinter.formattedLayout
  import org.bitbucket.inkytonik.kiama.util.Messaging.{check, collectMessages, message, Messages}

  /**
    * The semantic error messages for the tree. This one uses the `tipe`
    * attribute.
    */
  lazy val errors : Messages =
    collectMessages(tree) {
      case e : SValue => ???
//        checkType(e, tipe) ++
//            check(e) {
//              case App(e1, e2) =>
//                check(tipe(e1)) {
//                  case _ : IntType =>
//                    message(e1, "application of non-function")
//                }
//              case Var(x) =>
//                message(e, s"'$x' unknown", tipe(e) == UnknownType())
//            }
    }

  /**
    * The variables that are free in the given expression.
    */
  val fv : SValue => Set[Idn] =
    attr {
      case IntConstant(_)            => Set()
      case Ident(Seq(v), _)            => Set(v)
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
      case _ => SigmaPredef.predefinedEnv.mapValues(_.tpe).toList
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
      case IntConstant(_) => SInt

      // An identifier is looked up in the environement of the current
      // expression.  If we find it, then we use the type that we find.
      // Otherwise it's an error.
      case e @ Ident(Seq(x),_) =>
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

      // An operation must be applied to two integers and returns an
      // integer.
      case Plus(e1, e2) => binOpTipe(e1, e2)
      case Minus(e1, e2) => binOpTipe(e1, e2)

      // A parallel returns the type of the body expression
      case Block(bs, e) =>
        tipe(e)
    }

  def binOpTipe(e1: SValue, e2: SValue): SType =
    if ((tipe(e1) == SInt) && (tipe(e2) == SInt))
      SInt
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

