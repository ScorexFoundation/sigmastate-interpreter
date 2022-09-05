/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2008-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package sigmastate.kiama
package rewriting

/**
  * Strategy-based term rewriting in the style of Stratego (http://strategoxt.org/).
  * The implementation here is partially based on the semantics given in "Program
  * Transformation with Scoped Dynamic Rewrite Rules", by Bravenboer, van Dam, Olmos
  * and Visser, Fundamenta Informaticae, 69, 2005. The library strategies are mostly
  * based on the Stratego library, but also on combinators found in the Scrap Your
  * Boilerplate and Uniplate libraries for Haskell.
  */
trait Rewriter {

  import sigmastate.kiama.util.Comparison.same
  import sigmastate.kiama.util.Collections.{CanBuildFrom, Factory, newBuilder}

  /**
    * Rewrite a term.  Apply the strategy `s` to a term returning the result term
    * if `s` succeeds, otherwise return the original term.
    */
  def rewrite[T](s : Strategy)(t : T) : T = {
    s(t) match {
      case Some(t1) =>
        t1.asInstanceOf[T]
      case None =>
        t
    }
  }

  /**
    * Rewrite a tree.  Apply the strategy `s` to the root of a tree returning the
    * a tree formed from the result term if `s` succeeds, otherwise return the
    * original tree.
    *
    * The `shape` parameter specifies the tree shape that should be used
    * when creating the new tree. The default is `EnsureTree` since it is
    * likely that rewrites will result in node sharing that should be removed.
    */
//  def rewriteTree[T <: AnyRef with Product, U <: T](s : Strategy)(t : Tree[T, U], shape : TreeShape = EnsureTree) : Tree[T, U] = {
//    s(t.root) match {
//      case Some(t1) =>
//        new Tree[T, U](t1.asInstanceOf[U], shape)
//      case None =>
//        t
//    }
//  }

  // Strategy creation

  /**
    * Make a strategy with the body `f`. By default, make a basic strategy.
    */
  def mkStrategy(f : Any => Option[Any]) : Strategy =
    new Strategy {
      def apply(t : Any) =
        f(t)
    }

  // Builder combinators.

  /**
    * Construct a strategy that always succeeds, changing the subject term to
    * the given term `t`. The term `t` is evaluated at most once.
    */
  def build(t : Any) : Strategy =
    rulef(_ => t)

  /**
    * A strategy that always succeeds with the subject term unchanged (i.e.,
    * this is the identity strategy) with the side-effect that the subject
    * term is printed to the given emitter, prefixed by the string `s`.  The
    * emitter defaults to one that writes to standard output.
    */
//  def debug(msg : String, emitter : Emitter = new OutputEmitter) : Strategy =
//    strategyf(t => { emitter.emitln(msg + t); Some(t) })

  /**
    * A strategy that always fails.
    */
  val fail : Strategy =
    mkStrategy(_ => None)

  /**
    * A strategy that always succeeds.
    */
  val id : Strategy =
    mkStrategy(Some(_))

  /**
    * Create a logging strategy based on a strategy `s`. The returned strategy
    * succeeds or fails exactly as `s` does, but also prints the provided message,
    * the subject term, the success or failure status, and on success, the result
    * term, to the provided emitter. `s` is evaluated at most once.
    */
//  def log(s : Strategy, msg : String, emitter : Emitter) : Strategy = {
//    lazy val strat = s
//    mkStrategy(
//      t1 => {
//        emitter.emit(msg + t1)
//        val r = strat(t1)
//        r match {
//          case Some(t2) =>
//            emitter.emitln(s" succeeded with $t2")
//          case None =>
//            emitter.emitln(" failed")
//        }
//        r
//      }
//    )
//  }

  /**
    * Create a logging strategy based on a strategy `s`.  The returned strategy
    * succeeds or fails exactly as `s` does, but if `s` fails, also prints the
    * provided message and the subject term to the provided emitter. `s` is
    * evaluated at most once.
    */
//  def logfail[T](s : Strategy, msg : String, emitter : Emitter) : Strategy = {
//    lazy val strat = s
//    mkStrategy(
//      t1 => {
//        val r = strat(t1)
//        r match {
//          case Some(t2) =>
//          // Do nothing
//          case None =>
//            emitter.emitln(s"$msg$t1 failed")
//        }
//        r
//      }
//    )
//  }

  /**
    * Return a strategy that behaves as `s` does, but memoises its arguments and
    * results.  In other words, if `memo(s)` is called on a term `t` twice, the
    * second time will return the same result as the first, without having to
    * invoke `s`.  For best results, it is important that `s` should have no side
    * effects. `s` is evaluated at most once.
    */
//  def memo(s : Strategy) : Strategy = {
//
//    import com.google.common.base.Function
//    import com.google.common.cache.{CacheBuilder, CacheLoader}
//
//    lazy val strat = s
//
//    val cache =
//      CacheBuilder.newBuilder.build[AnyRef, Option[Any]](
//        CacheLoader.from(
//          new Function[AnyRef, Option[Any]] {
//            def apply(t : AnyRef) : Option[Any] =
//              strat(t)
//          }
//        )
//      )
//
//    mkStrategy(t => cache.get(t.asInstanceOf[AnyRef]))
//
//  }

  /**
    * Construct a strategy from an option value `o`. The strategy succeeds
    * or fails depending on whether `o` is a Some or None, respectively.
    * If `o` is a `Some`, then the subject term is changed to the term that
    * is wrapped by the `Some`. `o` is evaluated at most once.
    */
  def option(o : Option[Any]) : Strategy =
    strategyf(_ => o)

  /**
    * Perform a paramorphism over a value. This is a fold in which the
    * recursive step may refer to the recursive component of the value
    * and the results of folding over the children.  When the function `f`
    * is called, the first parameter is the value and the second is a
    * sequence of the values that `f` has returned for the children.  This
    * will work on any value, but will only decompose values that are
    * supported by the `Term` generic term deconstruction.  This operation
    * is similar to that used in the Uniplate library.
    */
  def para[T](f : (Any, Seq[T]) => T) : Any => T = {
    case Term(t, ts) => f(t, ts.map(para(f)))
  }

  /**
    * Define a term query by a partial function `f`. The query always succeeds
    * with no effect on the subject term but applies the given partial function
    * `f` to the subject term.  In other words, the strategy runs `f` for its
    * side-effects. If the subject term is not a `T` or the function is not
    * defined at the subject term, the strategy fails.
    *
    * Due to the type erasure performed on Scala programs the type test
    * will be imprecise for some types. E.g., it is not possible to tell
    * the difference between `List[Int]` and `List[String]`.
    */
  def query[T](f : T ==> Unit) : Strategy = {
    val anyf = f.asInstanceOf[===>[Any]]
    mkStrategy(
      (t : Any) => {
        val of = anyf andThen (_ => Some(t))
        try {
          of.applyOrElse(t, (a : Any) => None)
        } catch {
          case _ : ClassCastException =>
            None
        }
      }
    )
  }

  /**
    * Define a term query by a function `f`. The query always succeeds with
    * no effect on the subject term but applies the given (possibly partial)
    * function `f` to the subject term.  In other words, the strategy runs
    * `f` for its side-effects.
    */
  def queryf(f : Any => Unit) : Strategy =
    mkStrategy(
      t => {
        f(t)
        Some(t)
      }
    )

  /**
    * Define a rewrite rule using a partial function `f` defined on the type
    * `T`. If the subject term is a `T` and the function is defined at the
    * subject term, then the strategy succeeds with the return value of the
    * function applied to the subject term. Otherwise, the strategy fails.
    *
    * Due to the type erasure performed on Scala programs the type test
    * will be imprecise for some types. E.g., it is not possible to tell
    * the difference between `List[Int]` and `List[String]`.
    */
  def rule[T](f : ===>[T]) : Strategy = {
    val anyf = f.asInstanceOf[===>[Any]]
    val of = anyf andThen (Some(_))
    mkStrategy(
      (t : Any) =>
        try {
          of.applyOrElse(t, (a : Any) => None)
        } catch {
          case _ : ClassCastException =>
            None
        }
    )
  }

  /**
    * Define a rewrite rule using a function `f` that returns a term.
    * The rule always succeeds with the return value of the function.
    */
  def rulef(f : Any => Any) : Strategy =
    strategyf(t => Some(f(t)))

  /**
    * Define a rewrite rule using a function `f` defined on type `T` that returns
    * a strategy. If the subject term is a `T` and the function is defined at the
    * subject term, the rule applies the function to the subject term to get a
    * strategy which is then applied again to the subject term. In other words,
    * the function is only used for effects such as pattern matching.  The whole
    * thing also fails if `f` is not defined at the term in the first place.
    */
  def rulefs[T](f : T ==> Strategy) : Strategy = {
    val anyf = f.asInstanceOf[Any ==> Strategy]
    mkStrategy(
      (t : Any) => {
        val of = anyf andThen (_.apply(t))
        try {
          of.applyOrElse(t, (a : Any) => None)
        } catch {
          case _ : ClassCastException =>
            None
        }
      }
    )
  }

  /**
    * Make a strategy from a partial function `f` defined on the type `T`.
    * If the subject term is a `T` and the function is defined at the
    * subject term, then the function return value when applied to the
    * subject term determines whether the strategy succeeds or fails.
    * If the subject term is not a `T` or the function is not defined at
    * the subject term, the strategy fails.
    *
    * Due to the type erasure performed on Scala programs the type test
    * will be imprecise for some types. E.g., it is not possible to tell
    * the difference between `List[Int]` and `List[String]`.
    */
  def strategy[T](f : T ==> Option[T]) : Strategy = {
    val of = f.asInstanceOf[Any ==> Option[T]]
    mkStrategy(
      (t : Any) =>
        try {
          of.applyOrElse(t, (a : Any) => None)
        } catch {
          case _ : ClassCastException =>
            None
        }
    )
  }

  /**
    * Make a strategy from a function `f`. The function return value
    * determines whether the strategy succeeds or fails.
    */
  def strategyf(f : Any => Option[Any]) : Strategy =
    mkStrategy(f)

  /**
    * Construct a strategy that succeeds only if the subject term matches
    * the given term `t`.
    */
  def term[T](t : T) : Strategy =
    rule[T]({
      case `t` => t
    })

  // Product duplication support

  /**
    * General product duplication functionality. This object is a function
    * that returns a product that applies the same constructor as the
    * product `t`, but with the given children instead of `t`'s children.
    * The function fails if a constructor cannot be found, there are the
    * wrong number of new children, or if one of the new children is not
    * of the appropriate type.
    */
  object Duplicator {

    import com.google.common.base.Function
    import com.google.common.cache.{CacheBuilder, CacheLoader}
    import java.lang.{Class, IllegalArgumentException, NoSuchFieldException}
    import java.lang.reflect.Constructor

    type Duper = (Any, Array[AnyRef]) => Any

    object MakeDuper extends Function[Class[_], Duper] {

      def apply(clazz : Class[_]) : Duper =
        try {
          // See if this class has a MODULE$ field. This field is used by Scala
          // to hold a singleton instance and is only present in singleton classes
          // (e.g., ones arising from object declarations or case objects). If we
          // are trying to duplicate one of these then we want to return the same
          // singleton so we use an identity duper.
          clazz.getField("MODULE$")
          (t : Any, children : Array[AnyRef]) =>
            t
        } catch {
          // Otherwise, this is a normal class, so we try to make a
          // duper that uses the first constructor.
          case _ : NoSuchFieldException =>
            val ctors = clazz.getConstructors
            if (ctors.length == 0)
              sys.error(s"dup no constructors for ${clazz.getName}")
            else
              (t : Any, children : Array[AnyRef]) =>
                makeInstance(ctors(0), children)
        }

      def makeInstance(ctor : Constructor[_], children : Array[AnyRef]) : Any =
        try {
          ctor.newInstance(unboxPrimitives(ctor, children) : _*)
        } catch {
          case e : IllegalArgumentException =>
            sys.error(s"""dup illegal arguments: $ctor got (${children.mkString(",")})
                        |Common cause: term classes are nested in another class, move them to the top level""".stripMargin)
        }

      def unboxPrimitives(ctor : Constructor[_], children : Array[AnyRef]) : Array[AnyRef] = {
        val numChildren = ctor.getParameterTypes().length
        val childrenTypes = ctor.getParameterTypes()
        val newChildren = new Array[AnyRef](numChildren)
        var i = 0
        while (i < numChildren) {
          if (childrenTypes(i).isPrimitive())
            newChildren(i) = unboxAnyVal(children(i))
          else
            newChildren(i) = children(i)
          i = i + 1
        }
        newChildren
      }

      def unboxAnyVal(s : AnyRef) : AnyRef =
        s match {
          case p : Product if p.productArity == 1 =>
            p.productElement(0).asInstanceOf[AnyRef]
          case _ =>
            s
        }

    }

    val cache = CacheBuilder.newBuilder.weakKeys.build(
      CacheLoader.from(MakeDuper)
    )

    def apply[T <: Product](t : T, children : Array[AnyRef]) : T = {
      val duper = cache.get(t.getClass)
      duper(t, children).asInstanceOf[T]
    }

  }

  /**
    * The duplicator used by the generic traversals. Needs to be defined
    * as a method so we can override it in other rewriting modules.
    */
  def dup[T <: Product](t : T, children : Array[AnyRef]) : T =
    Duplicator(t, children)

  /**
    * Copy a product node by creating a new node of the same class type
    * using the same children.
    */
  def copy[T <: Product](t : T) : T =
    Duplicator(t, t.productIterator.map(makechild).toArray)

  /**
    * Make an arbitrary value `c` into a term child, checking that it worked
    * properly. Object references will be returned unchanged; other values
    * will be boxed.
    */
  protected def makechild(c : Any) : AnyRef =
    c.asInstanceOf[AnyRef]

  // Generic traversals

  /**
    * Traversal to a single child.  Construct a strategy that applies `s` to
    * the ''ith'' child of the subject term (counting from one).  If `s` succeeds on
    * the ''ith'' child producing `t`, then succeed, forming a new term that is the
    * same as the original term except that the ''ith'' child is now `t`.  If `s` fails
    * on the ''ith'' child or the subject term does not have an ''ith'' child, then fail.
    * `child(i, s)` is equivalent to Stratego's `i(s)` operator.  If `s` succeeds on
    * the ''ith'' child producing the same term (by `eq` for references and by `==` for
    * other values), then the overall strategy returns the subject term.
    * This operation works for instances of `Product` or finite `Seq` values.
    * `s` is evaluated at most once.
    */
  def child(i : Int, s : => Strategy) : Strategy =
    mkStrategy({
      lazy val strat = s
      t =>
        t match {
          case p : Product =>
            childProduct(strat, i, p)
          case t : Seq[_] =>
            childSeq(strat, i, t.asInstanceOf[Seq[Any]])
          case _ =>
            None
        }
    })

  /**
    * Implementation of `child` for `Product` values.
    */
  def childProduct(s : Strategy, i : Int, p : Product) : Option[Any] = {
    val numchildren = p.productArity
    if ((i < 1) || (i > numchildren)) {
      None
    } else {
      val ct = p.productElement(i - 1)
      s(ct) match {
        case Some(ti) if same(ct, ti) =>
          Some(p)
        case Some(ti) =>
          val newchildren = p.productIterator.map(makechild).toArray
          newchildren(i - 1) = makechild(ti)
          Some(dup(p, newchildren))
        case None =>
          None
      }
    }
  }

  /**
    * Implementation of `child` for `Seq` values.
    */
  def childSeq[CC[U] <: Seq[U]](s : Strategy, i : Int, t : CC[Any])(implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]]) : Option[CC[Any]] = {
    val numchildren = t.size
    if ((i < 1) || (i > numchildren)) {
      None
    } else {
      val ct = t(i - 1)
      s(ct) match {
        case Some(ti) if same(ct, ti) =>
          Some(t)
        case Some(ti) =>
          val b = newBuilder(cbf, t)
          b.sizeHint(t.size)
          t.foldLeft(0) {
            case (j, ct) =>
              b += (if (j == i - 1) ti else ct)
              j + 1
          }
          Some(b.result())
        case None =>
          None
      }
    }
  }

  /**
    * Traversal to all children.  Construct a strategy that applies `s` to all
    * term children of the subject term.  If `s` succeeds on all of the children,
    * then succeed, forming a new term from the constructor
    * of the original term and the result of `s` for each child.  If `s` fails on any
    * child, fail. If there are no children, succeed.  If `s` succeeds on all
    * children producing the same terms (by `eq` for references and by `==` for
    * other values), then the overall strategy returns the subject term.
    * This operation works on finite `Rewritable`, `Product`, `Map` and `Iterable`
    * values, checked for in that order.
    * Children of a `Rewritable` (resp. Product, collection) value are processed
    * in the order returned by the value's deconstruct (resp. `productElement`,
    * `foreach`) method.
    * `s` is evaluated at most once.
    */
  def all(s : => Strategy) : Strategy =
    mkStrategy({
      lazy val strat = s
      t =>
        t match {
//          case r : Rewritable =>
//            allRewritable(strat, r)
          case p : Product =>
            allProduct(strat, p)
          case m : Map[_, _] =>
            allMap(strat, m.asInstanceOf[Map[Any, Any]])
          case t : Iterable[_] =>
            allIterable(strat, t.asInstanceOf[Iterable[Any]])
          case _ =>
            Some(t)
        }
    })

  /**
    * Implementation of `all` for `Rewritable` values.
    */
//  def allRewritable(s : Strategy, r : Rewritable) : Option[Any] = {
//    val numchildren = r.arity
//    if (numchildren == 0)
//      Some(r)
//    else {
//      val newchildren = Seq.newBuilder[Any]
//      val changed =
//        r.deconstruct.foldLeft(false) {
//          case (changed, ct) =>
//            s(ct) match {
//              case Some(ti) =>
//                newchildren += makechild(ti)
//                changed || !same(ct, ti)
//              case None =>
//                return None
//            }
//        }
//      if (changed)
//        Some(r.reconstruct(newchildren.result()))
//      else
//        Some(r)
//    }
//  }

  /**
    * Implementation of `all` for `Product` values.
    */
  def allProduct(s : Strategy, p : Product) : Option[Any] = {
    val numchildren = p.productArity
    if (numchildren == 0)
      Some(p)
    else {
      val newchildren = Array.newBuilder[AnyRef]
      val changed =
        p.productIterator.foldLeft(false) {
          case (changed, ct) =>
            s(ct) match {
              case Some(ti) =>
                newchildren += makechild(ti)
                changed || !same(ct, ti)
              case None =>
                return None
            }
        }
      if (changed)
        Some(dup(p, newchildren.result()))
      else
        Some(p)
    }
  }

  /**
    * Implementation of `all` for `Iterable` values.
    */
  def allIterable[CC[U] <: Iterable[U]](s : Strategy, t : CC[Any])(implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]]) : Option[CC[Any]] =
    if (t.size == 0)
      Some(t)
    else {
      val b = newBuilder(cbf, t)
      b.sizeHint(t.size)
      val (changed, _) =
        t.foldLeft((false, 0)) {
          case ((changed, i), ct) =>
            s(ct) match {
              case Some(ti) =>
                b += ti
                (changed || !same(ct, ti), i + 1)
              case None =>
                return None
            }
        }
      if (changed)
        Some(b.result())
      else
        Some(t)
    }

  /**
    * Implementation of `all` for `Map` values.
    */
  def allMap[CC[V, W] <: Map[V, W]](s : Strategy, t : CC[Any, Any])(implicit cbf : CanBuildFrom[CC[Any, Any], (Any, Any), CC[Any, Any]]) : Option[CC[Any, Any]] =
    if (t.size == 0)
      Some(t)
    else {
      val b = newBuilder(cbf, t)
      b.sizeHint(t.size)
      val (changed, _) =
        t.foldLeft((false, 0)) {
          case ((changed, i), ct) =>
            s(ct) match {
              case Some(ti @ (tix, tiy)) =>
                b += ti
                (changed || !same(ct, ti), i + 1)
              case _ =>
                return None
            }
        }
      if (changed)
        Some(b.result())
      else
        Some(t)
    }

  /**
    * Traversal to one child.  Construct a strategy that applies `s` to the term
    * children of the subject term.  Assume that `c` is the
    * first child on which s succeeds.  Then stop applying `s` to the children and
    * succeed, forming a new term from the constructor of the original term and
    * the original children, except that `c` is replaced by the result of applying
    * `s` to `c`.  In the event that the strategy fails on all children, then fail.
    * If there are no children, fail.  If `s` succeeds on the one child producing
    * the same term (by `eq` for references and by `==` for other values), then
    * the overall strategy returns the subject term.
    * This operation works on instances of finite `Rewritable`, `Product`, `Map`
    * and `Iterable` values, checked for in that order.
    * Children of a `Rewritable` (resp. `Product`, collection) value are processed
    * in the order returned by the value's `deconstruct` (resp. `productElement`,
    * `foreach`) method.
    * `s` is evaluated at most once.
    */
  def one(s : => Strategy) : Strategy =
    mkStrategy({
      lazy val strat = s
      t =>
        t match {
//          case r : Rewritable =>
//            oneRewritable(strat, r)
          case p : Product =>
            oneProduct(strat, p)
          case m : Map[_, _] =>
            oneMap(strat, m.asInstanceOf[Map[Any, Any]])
          case t : Iterable[_] =>
            oneIterable(strat, t.asInstanceOf[Iterable[Any]])
          case _ =>
            None
        }
    })

  /**
    * Implementation of `one` for `Rewritable` values.
    */
//  def oneRewritable(s : Strategy, r : Rewritable) : Option[Any] = {
//    val children = r.deconstruct
//    children.foldLeft(0) {
//      case (i, ct) =>
//        s(ct) match {
//          case Some(ti) if same(ct, ti) =>
//            return Some(r)
//          case Some(ti) =>
//            val newchildren = children.updated(i, ti)
//            return Some(r.reconstruct(newchildren))
//          case None =>
//            i + 1
//        }
//    }
//    None
//  }

  /**
    * Implementation of `one` for `Product` values.
    */
  def oneProduct(s : Strategy, p : Product) : Option[Any] = {
    p.productIterator.foldLeft(0) {
      case (i, ct) =>
        s(ct) match {
          case Some(ti) if same(ct, ti) =>
            return Some(p)
          case Some(ti) =>
            val newchildren = p.productIterator.toArray.map(makechild)
            newchildren(i) = makechild(ti)
            return Some(dup(p, newchildren))
          case None =>
            i + 1
        }
    }
    None
  }

  /**
    * Implementation of `one` for `Iterable` values.
    */
  def oneIterable[CC[U] <: Iterable[U]](s : Strategy, t : CC[Any])(implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]]) : Option[CC[Any]] = {
    val b = newBuilder(cbf, t)
    b.sizeHint(t.size)
    val add =
      t.foldLeft(true) {
        case (add, ct) =>
          if (add)
            s(ct) match {
              case Some(ti) if same(ct, ti) =>
                return Some(t)
              case Some(ti) =>
                b += ti
                false
              case None =>
                b += ct
                true
            }
          else {
            b += ct
            false
          }
      }
    if (add)
      None
    else
      Some(b.result())
  }

  /**
    * Implementation of `one` for `Map` values.
    */
  def oneMap[CC[V, W] <: Map[V, W]](s : Strategy, t : CC[Any, Any])(implicit cbf : CanBuildFrom[CC[Any, Any], (Any, Any), CC[Any, Any]]) : Option[CC[Any, Any]] = {
    val b = newBuilder(cbf, t)
    b.sizeHint(t.size)
    val add =
      t.foldLeft(true) {
        case (add, ct) =>
          if (add)
            s(ct) match {
              case Some(ti @ (tix, tiy)) if same(ct, ti) =>
                return Some(t)
              case Some(ti @ (tix, tiy)) =>
                b += ti
                false
              case Some(ti) =>
                sys.error(s"oneMap: got non-pair $ti")
              case None =>
                b += ct
                true
            }
          else {
            b += ct
            false
          }
      }
    if (add)
      None
    else
      Some(b.result())
  }

  /**
    * Traversal to as many children as possible, but at least one.  Construct a
    * strategy that applies `s` to the term children of the subject term.
    * If `s` succeeds on any of the children, then succeed,
    * forming a new term from the constructor of the original term and the result
    * of `s` for each succeeding child, with other children unchanged.  In the event
    * that `s` fails on all children, then fail. If there are no
    * children, fail.  If `s` succeeds on children producing the same terms (by `eq`
    * for references and by `==` for other values), then the overall strategy
    * returns the subject term.
    * This operation works on instances of finite `Rewritable`, `Product`, `Map` and
    * `Iterable` values, checked for in that order.
    * Children of a `Rewritable` (resp. `Product`, collection) value are processed
    * in the order returned by the value's `deconstruct` (resp. `productElement`,
    * `foreach`) method.
    * `s` is evaluated at most once.
    */
  def some(s : => Strategy) : Strategy =
    mkStrategy({
      lazy val strat = s
      t =>
        t match {
//          case r : Rewritable =>
//            someRewritable(strat, r)
          case p : Product =>
            someProduct(strat, p)
          case m : Map[_, _] =>
            someMap(strat, m.asInstanceOf[Map[Any, Any]])
          case t : Iterable[_] =>
            someIterable(strat, t.asInstanceOf[Iterable[Any]])
          case _ =>
            None
        }
    })

  /**
    * Implementation of `some` for `Rewritable` values.
    */
//  def someRewritable(s : Strategy, r : Rewritable) : Option[Any] = {
//    val numchildren = r.arity
//    if (numchildren == 0)
//      None
//    else {
//      val newchildren = Seq.newBuilder[Any]
//      val (success, changed) =
//        r.deconstruct.foldLeft((false, false)) {
//          case ((success, changed), ct) =>
//            s(ct) match {
//              case Some(ti) =>
//                newchildren += makechild(ti)
//                (true, changed || !same(ct, ti))
//              case None =>
//                newchildren += makechild(ct)
//                (success, changed)
//            }
//        }
//      if (success)
//        if (changed)
//          Some(r.reconstruct(newchildren.result()))
//        else
//          Some(r)
//      else
//        None
//    }
//  }

  /**
    * Implementation of `some` for `Product` values.
    */
  def someProduct(s : Strategy, p : Product) : Option[Any] = {
    val numchildren = p.productArity
    if (numchildren == 0)
      None
    else {
      val newchildren = Array.newBuilder[AnyRef]
      val (success, changed) =
        p.productIterator.foldLeft((false, false)) {
          case ((success, changed), ct) =>
            s(ct) match {
              case Some(ti) =>
                newchildren += makechild(ti)
                (true, changed || !same(ct, ti))
              case None =>
                newchildren += makechild(ct)
                (success, changed)
            }
        }
      if (success)
        if (changed)
          Some(dup(p, newchildren.result()))
        else
          Some(p)
      else
        None
    }
  }

  /**
    * Implementation of `some` for `Iterable` values.
    */
  def someIterable[CC[U] <: Iterable[U]](s : Strategy, t : CC[Any])(implicit cbf : CanBuildFrom[CC[Any], Any, CC[Any]]) : Option[CC[Any]] =
    if (t.size == 0)
      None
    else {
      val b = newBuilder(cbf, t)
      b.sizeHint(t.size)
      val (success, changed) =
        t.foldLeft((false, false)) {
          case ((success, changed), ct) =>
            s(ct) match {
              case Some(ti) =>
                b += ti
                (true, changed || !same(ct, ti))
              case None =>
                b += ct
                (success, changed)
            }
        }
      if (success)
        if (changed)
          Some(b.result())
        else
          Some(t)
      else
        None
    }

  /**
    * Implementation of `some` for `Map` values.
    */
  def someMap[CC[V, W] <: Map[V, W]](s : Strategy, t : CC[Any, Any])(implicit cbf : CanBuildFrom[CC[Any, Any], (Any, Any), CC[Any, Any]]) : Option[CC[Any, Any]] =
    if (t.size == 0)
      None
    else {
      val b = newBuilder(cbf, t)
      b.sizeHint(t.size)
      val (success, changed) =
        t.foldLeft((false, false)) {
          case ((success, changed), ct) =>
            s(ct) match {
              case Some(ti @ (tix, tiy)) =>
                b += ti
                (true, changed || !same(ct, ti))
              case _ =>
                b += ct
                (success, changed)
            }
        }
      if (success)
        if (changed)
          Some(b.result())
        else
          Some(t)
      else
        None
    }

  /**
    * Make a strategy that applies the elements of ss pairwise to the
    * children of the subject term, returning a new term if all of the
    * strategies succeed, otherwise failing.  The constructor of the new
    * term is the same as that of the original term and the children
    * are the results of the strategies.  If the length of `ss` is not
    * the same as the number of children, then `congruence(ss)` fails.
    * If the argument strategies succeed on children producing the same
    * terms (by `eq` for references and by `==` for other values), then the
    * overall strategy returns the subject term.
    * This operation works on instances of `Product` values.
    */
  def congruence(ss : Strategy*) : Strategy =
    mkStrategy(
      t =>
        t match {
          case p : Product =>
            congruenceProduct(p, ss : _*)
          case _ =>
            Some(t)
        }
    )

  /**
    * Implementation of `congruence` for `Product` values.
    */
  def congruenceProduct(p : Product, ss : Strategy*) : Option[Any] = {
    val numchildren = p.productArity
    if (numchildren == ss.length) {
      val newchildren = Array.newBuilder[AnyRef]
      val (changed, _) =
        p.productIterator.foldLeft((false, 0)) {
          case ((changed, i), ct) =>
            (ss(i))(ct) match {
              case Some(ti) =>
                newchildren += makechild(ti)
                (changed || !same(ct, ti), i + 1)
              case None =>
                return None
            }
        }
      if (changed)
        Some(dup(p, newchildren.result()))
      else
        Some(p)
    } else
      None
  }

  // Extractors

  /**
    * Generic term deconstruction.
    */
  object Term {

    /**
      * Generic term deconstruction. An extractor that decomposes `Product`
      * `Rewritable` or `Seq` values into the value itself and a vector of
      * its children.  Terms that are not of these types are not decomposable
      * (i.e., the children will be empty).
      */
    def unapply(t : Any) : Some[(Any, Vector[Any])] = {
      t match {
//        case r : Rewritable =>
//          Some((r, r.deconstruct.toVector))
        case p : Product =>
          Some((p, p.productIterator.toVector))
        case s : Seq[_] =>
          Some((s, s.toVector))
        case _ =>
          Some((t, Vector()))
      }
    }

  }

  // Library combinators

  /**
    * Construct a strategy that applies `s` in a bottom-up fashion to all
    * subterms at each level, stopping at a frontier where s succeeds.
    */
  def allbu(s : Strategy) : Strategy = {
    lazy val result : Strategy = all(result) <+ s
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` in a top-down fashion, stopping
    * at a frontier where s succeeds.
    */
  def alltd(s : Strategy) : Strategy = {
    lazy val result : Strategy = s <+ all(result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s1` in a top-down, prefix fashion
    * stopping at a frontier where `s1` succeeds.  `s2` is applied in a bottom-up,
    * postfix fashion to the result.
    */
  def alldownup2(s1 : Strategy, s2 : Strategy) : Strategy = {
    lazy val result : Strategy = (s1 <+ all(result)) <* s2
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s1` in a top-down, prefix fashion
    * stopping at a frontier where `s1` succeeds.  `s2` is applied in a bottom-up,
    * postfix fashion to the results of the recursive calls.
    */
  def alltdfold(s1 : Strategy, s2 : Strategy) : Strategy = {
    lazy val result : Strategy = s1 <+ (all(result) <* s2)
    mkStrategy(result)
  }

  /**
    * `and(s1, s2)` applies `s1` and `s2` to the subject
    * term and succeeds if both succeed. `s2` will always
    * be applied, i.e., and is ''not'' a short-circuit
    * operator.
    */
  def and(s1 : Strategy, s2 : Strategy) : Strategy =
    where(s1) < (test(s2) + (test(s2) <* fail))

  /**
    * Construct a strategy that applies `s`, yielding the result of `s` if it
    * succeeds, otherwise leave the original subject term unchanged.  In
    * Stratego library this strategy is called `try`.
    */
  def attempt(s : Strategy) : Strategy =
    s <+ id

  /**
    * Construct a strategy that applies `s` in a bottom-up, postfix fashion
    * to the subject term.
    */
  def bottomup(s : Strategy) : Strategy =
    all(bottomup(s)) <* s

  /**
    * Construct a strategy that applies `s` in a bottom-up, postfix fashion
    * to the subject term but stops when the strategy produced by `stop`
    * succeeds. `stop` is given the whole strategy itself as its argument.
    */
  def bottomupS(s : Strategy, stop : (=> Strategy) => Strategy) : Strategy = {
    lazy val result : Strategy = (stop(result) <+ all(result)) <* s
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` in breadth first order. This
    * strategy does not apply `s` to the root of the subject term.
    *
    * It is called `breadthfirst` to follow Stratego's library, but is not
    * really conducting a breadth-first traversal since all of the
    * descendants of the first child of a term are visited before any of
    * the descendants of the second child of a term.
    */
  def breadthfirst(s : Strategy) : Strategy = {
    lazy val result : Strategy = all(s) <* all(result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` at least once and then repeats `s`
    * while `r` succeeds.  This operator is called `do-while` in the Stratego
    * library.
    */
  def doloop(s : Strategy, r : Strategy) : Strategy =
    s <* (loop(r, s))

  /**
    * A unit for `topdownS`, `bottomupS` and `downupS`.  For example, `topdown(s)`
    * is equivalent to `topdownS(s, dontstop)`.
    */
  def dontstop(s : => Strategy) : Strategy =
    fail

  /**
    * Construct a strategy that applies `s` in a combined top-down and
    * bottom-up fashion (i.e., both prefix and postfix) to the subject
    * term.
    */
  def downup(s : Strategy) : Strategy = {
    lazy val result : Strategy = s <* all(result) <* s
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s1` in a top-down, prefix fashion
    * and `s2` in a bottom-up, postfix fashion to the subject term.
    */
  def downup(s1 : Strategy, s2 : Strategy) : Strategy = {
    lazy val result : Strategy = s1 <* all(result) <* s2
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` in a combined top-down and
    * bottom-up fashion (i.e., both prefix and postfix) to the subject
    * but stops when the strategy produced by `stop` succeeds. `stop` is
    * given the whole strategy itself as its argument.
    */
  def downupS(s : Strategy, stop : (=> Strategy) => Strategy) : Strategy = {
    lazy val result : Strategy = s <* (stop(result) <+ all(result) <* s)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s1` in a top-down, prefix fashion
    * and `s2` in a bottom-up, postfix fashion to the subject term but stops
    * when the strategy produced by `stop` succeeds. `stop` is given the whole
    * strategy itself as its argument.
    */
  def downupS(s1 : Strategy, s2 : Strategy, stop : (=> Strategy) => Strategy) : Strategy = {
    lazy val result : Strategy = s1 <* (stop(result) <+ all(result) <* s2)
    mkStrategy(result)
  }

  /**
    * A strategy that tests whether the two sub-terms of a pair of terms are equal.
    */
  val eq : Strategy =
    rule[(Any, Any)]({
      case t @ (x, y) if x == y => t
    })

  /**
    * Construct a strategy that tests whether the two sub-terms of a
    * pair of terms are equal. Synonym for `eq`.
    */
  val equal : Strategy =
    eq

  /**
    * Same as `everywheretd`.
    */
  def everywhere(s : Strategy) : Strategy =
    everywheretd(s)

  /**
    * Construct a strategy that applies `s` at all terms in a bottom-up fashion
    * regardless of failure.  Terms for which the strategy fails are left
    * unchanged.
    */
  def everywherebu(s : Strategy) : Strategy =
    bottomup(attempt(s))

  /**
    * Construct a strategy that applies `s` at all terms in a top-down fashion
    * regardless of failure.  Terms for which the strategy fails are left
    * unchanged.
    */
  def everywheretd(s : Strategy) : Strategy =
    topdown(attempt(s))

  /**
    * Construct a strategy that applies `s` repeatedly to the innermost
    * (i.e., lowest and left-most) (sub-)term to which it applies.
    * Stop with the subject term if `s` doesn't apply anywhere.
    */
  def innermost(s : Strategy) : Strategy = {
    lazy val result : Strategy = bottomup(attempt(s <* result))
    mkStrategy(result)
  }

  /**
    * An alternative version of `innermost`.
    */
  def innermost2(s : Strategy) : Strategy =
    repeat(oncebu(s))

  /**
    * `ior(s1, s2)` implements inclusive OR, that is, the
    * inclusive choice of `s1` and `s2`. It first tries `s1`. If
    * that fails it applies `s2` (just like `s1 <+ s2`). However,
    * when `s1` succeeds it also tries to apply `s2`.
    */
  def ior(s1 : Strategy, s2 : Strategy) : Strategy =
    (s1 <* attempt(s2)) <+ s2

  /**
    * Construct a strategy that succeeds if the current term has at
    * least one direct subterm.
    */
  val isinnernode : Strategy =
    mkStrategy(one(id))

  /**
    * Construct a strategy that succeeds if the current term has no
    * direct subterms.
    */
  val isleaf : Strategy =
    mkStrategy(all(fail))

  /**
    * Construct a strategy that succeeds when applied to a pair `(x,y)`
    * if `x` is a sub-term of `y` but is not equal to `y`.
    */
  val ispropersubterm : Strategy =
    mkStrategy(not(eq) <* issubterm)

  /**
    * Construct a strategy that succeeds when applied to a pair `(x,y)`
    * if `x` is a sub-term of `y`.
    */
  val issubterm : Strategy =
    mkStrategy({
      case (x, y) =>
        where(oncetd(term(x)))(y)
      case _ =>
        None
    })

  /**
    * Construct a strategy that succeeds when applied to a pair `(x,y)`
    * if `x` is a superterm of `y`.
    */
  val issuperterm : Strategy =
    mkStrategy({
      case (x, y) =>
        issubterm((y, x))
      case _ =>
        None
    })

  /**
    * Construct a strategy that succeeds when applied to a pair `(x,y)`
    * if `x` is a super-term of `y` but is not equal to `y`.
    */
  val ispropersuperterm : Strategy =
    mkStrategy(not(eq) <* issuperterm)

  /**
    * Applies `s` followed by `f` whether `s` failed or not.
    * This operator is called `finally` in the Stratego library.
    */
  def lastly(s : Strategy, f : Strategy) : Strategy =
    s < (where(f) + (where(f) <* fail))

  /**
    * Construct a strategy that applies to all of the leaves of the
    * subject term, using `isleaf` as the leaf predicate.
    */
  def leaves(s : Strategy, isleaf : Strategy) : Strategy = {
    lazy val result : Strategy = (isleaf <* s) <+ all(result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies to all of the leaves of the
    * subject term, using `isleaf` as the leaf predicate, skipping
    * subterms for which `skip` when applied to the result succeeds.
    */
  def leaves(s : Strategy, isleaf : Strategy, skip : Strategy => Strategy) : Strategy = {
    lazy val result : Strategy = (isleaf <* s) <+ skip(result) <+ all(result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that while `r` succeeds applies `s`.  This operator
    * is called `while` in the Stratego library.
    */
  def loop(c : Strategy, s : Strategy) : Strategy = {
    lazy val result : Strategy = attempt(c <* s <* result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that repeats application of `s` while `r` fails, after
    * initialization with `i`.  This operator is called `for` in the Stratego
    * library.
    */
  def loopiter(i : Strategy, r : Strategy, s : Strategy) : Strategy =
    i <* loopnot(r, s)

  /**
    * Construct a strategy that applies `s(i)` for each integer `i` from `low` to
    * `high` (inclusive).  This operator is called `for` in the Stratego library.
    */
  def loopiter(s : Int => Strategy, low : Int, high : Int) : Strategy = {
    lazy val result =
      if (low <= high)
        s(low) <* loopiter(s, low + 1, high)
      else
        id
    mkStrategy(result)
  }

  /**
    * Construct a strategy that while `r` does not succeed applies `s`.  This
    * operator is called `while-not` in the Stratego library.
    */
  def loopnot(r : Strategy, s : Strategy) : Strategy = {
    lazy val result : Strategy = r <+ (s <* result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` to each element of a finite
    * sequence (type `Seq`) returning a new sequence of the results if
    * all of the applications succeed, otherwise fail.  If all of the
    * applications succeed without change, return the input sequence.
    */
  def map(s : Strategy) : Strategy =
    strategy[Seq[_]]({
      case l : Seq[_] =>
        allIterable[Seq](s, l)
    })

  /**
    * Construct a strategy that applies `s` as many times as possible, but
    * at least once, in bottom up order.
    */
  def manybu(s : Strategy) : Strategy = {
    lazy val result : Strategy = some(result) <* attempt(s) <+ s
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` as many times as possible, but
    * at least once, in top down order.
    */
  def manytd(s : Strategy) : Strategy = {
    lazy val result : Strategy = s <* all(attempt(result)) <+ some(result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s`, then fails if `s` succeeded or, if `s`
    * failed, succeeds with the subject term unchanged,  I.e., it tests if
    * `s` applies, but has no effect on the subject term.
    */
  def not(s : Strategy) : Strategy =
    s < (fail + id)

  /**
    * Construct a strategy that applies `s` in a bottom-up fashion to one
    * subterm at each level, stopping as soon as it succeeds once (at
    * any level).
    */
  def oncebu(s : Strategy) : Strategy = {
    lazy val result : Strategy = one(result) <+ s
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` in a top-down fashion to one
    * subterm at each level, stopping as soon as it succeeds once (at
    * any level).
    */
  def oncetd(s : Strategy) : Strategy = {
    lazy val result : Strategy = s <+ one(result)
    mkStrategy(result)
  }

  /**
    * `or(s1, s2)` is similar to `ior(s1, s2)`, but the application
    * of the strategies is only tested.
    */
  def or(s1 : Strategy, s2 : Strategy) : Strategy =
    where(s1) < (attempt(test(s2)) + test(s2))

  /**
    * Construct a strategy that applies `s` repeatedly in a top-down fashion
    * stopping each time as soon as it succeeds once (at any level). The
    * outermost fails when `s` fails to apply to any (sub-)term.
    */
  def outermost(s : Strategy) : Strategy =
    repeat(oncetd(s))

  /**
    * Construct a strategy that applies `s` repeatedly to subterms
    * until it fails on all of them.
    */
  def reduce(s : Strategy) : Strategy = {
    lazy val inner : Strategy = some(inner) + s
    repeat(inner)
  }

  /**
    * Construct a strategy that applies `s` repeatedly until it fails.
    */
  def repeat(s : Strategy) : Strategy = {
    lazy val result : Strategy = attempt(s <* result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that repeatedly applies `s` until it fails and
    * then terminates with application of `r`.
    */
  def repeat(s : Strategy, r : Strategy) : Strategy = {
    lazy val result : Strategy = (s <* result) <+ r
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` repeatedly exactly `n` times. If
    * `s` fails at some point during the n applications, the entire strategy
    * fails. The result of the strategy is that of the ''nth'' application of
    * `s`.
    */
  def repeat(s : Strategy, n : Int) : Strategy = {
    lazy val result = if (n == 0) id else s <* repeat(s, n - 1)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that repeatedly applies `s` (at least once).
    */
  def repeat1(s : Strategy) : Strategy =
    repeat1(s, id)

  /**
    * Construct a strategy that repeatedly applies `s` (at least once) and
    * terminates with application of `c`.
    */
  def repeat1(s : Strategy, r : Strategy) : Strategy = {
    lazy val result : Strategy = s <* (result <+ r)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that repeatedly applies `s` until `c` succeeds.
    */
  def repeatuntil(s : Strategy, r : Strategy) : Strategy = {
    lazy val result : Strategy = s <* (r <+ result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s`, then applies the restoring action
    * `rest` if `s` fails (and then fail). Otherwise, let the result of `s` stand.
    * Typically useful if `s` performs side effects that should be restored or
    * undone when `s` fails.
    */
  def restore(s : Strategy, rest : Strategy) : Strategy =
    s <+ (rest <* fail)

  /**
    * Construct a strategy that applies `s`, then applies the restoring action
    * `rest` regardless of the success or failure of `s`. The whole strategy
    * preserves the success or failure of `s`. Typically useful if `s` performs
    * side effects that should be restored always, e.g., when maintaining scope
    * information.
    */
  def restorealways(s : Strategy, rest : Strategy) : Strategy =
    s < (rest + (rest <* fail))

  /**
    * Construct a strategy that applies `s` in a bottom-up fashion to some
    * subterms at each level, stopping as soon as it succeeds once (at
    * any level).
    */
  def somebu(s : Strategy) : Strategy = {
    lazy val result : Strategy = some(result) <+ s
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` in a top-down, prefix fashion
    * stopping at a frontier where `s` succeeds on some children.  `s` is then
    * applied in a bottom-up, postfix fashion to the result.
    */
  def somedownup(s : Strategy) : Strategy = {
    lazy val result : Strategy = (s <* all(result) <* (attempt(s))) <+ some(result) <+ attempt(s)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` in a top-down fashion to some
    * subterms at each level, stopping as soon as it succeeds once (at
    * any level).
    */
  def sometd(s : Strategy) : Strategy = {
    lazy val result : Strategy = s <+ some(result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that tests whether strategy `s` succeeds,
    * restoring the original term on success.  A synonym for `where`.
    */
  def test(s : Strategy) : Strategy =
    where(s)

  /**
    * Construct a strategy that applies `s` in a top-down, prefix fashion
    * to the subject term.
    */
  def topdown(s : Strategy) : Strategy = {
    lazy val result : Strategy = s <* all(result)
    mkStrategy(result)
  }

  /**
    * Construct a strategy that applies `s` in a top-down, prefix fashion
    * to the subject term but stops when the strategy produced by `stop`
    * succeeds. `stop` is given the whole strategy itself as its argument.
    */
  def topdownS(s : Strategy, stop : (=> Strategy) => Strategy) : Strategy = {
    lazy val result : Strategy = s <* (stop(result) <+ all(result))
    mkStrategy(result)
  }

  /**
    * Construct a strategy that tests whether strategy `s` succeeds,
    * restoring the original term on success.  This is similar
    * to Stratego's `where`, except that in this version any effects on
    * bindings are not visible outside `s`.
    */
  def where(s : Strategy) : Strategy =
    strategyf(t => (s <* build(t))(t))

  // Queries below here

  /**
    * Collect query results in a Iterable collection.  Run the function
    * `f` as a top-down left-to-right query on the subject term.  Each
    * application of `f` returns a single value. All of these values are
    * accumulated in the collection.
    */
  def collect[CC[X] <: Iterable[X], U](f : Any ==> U)(implicit cbf : Factory[U, CC[U]]) : Any => CC[U] =
    (t : Any) => {
      val b = newBuilder(cbf)
      val add = (u : U) => { b += u; () }
      (everywhere(query(f andThen add)))(t)
      b.result()
    }

  /**
    * Collect query results in a Iterable collection.  Run the function
    * `f` as a top-down left-to-right query on the subject term.  Each
    * application of `f` returns a collection of values. All of these values
    * are accumulated in the collection.
    */
  def collectall[CC[X] <: Iterable[X], U](f : Any ==> CC[U])(implicit cbf : Factory[U, CC[U]]) : Any => CC[U] =
    (t : Any) => {
      val b = newBuilder(cbf)
      val addall = (us : CC[U]) => { b ++= us; () }
      (everywhere(query(f andThen addall)))(t)
      b.result()
    }

  /**
    * Collect query results in a list.  Run the function `f` as a top-down
    * left-to-right query on the subject term.  Accumulate the values
    * produced by the function in a list and return the final value of
    * the list.
    */
//  def collectl[U](f : Any ==> U) : Any => List[U] =
//    collect[List, U](f)

  /**
    * Collect query results in a set.  Run the function `f` as a top-down
    * left-to-right query on the subject term.  Accumulate the values
    * produced by the function in a set and return the final value of
    * the set.
    */
//  def collects[U](f : Any ==> U) : Any => Set[U] =
//    collect[Set, U](f)

  /**
    * Count function results.  Run the function `f` as a top-down query on
    * the subject term.  Sum the integer values returned by `f` from all
    * applications.
    */
  def count(f : Any ==> Int) : Any => Int =
    everything(0)(_ + _)(f)

  /**
    * Apply the function at every term in `t` in a top-down, left-to-right order.
    * Collect the resulting `T` values by accumulating them using `f` with initial
    * left value `v`.  Return the final value of the accumulation.
    */
  def everything[T](v : T)(f : (T, T) => T)(g : Any ==> T)(t : Any) : T = {
    val collector = collect[List, T](g)
    collector(t).foldLeft(v)(f)
  }

}

/**
  * Strategy-based term rewriting for arbitrary terms.
  */
object Rewriter extends Rewriter
