package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait CollsDefs extends scalan.Scalan with Colls {
  self: Library =>
import Coll._
import CollBuilder._
import Monoid._
import MonoidBuilder._
import PairColl._
import WOption._
import ReplColl._

object Coll extends EntityObject("Coll") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SColl[A] = special.collection.Coll[A]
  case class CollConst[SA, A](
        constValue: SColl[SA],
        lA: Liftable[SA, A]
      ) extends LiftedConst[SColl[SA], Coll[A]] with Coll[A]
  with Def[Coll[A]] with CollConstMethods[A] {
    implicit def eA: Elem[A] = lA.eW

    val liftable: Liftable[SColl[SA], Coll[A]] = liftableColl(lA)
    val resultType: Elem[Coll[A]] = liftable.eW
  }

  trait CollConstMethods[A] extends Coll[A]  { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val CollClass = classOf[Coll[A]]

    override def builder: Ref[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(self,
        CollClass.getMethod("builder"),
        WrappedArray.empty,
        true, false, element[CollBuilder]))
    }

    override def length: Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("length"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def isEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("isEmpty"),
        WrappedArray.empty,
        true, false, element[Boolean]))
    }

    override def nonEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("nonEmpty"),
        WrappedArray.empty,
        true, false, element[Boolean]))
    }

    override def apply(i: Ref[Int]): Ref[A] = {
      asRep[A](mkMethodCall(self,
        CollClass.getMethod("apply", classOf[Sym]),
        Array[AnyRef](i),
        true, false, element[A]))
    }

    override def isDefinedAt(idx: Ref[Int]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("isDefinedAt", classOf[Sym]),
        Array[AnyRef](idx),
        true, false, element[Boolean]))
    }

    override def getOrElse(index: Ref[Int], default: Ref[A]): Ref[A] = {
      asRep[A](mkMethodCall(self,
        CollClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, default),
        true, false, element[A]))
    }

    override def map[B](f: Ref[A => B]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(self,
        CollClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, false, element[Coll[B]]))
    }

    override def zip[B](ys: Ref[Coll[B]]): Ref[Coll[(A, B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[(A, B)]](mkMethodCall(self,
        CollClass.getMethod("zip", classOf[Sym]),
        Array[AnyRef](ys),
        true, false, element[Coll[(A, B)]]))
    }

    override def exists(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("exists", classOf[Sym]),
        Array[AnyRef](p),
        true, false, element[Boolean]))
    }

    override def forall(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        CollClass.getMethod("forall", classOf[Sym]),
        Array[AnyRef](p),
        true, false, element[Boolean]))
    }

    override def filter(p: Ref[A => Boolean]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, false, element[Coll[A]]))
    }

    override def foldLeft[B](zero: Ref[B], op: Ref[((B, A)) => B]): Ref[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(self,
        CollClass.getMethod("foldLeft", classOf[Sym], classOf[Sym]),
        Array[AnyRef](zero, op),
        true, false, element[B]))
    }

    override def indices: Ref[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(self,
        CollClass.getMethod("indices"),
        WrappedArray.empty,
        true, false, element[Coll[Int]]))
    }

    override def flatMap[B](f: Ref[A => Coll[B]]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asInstanceOf[Elem[B]]
      asRep[Coll[B]](mkMethodCall(self,
        CollClass.getMethod("flatMap", classOf[Sym]),
        Array[AnyRef](f),
        true, false, element[Coll[B]]))
    }

    override def segmentLength(p: Ref[A => Boolean], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, from),
        true, false, element[Int]))
    }

    override def find(p: Ref[A => Boolean]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(self,
        CollClass.getMethod("find", classOf[Sym]),
        Array[AnyRef](p),
        true, false, element[WOption[A]]))
    }

    override def indexWhere(p: Ref[A => Boolean], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, from),
        true, false, element[Int]))
    }

    override def indexOf(elem: Ref[A], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        Array[AnyRef](elem, from),
        true, false, element[Int]))
    }

    override def lastIndexWhere(p: Ref[A => Boolean], end: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        CollClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, end),
        true, false, element[Int]))
    }

    override def take(n: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("take", classOf[Sym]),
        Array[AnyRef](n),
        true, false, element[Coll[A]]))
    }

    override def partition(pred: Ref[A => Boolean]): Ref[(Coll[A], Coll[A])] = {
      asRep[(Coll[A], Coll[A])](mkMethodCall(self,
        CollClass.getMethod("partition", classOf[Sym]),
        Array[AnyRef](pred),
        true, false, element[(Coll[A], Coll[A])]))
    }

    override def patch(from: Ref[Int], patch: Ref[Coll[A]], replaced: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, patch, replaced),
        true, false, element[Coll[A]]))
    }

    override def updated(index: Ref[Int], elem: Ref[A]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, elem),
        true, false, element[Coll[A]]))
    }

    override def updateMany(indexes: Ref[Coll[Int]], values: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        Array[AnyRef](indexes, values),
        true, false, element[Coll[A]]))
    }

    override def mapReduce[K, V](m: Ref[A => (K, V)], r: Ref[((V, V)) => V]): Ref[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(self,
        CollClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        Array[AnyRef](m, r),
        true, false, element[Coll[(K, V)]]))
    }

    override def groupBy[K](key: Ref[A => K]): Ref[Coll[(K, Coll[A])]] = {
      implicit val eK = key.elem.eRange
      asRep[Coll[(K, Coll[A])]](mkMethodCall(self,
        CollClass.getMethod("groupBy", classOf[Sym]),
        Array[AnyRef](key),
        true, false, element[Coll[(K, Coll[A])]]))
    }

    override def groupByProjecting[K, V](key: Ref[A => K], proj: Ref[A => V]): Ref[Coll[(K, Coll[V])]] = {
      implicit val eK = key.elem.eRange
implicit val eV = proj.elem.eRange
      asRep[Coll[(K, Coll[V])]](mkMethodCall(self,
        CollClass.getMethod("groupByProjecting", classOf[Sym], classOf[Sym]),
        Array[AnyRef](key, proj),
        true, false, element[Coll[(K, Coll[V])]]))
    }

    override def unionSet(that: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("unionSet", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[Coll[A]]))
    }

    override def diff(that: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("diff", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[Coll[A]]))
    }

    override def intersect(that: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("intersect", classOf[Sym]),
        Array[AnyRef](that),
        true, false, element[Coll[A]]))
    }

    override def sum(m: Ref[Monoid[A]]): Ref[A] = {
      asRep[A](mkMethodCall(self,
        CollClass.getMethod("sum", classOf[Sym]),
        Array[AnyRef](m),
        true, false, element[A]))
    }

    override def slice(from: Ref[Int], until: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, until),
        true, false, element[Coll[A]]))
    }

    override def append(other: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("append", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[Coll[A]]))
    }

    override def reverse: Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        CollClass.getMethod("reverse"),
        WrappedArray.empty,
        true, false, element[Coll[A]]))
    }
  }

  case class LiftableColl[SA, A](lA: Liftable[SA, A])
    extends Liftable[SColl[SA], Coll[A]] {
    lazy val eW: Elem[Coll[A]] = collElement(lA.eW)
    lazy val sourceType: RType[SColl[SA]] = {
            implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[SColl[SA]]
    }
    def lift(x: SColl[SA]): Ref[Coll[A]] = CollConst(x, lA)
    def unlift(w: Ref[Coll[A]]): SColl[SA] = w match {
      case Def(CollConst(x: SColl[_], _lA))
            if _lA == lA => x.asInstanceOf[SColl[SA]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableColl[SA, A](implicit lA: Liftable[SA,A]): Liftable[SColl[SA], Coll[A]] =
    LiftableColl(lA)

  private val CollClass = classOf[Coll[_]]

  // entityAdapter for Coll trait
  case class CollAdapter[A](source: Ref[Coll[A]])
      extends Node with Coll[A]
      with Def[Coll[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asInstanceOf[Elem[A]]

    val resultType: Elem[Coll[A]] = element[Coll[A]]
    override def transform(t: Transformer) = CollAdapter[A](t(source))

    def builder: Ref[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(source,
        CollClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CollBuilder]))
    }

    def length: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CollClass.getMethod("length"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def isEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        CollClass.getMethod("isEmpty"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def nonEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        CollClass.getMethod("nonEmpty"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def apply(i: Ref[Int]): Ref[A] = {
      asRep[A](mkMethodCall(source,
        CollClass.getMethod("apply", classOf[Sym]),
        Array[AnyRef](i),
        true, true, element[A]))
    }

    def isDefinedAt(idx: Ref[Int]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        CollClass.getMethod("isDefinedAt", classOf[Sym]),
        Array[AnyRef](idx),
        true, true, element[Boolean]))
    }

    def getOrElse(index: Ref[Int], default: Ref[A]): Ref[A] = {
      asRep[A](mkMethodCall(source,
        CollClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, default),
        true, true, element[A]))
    }

    def map[B](f: Ref[A => B]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(source,
        CollClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[B]]))
    }

    def zip[B](ys: Ref[Coll[B]]): Ref[Coll[(A, B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[(A, B)]](mkMethodCall(source,
        CollClass.getMethod("zip", classOf[Sym]),
        Array[AnyRef](ys),
        true, true, element[Coll[(A, B)]]))
    }

    def exists(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        CollClass.getMethod("exists", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Boolean]))
    }

    def forall(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        CollClass.getMethod("forall", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Boolean]))
    }

    def filter(p: Ref[A => Boolean]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Coll[A]]))
    }

    def foldLeft[B](zero: Ref[B], op: Ref[((B, A)) => B]): Ref[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        CollClass.getMethod("foldLeft", classOf[Sym], classOf[Sym]),
        Array[AnyRef](zero, op),
        true, true, element[B]))
    }

    def indices: Ref[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        CollClass.getMethod("indices"),
        WrappedArray.empty,
        true, true, element[Coll[Int]]))
    }

    def flatMap[B](f: Ref[A => Coll[B]]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asInstanceOf[Elem[B]]
      asRep[Coll[B]](mkMethodCall(source,
        CollClass.getMethod("flatMap", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[B]]))
    }

    def segmentLength(p: Ref[A => Boolean], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CollClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, from),
        true, true, element[Int]))
    }

    override def find(p: Ref[A => Boolean]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(source,
        CollClass.getMethod("find", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[WOption[A]]))
    }

    def indexWhere(p: Ref[A => Boolean], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CollClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, from),
        true, true, element[Int]))
    }

    override def indexOf(elem: Ref[A], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CollClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        Array[AnyRef](elem, from),
        true, true, element[Int]))
    }

    def lastIndexWhere(p: Ref[A => Boolean], end: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CollClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, end),
        true, true, element[Int]))
    }

    def take(n: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("take", classOf[Sym]),
        Array[AnyRef](n),
        true, true, element[Coll[A]]))
    }

    def partition(pred: Ref[A => Boolean]): Ref[(Coll[A], Coll[A])] = {
      asRep[(Coll[A], Coll[A])](mkMethodCall(source,
        CollClass.getMethod("partition", classOf[Sym]),
        Array[AnyRef](pred),
        true, true, element[(Coll[A], Coll[A])]))
    }

    def patch(from: Ref[Int], patch: Ref[Coll[A]], replaced: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, patch, replaced),
        true, true, element[Coll[A]]))
    }

    def updated(index: Ref[Int], elem: Ref[A]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, elem),
        true, true, element[Coll[A]]))
    }

    def updateMany(indexes: Ref[Coll[Int]], values: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        Array[AnyRef](indexes, values),
        true, true, element[Coll[A]]))
    }

    def mapReduce[K, V](m: Ref[A => (K, V)], r: Ref[((V, V)) => V]): Ref[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(source,
        CollClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        Array[AnyRef](m, r),
        true, true, element[Coll[(K, V)]]))
    }

    override def groupBy[K](key: Ref[A => K]): Ref[Coll[(K, Coll[A])]] = {
      implicit val eK = key.elem.eRange
      asRep[Coll[(K, Coll[A])]](mkMethodCall(source,
        CollClass.getMethod("groupBy", classOf[Sym]),
        Array[AnyRef](key),
        true, true, element[Coll[(K, Coll[A])]]))
    }

    override def groupByProjecting[K, V](key: Ref[A => K], proj: Ref[A => V]): Ref[Coll[(K, Coll[V])]] = {
      implicit val eK = key.elem.eRange
implicit val eV = proj.elem.eRange
      asRep[Coll[(K, Coll[V])]](mkMethodCall(source,
        CollClass.getMethod("groupByProjecting", classOf[Sym], classOf[Sym]),
        Array[AnyRef](key, proj),
        true, true, element[Coll[(K, Coll[V])]]))
    }

    def unionSet(that: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("unionSet", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Coll[A]]))
    }

    override def diff(that: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("diff", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Coll[A]]))
    }

    override def intersect(that: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("intersect", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Coll[A]]))
    }

    def sum(m: Ref[Monoid[A]]): Ref[A] = {
      asRep[A](mkMethodCall(source,
        CollClass.getMethod("sum", classOf[Sym]),
        Array[AnyRef](m),
        true, true, element[A]))
    }

    def slice(from: Ref[Int], until: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, until),
        true, true, element[Coll[A]]))
    }

    def append(other: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("append", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[Coll[A]]))
    }

    def reverse: Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        CollClass.getMethod("reverse"),
        WrappedArray.empty,
        true, true, element[Coll[A]]))
    }
  }

  // entityUnref: single unref method for each type family
  val createCollAdapter: Ref[Coll[Any]] => Coll[Any] = x => CollAdapter(x)

  implicit def unrefColl[A](p: Ref[Coll[A]]): Coll[A] = {
    val sym = p.asInstanceOf[SingleRef[Coll[A]]]
    sym.getAdapter(
      p.node.isInstanceOf[Coll[A]@unchecked],
      createCollAdapter.asInstanceOf[Ref[Coll[A]] => Coll[A]])
  }

  implicit def castCollElement[A](elem: Elem[Coll[A]]): CollElem[A, Coll[A]] =
    elem.asInstanceOf[CollElem[A, Coll[A]]]

  implicit lazy val containerColl: Functor[Coll] = new Functor[Coll] {
    def lift[A](implicit evA: Elem[A]) = element[Coll[A]]
    def unlift[A](implicit eFT: Elem[Coll[A]]) =
      castCollElement(eFT).eA
    def unapply[T](e: Elem[_]) = e match {
      case e: CollElem[_,_] => Some(asElem[Coll[T]](e))
      case _ => None
    }
    def map[A,B](xs: Ref[Coll[A]])(f: Ref[A] => Ref[B]) = { implicit val eA = unlift(xs.elem); xs.map(fun(f))}
  }

  // manual fix: CollIso, collIso

  // familyElem
  class CollElem[A, To <: Coll[A]](implicit _eA: Elem[A])
    extends EntityElem1[A, To, Coll](_eA, container[Coll]) {
    def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SColl[_], To](liftableColl(_eA.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[Coll[A]], classOf[SColl[_]], Set(
        "builder", "length", "size", "isEmpty", "nonEmpty", "apply", "isDefinedAt", "getOrElse", "map", "zip", "exists", "forall", "filter", "foldLeft", "indices", "flatMap", "segmentLength", "find", "indexWhere", "indexOf", "lastIndexWhere", "take", "partition", "patch", "updated", "updateMany", "mapReduce", "groupBy", "groupByProjecting", "unionSet", "diff", "intersect", "sum", "slice", "append", "reverse"
        ))
    }

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }

  implicit def collElement[A](implicit eA: Elem[A]): Elem[Coll[A]] =
    cachedElemByClass(eA)(classOf[CollElem[A, Coll[A]]])

  implicit case object CollCompanionElem extends CompanionElem[CollCompanionCtor]

  abstract class CollCompanionCtor extends CompanionDef[CollCompanionCtor] with CollCompanion {
    def resultType = CollCompanionElem
    override def toString = "Coll"
  }
  implicit def unrefCollCompanionCtor(p: Ref[CollCompanionCtor]): CollCompanionCtor =
    p.node.asInstanceOf[CollCompanionCtor]

  lazy val RColl: Ref[CollCompanionCtor] = new CollCompanionCtor {
    private val thisClass = classOf[CollCompanion]
  }

  // manual fix: ViewColl

  object CollMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Ref[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "builder" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Coll[A]] forSome {type A}] = unapply(exp.node)
    }

    object length {
      def unapply(d: Def[_]): Nullable[Ref[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "length" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Coll[A]] forSome {type A}] = unapply(exp.node)
    }

    object size {
      def unapply(d: Def[_]): Nullable[Ref[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "size" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Coll[A]] forSome {type A}] = unapply(exp.node)
    }

    object isEmpty {
      def unapply(d: Def[_]): Nullable[Ref[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "isEmpty" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Coll[A]] forSome {type A}] = unapply(exp.node)
    }

    object nonEmpty {
      def unapply(d: Def[_]): Nullable[Ref[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "nonEmpty" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Coll[A]] forSome {type A}] = unapply(exp.node)
    }

    object apply {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "apply" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object isDefinedAt {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "isDefinedAt" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object getOrElse {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int], Ref[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "getOrElse" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int], Ref[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int], Ref[A]) forSome {type A}] = unapply(exp.node)
    }

    object map {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "map" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => B]) forSome {type A; type B}] = unapply(exp.node)
    }

    object zip {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "zip" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Coll[B]]) forSome {type A; type B}] = unapply(exp.node)
    }

    object exists {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "exists" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object forall {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "forall" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object filter {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "filter" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object foldLeft {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[B], Ref[((B, A)) => B]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "foldLeft" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[B], Ref[((B, A)) => B]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[B], Ref[((B, A)) => B]) forSome {type A; type B}] = unapply(exp.node)
    }

    object indices {
      def unapply(d: Def[_]): Nullable[Ref[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "indices" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Coll[A]] forSome {type A}] = unapply(exp.node)
    }

    object flatMap {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "flatMap" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Coll[B]]) forSome {type A; type B}] = unapply(exp.node)
    }

    object segmentLength {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "segmentLength" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object find {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "find" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object indexWhere {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "indexWhere" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object indexOf {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "indexOf" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object lastIndexWhere {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "lastIndexWhere" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object take {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "take" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object partition {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "partition" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => Boolean]) forSome {type A}] = unapply(exp.node)
    }

    object patch {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int], Ref[Coll[A]], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "patch" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int], Ref[Coll[A]], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int], Ref[Coll[A]], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object updated {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int], Ref[A]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "updated" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int], Ref[A]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int], Ref[A]) forSome {type A}] = unapply(exp.node)
    }

    object updateMany {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Coll[Int]], Ref[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "updateMany" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Coll[Int]], Ref[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Coll[Int]], Ref[Coll[A]]) forSome {type A}] = unapply(exp.node)
    }

    object mapReduce {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => (K, V)], Ref[((V, V)) => V]) forSome {type A; type K; type V}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mapReduce" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => (K, V)], Ref[((V, V)) => V]) forSome {type A; type K; type V}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => (K, V)], Ref[((V, V)) => V]) forSome {type A; type K; type V}] = unapply(exp.node)
    }

    object groupBy {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => K]) forSome {type A; type K}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "groupBy" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => K]) forSome {type A; type K}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => K]) forSome {type A; type K}] = unapply(exp.node)
    }

    object groupByProjecting {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[A => K], Ref[A => V]) forSome {type A; type K; type V}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "groupByProjecting" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[A => K], Ref[A => V]) forSome {type A; type K; type V}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[A => K], Ref[A => V]) forSome {type A; type K; type V}] = unapply(exp.node)
    }

    object unionSet {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "unionSet" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = unapply(exp.node)
    }

    object diff {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "diff" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = unapply(exp.node)
    }

    object intersect {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "intersect" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = unapply(exp.node)
    }

    object sum {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Monoid[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "sum" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Monoid[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Monoid[A]]) forSome {type A}] = unapply(exp.node)
    }

    object slice {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Int], Ref[Int]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "slice" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Int], Ref[Int]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Int], Ref[Int]) forSome {type A}] = unapply(exp.node)
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "append" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[Coll[A]], Ref[Coll[A]]) forSome {type A}] = unapply(exp.node)
    }

    object reverse {
      def unapply(d: Def[_]): Nullable[Ref[Coll[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "reverse" && receiver.elem.isInstanceOf[CollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Coll[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Coll[A]] forSome {type A}] = unapply(exp.node)
    }
  }

  object CollCompanionMethods {
  }
} // of object Coll
  registerEntityObject("Coll", Coll)

  // manual fix: UserTypeColl removed
  // manual fix: unapplyViews removed
  // manual fix: RepColl removed
  // manual fix: rewriteDef removed

object PairColl extends EntityObject("PairColl") {
  private val PairCollClass = classOf[PairColl[_, _]]

  // entityAdapter for PairColl trait
  case class PairCollAdapter[L, R](source: Ref[PairColl[L, R]])
      extends Node with PairColl[L, R]
      with Def[PairColl[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asInstanceOf[Elem[L]];
implicit lazy val eR = source.elem.typeArgs("R")._1.asInstanceOf[Elem[R]]
    override lazy val eA: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    val resultType: Elem[PairColl[L, R]] = element[PairColl[L, R]]
    override def transform(t: Transformer) = PairCollAdapter[L, R](t(source))

    def ls: Ref[Coll[L]] = {
      asRep[Coll[L]](mkMethodCall(source,
        PairCollClass.getMethod("ls"),
        WrappedArray.empty,
        true, true, element[Coll[L]]))
    }

    def rs: Ref[Coll[R]] = {
      asRep[Coll[R]](mkMethodCall(source,
        PairCollClass.getMethod("rs"),
        WrappedArray.empty,
        true, true, element[Coll[R]]))
    }

    def mapFirst[T1](f: Ref[L => T1]): Ref[Coll[(T1, R)]] = {
      implicit val eT1 = f.elem.eRange
      asRep[Coll[(T1, R)]](mkMethodCall(source,
        PairCollClass.getMethod("mapFirst", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[(T1, R)]]))
    }

    def mapSecond[T1](f: Ref[R => T1]): Ref[Coll[(L, T1)]] = {
      implicit val eT1 = f.elem.eRange
      asRep[Coll[(L, T1)]](mkMethodCall(source,
        PairCollClass.getMethod("mapSecond", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[(L, T1)]]))
    }

    def builder: Ref[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(source,
        PairCollClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CollBuilder]))
    }

    def length: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        PairCollClass.getMethod("length"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def isEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        PairCollClass.getMethod("isEmpty"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def nonEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        PairCollClass.getMethod("nonEmpty"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def apply(i: Ref[Int]): Ref[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        PairCollClass.getMethod("apply", classOf[Sym]),
        Array[AnyRef](i),
        true, true, element[(L, R)]))
    }

    def isDefinedAt(idx: Ref[Int]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        PairCollClass.getMethod("isDefinedAt", classOf[Sym]),
        Array[AnyRef](idx),
        true, true, element[Boolean]))
    }

    def getOrElse(index: Ref[Int], default: Ref[(L, R)]): Ref[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        PairCollClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, default),
        true, true, element[(L, R)]))
    }

    def map[B](f: Ref[((L, R)) => B]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(source,
        PairCollClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[B]]))
    }

    // manual fix
    def zip[B](ys: Ref[Coll[B]]): Ref[Coll[((L, R), B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[((L, R), B)]](mkMethodCall(source,
        PairCollClass.getMethod("zip", classOf[Sym]),
        Array[AnyRef](ys),
        true, true, element[Coll[((L, R), B)]](collElement(pairElement(pairElement(eL, eR), eB)))))
    }

    def exists(p: Ref[((L, R)) => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        PairCollClass.getMethod("exists", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Boolean]))
    }

    def forall(p: Ref[((L, R)) => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        PairCollClass.getMethod("forall", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Boolean]))
    }

    def filter(p: Ref[((L, R)) => Boolean]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Coll[(L, R)]]))
    }

    def foldLeft[B](zero: Ref[B], op: Ref[((B, (L, R))) => B]): Ref[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        PairCollClass.getMethod("foldLeft", classOf[Sym], classOf[Sym]),
        Array[AnyRef](zero, op),
        true, true, element[B]))
    }

    def indices: Ref[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        PairCollClass.getMethod("indices"),
        WrappedArray.empty,
        true, true, element[Coll[Int]]))
    }

    def flatMap[B](f: Ref[((L, R)) => Coll[B]]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asInstanceOf[Elem[B]]
      asRep[Coll[B]](mkMethodCall(source,
        PairCollClass.getMethod("flatMap", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[B]]))
    }

    def segmentLength(p: Ref[((L, R)) => Boolean], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        PairCollClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, from),
        true, true, element[Int]))
    }

    override def find(p: Ref[((L, R)) => Boolean]): Ref[WOption[(L, R)]] = {
      asRep[WOption[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("find", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[WOption[(L, R)]]))
    }

    def indexWhere(p: Ref[((L, R)) => Boolean], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        PairCollClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, from),
        true, true, element[Int]))
    }

    override def indexOf(elem: Ref[(L, R)], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        PairCollClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        Array[AnyRef](elem, from),
        true, true, element[Int]))
    }

    def lastIndexWhere(p: Ref[((L, R)) => Boolean], end: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        PairCollClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, end),
        true, true, element[Int]))
    }

    def take(n: Ref[Int]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("take", classOf[Sym]),
        Array[AnyRef](n),
        true, true, element[Coll[(L, R)]]))
    }

    // manual fix
    def partition(pred: Ref[((L, R)) => Boolean]): Ref[(Coll[(L, R)], Coll[(L, R)])] = {
      asRep[(Coll[(L, R)], Coll[(L, R)])](mkMethodCall(source,
        PairCollClass.getMethod("partition", classOf[Sym]),
        Array[AnyRef](pred),
        true, true, element[(Coll[(L, R)], Coll[(L, R)])](pairElement(collElement(pairElement(eL,eR)), collElement(pairElement(eL,eR))))))
    }

    def patch(from: Ref[Int], patch: Ref[Coll[(L, R)]], replaced: Ref[Int]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, patch, replaced),
        true, true, element[Coll[(L, R)]]))
    }

    def updated(index: Ref[Int], elem: Ref[(L, R)]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, elem),
        true, true, element[Coll[(L, R)]]))
    }

    def updateMany(indexes: Ref[Coll[Int]], values: Ref[Coll[(L, R)]]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        Array[AnyRef](indexes, values),
        true, true, element[Coll[(L, R)]]))
    }

    def mapReduce[K, V](m: Ref[((L, R)) => (K, V)], r: Ref[((V, V)) => V]): Ref[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(source,
        PairCollClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        Array[AnyRef](m, r),
        true, true, element[Coll[(K, V)]]))
    }

    // manual fix
    override def groupBy[K](key: Ref[((L, R)) => K]): Ref[Coll[(K, Coll[(L, R)])]] = {
      implicit val eK = key.elem.eRange
      asRep[Coll[(K, Coll[(L, R)])]](mkMethodCall(source,
        PairCollClass.getMethod("groupBy", classOf[Sym]),
        Array[AnyRef](key),
        true, true, element[Coll[(K, Coll[(L, R)])]](collElement(pairElement(eK, collElement(pairElement(eL, eR)))))))
    }

    override def groupByProjecting[K, V](key: Ref[((L, R)) => K], proj: Ref[((L, R)) => V]): Ref[Coll[(K, Coll[V])]] = {
      implicit val eK = key.elem.eRange
implicit val eV = proj.elem.eRange
      asRep[Coll[(K, Coll[V])]](mkMethodCall(source,
        PairCollClass.getMethod("groupByProjecting", classOf[Sym], classOf[Sym]),
        Array[AnyRef](key, proj),
        true, true, element[Coll[(K, Coll[V])]]))
    }

    def unionSet(that: Ref[Coll[(L, R)]]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("unionSet", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Coll[(L, R)]]))
    }

    override def diff(that: Ref[Coll[(L, R)]]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("diff", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Coll[(L, R)]]))
    }

    override def intersect(that: Ref[Coll[(L, R)]]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("intersect", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Coll[(L, R)]]))
    }

    def sum(m: Ref[Monoid[(L, R)]]): Ref[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        PairCollClass.getMethod("sum", classOf[Sym]),
        Array[AnyRef](m),
        true, true, element[(L, R)]))
    }

    def slice(from: Ref[Int], until: Ref[Int]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, until),
        true, true, element[Coll[(L, R)]]))
    }

    def append(other: Ref[Coll[(L, R)]]): Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("append", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[Coll[(L, R)]]))
    }

    def reverse: Ref[Coll[(L, R)]] = {
      asRep[Coll[(L, R)]](mkMethodCall(source,
        PairCollClass.getMethod("reverse"),
        WrappedArray.empty,
        true, true, element[Coll[(L, R)]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefPairColl[L, R](p: Ref[PairColl[L, R]]): PairColl[L, R] = {
    if (p.node.isInstanceOf[PairColl[L, R]@unchecked]) p.node.asInstanceOf[PairColl[L, R]]
    else
      PairCollAdapter(p)
  }

  // familyElem
  class PairCollElem[L, R, To <: PairColl[L, R]](implicit _eL: Elem[L], _eR: Elem[R])
    extends CollElem[(L, R), To] {
    def eL = _eL
    def eR = _eR

    override lazy val parent: Option[Elem[_]] = Some(collElement(pairElement(element[L],element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }

  implicit def pairCollElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[PairColl[L, R]] =
    cachedElemByClass(eL, eR)(classOf[PairCollElem[L, R, PairColl[L, R]]])

  implicit case object PairCollCompanionElem extends CompanionElem[PairCollCompanionCtor]

  abstract class PairCollCompanionCtor extends CompanionDef[PairCollCompanionCtor] with PairCollCompanion {
    def resultType = PairCollCompanionElem
    override def toString = "PairColl"
  }
  implicit def unrefPairCollCompanionCtor(p: Ref[PairCollCompanionCtor]): PairCollCompanionCtor =
    p.node.asInstanceOf[PairCollCompanionCtor]

  lazy val RPairColl: Ref[PairCollCompanionCtor] = new PairCollCompanionCtor {
    private val thisClass = classOf[PairCollCompanion]
  }

  object PairCollMethods {
    object ls {
      def unapply(d: Def[_]): Nullable[Ref[PairColl[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "ls" && receiver.elem.isInstanceOf[PairCollElem[_, _, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[PairColl[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[PairColl[L, R]] forSome {type L; type R}] = unapply(exp.node)
    }

    object rs {
      def unapply(d: Def[_]): Nullable[Ref[PairColl[L, R]] forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "rs" && receiver.elem.isInstanceOf[PairCollElem[_, _, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[PairColl[L, R]] forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[PairColl[L, R]] forSome {type L; type R}] = unapply(exp.node)
    }

    object mapFirst {
      def unapply(d: Def[_]): Nullable[(Ref[PairColl[L, R]], Ref[L => T1]) forSome {type L; type R; type T1}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mapFirst" && receiver.elem.isInstanceOf[PairCollElem[_, _, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[PairColl[L, R]], Ref[L => T1]) forSome {type L; type R; type T1}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[PairColl[L, R]], Ref[L => T1]) forSome {type L; type R; type T1}] = unapply(exp.node)
    }

    object mapSecond {
      def unapply(d: Def[_]): Nullable[(Ref[PairColl[L, R]], Ref[R => T1]) forSome {type L; type R; type T1}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mapSecond" && receiver.elem.isInstanceOf[PairCollElem[_, _, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[PairColl[L, R]], Ref[R => T1]) forSome {type L; type R; type T1}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[PairColl[L, R]], Ref[R => T1]) forSome {type L; type R; type T1}] = unapply(exp.node)
    }
  }

  object PairCollCompanionMethods {
  }
} // of object PairColl
  registerEntityObject("PairColl", PairColl)

object ReplColl extends EntityObject("ReplColl") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SReplColl[A] = special.collection.ReplColl[A]
  case class ReplCollConst[SA, A](
        constValue: SReplColl[SA],
        lA: Liftable[SA, A]
      ) extends LiftedConst[SReplColl[SA], ReplColl[A]] with ReplColl[A]
        with Def[ReplColl[A]] with ReplCollConstMethods[A] {
    implicit def eA: Elem[A] = lA.eW

    val liftable: Liftable[SReplColl[SA], ReplColl[A]] = liftableReplColl(lA)
    val resultType: Elem[ReplColl[A]] = liftable.eW
  }

  trait ReplCollConstMethods[A] extends ReplColl[A] with CollConstMethods[A] { thisConst: Def[_] =>
    implicit def eA: Elem[A]
    private val ReplCollClass = classOf[ReplColl[A]]

    override def value: Ref[A] = {
      asRep[A](mkMethodCall(self,
        ReplCollClass.getMethod("value"),
        WrappedArray.empty,
        true, false, element[A]))
    }

    override def length: Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        ReplCollClass.getMethod("length"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def append(other: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(self,
        ReplCollClass.getMethod("append", classOf[Sym]),
        Array[AnyRef](other),
        true, false, element[Coll[A]]))
    }
  }

  case class LiftableReplColl[SA, A](lA: Liftable[SA, A])
    extends Liftable[SReplColl[SA], ReplColl[A]] {
    lazy val eW: Elem[ReplColl[A]] = replCollElement(lA.eW)
    lazy val sourceType: RType[SReplColl[SA]] = {
            implicit val tagSA = lA.sourceType.asInstanceOf[RType[SA]]
      RType[SReplColl[SA]]
    }
    def lift(x: SReplColl[SA]): Ref[ReplColl[A]] = ReplCollConst(x, lA)
    def unlift(w: Ref[ReplColl[A]]): SReplColl[SA] = w match {
      case Def(ReplCollConst(x: SReplColl[_], _lA))
            if _lA == lA => x.asInstanceOf[SReplColl[SA]]
      case _ => unliftError(w)
    }
  }
  implicit def liftableReplColl[SA, A](implicit lA: Liftable[SA,A]): Liftable[SReplColl[SA], ReplColl[A]] =
    LiftableReplColl(lA)

  private val ReplCollClass = classOf[ReplColl[_]]

  // entityAdapter for ReplColl trait
  case class ReplCollAdapter[A](source: Ref[ReplColl[A]])
      extends Node with ReplColl[A]
      with Def[ReplColl[A]] {
    implicit lazy val eA = source.elem.typeArgs("A")._1.asInstanceOf[Elem[A]]

    val resultType: Elem[ReplColl[A]] = element[ReplColl[A]]
    override def transform(t: Transformer) = ReplCollAdapter[A](t(source))

    def value: Ref[A] = {
      asRep[A](mkMethodCall(source,
        ReplCollClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[A]))
    }

    def length: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        ReplCollClass.getMethod("length"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def append(other: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("append", classOf[Sym]),
        Array[AnyRef](other),
        true, true, element[Coll[A]]))
    }

    def builder: Ref[CollBuilder] = {
      asRep[CollBuilder](mkMethodCall(source,
        ReplCollClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CollBuilder]))
    }

    def isEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        ReplCollClass.getMethod("isEmpty"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def nonEmpty: Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        ReplCollClass.getMethod("nonEmpty"),
        WrappedArray.empty,
        true, true, element[Boolean]))
    }

    def apply(i: Ref[Int]): Ref[A] = {
      asRep[A](mkMethodCall(source,
        ReplCollClass.getMethod("apply", classOf[Sym]),
        Array[AnyRef](i),
        true, true, element[A]))
    }

    def isDefinedAt(idx: Ref[Int]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        ReplCollClass.getMethod("isDefinedAt", classOf[Sym]),
        Array[AnyRef](idx),
        true, true, element[Boolean]))
    }

    def getOrElse(index: Ref[Int], default: Ref[A]): Ref[A] = {
      asRep[A](mkMethodCall(source,
        ReplCollClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, default),
        true, true, element[A]))
    }

    def map[B](f: Ref[A => B]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange
      asRep[Coll[B]](mkMethodCall(source,
        ReplCollClass.getMethod("map", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[B]]))
    }

    def zip[B](ys: Ref[Coll[B]]): Ref[Coll[(A, B)]] = {
      implicit val eB = ys.eA
      asRep[Coll[(A, B)]](mkMethodCall(source,
        ReplCollClass.getMethod("zip", classOf[Sym]),
        Array[AnyRef](ys),
        true, true, element[Coll[(A, B)]]))
    }

    def exists(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        ReplCollClass.getMethod("exists", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Boolean]))
    }

    def forall(p: Ref[A => Boolean]): Ref[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        ReplCollClass.getMethod("forall", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Boolean]))
    }

    def filter(p: Ref[A => Boolean]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("filter", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[Coll[A]]))
    }

    def foldLeft[B](zero: Ref[B], op: Ref[((B, A)) => B]): Ref[B] = {
      implicit val eB = zero.elem
      asRep[B](mkMethodCall(source,
        ReplCollClass.getMethod("foldLeft", classOf[Sym], classOf[Sym]),
        Array[AnyRef](zero, op),
        true, true, element[B]))
    }

    def indices: Ref[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        ReplCollClass.getMethod("indices"),
        WrappedArray.empty,
        true, true, element[Coll[Int]]))
    }

    def flatMap[B](f: Ref[A => Coll[B]]): Ref[Coll[B]] = {
      implicit val eB = f.elem.eRange.typeArgs("A")._1.asInstanceOf[Elem[B]]
      asRep[Coll[B]](mkMethodCall(source,
        ReplCollClass.getMethod("flatMap", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[Coll[B]]))
    }

    def segmentLength(p: Ref[A => Boolean], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        ReplCollClass.getMethod("segmentLength", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, from),
        true, true, element[Int]))
    }

    override def find(p: Ref[A => Boolean]): Ref[WOption[A]] = {
      asRep[WOption[A]](mkMethodCall(source,
        ReplCollClass.getMethod("find", classOf[Sym]),
        Array[AnyRef](p),
        true, true, element[WOption[A]]))
    }

    def indexWhere(p: Ref[A => Boolean], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        ReplCollClass.getMethod("indexWhere", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, from),
        true, true, element[Int]))
    }

    override def indexOf(elem: Ref[A], from: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        ReplCollClass.getMethod("indexOf", classOf[Sym], classOf[Sym]),
        Array[AnyRef](elem, from),
        true, true, element[Int]))
    }

    def lastIndexWhere(p: Ref[A => Boolean], end: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        ReplCollClass.getMethod("lastIndexWhere", classOf[Sym], classOf[Sym]),
        Array[AnyRef](p, end),
        true, true, element[Int]))
    }

    def take(n: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("take", classOf[Sym]),
        Array[AnyRef](n),
        true, true, element[Coll[A]]))
    }

    def partition(pred: Ref[A => Boolean]): Ref[(Coll[A], Coll[A])] = {
      asRep[(Coll[A], Coll[A])](mkMethodCall(source,
        ReplCollClass.getMethod("partition", classOf[Sym]),
        Array[AnyRef](pred),
        true, true, element[(Coll[A], Coll[A])]))
    }

    def patch(from: Ref[Int], patch: Ref[Coll[A]], replaced: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("patch", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, patch, replaced),
        true, true, element[Coll[A]]))
    }

    def updated(index: Ref[Int], elem: Ref[A]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("updated", classOf[Sym], classOf[Sym]),
        Array[AnyRef](index, elem),
        true, true, element[Coll[A]]))
    }

    def updateMany(indexes: Ref[Coll[Int]], values: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("updateMany", classOf[Sym], classOf[Sym]),
        Array[AnyRef](indexes, values),
        true, true, element[Coll[A]]))
    }

    def mapReduce[K, V](m: Ref[A => (K, V)], r: Ref[((V, V)) => V]): Ref[Coll[(K, V)]] = {
      implicit val eK = m.elem.eRange.eFst
implicit val eV = m.elem.eRange.eSnd
      asRep[Coll[(K, V)]](mkMethodCall(source,
        ReplCollClass.getMethod("mapReduce", classOf[Sym], classOf[Sym]),
        Array[AnyRef](m, r),
        true, true, element[Coll[(K, V)]]))
    }

    override def groupBy[K](key: Ref[A => K]): Ref[Coll[(K, Coll[A])]] = {
      implicit val eK = key.elem.eRange
      asRep[Coll[(K, Coll[A])]](mkMethodCall(source,
        ReplCollClass.getMethod("groupBy", classOf[Sym]),
        Array[AnyRef](key),
        true, true, element[Coll[(K, Coll[A])]]))
    }

    override def groupByProjecting[K, V](key: Ref[A => K], proj: Ref[A => V]): Ref[Coll[(K, Coll[V])]] = {
      implicit val eK = key.elem.eRange
implicit val eV = proj.elem.eRange
      asRep[Coll[(K, Coll[V])]](mkMethodCall(source,
        ReplCollClass.getMethod("groupByProjecting", classOf[Sym], classOf[Sym]),
        Array[AnyRef](key, proj),
        true, true, element[Coll[(K, Coll[V])]]))
    }

    def unionSet(that: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("unionSet", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Coll[A]]))
    }

    override def diff(that: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("diff", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Coll[A]]))
    }

    override def intersect(that: Ref[Coll[A]]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("intersect", classOf[Sym]),
        Array[AnyRef](that),
        true, true, element[Coll[A]]))
    }

    def sum(m: Ref[Monoid[A]]): Ref[A] = {
      asRep[A](mkMethodCall(source,
        ReplCollClass.getMethod("sum", classOf[Sym]),
        Array[AnyRef](m),
        true, true, element[A]))
    }

    def slice(from: Ref[Int], until: Ref[Int]): Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("slice", classOf[Sym], classOf[Sym]),
        Array[AnyRef](from, until),
        true, true, element[Coll[A]]))
    }

    def reverse: Ref[Coll[A]] = {
      asRep[Coll[A]](mkMethodCall(source,
        ReplCollClass.getMethod("reverse"),
        WrappedArray.empty,
        true, true, element[Coll[A]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefReplColl[A](p: Ref[ReplColl[A]]): ReplColl[A] = {
    if (p.node.isInstanceOf[ReplColl[A]@unchecked]) p.node.asInstanceOf[ReplColl[A]]
    else
      ReplCollAdapter(p)
  }

  // familyElem
  class ReplCollElem[A, To <: ReplColl[A]](implicit _eA: Elem[A])
    extends CollElem[A, To] {
    override def eA = _eA

    override val liftable: Liftables.Liftable[_, To] = asLiftable[SReplColl[_], To](liftableReplColl(_eA.liftable))

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[ReplColl[A]], classOf[SReplColl[_]], Set(
        "value", "length", "append"
        ))
    }

    override lazy val parent: Option[Elem[_]] = Some(collElement(element[A]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant))
  }

  implicit def replCollElement[A](implicit eA: Elem[A]): Elem[ReplColl[A]] =
    cachedElemByClass(eA)(classOf[ReplCollElem[A, ReplColl[A]]])

  implicit case object ReplCollCompanionElem extends CompanionElem[ReplCollCompanionCtor]

  abstract class ReplCollCompanionCtor extends CompanionDef[ReplCollCompanionCtor] with ReplCollCompanion {
    def resultType = ReplCollCompanionElem
    override def toString = "ReplColl"
  }
  implicit def unrefReplCollCompanionCtor(p: Ref[ReplCollCompanionCtor]): ReplCollCompanionCtor =
    p.node.asInstanceOf[ReplCollCompanionCtor]

  lazy val RReplColl: Ref[ReplCollCompanionCtor] = new ReplCollCompanionCtor {
    private val thisClass = classOf[ReplCollCompanion]
  }

  object ReplCollMethods {
    object value {
      def unapply(d: Def[_]): Nullable[Ref[ReplColl[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "value" && receiver.elem.isInstanceOf[ReplCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[ReplColl[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[ReplColl[A]] forSome {type A}] = unapply(exp.node)
    }

    object length {
      def unapply(d: Def[_]): Nullable[Ref[ReplColl[A]] forSome {type A}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "length" && receiver.elem.isInstanceOf[ReplCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[ReplColl[A]] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[ReplColl[A]] forSome {type A}] = unapply(exp.node)
    }

    object append {
      def unapply(d: Def[_]): Nullable[(Ref[ReplColl[A]], Ref[Coll[A]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "append" && receiver.elem.isInstanceOf[ReplCollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[ReplColl[A]], Ref[Coll[A]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[ReplColl[A]], Ref[Coll[A]]) forSome {type A}] = unapply(exp.node)
    }
  }

  object ReplCollCompanionMethods {
  }
} // of object ReplColl
  registerEntityObject("ReplColl", ReplColl)

object CollBuilder extends EntityObject("CollBuilder") {
  // entityConst: single const for each entity
  import Liftables._
  import scala.reflect.{ClassTag, classTag}
  type SCollBuilder = special.collection.CollBuilder
  case class CollBuilderConst(
        constValue: SCollBuilder
      ) extends LiftedConst[SCollBuilder, CollBuilder] with CollBuilder
        with Def[CollBuilder] with CollBuilderConstMethods {
    val liftable: Liftable[SCollBuilder, CollBuilder] = LiftableCollBuilder
    val resultType: Elem[CollBuilder] = liftable.eW
  }

  trait CollBuilderConstMethods extends CollBuilder  { thisConst: Def[_] =>

    private val CollBuilderClass = classOf[CollBuilder]

    override def Monoids: Ref[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(self,
        CollBuilderClass.getMethod("Monoids"),
        WrappedArray.empty,
        true, false, element[MonoidBuilder]))
    }

    override def pairColl[A, B](as: Ref[Coll[A]], bs: Ref[Coll[B]]): Ref[PairColl[A, B]] = {
      implicit val eA = as.eA
implicit val eB = bs.eA
      asRep[PairColl[A, B]](mkMethodCall(self,
        CollBuilderClass.getMethod("pairColl", classOf[Sym], classOf[Sym]),
        Array[AnyRef](as, bs),
        true, false, element[PairColl[A, B]]))
    }

    override def fromItems[T](items: Ref[T]*)(implicit cT: Elem[T]): Ref[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(self,
        CollBuilderClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        Array[AnyRef](items, cT),
        true, false, element[Coll[T]]))
    }

    override def unzip[A, B](xs: Ref[Coll[(A, B)]]): Ref[(Coll[A], Coll[B])] = {
      implicit val eA = xs.eA.eFst
implicit val eB = xs.eA.eSnd
      asRep[(Coll[A], Coll[B])](mkMethodCall(self,
        CollBuilderClass.getMethod("unzip", classOf[Sym]),
        Array[AnyRef](xs),
        true, false, element[(Coll[A], Coll[B])]))
    }

    override def xor(left: Ref[Coll[Byte]], right: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        CollBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](left, right),
        true, false, element[Coll[Byte]]))
    }

    override def replicate[T](n: Ref[Int], v: Ref[T]): Ref[Coll[T]] = {
      implicit val eT = v.elem
      asRep[Coll[T]](mkMethodCall(self,
        CollBuilderClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        Array[AnyRef](n, v),
        true, false, element[Coll[T]]))
    }

    override def emptyColl[T](implicit tT: Elem[T]): Ref[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(self,
        CollBuilderClass.getMethod("emptyColl", classOf[Elem[_]]),
        Array[AnyRef](tT),
        true, false, element[Coll[T]]))
    }

    override def outerJoin[K, L, R, O](left: Ref[Coll[(K, L)]], right: Ref[Coll[(K, R)]])(l: Ref[((K, L)) => O], r: Ref[((K, R)) => O], inner: Ref[((K, (L, R))) => O]): Ref[Coll[(K, O)]] = {
      implicit val eK = left.eA.eFst
implicit val eL = left.eA.eSnd
implicit val eR = right.eA.eSnd
implicit val eO = l.elem.eRange
      asRep[Coll[(K, O)]](mkMethodCall(self,
        CollBuilderClass.getMethod("outerJoin", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](left, right, l, r, inner),
        true, false, element[Coll[(K, O)]]))
    }

    override def flattenColl[A](coll: Ref[Coll[Coll[A]]]): Ref[Coll[A]] = {
      implicit val eA = coll.eA.typeArgs("A")._1.asInstanceOf[Elem[A]]
      asRep[Coll[A]](mkMethodCall(self,
        CollBuilderClass.getMethod("flattenColl", classOf[Sym]),
        Array[AnyRef](coll),
        true, false, element[Coll[A]]))
    }
  }

  implicit object LiftableCollBuilder
    extends Liftable[SCollBuilder, CollBuilder] {
    lazy val eW: Elem[CollBuilder] = collBuilderElement
    lazy val sourceType: RType[SCollBuilder] = {
      RType[SCollBuilder]
    }
    def lift(x: SCollBuilder): Ref[CollBuilder] = CollBuilderConst(x)
    def unlift(w: Ref[CollBuilder]): SCollBuilder = w match {
      case Def(CollBuilderConst(x: SCollBuilder))
            => x.asInstanceOf[SCollBuilder]
      case _ => unliftError(w)
    }
  }

  private val CollBuilderClass = classOf[CollBuilder]

  // entityAdapter for CollBuilder trait
  case class CollBuilderAdapter(source: Ref[CollBuilder])
      extends Node with CollBuilder
      with Def[CollBuilder] {
    val resultType: Elem[CollBuilder] = element[CollBuilder]
    override def transform(t: Transformer) = CollBuilderAdapter(t(source))

    def Monoids: Ref[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(source,
        CollBuilderClass.getMethod("Monoids"),
        WrappedArray.empty,
        true, true, element[MonoidBuilder]))
    }

    def pairColl[A, B](as: Ref[Coll[A]], bs: Ref[Coll[B]]): Ref[PairColl[A, B]] = {
      implicit val eA = as.eA
implicit val eB = bs.eA
      asRep[PairColl[A, B]](mkMethodCall(source,
        CollBuilderClass.getMethod("pairColl", classOf[Sym], classOf[Sym]),
        Array[AnyRef](as, bs),
        true, true, element[PairColl[A, B]]))
    }

    def fromItems[T](items: Ref[T]*)(implicit cT: Elem[T]): Ref[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(source,
        CollBuilderClass.getMethod("fromItems", classOf[Seq[_]], classOf[Elem[_]]),
        Array[AnyRef](items, cT),
        true, true, element[Coll[T]]))
    }

    def unzip[A, B](xs: Ref[Coll[(A, B)]]): Ref[(Coll[A], Coll[B])] = {
      implicit val eA = xs.eA.eFst
implicit val eB = xs.eA.eSnd
      asRep[(Coll[A], Coll[B])](mkMethodCall(source,
        CollBuilderClass.getMethod("unzip", classOf[Sym]),
        Array[AnyRef](xs),
        true, true, element[(Coll[A], Coll[B])]))
    }

    def xor(left: Ref[Coll[Byte]], right: Ref[Coll[Byte]]): Ref[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        CollBuilderClass.getMethod("xor", classOf[Sym], classOf[Sym]),
        Array[AnyRef](left, right),
        true, true, element[Coll[Byte]]))
    }

    def replicate[T](n: Ref[Int], v: Ref[T]): Ref[Coll[T]] = {
      implicit val eT = v.elem
      asRep[Coll[T]](mkMethodCall(source,
        CollBuilderClass.getMethod("replicate", classOf[Sym], classOf[Sym]),
        Array[AnyRef](n, v),
        true, true, element[Coll[T]]))
    }

    def emptyColl[T](implicit tT: Elem[T]): Ref[Coll[T]] = {
      asRep[Coll[T]](mkMethodCall(source,
        CollBuilderClass.getMethod("emptyColl", classOf[Elem[_]]),
        Array[AnyRef](tT),
        true, true, element[Coll[T]]))
    }

    def outerJoin[K, L, R, O](left: Ref[Coll[(K, L)]], right: Ref[Coll[(K, R)]])(l: Ref[((K, L)) => O], r: Ref[((K, R)) => O], inner: Ref[((K, (L, R))) => O]): Ref[Coll[(K, O)]] = {
      implicit val eK = left.eA.eFst
implicit val eL = left.eA.eSnd
implicit val eR = right.eA.eSnd
implicit val eO = l.elem.eRange
      asRep[Coll[(K, O)]](mkMethodCall(source,
        CollBuilderClass.getMethod("outerJoin", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](left, right, l, r, inner),
        true, true, element[Coll[(K, O)]]))
    }

    def flattenColl[A](coll: Ref[Coll[Coll[A]]]): Ref[Coll[A]] = {
      implicit val eA = coll.eA.typeArgs("A")._1.asInstanceOf[Elem[A]]
      asRep[Coll[A]](mkMethodCall(source,
        CollBuilderClass.getMethod("flattenColl", classOf[Sym]),
        Array[AnyRef](coll),
        true, true, element[Coll[A]]))
    }
  }

  // entityUnref: single unref method for each type family
  val createCollBuilderAdapter: Ref[CollBuilder] => CollBuilder = x => CollBuilderAdapter(x)

  implicit def unrefCollBuilder(p: Ref[CollBuilder]): CollBuilder =
    p.asInstanceOf[SingleRef[CollBuilder]].getAdapter(
      p.node.isInstanceOf[CollBuilder],
      createCollBuilderAdapter.asInstanceOf[Ref[CollBuilder] => CollBuilder])

  // familyElem
  class CollBuilderElem[To <: CollBuilder]
    extends EntityElem[To] {
    override val liftable: Liftables.Liftable[_, To] = asLiftable[SCollBuilder, To](LiftableCollBuilder)

    override protected def collectMethods: Map[java.lang.reflect.Method, MethodDesc] = {
      super.collectMethods ++
        Elem.declaredMethods(classOf[CollBuilder], classOf[SCollBuilder], Set(
        "Monoids", "pairColl", "fromItems", "unzip", "xor", "replicate", "emptyColl", "outerJoin", "flattenColl"
        ))
    }
  }

  implicit lazy val collBuilderElement: Elem[CollBuilder] =
    new CollBuilderElem[CollBuilder]

  implicit case object CollBuilderCompanionElem extends CompanionElem[CollBuilderCompanionCtor]

  abstract class CollBuilderCompanionCtor extends CompanionDef[CollBuilderCompanionCtor] with CollBuilderCompanion {
    def resultType = CollBuilderCompanionElem
    override def toString = "CollBuilder"
  }
  implicit def unrefCollBuilderCompanionCtor(p: Ref[CollBuilderCompanionCtor]): CollBuilderCompanionCtor =
    p.node.asInstanceOf[CollBuilderCompanionCtor]

  lazy val RCollBuilder: Ref[CollBuilderCompanionCtor] = new CollBuilderCompanionCtor {
    private val thisClass = classOf[CollBuilderCompanion]
  }

  object CollBuilderMethods {
    object Monoids {
      def unapply(d: Def[_]): Nullable[Ref[CollBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "Monoids" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CollBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CollBuilder]] = unapply(exp.node)
    }

    object pairColl {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Ref[Coll[A]], Ref[Coll[B]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "pairColl" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Ref[Coll[A]], Ref[Coll[B]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Ref[Coll[A]], Ref[Coll[B]]) forSome {type A; type B}] = unapply(exp.node)
    }

    object fromItems {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Seq[Ref[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "fromItems" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Seq[Ref[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Seq[Ref[T]], Elem[T]) forSome {type T}] = unapply(exp.node)
    }

    object unzip {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Ref[Coll[(A, B)]]) forSome {type A; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "unzip" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Ref[Coll[(A, B)]]) forSome {type A; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Ref[Coll[(A, B)]]) forSome {type A; type B}] = unapply(exp.node)
    }

    object xor {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Ref[Coll[Byte]], Ref[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "xor" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Ref[Coll[Byte]], Ref[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Ref[Coll[Byte]], Ref[Coll[Byte]])] = unapply(exp.node)
    }

    object replicate {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Ref[Int], Ref[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "replicate" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Ref[Int], Ref[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Ref[Int], Ref[T]) forSome {type T}] = unapply(exp.node)
    }

    object emptyColl {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "emptyColl" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Elem[T]) forSome {type T}] = unapply(exp.node)
    }

    object outerJoin {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Ref[Coll[(K, L)]], Ref[Coll[(K, R)]], Ref[((K, L)) => O], Ref[((K, R)) => O], Ref[((K, (L, R))) => O]) forSome {type K; type L; type R; type O}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "outerJoin" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3), args(4))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Ref[Coll[(K, L)]], Ref[Coll[(K, R)]], Ref[((K, L)) => O], Ref[((K, R)) => O], Ref[((K, (L, R))) => O]) forSome {type K; type L; type R; type O}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Ref[Coll[(K, L)]], Ref[Coll[(K, R)]], Ref[((K, L)) => O], Ref[((K, R)) => O], Ref[((K, (L, R))) => O]) forSome {type K; type L; type R; type O}] = unapply(exp.node)
    }

    object flattenColl {
      def unapply(d: Def[_]): Nullable[(Ref[CollBuilder], Ref[Coll[Coll[A]]]) forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "flattenColl" && receiver.elem.isInstanceOf[CollBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[CollBuilder], Ref[Coll[Coll[A]]]) forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CollBuilder], Ref[Coll[Coll[A]]]) forSome {type A}] = unapply(exp.node)
    }
  }

  object CollBuilderCompanionMethods {
  }
} // of object CollBuilder
  registerEntityObject("CollBuilder", CollBuilder)

  registerModule(CollsModule)
}

object CollsModule extends scalan.ModuleInfo("special.collection", "Colls")
}

trait CollsModule extends special.collection.impl.CollsDefs {self: Library =>}
