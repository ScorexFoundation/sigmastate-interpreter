package sigmastate.verification

import sigmastate.verification.contract.Helpers.CollT
import sigmastate.verification.contract.RType
import stainless.collection._
import stainless.annotation.{extern, library, pure}

final case class Coll[A](toColl: CollT[A]) {

  @extern @pure
  def length: Int = ???

  @extern @pure
  def size: Int = {
    this.length
  } ensuring(_ == length)

  @extern @pure
  def nonEmpty: Boolean = {
    length > 0
  } ensuring(_ == length > 0)

  @extern @pure
  def isEmpty: Boolean = length == 0

  @extern @pure
  def apply(i: Int): A = ???

  @extern @pure
  def slice(from: Int, until: Int): Coll[A] = ???

  @extern @pure
  def map[B: RType](f: A => B): Coll[B] = ???

  @extern @pure
  def flatMap[B: RType](f: A => Coll[B]): Coll[B] = ???

  @extern @pure
  def forall(p: A => Boolean): Boolean = ???

  @extern @pure
  def foldLeft[B](zero: B, op: ((B, A)) => B): B =  ???
}


/*
import CollOverList._

@library
case class CollOverList[A](toList: List[A]) extends Coll[A] {

  override def length: Int = ??? //toList.length.toInt

  override def apply(i: Int): A = toList.apply(i)

  override def isEmpty: Boolean = length == 0

  override def nonEmpty: Boolean = length > 0

  def map[@specialized B: RType](f: A => B): Coll[B] = {
    toList.map(f).toColl
  }

  def exists(p: A => Boolean): Boolean = toList.exists(p)

  def forall(p: A => Boolean): Boolean = toList.forall(p)

  //  def filter(p: A => Boolean): Coll[A] = builder.fromArray(toList.filter(p))

  def foldLeft[B](zero: B, op: ((B, A)) => B): B = toList.foldLeft(zero)((b, a) => op((b, a)))

  def slice(from: Int, until: Int): Coll[A] = ??? // toList.slice(from, until).toColl

  override def flatMap[B: RType](f: A => Coll[B]): Coll[B] = {
//    implicit val ctB = RType[B].classTag
    toList.flatMap(x => f(x).toList).toColl
  }

  def sum(m: Monoid[A]): A = ??? // toList.foldLeft(m.zero)((b, a) => m.plus(b, a))

  def zip[@specialized B](ys: Coll[B]): PairColl[A, B] = ???

  override def builder: CollBuilder = ???

  override def toArray: Array[A] = ???

  override def isDefinedAt(idx: Int): Boolean = ???

  override def getOrElse(index: Int, default: A): A = ???

  override def zip[B](ys: EColl[B]): EColl[(A, B)] = ???

  override def filter(p: A => Boolean): EColl[A] = ???

  override def indices: EColl[Int] = ???

  override def flatMap[B](f: A => EColl[B])(implicit evidence$2: RType[B]): EColl[B] = ???

  override def segmentLength(p: A => Boolean, from: Int): Int = ???

  override def indexWhere(p: A => Boolean, from: Int): Int = ???

  override def lastIndexWhere(p: A => Boolean, end: Int): Int = ???

  override def take(n: Int): EColl[A] = ???

  override def partition(pred: A => Boolean): (EColl[A], EColl[A]) = ???

  override def patch(from: Int, patch: EColl[A], replaced: Int): EColl[A] = ???

  override def updated(index: Int, elem: A): EColl[A] = ???

  override def updateMany(indexes: EColl[Int], values: EColl[A]): EColl[A] = ???

  override def mapReduce[K, V](m: A => (K, V), r: ((V, V)) => V)(implicit evidence$3: RType[K], evidence$4: RType[V]): EColl[(K, V)] = ???

  override def unionSet(that: EColl[A]): EColl[A] = ???

  override def append(other: EColl[A]): EColl[A] = ???

  override def reverse: EColl[A] = ???

  override implicit def tItem: RType[A] = ???
}
*/

object Coll {

  @extern @pure
  def empty[A]: Coll[A] = ??? //new CollOverList(List())

  @extern @pure
  def apply[A](a: A): Coll[A] = ??? // new CollOverList(List(a))

//  @library @extern
//  def apply[A](list: List[A]): Coll[A] = ??? // new CollOverList(list)

//  @library
//  implicit class ListOps[A](val list: List[A]) {
//    @extern
//    def toColl: Coll[A] = Coll(list)
//  }

//  @library
//  implicit class CollOps[A](val coll: Coll[A]) {
//    @extern
//    def toList: List[A] = ???
//  }
}

