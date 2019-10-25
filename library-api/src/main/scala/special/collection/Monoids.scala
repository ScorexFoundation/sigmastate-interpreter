package special.collection

import scalan.{WithMethodCallRecognizers, Internal}

trait Monoid[@specialized(Int, Long) T] {
  def zero: T
  def plus(x: T, y: T): T
  def power(x: T, n: Int): T
}

@WithMethodCallRecognizers
trait MonoidBuilder {
  def intPlusMonoid: Monoid[Int]
  @Internal
  def intMaxMonoid: Monoid[Int]
  @Internal
  def intMinMonoid: Monoid[Int]

  def longPlusMonoid: Monoid[Long]
  @Internal
  def longMaxMonoid: Monoid[Long]
  @Internal
  def longMinMonoid: Monoid[Long]

  @Internal
  def pairMonoid[@specialized(Int, Long) A, @specialized(Int, Long) B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A,B)]
}


