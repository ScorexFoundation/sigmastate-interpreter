package sigmastate.verification.SigmaDsl.api

import stainless.annotation.library

@library
trait Monoid[@specialized(Int, Long) T] {
  def zero: T
  def plus(x: T, y: T): T
  def power(x: T, n: Int): T
}

@library
trait MonoidBuilder {
  def intPlusMonoid: Monoid[Int]
  def intMaxMonoid: Monoid[Int]
  def intMinMonoid: Monoid[Int]

  def longPlusMonoid: Monoid[Long]
  def longMaxMonoid: Monoid[Long]
  def longMinMonoid: Monoid[Long]
  def pairMonoid[@specialized(Int, Long) A, @specialized(Int, Long) B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A, B)]

  //  def sizePlusMonoid[A](mA: Monoid[A]): Monoid[Size[A]]
  //  def sizeMaxMonoid[A](mA: Monoid[A]): Monoid[Size[A]]
  //  def sizeMinMonoid[A](mA: Monoid[A]): Monoid[Size[A]]
}

