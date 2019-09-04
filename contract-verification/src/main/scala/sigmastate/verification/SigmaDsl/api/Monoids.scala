package sigmastate.verification.SigmaDsl.api

import stainless.annotation.library

@library
trait Monoid[@specialized(Int, Long) T] {
  @library
  def zero: T
  @library
  def plus(x: T, y: T): T
  @library
  def power(x: T, n: Int): T
}

@library
trait MonoidBuilder {
  @library
  def intPlusMonoid: Monoid[Int]
  @library
  def intMaxMonoid: Monoid[Int]
  @library
  def intMinMonoid: Monoid[Int]

  @library
  def longPlusMonoid: Monoid[Long]
  @library
  def longMaxMonoid: Monoid[Long]
  @library
  def longMinMonoid: Monoid[Long]
  @library
  def pairMonoid[@specialized(Int, Long) A, @specialized(Int, Long) B](m1: Monoid[A], m2: Monoid[B]): Monoid[(A, B)]

  //  def sizePlusMonoid[A](mA: Monoid[A]): Monoid[Size[A]]
  //  def sizeMaxMonoid[A](mA: Monoid[A]): Monoid[Size[A]]
  //  def sizeMinMonoid[A](mA: Monoid[A]): Monoid[Size[A]]
}

