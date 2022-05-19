package scalan

trait TestLibrary extends Library {
  import CollBuilder._
  import MonoidBuilder._

  lazy val colBuilder: Ref[CollBuilder] = variable[CollBuilder]
  lazy val intPlusMonoid: Ref[Monoid[Int]] = colBuilder.Monoids.intPlusMonoid
  lazy val longPlusMonoid: Ref[Monoid[Long]] = colBuilder.Monoids.longPlusMonoid
}
