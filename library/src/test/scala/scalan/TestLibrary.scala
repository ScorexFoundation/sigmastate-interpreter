package scalan

trait TestLibrary extends Library {
  import CollBuilder._
  import CCostedBuilder._
  import CostedBuilder._
  import MonoidBuilder._

  lazy val colBuilder: Ref[CollBuilder] = variable[CollBuilder]
  lazy val costedBuilder: Ref[CostedBuilder] = RCCostedBuilder()
  lazy val intPlusMonoid: Ref[Monoid[Int]] = costedBuilder.monoidBuilder.intPlusMonoid
  lazy val longPlusMonoid: Ref[Monoid[Long]] = costedBuilder.monoidBuilder.longPlusMonoid
}
