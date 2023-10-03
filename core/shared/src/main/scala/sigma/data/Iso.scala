package sigma.data

/** Type-class of isomorphisms between types.
  * Isomorphism between two types `A` and `B` essentially say that both types
  * represents the same information (entity) but in a different way.
  * <p>
  * The information is not lost so that both are true:
  * 1) a == from(to(a))
  * 2) b == to(from(b))
  * <p>
  * It is used to define type-full conversions:
  * - different conversions between Java and Scala data types.
  * - conversion between Ergo representations and generated API representations
  */
abstract class Iso[A, B] {
  def to(a: A): B
  def from(b: B): A
  def andThen[C](iso: Iso[B,C]): Iso[A,C] = ComposeIso(iso, this)
  def inverse: Iso[B, A] = InverseIso(this)
}
final case class InverseIso[A,B](iso: Iso[A,B]) extends Iso[B,A] {
  override def to(a: B): A = iso.from(a)
  override def from(b: A): B = iso.to(b)
}
final case class ComposeIso[A, B, C](iso2: Iso[B, C], iso1: Iso[A, B]) extends Iso[A, C] {
  def from(c: C): A = iso1.from(iso2.from(c))
  def to(a: A): C = iso2.to(iso1.to(a))
}

