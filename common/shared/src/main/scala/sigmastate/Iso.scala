package sigmastate

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
  /** Converts from source `to` destination.
    *
    * @param a the source to be converted
    */
  def to(a: A): B

  /** Converts `from` destination to source.
    *
    * @param b the destination value of type B to be converted
    */
  def from(b: B): A

  /** Composes this isomorphism with another isomorphism, creating a new isomorphism
    * that maps from the source type A to a new type C.
    *
    * @tparam C the new destination type
    * @param iso the isomorphism to compose with
    */
  def andThen[C](iso: Iso[B, C]): Iso[A, C] = ComposeIso(this, iso)

  /**
    * Returns the inverse isomorphism, which swaps the roles of source and destination types.
    */
  def inverse: Iso[B, A] = InverseIso(this)
}
/** Represents inverse Iso for the given `iso`. */
final case class InverseIso[A,B](iso: Iso[A,B]) extends Iso[B,A] {
  override def to(a: B): A = iso.from(a)
  override def from(b: A): B = iso.to(b)
}
/** Represents composition of two isos. */
final case class ComposeIso[A, B, C](iso1: Iso[A, B], iso2: Iso[B, C]) extends Iso[A, C] {
  override def from(c: C): A = iso1.from(iso2.from(c))
  override def to(a: A): C = iso2.to(iso1.to(a))
}

object Iso {
  /** For any type A there exists identity Iso. */
  implicit def identityIso[A]: Iso[A, A] = new Iso[A, A] {
    override def to(a: A): A = a
    override def from(b: A): A = b
  }

  /** For every Iso[A, B] there exists an inverse Iso[B, A]. */
  implicit def inverseIso[A, B](implicit iso: Iso[A, B]): Iso[B, A] = InverseIso[A, B](iso)
}