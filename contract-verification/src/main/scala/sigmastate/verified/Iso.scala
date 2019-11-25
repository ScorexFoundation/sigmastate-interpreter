package sigmastate.verified

import stainless.annotation.ignore

/** Type-class of isomorphisms between types.
  * Isomorphism between two types `A` and `B` essentially say that both types
  * represents the same information (entity) but in a different way.
  * <p>
  * The information is not lost so that both are true:
  * 1) a == from(to(a))
  * 2) b == to(from(b))
  * <p>
  */
@ignore
abstract class Iso[A, B] {
  def to(a: A): B
  def from(b: B): A
}

@ignore
final case class InverseIso[A, B](iso: Iso[A, B]) extends Iso[B, A] {
  override def to(a: B): A = iso.from(a)
  override def from(b: A): B = iso.to(b)
}

@ignore
trait LowPriorityIsos {

}

@ignore
object Iso extends LowPriorityIsos {

  implicit def identityIso[A]: Iso[A, A] = new Iso[A, A] {
    override def to(a: A): A = a
    override def from(b: A): A = b
  }

  implicit def inverseIso[A, B](implicit iso: Iso[A, B]): Iso[B, A] = InverseIso[A, B](iso)

  def roundTrip[A, B](a: A)(implicit iso: Iso[A, B]): A = iso.from(iso.to(a))
}
