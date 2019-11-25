package sigmastate

import stainless.annotation.ignore

package object verified {

  @ignore
  implicit def implicitTo[A, B](a: A)(implicit iso: Iso[A, B]): B = iso.to(a)

  @ignore
  implicit def implicitFrom[A, B](b: B)(implicit iso: Iso[A, B]): A = InverseIso(iso).to(b)
}
