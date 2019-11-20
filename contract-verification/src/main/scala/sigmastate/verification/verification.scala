package sigmastate

import stainless.annotation.ignore

package object verification {

  import sigmastate.verification.SigmaDsl.api._
  import sigmastate.verification.SigmaDsl.api.Iso._
  import sigmastate.verification.SigmaDsl.api.VerifiedTypeConverters._

  @ignore
  implicit def implicitTo[A, B](a: A)(implicit iso: Iso[A, B]): B = iso.to(a)

  @ignore
  implicit def implicitFrom[A, B](b: B)(implicit iso: Iso[A, B]): A = InverseIso(iso).to(b)
}
