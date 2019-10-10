package sigmastate.verification.SigmaDsl.impl.sigma

import sigmastate.verification.SigmaDsl.api.collection.Coll
import sigmastate.verification.SigmaDsl.api.sigma.SigmaProp
import stainless.annotation.library

@library
case class PK(base58Encoded: String) extends SigmaProp {
  override def isValid: Boolean = ???

  override def propBytes: Coll[Byte] = ???

  override def &&(other: SigmaProp): SigmaProp = ???

  override def &&(other: Boolean): SigmaProp = ???

  override def ||(other: SigmaProp): SigmaProp = ???

  override def ||(other: Boolean): SigmaProp = ???
}
