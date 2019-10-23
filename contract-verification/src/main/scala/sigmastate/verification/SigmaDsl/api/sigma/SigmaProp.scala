package sigmastate.verification.SigmaDsl.api.sigma

import scalan.OverloadId
import sigmastate.verification.SigmaDsl.api.collection.Coll
import stainless.annotation.library

// TODO: extract

/** Proposition which can be proven and verified by sigma protocol. */
@scalan.Liftable
@library
sealed trait SigmaProp {
  def isValid: Boolean

  /** Serialized bytes of this sigma proposition taken as ErgoTree and then serialized. */
  def propBytes: Coll[Byte]

  /** Logical AND between this SigmaProp and other SigmaProp.
    * This constructs a new CAND node of sigma tree with two children. */
  @OverloadId("and_sigma") def &&(other: SigmaProp): SigmaProp

  /** Logical AND between this `SigmaProp` and `Boolean` value on the right.
    * The boolean value will be wrapped into `SigmaProp` using `sigmaProp` function.
    * This constructs a new CAND node of sigma tree with two children. */
  @OverloadId("and_bool") def &&(other: Boolean): SigmaProp

  /** Logical OR between this SigmaProp and other SigmaProp.
    * This constructs a new COR node of sigma tree with two children. */
  @OverloadId("or_sigma") def ||(other: SigmaProp): SigmaProp

  /** Logical OR between this `SigmaProp` and `Boolean` value on the right.
    * The boolean value will be wrapped into `SigmaProp` using `sigmaProp` function.
    * This constructs a new COR node of sigma tree with two children. */
  @OverloadId("or_bool") def ||(other: Boolean): SigmaProp
}

@library
trait SigmaBoolean {
}

@library
case class TrivialProp(condition: Boolean) extends SigmaBoolean {
}

@library
case class SigmaPropProof(sigmaTree: SigmaBoolean) extends SigmaProp {

  override def isValid: Boolean = sigmaTree match {
    case p: TrivialProp => p.condition
    case _ => false
  }

  override def propBytes: Coll[Byte] = ???

  override def &&(other: SigmaProp): SigmaProp = ???

  override def &&(other: Boolean): SigmaProp = ???

  override def ||(other: SigmaProp): SigmaProp = ???

  override def ||(other: Boolean): SigmaProp = ???
}
