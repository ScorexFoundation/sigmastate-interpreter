package sigmastate.interpreter

import java.math.BigInteger

import sigmastate.UncheckedTree
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.FirstProverMessage
import sigmastate.basics.VerifierMessage.Challenge

/**
  * A hint for a prover which helps the prover to prove a statement. For example, if the statement is "pk1 && pk2",
  * and the prover knows only a secret for the public key pk1, the prover fails on proving without a hint. But if the
  * prover knows that pk2 is known to another party, the prover may prove the statement (with an empty proof for "pk2").
  */
trait Hint

/**
  * A hint which is indicating that a secret associated with its public image "image" is known by some other party.
  */
trait OtherSecret extends Hint {
  val image: SigmaBoolean
}

/**
  * A hint which contains a proof-of-knowledge for a secret associated with its public image "image".
  */
case class OtherSecretProven(override val image: SigmaBoolean,
                             challenge: Challenge,
                             uncheckedTree: UncheckedTree) extends OtherSecret

/**
  * A family of hints which are about a correspondence between a public image of a secret image and prover's commitment
  * to randomness ("a" in a sigma protocol).
  */
trait CommitmentHint extends Hint {
  val image: SigmaBoolean
  val commitment: FirstProverMessage
}

/**
  * A hint which a commitment to randomness associated with a public image of a secret, as well as randomness itself.
  * Please note that this randomness should be kept in secret by the prover.
  *
  * @param image      - image of a secret
  * @param randomness - randomness
  * @param commitment - commitment to randomness used while proving knowledge of the secret
  */
case class OwnCommitment(override val image: SigmaBoolean, randomness: BigInteger, commitment: FirstProverMessage) extends CommitmentHint

/**
  * A hint which contains a commitment to randomness associated with a public image of a secret.
  *
  * @param image      - image of a secret
  * @param commitment - commitment to randomness used while proving knowledge of the secret
  */
case class OtherCommitment(override val image: SigmaBoolean, commitment: FirstProverMessage) extends OtherSecret with CommitmentHint

/**
  * Collection of hints to be used by a prover
  *
  * @param hints - hints stored in the bag
  */
case class HintsBag(hints: Seq[Hint]) {

  lazy val otherSecrets: Seq[OtherSecret] = hints.collect { case os: OtherSecret => os }

  lazy val otherSecretsImages: Seq[SigmaBoolean] = otherSecrets.map(_.image)

  lazy val commitments: Seq[CommitmentHint] = hints.collect { case ch: CommitmentHint => ch }

  lazy val proofs: Seq[OtherSecretProven] = hints.collect { case osp: OtherSecretProven => osp }

  def addHint(hint: Hint): HintsBag = HintsBag(hint +: hints)

  def addHints(newHints: Hint*): HintsBag = HintsBag(newHints ++ hints)

  def ++(other: HintsBag): HintsBag = HintsBag(other.hints ++ hints)

  override def toString: String = s"HintsBag(${hints.mkString("\n")})"
}

object HintsBag {
  val empty = HintsBag(Seq.empty)
}
