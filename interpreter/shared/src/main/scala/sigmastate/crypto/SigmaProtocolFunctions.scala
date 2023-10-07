package sigmastate.crypto

import sigma.crypto.CryptoConstants.dlogGroup
import sigmastate.crypto.VerifierMessage.Challenge
import sigma.Coll
import sigma.data.SigmaLeaf
import supertagged.TaggedType

import java.math.BigInteger

/*
  Abstracting Sigma protocols
  Functionality to get:
  - Interactive Sigma protocols(via actors)
  - Zero-knowledge proof from a Sigma protocol
  - Non-interactive Sigma protocols
  - Commitment from any Sigma protocol
  - Signature from any Sigma protocol
  - Json and ultra-compact binary serialization/deserialization
*/


trait TranscriptMessage {
}

/** The message sent by a prover to its associated verifier as part of a sigma protocol interaction. */
trait ProverMessage extends TranscriptMessage

/** The message sent by a verifier to its associated prover as part of a sigma protocol interaction. */
trait VerifierMessage extends TranscriptMessage

object VerifierMessage {
  /** A challenge from the verifier (message `e` of `SigmaProtocol`)*/
  object Challenge extends TaggedType[Coll[Byte]]
  type Challenge = Challenge.Type
}

/** First message from the prover (message `a` of `SigmaProtocol`)*/
trait FirstProverMessage extends ProverMessage {
  type SP <: SigmaProtocol[SP]

  def bytes: Array[Byte]
}

/** Second message from the prover (message `z` of `SigmaProtocol`)*/
trait SecondProverMessage extends ProverMessage {
  type SP <: SigmaProtocol[SP]
}

/** Abstract template for sigma protocols.
  * For details see the following book
  * [1] Efficient Secure Two-Party Protocols - Techniques and Constructions, p.150)*/
trait SigmaProtocol[SP <: SigmaProtocol[SP]] {
  type A <: FirstProverMessage
  type Z <: SecondProverMessage
}


trait SigmaProtocolPrivateInput[CI <: SigmaLeaf] {
  /** Public image generated from the secret.
    * Represents proof of knowledge proposition.
    */
  def publicImage: CI

  /** Secret random number known to the prover. */
  def w: BigInteger
}

trait SigmaProtocolProver {
  /** Computes response for the challenge in non-interactive sigma protocol.
    *
    * @param privateInput private input of the prover (secret)
    * @param rnd          random number generated by the prover (secret random number used to
    *                     compute commitment)
    * @param challenge    challenge from the verifier (also computed by the prover in non-interactive case)
    * @return response computed by the prover
    */
  protected def responseToChallenge(
      privateInput: SigmaProtocolPrivateInput[_ <: SigmaLeaf],
      rnd: BigInteger,
      challenge: Challenge): BigInteger = {
    val q: BigInteger = dlogGroup.order
    val e: BigInteger = new BigInteger(1, challenge.toArray)
    val ew: BigInteger = e.multiply(privateInput.w).mod(q)
    val z: BigInteger = rnd.add(ew).mod(q)
    z
  }
}


