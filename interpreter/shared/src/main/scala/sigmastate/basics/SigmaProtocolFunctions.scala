package sigmastate.basics

import sigmastate.SigmaLeaf
import special.collection.Coll
import supertagged.TaggedType

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
  def publicImage: CI
}

trait SigmaProtocolProver {

}


