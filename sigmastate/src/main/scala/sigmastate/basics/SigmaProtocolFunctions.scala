package sigmastate.basics

import java.security.SecureRandom

import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.interpreter.CryptoConstants
import sigmastate.{SigmaProofOfKnowledgeLeaf, UncheckedTree}
import supertagged.TaggedType

import scala.concurrent.Future


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
  object Challenge extends TaggedType[Array[Byte]]
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


trait SigmaProtocolCommonInput[SP <: SigmaProtocol[SP]] {
  val soundness: Int = CryptoConstants.soundnessBits
}

trait SigmaProtocolPrivateInput[SP <: SigmaProtocol[SP], CI <: SigmaProtocolCommonInput[SP]] {
  def publicImage: CI
}

/**
  * common interface for both Prover and Verifier
  */
trait Party[SP <: SigmaProtocol[SP], CI <: SigmaProtocolCommonInput[SP]] {
  val publicInput: CI
}

//implement it as a FSM-DSL waitfor - then - action - then - waitfor - etc
trait InteractiveParty

trait Prover[SP <: SigmaProtocol[SP],
CI <: SigmaProtocolCommonInput[SP],
PI <: SigmaProtocolPrivateInput[SP, CI]] extends Party[SP, CI] {
  val privateInputOpt: Option[PI]
}


trait InteractiveProver[SP <: SigmaProtocol[SP], CI <: SigmaProtocolCommonInput[SP], PI <: SigmaProtocolPrivateInput[SP, CI]]
  extends Prover[SP, CI, PI] with InteractiveParty {

  def firstMessage: SP#A
  def secondMessage(challenge: Challenge): SP#Z

  def simulate(challenge: Challenge): (SP#A, SP#Z)
}

trait SimulatingProver[SP <: SigmaProtocol[SP], CI <: SigmaProtocolCommonInput[SP]] {
  val challenge: Challenge
}


trait ZeroKnowledgeProofOfKnowledge[SP <: SigmaProtocol[SP]]

trait NonInteractiveProver[SP <: SigmaProtocol[SP],
  PI <: SigmaProtocolPrivateInput[SP, CI],
  CI <: SigmaProofOfKnowledgeLeaf[SP, PI],
  P <: UncheckedTree]
  extends Prover[SP, CI, PI] {

  def prove(challenge: Array[Byte]): P
}

trait Verifier[SP <: SigmaProtocol[SP], CI <: SigmaProtocolCommonInput[SP]] extends Party[SP, CI] {
  type P <: Prover[SP, CI, _]
  type ST <: SigmaProtocolTranscript[SP, CI]

  lazy val challenge = Challenge({
    val ch = new Array[Byte](publicInput.soundness / 8)
    new SecureRandom().nextBytes(ch) //modifies ch
    ch
  })

  val publicInput: CI

  def prover: P

  def transcript: Future[Option[ST]]
}

/**
  * Sigma Protocol transcript enough for verification
  *
  * @tparam SP
  * @tparam CI
  */
trait SigmaProtocolTranscript[SP <: SigmaProtocol[SP], CI <: SigmaProtocolCommonInput[SP]] {

  /** Common input known to both prover and verifier. */
  val x: CI

  /** First prover message */
  val a: SP#A

  /** Challenge created by verifier and sent to prover */
  val e: Challenge

  /** Second prover message - response to the challenge */
  val z: SP#Z

  /** Returns true if the verifier has accepted the prover's reponse to the challenge. */
  def accepted: Boolean
}

trait SigmaProtocolMsg

object SigmaProtocolFunctions {

  case class FirstMessage(s: SigmaProtocolMsg)

  case class RandomChallenge(challenge: Challenge)

  case class SecondMessage(s: SigmaProtocolMsg)

  case object StartInteraction

  case class Transcript(a: SigmaProtocolMsg, e: Challenge, z: SigmaProtocolMsg, accepted: Boolean)
}
