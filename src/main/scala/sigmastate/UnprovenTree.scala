package sigmastate

import java.math.BigInteger

import com.google.common.primitives.Shorts
import scapi.sigma.DLogProtocol.{FirstDLogProverMessage, ProveDlog}
import scapi.sigma.VerifierMessage.Challenge
import scapi.sigma.{FirstDiffieHellmanTupleProverMessage, FirstProverMessage, ProveDiffieHellmanTuple}
import sigmastate.Values.SigmaBoolean
import sigmastate.serialization.ValueSerializer


object ConjectureType extends Enumeration {
  val AndConjecture = Value(0)
  val OrConjecture = Value(1)
}

//Proof tree

trait ProofTree extends Product

trait ProofTreeLeaf extends ProofTree {
  val proposition: SigmaBoolean
  val commitmentOpt: Option[FirstProverMessage[_]]
}

trait ProofTreeConjecture extends ProofTree {
  val conjectureType: ConjectureType.Value
  val children: Seq[ProofTree]
}


sealed trait UnprovenTree extends ProofTree {
  val proposition: SigmaBoolean

  val simulated: Boolean

  def real: Boolean = !simulated

  val challengeOpt: Option[Array[Byte]]

  def withChallenge(challenge: Challenge): UnprovenTree

  def withSimulated(newSimulated: Boolean): UnprovenTree
}

sealed trait UnprovenLeaf extends UnprovenTree with ProofTreeLeaf

sealed trait UnprovenConjecture extends UnprovenTree with ProofTreeConjecture {
}

case class CAndUnproven(override val proposition: CAND,
                        override val challengeOpt: Option[Challenge] = None,
                        override val simulated: Boolean,
                        children: Seq[ProofTree]) extends UnprovenConjecture {

  override val conjectureType = ConjectureType.AndConjecture

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class COrUnproven(override val proposition: COR,
                       override val challengeOpt: Option[Challenge] = None,
                       override val simulated: Boolean,
                       children: Seq[ProofTree]) extends UnprovenConjecture {

  override val conjectureType = ConjectureType.OrConjecture

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class UnprovenSchnorr(override val proposition: ProveDlog,
                           override val commitmentOpt: Option[FirstDLogProverMessage],
                           randomnessOpt: Option[BigInteger],
                           override val challengeOpt: Option[Challenge] = None,
                           override val simulated: Boolean) extends UnprovenLeaf {

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class UnprovenDiffieHellmanTuple(override val proposition: ProveDiffieHellmanTuple,
                                      override val commitmentOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                      randomnessOpt: Option[BigInteger],
                                      override val challengeOpt: Option[Challenge] = None,
                                      override val simulated: Boolean
                                     ) extends UnprovenLeaf {
  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}


/**
  *  Prover Step 7: Convert the tree to a string s for input to the Fiat-Shamir hash function.
  *  The conversion should be such that the tree can be unambiguously parsed and restored given the string.
  *  For each non-leaf node, the string should contain its type (OR or AND).
  *  For each leaf node, the string should contain the Sigma-protocol statement being proven and the commitment.
  *  The string should not contain information on whether a node is marked "real" or "simulated",
  *  and should not contain challenges, responses, or the real/simulated flag for any node.
  *
  */
// todo: write a test that restores the tree from this string and check that the result is equal,
// todo: in order to make sure this conversion is unambiguous
object FiatShamirTree {
  val internalNodePrefix = 0: Byte
  val leafPrefix = 1: Byte

  def toBytes(tree: ProofTree): Array[Byte] = {

    def traverseNode(node: ProofTree): Array[Byte] = node match {
      case l: ProofTreeLeaf =>
        val propBytes = ValueSerializer.serialize(l.proposition)
        val commitmentBytes = l.commitmentOpt.get.bytes
        leafPrefix +:
          ((Shorts.toByteArray(propBytes.length.toShort) ++ propBytes) ++
            (Shorts.toByteArray(commitmentBytes.length.toShort) ++ commitmentBytes))

      case c: ProofTreeConjecture =>
        val childrenCountBytes = Shorts.toByteArray(c.children.length.toShort)
        val conjBytes = Array(internalNodePrefix, c.conjectureType.id.toByte) ++ childrenCountBytes

        c.children.foldLeft(conjBytes) { case (acc, ch) =>
          acc ++ traverseNode(ch)
        }
    }

    traverseNode(tree)
  }
}