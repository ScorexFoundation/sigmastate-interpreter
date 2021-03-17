package sigmastate

import java.math.BigInteger

import com.google.common.primitives.Shorts
import gf2t.GF2_192_Poly
import sigmastate.basics.DLogProtocol.{FirstDLogProverMessage, ProveDlog}
import sigmastate.basics.VerifierMessage.Challenge
import sigmastate.Values.{ErgoTree, SigmaBoolean, SigmaPropConstant}
import sigmastate.basics.{FirstDiffieHellmanTupleProverMessage, FirstProverMessage, ProveDHTuple}
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer

import scala.language.existentials

object ConjectureType extends Enumeration {
  val AndConjecture = Value(0)
  val OrConjecture = Value(1)
  val ThresholdConjecture = Value(2)
}

//Proof tree

trait ProofTree extends Product

trait ProofTreeLeaf extends ProofTree {
  val proposition: SigmaBoolean
  val commitmentOpt: Option[FirstProverMessage]
}

trait ProofTreeConjecture extends ProofTree {
  val conjectureType: ConjectureType.Value
  val children: Seq[ProofTree]
}

/**
  * Data type which encodes position of a node in a tree.
  *
  * Position is encoded like following (the example provided is for CTHRESHOLD(2, Seq(pk1, pk2, pk3 && pk4)) :
  *
  *            0
  *          / | \
  *         /  |  \
  *       0-0 0-1 0-2
  *               /|
  *              / |
  *             /  |
  *            /   |
  *          0-2-0 0-2-1
  *
  * So a hint associated with pk1 has a position "0-0", pk4 - "0-2-1" .
  *
  * Please note that "0" prefix is for a crypto tree. There are several kinds of trees during evaluation.
  * Initial mixed tree (ergoTree) would have another prefix.
  *
  * @param positions - positions from root (inclusive) in top-down order
  */
case class NodePosition(positions: Seq[Int]) {

  def child(childIdx: Int): NodePosition = NodePosition(positions :+ childIdx)

  override def toString: String = positions.mkString("-")
}

object NodePosition {
  /**
    * Prefix to encode node positions in a crypto tree.
    */
  val CryptoTreePrefix = NodePosition(Seq(0))

  /**
    * Prefix to encode node positions in an ErgoTree instance.
    */
  val ErgoTreePrefix = NodePosition(Seq(1))
}

/**
  * A node of a sigma-tree used by the prover. See ProverInterpreter comments and the
  * ErgoScript white-paper https://ergoplatform.org/docs/ErgoScript.pdf , Appendix A, for details
  */
sealed trait UnprovenTree extends ProofTree {

  /**
    * Position of the node in the tree, see comments for `position` field in
    * `sigmastate.interpreter.Hint`
    */
  val position: NodePosition

  /**
    * Node's sigma-protocol statement to be proven.
    */
  val proposition: SigmaBoolean

  /**
    * Whether the node represents simulated sigma-protocol
    */
  val simulated: Boolean

  def real: Boolean = !simulated

  /**
    * Challenge used by the prover.
    */
  val challengeOpt: Option[Array[Byte]]

  def withChallenge(challenge: Challenge): UnprovenTree

  def withSimulated(newSimulated: Boolean): UnprovenTree

  def withPosition(updatedPosition: NodePosition): UnprovenTree
}

sealed trait UnprovenLeaf extends UnprovenTree with ProofTreeLeaf

sealed trait UnprovenConjecture extends UnprovenTree with ProofTreeConjecture

case class CAndUnproven(override val proposition: CAND,
                        override val challengeOpt: Option[Challenge] = None,
                        override val simulated: Boolean,
                        children: Seq[ProofTree],
                        override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenConjecture {

  override val conjectureType = ConjectureType.AndConjecture

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition): UnprovenTree = this.copy(position = updatedPosition)
}

case class COrUnproven(override val proposition: COR,
                       override val challengeOpt: Option[Challenge] = None,
                       override val simulated: Boolean,
                       children: Seq[ProofTree],
                       override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenConjecture {

  override val conjectureType = ConjectureType.OrConjecture

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition): UnprovenTree = this.copy(position = updatedPosition)
}

/**
  * Unproven threshold k-out-n conjecture. k secrets will be proven, (n-k) simulated.
  * For details on challenge and polynomial used in this case, see [CramerDamgardSchoenmakers94].
  */
case class CThresholdUnproven(override val proposition: CTHRESHOLD,
                       override val challengeOpt: Option[Challenge] = None,
                       override val simulated: Boolean,
                       k: Integer,
                       children: Seq[ProofTree],
                       polynomialOpt: Option[GF2_192_Poly],
                       override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenConjecture {

  require(k >= 0 && k <= children.length, "Wrong k value")
  require(children.size <= 255) // Our polynomial arithmetic can take only byte inputs

  override val conjectureType = ConjectureType.ThresholdConjecture

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition) = this.copy(position = updatedPosition)

  def withPolynomial(newPolynomial: GF2_192_Poly) = this.copy(polynomialOpt = Some(newPolynomial))
}


case class UnprovenSchnorr(override val proposition: ProveDlog,
                           override val commitmentOpt: Option[FirstDLogProverMessage],
                           randomnessOpt: Option[BigInteger],
                           override val challengeOpt: Option[Challenge] = None,
                           override val simulated: Boolean,
                           override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenLeaf {

  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition) = this.copy(position = updatedPosition)
}

case class UnprovenDiffieHellmanTuple(override val proposition: ProveDHTuple,
                                      override val commitmentOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                      randomnessOpt: Option[BigInteger],
                                      override val challengeOpt: Option[Challenge] = None,
                                      override val simulated: Boolean,
                                      override val position: NodePosition = NodePosition.CryptoTreePrefix) extends UnprovenLeaf {
  override def withChallenge(challenge: Challenge) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)

  override def withPosition(updatedPosition: NodePosition) = this.copy(position = updatedPosition)
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
// TODO coverage (8h): write a test that restores the tree from this string and check that the result is equal,
// in order to make sure this conversion is unambiguous
object FiatShamirTree {
  val internalNodePrefix = 0: Byte
  val leafPrefix = 1: Byte

  // TODO optimize: this serialization is not the hot path during verification
  /** HOTSPOT: */
  def toBytes(tree: ProofTree): Array[Byte] = {

    def traverseNode(node: ProofTree): Array[Byte] = node match {
      case l: ProofTreeLeaf =>
        val propTree = ErgoTree.withSegregation(SigmaPropConstant(l.proposition))
        val propBytes = DefaultSerializer.serializeErgoTree(propTree)
        val commitmentBytes = l.commitmentOpt.get.bytes
        leafPrefix +:
          ((Shorts.toByteArray(propBytes.length.toShort) ++ propBytes) ++
            (Shorts.toByteArray(commitmentBytes.length.toShort) ++ commitmentBytes))

      case c: ProofTreeConjecture =>
        val childrenCountBytes = Shorts.toByteArray(c.children.length.toShort)
        val conjBytes = Array(internalNodePrefix, c.conjectureType.id.toByte)
        val thresholdByte = c match {
          case unproven: CThresholdUnproven =>
            Array(unproven.k.toByte)
          case unchecked: CThresholdUncheckedNode =>
            Array(unchecked.k.toByte)
          case _ => Array()
        }

        c.children.foldLeft(conjBytes ++ thresholdByte ++ childrenCountBytes) { case (acc, ch) =>
          acc ++ traverseNode(ch)
        }
    }

    traverseNode(tree)
  }
}
