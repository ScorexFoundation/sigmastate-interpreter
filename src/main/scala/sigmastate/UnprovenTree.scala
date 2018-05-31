package sigmastate

import java.math.BigInteger

import com.google.common.primitives.Shorts
import scapi.sigma.DLogProtocol.{FirstDLogProverMessage, ProveDlog}
import scapi.sigma.{FirstDiffieHellmanTupleProverMessage, FirstProverMessage, ProveDiffieHellmanTuple}
import sigmastate.Values.SigmaBoolean
import sigmastate.serialization.{Serializer, ValueSerializer}
import sigmastate.serialization.Serializer.{Consumed, Position}



//Proof tree

trait ProofTree extends Product

sealed trait UnprovenTree extends ProofTree {
  val proposition: SigmaBoolean

  val simulated: Boolean

  def real: Boolean = !simulated

  val challengeOpt: Option[Array[Byte]]

  def withChallenge(challenge: Array[Byte]): UnprovenTree

  def withSimulated(newSimulated: Boolean): UnprovenTree
}

sealed trait UnprovenLeaf extends UnprovenTree {
  val commitmentOpt: Option[FirstProverMessage[_]]
}

sealed trait UnprovenConjecture extends UnprovenTree {
  val childrenCommitments: Seq[FirstProverMessage[_]]
}

case class CAndUnproven(override val proposition: CAND,
                        override val childrenCommitments: Seq[FirstProverMessage[_]] = Seq(),
                        override val challengeOpt: Option[Array[Byte]] = None,
                        override val simulated: Boolean,
                        children: Seq[ProofTree]) extends UnprovenConjecture {
  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class COrUnproven(override val proposition: COR,
                       override val childrenCommitments: Seq[FirstProverMessage[_]] = Seq(),
                       override val challengeOpt: Option[Array[Byte]] = None,
                       override val simulated: Boolean,
                       children: Seq[ProofTree]) extends UnprovenConjecture {
  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class UnprovenSchnorr(override val proposition: ProveDlog,
                           override val commitmentOpt: Option[FirstDLogProverMessage],
                           randomnessOpt: Option[BigInteger],
                           override val challengeOpt: Option[Array[Byte]] = None,
                           override val simulated: Boolean) extends UnprovenLeaf {

  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class UnprovenDiffieHellmanTuple(override val proposition: ProveDiffieHellmanTuple,
                                      override val commitmentOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                      randomnessOpt: Option[BigInteger],
                                      override val challengeOpt: Option[Array[Byte]] = None,
                                      override val simulated: Boolean
                                     ) extends UnprovenLeaf {
  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}



object UnprovenTreeSerializer extends Serializer[ProofTree, ProofTree] with App {
  val internalNodePrefix = 0: Byte
  val leafPrefix = 1: Byte
  val andCode = 0: Byte
  val orCode = 1: Byte

  override def toBytes(tree: ProofTree): Array[Byte] = {

    def traverseNode(node: ProofTree): Array[Byte] = node match {
      case l: UnprovenLeaf =>
        val propBytes = ValueSerializer.serialize(l.proposition)
        val commitmentBytes = l.commitmentOpt.get.bytes
        leafPrefix +:
          ((Shorts.toByteArray(propBytes.length.toShort) ++ propBytes) ++
                  (Shorts.toByteArray(commitmentBytes.length.toShort) ++ commitmentBytes))

      case l: UncheckedLeaf[_] =>
        val propBytes = ValueSerializer.serialize(l.proposition)
        val commitmentBytes = l.commitmentOpt.get.bytes
        leafPrefix +:
          ((Shorts.toByteArray(propBytes.length.toShort) ++ propBytes) ++
            (Shorts.toByteArray(commitmentBytes.length.toShort) ++ commitmentBytes))

      case c: CAndUnproven =>
        val childrenCountBytes = Shorts.toByteArray(c.children.length.toShort)
         c.children.foldLeft(Array(internalNodePrefix, andCode) ++ childrenCountBytes){case (acc, ch) =>
           acc ++ traverseNode(ch)
         }

      case c: COrUnproven =>
        val childrenCountBytes = Shorts.toByteArray(c.children.length.toShort)
        c.children.foldLeft(Array(internalNodePrefix, orCode) ++ childrenCountBytes){case (acc, ch) =>
          acc ++ traverseNode(ch)
        }


      case c: CAndUncheckedNode =>
        val childrenCountBytes = Shorts.toByteArray(c.leafs.length.toShort)
        c.leafs.foldLeft(Array(internalNodePrefix, andCode) ++ childrenCountBytes){case (acc, ch) =>
          acc ++ traverseNode(ch)
        }

      case c: COrUncheckedNode =>
        val childrenCountBytes = Shorts.toByteArray(c.leafs.length.toShort)
        c.leafs.foldLeft(Array(internalNodePrefix, orCode) ++ childrenCountBytes){case (acc, ch) =>
          acc ++ traverseNode(ch)
        }
    }

    traverseNode(tree)
  }

  //not implemented, and should not be called
  override def parseBody(bytes: Array[Byte], pos: Position): (UnprovenTree, Consumed) = ???

}