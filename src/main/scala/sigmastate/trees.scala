package sigmastate

import edu.biu.scapi.primitives.dlog.ECElementSendableData
import scapi.sigma.rework.DLogProtocol._
import scapi.sigma.rework.{Challenge, SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import sigmastate.SigmaProposition.PropositionCode
import sigmastate.utils.Helpers


sealed trait SigmaStateTree extends Product with SigmaStateProposition

trait StateTree extends SigmaStateTree with StateProposition

trait SigmaTree extends SigmaStateTree with SigmaProposition

case class CAND(sigmaTrees: Seq[SigmaTree]) extends SigmaTree {
  override val code: PropositionCode = CAND.Code
  override type M = this.type
}

object CAND {
  val Code: PropositionCode = 101: Byte
}

case class COR(sigmaTrees: Seq[SigmaTree]) extends SigmaTree {
  override val code: PropositionCode = COR.Code
  override type M = this.type
}

object COR {
  val Code: PropositionCode = 101: Byte
}

trait SigmaProofOfKnowledgeTree[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP]]
  extends SigmaTree with ProofOfKnowledgeProposition[S] with SigmaProtocolCommonInput[SP]


case class OR(children: Seq[SigmaStateTree]) extends SigmaStateTree

object OR {
  def apply(left: SigmaStateTree, right: SigmaStateTree): OR = apply(Seq(left, right))
}


case class AND(children: Seq[SigmaStateTree]) extends SigmaStateTree

object AND {
  def apply(left: SigmaStateTree, right: SigmaStateTree): AND = apply(Seq(left, right))
}


trait Value extends StateTree

case class IntLeaf(value: Long) extends Value {
  require(value >= 0)
}

case class ByteArrayLeaf(value: Array[Byte]) extends Value {
  override def equals(obj: scala.Any): Boolean = obj match {
    case ob: ByteArrayLeaf => value sameElements ob.value
    case _ => false
  }
}

case class PropLeaf(value: SigmaStateTree) extends Value

sealed abstract class BooleanConstantNode(val value: Boolean) extends Value

object BooleanConstantNode {
  def fromBoolean(v: Boolean): BooleanConstantNode = if (v) TrueConstantNode else FalseConstantNode
}

case object TrueConstantNode extends BooleanConstantNode(true)

case object FalseConstantNode extends BooleanConstantNode(false)


trait Variable[V <: Value] extends Value

case object Height extends Variable[IntLeaf]

trait CustomVariable[V <: Value] extends Variable[Value] {
  val id: Int
}

case class CustomByteArray(override val id: Int) extends CustomVariable[ByteArrayLeaf]

sealed trait OneArgumentOperation extends StateTree {
  val operand: SigmaStateTree
}

case class CalcBlake2b256(operand: SigmaStateTree) extends OneArgumentOperation

/**
  * A tree node with left and right descendants
  */
sealed trait Triple extends StateTree {
  val left: SigmaStateTree
  val right: SigmaStateTree

  //todo: define via F-Bounded polymorphism?
  def replaceLeft(newLeft: SigmaStateTree): Relation

  def replaceRight(newRight: SigmaStateTree): Relation
}


sealed trait TwoArgumentsOperation extends Triple

case class Plus(override val left: SigmaStateTree,
                override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): Plus = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): Plus = copy(right = newRight)
}

case class Minus(override val left: SigmaStateTree,
                 override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): Minus = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): Minus = copy(right = newRight)
}

case class Xor(override val left: SigmaStateTree,
               override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): Xor = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): Xor = copy(right = newRight)
}

case class Append(override val left: SigmaStateTree,
                  override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): Append = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): Append = copy(right = newRight)
}

sealed trait Relation extends Triple

case class LT(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): LT = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): LT = copy(right = newRight)
}

case class LE(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): LE = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): LE = copy(right = newRight)
}

case class GT(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): GT = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): GT = copy(right = newRight)
}

case class GE(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): GE = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): GE = copy(right = newRight)
}

case class EQ(override val left: SigmaStateTree,
              override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): EQ = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): EQ = copy(right = newRight)
}

case class NEQ(override val left: SigmaStateTree,
               override val right: SigmaStateTree) extends Relation {
  def replaceLeft(newLeft: SigmaStateTree): NEQ = copy(left = newLeft)

  def replaceRight(newRight: SigmaStateTree): NEQ = copy(right = newRight)
}


//Proof tree

trait ProofTree extends Product

sealed trait UnprovenTree extends ProofTree {
  val proposition: SigmaTree

  val challengeOpt: Option[Array[Byte]]

  def setChallenge(challenge: Array[Byte]): UnprovenTree
}

sealed trait UnprovenLeaf extends UnprovenTree {
  val simulated: Boolean
}

case class CAndUnproven(override val proposition: CAND,
                        override val challengeOpt: Option[Array[Byte]] = None, children: Seq[UnprovenTree]) extends UnprovenTree {
  override def setChallenge(challenge: Array[Byte]) = CAndUnproven(proposition, Some(challenge), children)
}

case class COrUnproven(override val proposition: COR,
                       override val challengeOpt: Option[Array[Byte]] = None, children: Seq[UnprovenTree]) extends UnprovenTree {
  override def setChallenge(challenge: Array[Byte]) = COrUnproven(proposition, Some(challenge), children)
}


case class SchnorrUnproven(override val challengeOpt: Option[Array[Byte]] = None,
                           override val simulated: Boolean,
                           override val proposition: DLogNode) extends UnprovenLeaf {
  override def setChallenge(challenge: Array[Byte]) = SchnorrUnproven(Some(challenge), simulated, proposition)
}

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

abstract class UncheckedSigmaTree[ST <: SigmaTree](val proposition: ST, val challenge: Array[Byte])
  extends Proof[ST] with UncheckedTree

case class SchnorrNode(override val proposition: DLogNode,
                       override val challenge: Array[Byte], signature: Array[Byte]) extends
  UncheckedSigmaTree[DLogNode](proposition, challenge)
  with ProofOfKnowledge[DLogSigmaProtocol, DLogNode] {

  override def verify(): Boolean = {
    //signature is g^r as a pair of points, and z
    val (grx, gry, zb) = EcPointFunctions.decodeBigIntTriple(signature).get
    val gr = new ECElementSendableData(grx, gry)

    //h = g^w is a pubkey
    val x: DLogNode = proposition

    val a: FirstDLogProverMessage = FirstDLogProverMessage(gr)

    val z: SecondDLogProverMessage = SecondDLogProverMessage(zb)

    val sigmaTranscript = DLogTranscript(x, a, Challenge(challenge), z)
    sigmaTranscript.accepted
  }

  override val propCode: SigmaProposition.PropositionCode = DLogNode.Code
  override type M = this.type

  override def serializer: Serializer[M] = ???
}

case class CAndUncheckedNode(override val proposition: CAND, override val challenge: Array[Byte], leafs: Seq[UncheckedTree])
  extends UncheckedSigmaTree[CAND](proposition, challenge) {

  /**
    * To verify an AND sigma protocol, we check sub-protocols with the same challenge
    *
    * @return whether a proof is valid or not
    */
  override def verify(): Boolean =
    leafs.zip(proposition.sigmaTrees).forall { case (proof, prop) =>
      proof match {
        case NoProof => true
        case ut: UncheckedSigmaTree[_] => ut.challenge.sameElements(this.challenge) && ut.verify()
      }
    }

  override val propCode: PropositionCode = CAND.Code
  override type M = CAndUncheckedNode

  override def serializer: Serializer[M] = ???
}


case class COrUncheckedNode(override val proposition: COR, override val challenge: Array[Byte], leafs: Seq[UncheckedTree])
  extends UncheckedSigmaTree(proposition, challenge) {

  override def verify(): Boolean = {
    lazy val challenges = leafs.flatMap {
      _ match {
        case NoProof => None
        case ut: UncheckedSigmaTree[_] => Some(ut.challenge)
      }
    }

    lazy val challengeCheck = challenges.reduce {
      {
        case (c1: Array[Byte], c2: Array[Byte]) =>
          Helpers.xor(c1, c2)
      }: ((Array[Byte], Array[Byte]) => Array[Byte])
    }.sameElements(challenge)

    lazy val subprotocolsCheck = leafs.forall {
      case NoProof => true
      case ut: UncheckedSigmaTree[_] => ut.verify()
    }

    challengeCheck && subprotocolsCheck
  }

  override val propCode: PropositionCode = COR.Code

  override type M = COrUncheckedNode

  override def serializer: Serializer[M] = ???
}