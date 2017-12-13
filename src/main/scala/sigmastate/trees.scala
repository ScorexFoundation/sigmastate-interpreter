package sigmastate

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.GroupElement
import scapi.sigma.DLogProtocol._
import scapi.sigma.{FirstDiffieHellmanTupleProverMessage, ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import scapi.sigma.rework.{FirstProverMessage, SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import scorex.core.serialization.{BytesSerializable, Serializer}
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition}
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.authds.avltree.batch.BatchAVLVerifier
import scorex.crypto.hash.{Blake2b256, Blake2b256Unsafe, Digest32}
import sigmastate.SigmaProposition.PropositionCode
import sigmastate.utxo.{BoxWithMetadata, Transformer}
import sigmastate.utxo.CostTable.Cost

import scala.annotation.tailrec
import scala.collection.mutable


sealed trait SigmaStateTree extends Product with SigmaStateProposition {
  def cost: Int
}

trait StateTree extends SigmaStateTree with StateProposition

trait SigmaTree extends SigmaStateTree with SigmaProposition with FakeBoolean

case class CAND(sigmaTrees: Seq[SigmaTree]) extends SigmaTree {
  override def cost: Int = sigmaTrees.map(_.cost).sum + sigmaTrees.length * Cost.AndPerChild + Cost.AndDeclaration

  override val code: PropositionCode = CAND.Code
  override type M = this.type
}

object CAND {
  val Code: PropositionCode = 101: Byte
}

case class COR(sigmaTrees: Seq[SigmaTree]) extends SigmaTree {
  override def cost: Int = sigmaTrees.map(_.cost).sum + sigmaTrees.length * Cost.OrPerChild + Cost.OrDeclaration

  override val code: PropositionCode = COR.Code
  override type M = this.type
}

object COR {
  val Code: PropositionCode = 101: Byte
}

trait SigmaProofOfKnowledgeTree[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP]]
  extends SigmaTree with ProofOfKnowledgeProposition[S] with SigmaProtocolCommonInput[SP]


//todo: reduce AND + OR boilerplate by introducing a Connective superclass for both
case class OR(input: CollectionLeaf[BooleanLeaf])
  extends Transformer[CollectionLeaf[BooleanLeaf], BooleanLeaf] with NotReadyValueBoolean {


  override def cost: Int = input match {
    case c: EvaluatedValue[CollectionLeaf[BooleanLeaf]] =>
      c.value.map(_.cost).sum + c.value.length * Cost.AndPerChild + Cost.AndDeclaration
    case _ => Cost.AndDeclaration
  }

  //todo: reduce such boilerplate around AND/OR, folders, map etc
  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[BooleanLeaf]].value.forall(_.evaluated)

  override def function(input: EvaluatedValue[CollectionLeaf[BooleanLeaf]]) = {
    @tailrec
    def iterChildren(children: Seq[BooleanLeaf],
                     currentBuffer: mutable.Buffer[BooleanLeaf]): mutable.Buffer[BooleanLeaf] = {
      if (children.isEmpty) currentBuffer else children.head match {
        case TrueLeaf => mutable.Buffer(TrueLeaf)
        case FalseLeaf => iterChildren(children.tail, currentBuffer)
        case s: BooleanLeaf => iterChildren(children.tail, currentBuffer += s)
      }
    }

    val reduced = iterChildren(input.value, mutable.Buffer())

    reduced.size match {
      case i: Int if i == 0 => FalseLeaf
      case i: Int if i == 1 => reduced.head
      case _ =>
        if (reduced.forall(_.isInstanceOf[SigmaTree])) COR(reduced.map(_.asInstanceOf[SigmaTree]))
        else OR(reduced)
    }
  }

  override type M = this.type
}


object OR {
  def apply(children: Seq[BooleanLeaf]): OR = OR(ConcreteCollection(children.toIndexedSeq))

  def apply(left: BooleanLeaf, right: BooleanLeaf): OR = apply(Seq(left, right))

  def apply(arg1: BooleanLeaf, arg2: BooleanLeaf, arg3: BooleanLeaf): OR = apply(Seq(arg1, arg2, arg3))
}

case class AND(input: CollectionLeaf[BooleanLeaf])
  extends Transformer[CollectionLeaf[BooleanLeaf], BooleanLeaf] with NotReadyValueBoolean {

  override def cost: Int = input match {
    case c: EvaluatedValue[CollectionLeaf[BooleanLeaf]] =>
      c.value.map(_.cost).sum + c.value.length * Cost.AndPerChild + Cost.AndDeclaration
    case _ => Cost.AndDeclaration
  }

  //todo: reduce such boilerplate around AND/OR, folders, map etc
  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[BooleanLeaf]].value.forall(_.evaluated)

  override def function(input: EvaluatedValue[CollectionLeaf[BooleanLeaf]]) = {
    @tailrec
    def iterChildren(children: Seq[BooleanLeaf],
                     currentBuffer: mutable.Buffer[BooleanLeaf]): mutable.Buffer[BooleanLeaf] = {
      if (children.isEmpty) currentBuffer else children.head match {
        case FalseLeaf => mutable.Buffer(FalseLeaf)
        case TrueLeaf => iterChildren(children.tail, currentBuffer)
        case s: BooleanLeaf => iterChildren(children.tail, currentBuffer += s)
      }
    }

    val reduced = iterChildren(input.value, mutable.Buffer())

    reduced.size match {
      case i: Int if i == 0 => TrueLeaf
      case i: Int if i == 1 => reduced.head
      case _ =>
        if (reduced.forall(_.isInstanceOf[SigmaTree]))
          CAND(reduced.map(_.asInstanceOf[SigmaTree]))
        else AND(reduced)
    }
  }

  override type M = this.type
}

object AND {
  def apply(children: Seq[BooleanLeaf]): AND = new AND(ConcreteCollection(children.toIndexedSeq))
  def apply(left: BooleanLeaf, right: BooleanLeaf): AND = apply(Seq(left, right))
}


trait Value extends StateTree {
  type WrappedValue

  def evaluated: Boolean = ???
}

trait EvaluatedValue[V <: Value] extends Value {
  self: V =>

  val value: V#WrappedValue

  override lazy val evaluated = true
}

trait NotReadyValue[V <: Value] extends Value {
   self: V =>
  override lazy val evaluated = false
}

trait TaggedVariable[V <: Value] extends NotReadyValue[V] {
  self: V =>
  val id: Byte
}


//todo: make PreservingNonNegativeIntLeaf for registers which value should be preserved?
sealed trait IntLeaf extends Value {
  override type WrappedValue = Long
}

case class IntLeafConstant(value: Long) extends IntLeaf with EvaluatedValue[IntLeaf] {
  override val cost = 1
}

trait NotReadyValueIntLeaf extends IntLeaf with NotReadyValue[IntLeaf]{
  override lazy val cost: Int = 1
}

case object Height extends NotReadyValueIntLeaf {
  override lazy val cost: Int = Cost.HeightAccess
}

case object UnknownIntLeaf extends NotReadyValueIntLeaf

case class TaggedInt(override val id: Byte) extends TaggedVariable[IntLeaf] with NotReadyValueIntLeaf


sealed trait ByteArrayLeaf extends Value {
  override type WrappedValue = Array[Byte]
}

case class ByteArrayLeafConstant(value: Array[Byte]) extends EvaluatedValue[ByteArrayLeaf] with ByteArrayLeaf {

  override def cost: Int = (value.length / 1024.0).ceil.round.toInt * Cost.ByteArrayPerKilobyte

  override def equals(obj: scala.Any): Boolean = obj match {
    case ob: ByteArrayLeafConstant => value sameElements ob.value
    case _ => false
  }
}

object EmptyByteArray extends ByteArrayLeafConstant(Array.emptyByteArray)

trait NotReadyValueByteArray extends ByteArrayLeaf with NotReadyValue[ByteArrayLeaf]{
  override lazy val cost: Int = Cost.ByteArrayDeclaration
}

case object UnknownByteArrayLeaf extends NotReadyValueByteArray


case class TaggedByteArray(override val id: Byte) extends TaggedVariable[ByteArrayLeaf] with NotReadyValueByteArray


//todo: merge with ByteArrayLeaf?

sealed trait PropLeaf extends Value {
  override type WrappedValue = Array[Byte]
}

case class PropLeafConstant(value: Array[Byte]) extends EvaluatedValue[PropLeaf] with PropLeaf {
  override def cost: Int = value.length + Cost.PropLeafDeclaration

  override def equals(obj: scala.Any): Boolean = obj match {
    case ob: PropLeafConstant => value sameElements ob.value
    case _ => false
  }
}

object PropLeafConstant {
  def apply(value: BoxWithMetadata): PropLeafConstant = new PropLeafConstant(value.box.propositionBytes)

  def apply(value: SigmaStateTree): PropLeafConstant = new PropLeafConstant(value.toString.getBytes)
}

trait NotReadyValueProp extends PropLeaf with NotReadyValue[PropLeaf] {
  override def cost: Int = Cost.PropLeafDeclaration
}

case class TaggedPropLeaf(override val id: Byte) extends TaggedVariable[PropLeaf] with NotReadyValueProp


class AvlTreeData(val startingDigest: ADDigest,
                  val keyLength: Int,
                  val valueLengthOpt: Option[Int],
                  val maxNumOperations: Option[Int] = None,
                  val maxDeletes: Option[Int] = None)

sealed trait AvlTreeLeaf extends Value {
  override type WrappedValue = AvlTreeData
}

case class AvlTreeLeafConstant(value: AvlTreeData) extends AvlTreeLeaf with EvaluatedValue[AvlTreeLeaf] {
  override val cost = 50

  def createVerifier(proof: SerializedAdProof) =
    new BatchAVLVerifier[Digest32, Blake2b256Unsafe](
      value.startingDigest,
      proof,
      value.keyLength,
      value.valueLengthOpt,
      value.maxNumOperations,
      value.maxDeletes)
}

trait NotReadyValueAvlTree extends AvlTreeLeaf with NotReadyValue[AvlTreeLeaf] {
  override val cost = 50
}

case class TaggedAvlTree(override val id: Byte) extends TaggedVariable[AvlTreeLeaf] with NotReadyValueAvlTree



sealed trait GroupElementLeaf extends Value with Proposition {
  override type WrappedValue = GroupElement
}

case class GroupElementConstant(value: GroupElement) extends GroupElementLeaf with EvaluatedValue[GroupElementLeaf] {
  override val cost = 10
}

trait NotReadyValueGroupElement extends GroupElementLeaf with NotReadyValue[GroupElementLeaf] {
  override val cost = 10
}

case class TaggedGroupElement(override val id: Byte)
  extends TaggedVariable[GroupElementLeaf] with NotReadyValueGroupElement



sealed trait BooleanLeaf extends Value {
  override type WrappedValue = Boolean
}

sealed abstract class BooleanLeafConstant(val value: Boolean) extends BooleanLeaf with EvaluatedValue[BooleanLeaf]

object BooleanLeafConstant {
  def fromBoolean(v: Boolean): BooleanLeafConstant = if (v) TrueLeaf else FalseLeaf
}

case object TrueLeaf extends BooleanLeafConstant(true) {
  override def cost: Int = Cost.ConstantNode
}

case object FalseLeaf extends BooleanLeafConstant(false) {
  override def cost: Int = Cost.ConstantNode
}

trait NotReadyValueBoolean extends BooleanLeaf with NotReadyValue[BooleanLeaf] {
  override def cost: Int = 1
}

case class TaggedBoolean(override val id: Byte) extends TaggedVariable[BooleanLeaf] with NotReadyValueBoolean

/**
  * For sigma statements
  */
trait FakeBoolean extends BooleanLeaf with NotReadyValue[BooleanLeaf]{
  override lazy val evaluated = true
}

sealed trait BoxLeaf extends Value {
  override type WrappedValue = BoxWithMetadata
}

case class BoxLeafConstant(value: BoxWithMetadata) extends BoxLeaf with EvaluatedValue[BoxLeaf] {
  override def cost: Int = 10
}

trait NotReadyValueBoxLeaf extends BoxLeaf with NotReadyValue[BoxLeaf] {
  override def cost: Int = 10
}

case class TaggedBoxLeaf(override val id: Byte) extends TaggedVariable[BoxLeaf] with NotReadyValueBoxLeaf


case object Self extends NotReadyValueBoxLeaf {
  override def cost: Int = 10

  override type M = this.type
}


trait CollectionLeaf[V <: Value] extends Value {
  override type WrappedValue = IndexedSeq[V]
}

case class ConcreteCollection[V <: Value](value: IndexedSeq[V])
  extends CollectionLeaf[V] with EvaluatedValue[CollectionLeaf[V]] {
  val cost = value.size
}

trait LazyCollection[V <: Value] extends CollectionLeaf[V] with NotReadyValue[CollectionLeaf[V]]


case object Inputs extends LazyCollection[BoxLeaf] {
  val cost = 1
}

case object Outputs extends LazyCollection[BoxLeaf] {
  val cost = 1
}

case class CalcBlake2b256(input: ByteArrayLeaf)
  extends Transformer[ByteArrayLeaf, ByteArrayLeaf] with NotReadyValueByteArray {

  override def function(bal: EvaluatedValue[ByteArrayLeaf]): ByteArrayLeaf =
    ByteArrayLeafConstant(Blake2b256(bal.value))

  override lazy val cost: Int = input.cost + Cost.Blake256bDeclaration
}



/**
  * A tree node with left and right descendants
  */
sealed trait Triple[LIV <: Value, RIV <: Value, OV <: Value] extends NotReadyValue[OV] {
  self: OV =>

  val left: LIV
  val right: RIV

  override def cost: Int = left.cost + right.cost + Cost.TripleDeclaration
}


sealed trait TwoArgumentsOperation[LIV <: Value, RIV <: Value, OV <: Value] extends Triple[LIV, RIV, OV] {
  self: OV =>
}

case class Plus(override val left: IntLeaf,
                override val right: IntLeaf)
  extends TwoArgumentsOperation[IntLeaf, IntLeaf, IntLeaf] with NotReadyValueIntLeaf

case class Minus(override val left: IntLeaf,
                 override val right: IntLeaf)
  extends TwoArgumentsOperation[IntLeaf, IntLeaf, IntLeaf] with NotReadyValueIntLeaf

case class Xor(override val left: ByteArrayLeaf,
               override val right: ByteArrayLeaf)
  extends TwoArgumentsOperation[ByteArrayLeaf, ByteArrayLeaf, ByteArrayLeaf] with NotReadyValueByteArray

case class Append(override val left: ByteArrayLeaf,
                  override val right: ByteArrayLeaf)
  extends TwoArgumentsOperation[ByteArrayLeaf, ByteArrayLeaf, ByteArrayLeaf] with NotReadyValueByteArray

sealed trait Relation[LIV <: Value, RIV <: Value] extends Triple[LIV, RIV, BooleanLeaf] with NotReadyValueBoolean

case class LT(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[IntLeaf, IntLeaf]

case class LE(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[IntLeaf, IntLeaf]

case class GT(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[IntLeaf, IntLeaf]

case class GE(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[IntLeaf, IntLeaf]

case class EQ[V <: Value](override val left: V,
                          override val right: V) extends Relation[V, V]

case class NEQ[V <: Value](override val left: V, override val right: V) extends Relation[V, V]


/**
  * A tree node with three descendants
  */
sealed trait Quadruple[IV1 <: Value, IV2 <: Value, IV3 <: Value, OV <: Value] extends NotReadyValue[OV] {
  self: OV =>

  val first: IV1
  val second: IV2
  val third: IV3

  override def cost: Int = first.cost + second.cost + third.cost + Cost.QuadrupleDeclaration
}

sealed trait Relation3[IV1 <: Value, IV2 <: Value, IV3 <: Value]
  extends Quadruple[IV1, IV2, IV3, BooleanLeaf] with NotReadyValueBoolean


/**
  *
  * @param first - tree
  * @param second - key
  * @param third - proof
  */
case class IsMember(override val first: AvlTreeLeaf,
                    override val second: ByteArrayLeaf,
                    override val third: ByteArrayLeaf) extends Relation3[AvlTreeLeaf, ByteArrayLeaf, ByteArrayLeaf]

//Proof tree

trait ProofTree extends Product

sealed trait UnprovenTree extends ProofTree {
  val proposition: SigmaTree

  val simulated: Boolean

  lazy val real: Boolean = !simulated

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

case class SchnorrUnproven(override val proposition: ProveDlog,
                           override val commitmentOpt: Option[FirstDLogProverMessage],
                           randomnessOpt: Option[BigInteger],
                           override val challengeOpt: Option[Array[Byte]] = None,
                           override val simulated: Boolean) extends UnprovenLeaf {

  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

case class DiffieHellmanTupleUnproven(override val proposition: ProveDiffieHellmanTuple,
                                      override val commitmentOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                      randomnessOpt: Option[BigInteger],
                                      override val challengeOpt: Option[Array[Byte]] = None,
                                      override val simulated: Boolean
                                     ) extends UnprovenLeaf {
  override def withChallenge(challenge: Array[Byte]) = this.copy(challengeOpt = Some(challenge))

  override def withSimulated(newSimulated: Boolean) = this.copy(simulated = newSimulated)
}

sealed trait UncheckedTree extends ProofTree

case object NoProof extends UncheckedTree

sealed trait UncheckedSigmaTree[ST <: SigmaTree] extends UncheckedTree with BytesSerializable {
  val proposition: ST
  val propCode: SigmaProposition.PropositionCode
}

trait UncheckedConjecture[ST <: SigmaTree] extends UncheckedSigmaTree[ST] {
  val challengeOpt: Option[Array[Byte]]
  val commitments: Seq[FirstProverMessage[_]]
}


case class SchnorrNode(override val proposition: ProveDlog,
                       firstMessageOpt: Option[FirstDLogProverMessage],
                       challenge: Array[Byte],
                       secondMessage: SecondDLogProverMessage)
  extends UncheckedSigmaTree[ProveDlog] {

  override val propCode: SigmaProposition.PropositionCode = ProveDlog.Code
  override type M = this.type

  override def serializer: Serializer[M] = ???
}

case class DiffieHellmanTupleUncheckedNode(override val proposition: ProveDiffieHellmanTuple,
                                           firstMessageOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                           challenge: Array[Byte],
                                           secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedSigmaTree[ProveDiffieHellmanTuple] {

  override val propCode: SigmaProposition.PropositionCode = ProveDiffieHellmanTuple.Code
  override type M = DiffieHellmanTupleUncheckedNode

  override def serializer: Serializer[M] = ???
}

case class CAndUncheckedNode(override val proposition: CAND,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             leafs: Seq[ProofTree])
  extends UncheckedConjecture[CAND] {

  override val propCode: PropositionCode = CAND.Code
  override type M = CAndUncheckedNode

  override def serializer: Serializer[M] = ???
}


case class COr2UncheckedNode(override val proposition: COR,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             children: Seq[ProofTree]) extends UncheckedConjecture[COR] {

  override val propCode: PropositionCode = COR.Code

  override type M = COr2UncheckedNode

  override def serializer: Serializer[M] = ???
}