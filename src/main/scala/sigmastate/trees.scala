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
case class OR(input: CollectionLeaf[SBoolean.type])
  extends Transformer[SCollection[SBoolean.type], SBoolean.type] with NotReadyValueBoolean {


  override def cost: Int = input match {
    case c: ConcreteCollection[SBoolean.type] =>
      c.value.map(_.cost).sum + c.value.length * Cost.AndPerChild + Cost.AndDeclaration
    case _ => Cost.AndDeclaration
  }

  //todo: reduce such boilerplate around AND/OR, folders, map etc
  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[SBoolean.type]].value.forall(_.evaluated)

  override def function(input: EvaluatedValue[SCollection[SBoolean.type]]): Value[SBoolean.type] = {
    @tailrec
    def iterChildren(children: Seq[Value[SBoolean.type]],
                     currentBuffer: mutable.Buffer[BooleanLeaf]): mutable.Buffer[BooleanLeaf] = {
      if (children.isEmpty) currentBuffer else children.head.asInstanceOf[BooleanLeaf] match {
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

case class AND(input: CollectionLeaf[SBoolean.type])
  extends Transformer[SCollection[SBoolean.type], SBoolean.type] with NotReadyValueBoolean {

  override def cost: Int = input match {
    case c: EvaluatedValue[SCollection[SBoolean.type]] =>
      c.value.map(_.cost).sum + c.value.length * Cost.AndPerChild + Cost.AndDeclaration
    case _ => Cost.AndDeclaration
  }

  //todo: reduce such boilerplate around AND/OR, folders, map etc
  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[SBoolean.type]].value.forall(_.evaluated)

  override def function(input: EvaluatedValue[SCollection[SBoolean.type]]) = {
    @tailrec
    def iterChildren(children: IndexedSeq[Value[SBoolean.type]],
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
  def apply(children: Seq[Value[SBoolean.type]]): AND = new AND(ConcreteCollection(children.toIndexedSeq))
  def apply(left: BooleanLeaf, right: BooleanLeaf): AND = apply(Seq(left, right))
}


class AvlTreeData(val startingDigest: ADDigest,
                  val keyLength: Int,
                  val valueLengthOpt: Option[Int],
                  val maxNumOperations: Option[Int] = None,
                  val maxDeletes: Option[Int] = None)

sealed trait SType {
  type WrappedType
}

case object SInt extends SType {override type WrappedType = Long}
case object SBoolean extends SType {override type WrappedType = Boolean}
case object SByteArray extends SType {override type WrappedType = Array[Byte]}
case object SProp extends SType {override type WrappedType = Array[Byte]}
case class  SCollection[ElemType <: SType]() extends SType {override type WrappedType = IndexedSeq[Value[ElemType]]}
case object SAvlTree extends SType {override type WrappedType = AvlTreeData}
case object SGroupElement extends SType {override type WrappedType = GroupElement}
case object SBox extends SType {override type WrappedType = BoxWithMetadata}

trait Value[S <: SType] extends StateTree {
  def evaluated: Boolean
}

trait EvaluatedValue[S <: SType] extends Value[S] {
  val value: S#WrappedType
  override lazy val evaluated = true
}

trait NotReadyValue[S <: SType] extends Value[S] {
  override lazy val evaluated = false
}

trait TaggedVariable[S <: SType] extends NotReadyValue[S] {
  val id: Byte
}


//todo: make PreservingNonNegativeIntLeaf for registers which value should be preserved?
sealed trait IntLeaf extends Value[SInt.type]

case class IntLeafConstant(value: Long) extends IntLeaf with EvaluatedValue[SInt.type] {
  override val cost = 1
}

trait NotReadyValueIntLeaf extends IntLeaf with NotReadyValue[SInt.type]{
  override lazy val cost: Int = 1
}

case object Height extends NotReadyValueIntLeaf {
  override lazy val cost: Int = Cost.HeightAccess
}

case object UnknownIntLeaf extends NotReadyValueIntLeaf

case class TaggedInt(override val id: Byte) extends TaggedVariable[SInt.type] with NotReadyValueIntLeaf




sealed trait ByteArrayLeaf extends Value[SByteArray.type]

case class ByteArrayLeafConstant(value: Array[Byte]) extends EvaluatedValue[SByteArray.type] with ByteArrayLeaf {

  override def cost: Int = (value.length / 1024.0).ceil.round.toInt * Cost.ByteArrayPerKilobyte

  override def equals(obj: scala.Any): Boolean = obj match {
    case ob: ByteArrayLeafConstant => value sameElements ob.value
    case _ => false
  }
}

object EmptyByteArray extends ByteArrayLeafConstant(Array.emptyByteArray)

trait NotReadyValueByteArray extends ByteArrayLeaf with NotReadyValue[SByteArray.type]{
  override lazy val cost: Int = Cost.ByteArrayDeclaration
}

case object UnknownByteArrayLeaf extends NotReadyValueByteArray


case class TaggedByteArray(override val id: Byte) extends TaggedVariable[SByteArray.type] with NotReadyValueByteArray


//todo: merge with ByteArrayLeaf?

sealed trait PropLeaf extends Value[SProp.type]

case class PropLeafConstant(value: Array[Byte]) extends EvaluatedValue[SProp.type] with PropLeaf {
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

trait NotReadyValueProp extends PropLeaf with NotReadyValue[SProp.type] {
  override def cost: Int = Cost.PropLeafDeclaration
}

case class TaggedPropLeaf(override val id: Byte) extends TaggedVariable[SProp.type] with NotReadyValueProp


sealed trait AvlTreeLeaf extends Value[SAvlTree.type]

case class AvlTreeLeafConstant(value: AvlTreeData) extends AvlTreeLeaf with EvaluatedValue[SAvlTree.type] {
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

trait NotReadyValueAvlTree extends AvlTreeLeaf with NotReadyValue[SAvlTree.type] {
  override val cost = 50
}

case class TaggedAvlTree(override val id: Byte) extends TaggedVariable[SAvlTree.type] with NotReadyValueAvlTree



sealed trait GroupElementLeaf extends Value[SGroupElement.type] with Proposition

case class GroupElementConstant(value: GroupElement) extends GroupElementLeaf with EvaluatedValue[SGroupElement.type] {
  override val cost = 10
}

trait NotReadyValueGroupElement extends GroupElementLeaf with NotReadyValue[SGroupElement.type] {
  override val cost = 10
}

case class TaggedGroupElement(override val id: Byte)
  extends TaggedVariable[SGroupElement.type] with NotReadyValueGroupElement


sealed trait BooleanLeaf extends Value[SBoolean.type]

sealed abstract class BooleanLeafConstant(val value: Boolean) extends BooleanLeaf with EvaluatedValue[SBoolean.type]

object BooleanLeafConstant {
  def fromBoolean(v: Boolean): BooleanLeafConstant = if (v) TrueLeaf else FalseLeaf
}

case object TrueLeaf extends BooleanLeafConstant(true) {
  override def cost: Int = Cost.ConstantNode
}

case object FalseLeaf extends BooleanLeafConstant(false) {
  override def cost: Int = Cost.ConstantNode
}

trait NotReadyValueBoolean extends BooleanLeaf with NotReadyValue[SBoolean.type] {
  override def cost: Int = 1
}

case class TaggedBoolean(override val id: Byte) extends TaggedVariable[SBoolean.type] with NotReadyValueBoolean

/**
  * For sigma statements
  */
trait FakeBoolean extends BooleanLeaf with NotReadyValue[SBoolean.type]{
  override lazy val evaluated = true
}

sealed trait BoxLeaf extends Value[SBox.type]

case class BoxLeafConstant(value: BoxWithMetadata) extends BoxLeaf with EvaluatedValue[SBox.type] {
  override def cost: Int = 10
}

trait NotReadyValueBoxLeaf extends BoxLeaf with NotReadyValue[SBox.type] {
  override def cost: Int = 10
}

case class TaggedBoxLeaf(override val id: Byte) extends TaggedVariable[SBox.type] with NotReadyValueBoxLeaf


case object Self extends NotReadyValueBoxLeaf {
  override def cost: Int = 10

  override type M = this.type
}


trait CollectionLeaf[V <: SType] extends Value[SCollection[V]]

case class ConcreteCollection[V <: SType](value: IndexedSeq[Value[V]])
  extends CollectionLeaf[V] with EvaluatedValue[SCollection[V]] {
  val cost = value.size
}

trait LazyCollection[V <: SType] extends CollectionLeaf[V] with NotReadyValue[SCollection[V]]


case object Inputs extends LazyCollection[SBox.type] {
  val cost = 1
}

case object Outputs extends LazyCollection[SBox.type] {
  val cost = 1
}

case class CalcBlake2b256(input: Value[SByteArray.type])
  extends Transformer[SByteArray.type, SByteArray.type] with NotReadyValueByteArray {

  override def function(bal: EvaluatedValue[SByteArray.type]): ByteArrayLeaf =
    ByteArrayLeafConstant(Blake2b256(bal.value))

  override lazy val cost: Int = input.cost + Cost.Blake256bDeclaration
}



/**
  * A tree node with left and right descendants
  */
sealed trait Triple[LIV <: SType, RIV <: SType, OV <: SType] extends NotReadyValue[OV] {

  val left: Value[LIV]
  val right: Value[RIV]

  override def cost: Int = left.cost + right.cost + Cost.TripleDeclaration
}


sealed trait TwoArgumentsOperation[LIV <: SType, RIV <: SType, OV <: SType] extends Triple[LIV, RIV, OV]

case class Plus[T1 <: SInt.type, T2 <: SInt.type](override val left: Value[T1], override val right: Value[T2])
  extends TwoArgumentsOperation[T1, T2, SInt.type] with NotReadyValueIntLeaf

case class Minus[T1 <: SInt.type, T2 <: SInt.type](override val left: Value[T1],
                 override val right: Value[T2]) extends TwoArgumentsOperation[T1, T2, SInt.type] with NotReadyValueIntLeaf

case class Xor(override val left: ByteArrayLeaf,
               override val right: ByteArrayLeaf)
  extends TwoArgumentsOperation[SByteArray.type, SByteArray.type, SByteArray.type] with NotReadyValueByteArray

case class Append(override val left: Value[SByteArray.type],
                  override val right: Value[SByteArray.type])
  extends TwoArgumentsOperation[SByteArray.type, SByteArray.type, SByteArray.type] with NotReadyValueByteArray

sealed trait Relation[LIV <: SType, RIV <: SType] extends Triple[LIV, RIV, SBoolean.type] with NotReadyValueBoolean

case class LT(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[SInt.type, SInt.type]

case class LE(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[SInt.type, SInt.type]

case class GT(override val left: Value[SInt.type],
              override val right: Value[SInt.type]) extends Relation[SInt.type, SInt.type]

case class GE(override val left: IntLeaf,
              override val right: IntLeaf) extends Relation[SInt.type, SInt.type]

case class EQ[T1 <: SType, T2 <: SType](override val left: Value[T1],
                                        override val right: Value[T2]) extends Relation[T1, T2]

case class NEQ[T1 <: SType, T2 <: SType](override val left: Value[T1],
                                         override val right: Value[T2]) extends Relation[T1, T2]


/**
  * A tree node with three descendants
  */
sealed trait Quadruple[IV1 <: SType, IV2 <: SType, IV3 <: SType, OV <: SType] extends NotReadyValue[OV] {

  val first: Value[IV1]
  val second: Value[IV2]
  val third: Value[IV3]

  override def cost: Int = first.cost + second.cost + third.cost + Cost.QuadrupleDeclaration
}

sealed trait Relation3[IV1 <: SType, IV2 <: SType, IV3 <: SType]
  extends Quadruple[IV1, IV2, IV3, SBoolean.type] with NotReadyValueBoolean


/**
  *
  * @param first - tree
  * @param second - key
  * @param third - proof
  */
case class IsMember(override val first: AvlTreeLeaf,
                    override val second: ByteArrayLeaf,
                    override val third: ByteArrayLeaf) extends Relation3[SAvlTree.type, SByteArray.type, SByteArray.type]

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