package sigmastate

import java.math.BigInteger

import com.google.common.primitives.Longs
import edu.biu.scapi.primitives.dlog.GroupElement
import scapi.sigma.DLogProtocol._
import scapi.sigma.rework.{FirstProverMessage, SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import scapi.sigma.{FirstDiffieHellmanTupleProverMessage, ProveDiffieHellmanTuple, SecondDiffieHellmanTupleProverMessage}
import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition
import scorex.crypto.hash.Blake2b256
import sigmastate.SigmaProposition.PropositionCode
import sigmastate.utxo.CostTable.Cost
import sigmastate.utxo.Transformer

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
}

object CAND {
  val Code: PropositionCode = 101: Byte
}

case class COR(sigmaTrees: Seq[SigmaTree]) extends SigmaTree {
  override def cost: Int = sigmaTrees.map(_.cost).sum + sigmaTrees.length * Cost.OrPerChild + Cost.OrDeclaration

  override val code: PropositionCode = COR.Code
}

object COR {
  val Code: PropositionCode = 101: Byte
}

trait SigmaProofOfKnowledgeTree[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP]]
  extends SigmaTree with ProofOfKnowledgeProposition[S] with SigmaProtocolCommonInput[SP]


//todo: reduce AND + OR boilerplate by introducing a Connective superclass for both
case class OR(input: Value[IndexedSeq[Value[Boolean, SBoolean.type]], SCollection[Boolean, SBoolean.type]])
  extends Transformer[IndexedSeq[Value[Boolean, SBoolean.type]], SCollection[Boolean, SBoolean.type], Boolean, SBoolean.type] with NotReadyValueBoolean {


  override def cost: Int = input match {
    case c: ConcreteCollection[Boolean, SBoolean.type] =>
      c.value.map(_.cost).sum + c.value.length * Cost.AndPerChild + Cost.AndDeclaration
    case _ => Cost.AndDeclaration
  }

  //todo: reduce such boilerplate around AND/OR, folders, map etc
  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[Boolean, SBoolean.type]].value.forall(_.evaluated)

  override def function(input: EvaluatedValue[IndexedSeq[Value[Boolean, SBoolean.type]], SCollection[Boolean, SBoolean.type]]): Value[Boolean, SBoolean.type] = {
    @tailrec
    def iterChildren(children: Seq[Value[Boolean, SBoolean.type]],
                     currentBuffer: mutable.Buffer[Value[Boolean, SBoolean.type]]): mutable.Buffer[Value[Boolean, SBoolean.type]] = {
      if (children.isEmpty) currentBuffer else children.head.asInstanceOf[Value[Boolean, SBoolean.type]] match {
        case TrueLeaf => mutable.Buffer(TrueLeaf)
        case FalseLeaf => iterChildren(children.tail, currentBuffer)
        case s: Value[Boolean, SBoolean.type] => iterChildren(children.tail, currentBuffer += s)
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
}


object OR {
  def apply(children: Seq[Value[Boolean, SBoolean.type]]): OR = OR(ConcreteCollection(children.toIndexedSeq))

  def apply(left: Value[Boolean, SBoolean.type], right: Value[Boolean, SBoolean.type]): OR = apply(Seq(left, right))

  def apply(arg1: Value[Boolean, SBoolean.type], arg2: Value[Boolean, SBoolean.type], arg3: Value[Boolean, SBoolean.type]): OR = apply(Seq(arg1, arg2, arg3))
}

case class AND(input: Value[IndexedSeq[Value[Boolean, SBoolean.type]], SCollection[Boolean, SBoolean.type]])
  extends Transformer[IndexedSeq[Value[Boolean, SBoolean.type]], SCollection[Boolean, SBoolean.type], Boolean, SBoolean.type] with NotReadyValueBoolean {

  override def cost: Int = input match {
    case c: EvaluatedValue[IndexedSeq[Value[Boolean, SBoolean.type]], SCollection[Boolean, SBoolean.type]] =>
      c.value.map(_.cost).sum + c.value.length * Cost.AndPerChild + Cost.AndDeclaration
    case _ => Cost.AndDeclaration
  }

  //todo: reduce such boilerplate around AND/OR, folders, map etc
  override def transformationReady: Boolean =
    input.evaluated && input.asInstanceOf[ConcreteCollection[Boolean, SBoolean.type]].value.forall(_.evaluated)

  override def function(input: EvaluatedValue[IndexedSeq[Value[Boolean, SBoolean.type]], SCollection[Boolean, SBoolean.type]]) = {
    @tailrec
    def iterChildren(children: IndexedSeq[Value[Boolean, SBoolean.type]],
                     currentBuffer: mutable.Buffer[Value[Boolean, SBoolean.type]]): mutable.Buffer[Value[Boolean, SBoolean.type]] = {
      if (children.isEmpty) currentBuffer else children.head match {
        case FalseLeaf => mutable.Buffer(FalseLeaf)
        case TrueLeaf => iterChildren(children.tail, currentBuffer)
        case s: Value[Boolean, SBoolean.type] => iterChildren(children.tail, currentBuffer += s)
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
}

object AND {
  def apply(children: Seq[Value[Boolean, SBoolean.type]]): AND = new AND(ConcreteCollection(children.toIndexedSeq))

  def apply(left: Value[Boolean, SBoolean.type], right: Value[Boolean, SBoolean.type]): AND = apply(Seq(left, right))

  def apply(arg1: Value[Boolean, SBoolean.type],
            arg2: Value[Boolean, SBoolean.type],
            arg3: Value[Boolean, SBoolean.type]): AND = apply(Seq(arg1, arg2, arg3))

  def apply(arg1: Value[Boolean, SBoolean.type],
            arg2: Value[Boolean, SBoolean.type],
            arg3: Value[Boolean, SBoolean.type],
            arg4: Value[Boolean, SBoolean.type]): AND = apply(Seq(arg1, arg2, arg3, arg4))
}


case class IntToByteArray(input: Value[Long, SInt.type])
  extends Transformer[Long, SInt.type, Array[Byte], SByteArray.type] with NotReadyValueByteArray {

  override def function(bal: EvaluatedValue[Long, SInt.type]): Value[Array[Byte], SByteArray.type] =
    ByteArrayConstant(Longs.toByteArray(bal.value))

  override lazy val cost: Int = input.cost + 1 //todo: externalize cost
}


case class ByteArrayToBigInt(input: Value[Array[Byte], SByteArray.type])
  extends Transformer[Array[Byte], SByteArray.type, BigInteger, SBigInt.type] with NotReadyValueBigInt {

  override def function(bal: EvaluatedValue[Array[Byte], SByteArray.type]): Value[BigInteger, SBigInt.type] =
    BigIntConstant(new BigInteger(1, bal.value))

  override lazy val cost: Int = input.cost + 1 //todo: externalize cost
}


case class CalcBlake2b256(input: Value[Array[Byte], SByteArray.type])
  extends Transformer[Array[Byte], SByteArray.type, Array[Byte], SByteArray.type] with NotReadyValueByteArray {

  override def function(bal: EvaluatedValue[Array[Byte], SByteArray.type]): Value[Array[Byte], SByteArray.type] =
    ByteArrayConstant(Blake2b256(bal.value))

  override lazy val cost: Int = input.cost + Cost.Blake256bDeclaration
}


/**
  * A tree node with left and right descendants
  */
sealed trait Triple[LIT, LIV <: SType[LIT], RIT, RIV <: SType[RIT], OT, OV <: SType[OT]] extends NotReadyValue[OT, OV] {

  val left: Value[LIT, LIV]
  val right: Value[RIT, RIV]

  override def cost: Int = left.cost + right.cost + Cost.TripleDeclaration
}


sealed trait TwoArgumentsOperation[LIT, LIV <: SType[LIT], RIT, RIV <: SType[RIT], OT, OV <: SType[OT]] extends Triple[LIT, LIV, RIT, RIV, OT, OV]

case class Plus(override val left: Value[Long, SInt.type], override val right: Value[Long, SInt.type])
  extends TwoArgumentsOperation[Long, SInt.type, Long, SInt.type, Long, SInt.type] with NotReadyValueInt

case class Minus(override val left: Value[Long, SInt.type], override val right: Value[Long, SInt.type])
  extends TwoArgumentsOperation[Long, SInt.type, Long, SInt.type, Long, SInt.type] with NotReadyValueInt

case class Xor(override val left: Value[Array[Byte], SByteArray.type],
               override val right: Value[Array[Byte], SByteArray.type])
  extends TwoArgumentsOperation[Array[Byte], SByteArray.type, Array[Byte], SByteArray.type, Array[Byte], SByteArray.type] with NotReadyValueByteArray

case class AppendBytes(override val left: Value[Array[Byte], SByteArray.type],
                       override val right: Value[Array[Byte], SByteArray.type])
  extends TwoArgumentsOperation[Array[Byte], SByteArray.type, Array[Byte], SByteArray.type, Array[Byte], SByteArray.type] with NotReadyValueByteArray


case class Exponentiate(override val left: Value[GroupElement, SGroupElement.type], override val right: Value[BigInteger, SBigInt.type])
  extends TwoArgumentsOperation[GroupElement, SGroupElement.type, BigInteger, SBigInt.type, GroupElement, SGroupElement.type]
    with NotReadyValueGroupElement

case class MultiplyGroup(override val left: Value[GroupElement, SGroupElement.type], override val right: Value[GroupElement, SGroupElement.type])
  extends TwoArgumentsOperation[GroupElement, SGroupElement.type, GroupElement, SGroupElement.type, GroupElement, SGroupElement.type]
    with NotReadyValueGroupElement


sealed trait Relation[LIT, LIV <: SType[LIT], RIT, RIV <: SType[RIT]] extends Triple[LIT, LIV, RIT, RIV, Boolean, SBoolean.type] with NotReadyValueBoolean

case class LT(override val left: Value[Long, SInt.type],
              override val right: Value[Long, SInt.type]) extends Relation[Long, SInt.type, Long, SInt.type]

case class LE(override val left: Value[Long, SInt.type],
              override val right: Value[Long, SInt.type]) extends Relation[Long, SInt.type, Long, SInt.type]

case class GT(override val left: Value[Long, SInt.type],
              override val right: Value[Long, SInt.type]) extends Relation[Long, SInt.type, Long, SInt.type]

case class GE(override val left: Value[Long, SInt.type],
              override val right: Value[Long, SInt.type]) extends Relation[Long, SInt.type, Long, SInt.type]

case class EQ[TT1, T1 <: SType[TT1], TT2, T2 <: SType[TT2]](override val left: Value[TT1, T1],
                                                            override val right: Value[TT2, T2]) extends Relation[TT1, T1, TT2, T2]

case class NEQ[TT1, T1 <: SType[TT1], TT2, T2 <: SType[TT2]](override val left: Value[TT1, T1],
                                                             override val right: Value[TT2, T2]) extends Relation[TT1, T1, TT2, T2]


/**
  * A tree node with three descendants
  */
sealed trait Quadruple[IT1, IV1 <: SType[IT1], IT2, IV2 <: SType[IT2], IT3, IV3 <: SType[IT3], OT, OV <: SType[OT]] extends NotReadyValue[OT, OV] {

  val first: Value[IT1, IV1]
  val second: Value[IT2, IV2]
  val third: Value[IT3, IV3]

  override def cost: Int = first.cost + second.cost + third.cost + Cost.QuadrupleDeclaration
}

sealed trait Relation3[IT1, IV1 <: SType[IT1], IT2, IV2 <: SType[IT2], IT3, IV3 <: SType[IT3]]
  extends Quadruple[IT1, IV1, IT2, IV2, IT3, IV3, Boolean, SBoolean.type] with NotReadyValueBoolean


/**
  * Predicate which checks whether a key is in a tree, by using a membership proof
  */
case class IsMember(tree: Value[AvlTreeData, SAvlTree.type],
                    key: Value[Array[Byte], SByteArray.type],
                    proof: Value[Array[Byte], SByteArray.type]) extends Relation3[AvlTreeData, SAvlTree.type, Array[Byte], SByteArray.type, Array[Byte], SByteArray.type] {
  override lazy val first = tree
  override lazy val second = key
  override lazy val third = proof
}


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
}

case class DiffieHellmanTupleUncheckedNode(override val proposition: ProveDiffieHellmanTuple,
                                           firstMessageOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                           challenge: Array[Byte],
                                           secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedSigmaTree[ProveDiffieHellmanTuple] {

  override val propCode: SigmaProposition.PropositionCode = ProveDiffieHellmanTuple.Code
}

case class CAndUncheckedNode(override val proposition: CAND,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             leafs: Seq[ProofTree])
  extends UncheckedConjecture[CAND] {
  override val propCode: PropositionCode = CAND.Code
}


case class COr2UncheckedNode(override val proposition: COR,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             children: Seq[ProofTree]) extends UncheckedConjecture[COR] {
  override val propCode: PropositionCode = COR.Code
}