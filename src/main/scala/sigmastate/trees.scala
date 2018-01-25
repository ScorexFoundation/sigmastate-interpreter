package sigmastate

import java.math.BigInteger

import com.google.common.primitives.Longs
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
case class OR(input: Value[SCollection[SBoolean.type]])
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
                     currentBuffer: mutable.Buffer[Value[SBoolean.type]]): mutable.Buffer[Value[SBoolean.type]] = {
      if (children.isEmpty) currentBuffer else children.head.asInstanceOf[Value[SBoolean.type]] match {
        case TrueLeaf => mutable.Buffer(TrueLeaf)
        case FalseLeaf => iterChildren(children.tail, currentBuffer)
        case s: Value[SBoolean.type] => iterChildren(children.tail, currentBuffer += s)
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
  def apply(children: Seq[Value[SBoolean.type]]): OR = OR(ConcreteCollection(children.toIndexedSeq))

  def apply(left: Value[SBoolean.type], right: Value[SBoolean.type]): OR = apply(Seq(left, right))

  def apply(arg1: Value[SBoolean.type], arg2: Value[SBoolean.type], arg3: Value[SBoolean.type]): OR = apply(Seq(arg1, arg2, arg3))
}

case class AND(input: Value[SCollection[SBoolean.type]])
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
                     currentBuffer: mutable.Buffer[Value[SBoolean.type]]): mutable.Buffer[Value[SBoolean.type]] = {
      if (children.isEmpty) currentBuffer else children.head match {
        case FalseLeaf => mutable.Buffer(FalseLeaf)
        case TrueLeaf => iterChildren(children.tail, currentBuffer)
        case s: Value[SBoolean.type] => iterChildren(children.tail, currentBuffer += s)
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
  def apply(children: Seq[Value[SBoolean.type]]): AND = new AND(ConcreteCollection(children.toIndexedSeq))

  def apply(left: Value[SBoolean.type], right: Value[SBoolean.type]): AND = apply(Seq(left, right))

  def apply(arg1: Value[SBoolean.type],
            arg2: Value[SBoolean.type],
            arg3: Value[SBoolean.type]): AND = apply(Seq(arg1, arg2, arg3))

  def apply(arg1: Value[SBoolean.type],
            arg2: Value[SBoolean.type],
            arg3: Value[SBoolean.type],
            arg4: Value[SBoolean.type]): AND = apply(Seq(arg1, arg2, arg3, arg4))
}


case class IntToByteArray(input: Value[SInt.type])
  extends Transformer[SInt.type, SByteArray.type] with NotReadyValueByteArray {

  override def function(bal: EvaluatedValue[SInt.type]): Value[SByteArray.type] =
    ByteArrayConstant(Longs.toByteArray(bal.value))

  override lazy val cost: Int = input.cost + 1 //todo: externalize cost
}


case class ByteArrayToBigInt(input: Value[SByteArray.type])
  extends Transformer[SByteArray.type, SBigInt.type] with NotReadyValueBigInt {

  override def function(bal: EvaluatedValue[SByteArray.type]): Value[SBigInt.type] =
    BigIntConstant(new BigInteger(1, bal.value))

  override lazy val cost: Int = input.cost + 1 //todo: externalize cost
}


case class CalcBlake2b256(input: Value[SByteArray.type])
  extends Transformer[SByteArray.type, SByteArray.type] with NotReadyValueByteArray {

  override def function(bal: EvaluatedValue[SByteArray.type]): Value[SByteArray.type] =
    ByteArrayConstant(Blake2b256(bal.value))

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

case class Plus(override val left: Value[SInt.type], override val right: Value[SInt.type])
  extends TwoArgumentsOperation[SInt.type, SInt.type, SInt.type] with NotReadyValueInt

case class Minus(override val left: Value[SInt.type], override val right: Value[SInt.type])
  extends TwoArgumentsOperation[SInt.type, SInt.type, SInt.type] with NotReadyValueInt

case class Xor(override val left: Value[SByteArray.type],
               override val right: Value[SByteArray.type])
  extends TwoArgumentsOperation[SByteArray.type, SByteArray.type, SByteArray.type] with NotReadyValueByteArray

case class AppendBytes(override val left: Value[SByteArray.type],
                       override val right: Value[SByteArray.type])
  extends TwoArgumentsOperation[SByteArray.type, SByteArray.type, SByteArray.type] with NotReadyValueByteArray


case class Exponentiate(override val left: Value[SGroupElement.type], override val right: Value[SBigInt.type])
  extends TwoArgumentsOperation[SGroupElement.type, SBigInt.type, SGroupElement.type]
    with NotReadyValueGroupElement

case class MultiplyGroup(override val left: Value[SGroupElement.type], override val right: Value[SGroupElement.type])
  extends TwoArgumentsOperation[SGroupElement.type, SGroupElement.type, SGroupElement.type]
    with NotReadyValueGroupElement


sealed trait Relation[LIV <: SType, RIV <: SType] extends Triple[LIV, RIV, SBoolean.type] with NotReadyValueBoolean

case class LT(override val left: Value[SInt.type],
              override val right: Value[SInt.type]) extends Relation[SInt.type, SInt.type]

case class LE(override val left: Value[SInt.type],
              override val right: Value[SInt.type]) extends Relation[SInt.type, SInt.type]

case class GT(override val left: Value[SInt.type],
              override val right: Value[SInt.type]) extends Relation[SInt.type, SInt.type]

case class GE(override val left: Value[SInt.type],
              override val right: Value[SInt.type]) extends Relation[SInt.type, SInt.type]

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
  * Predicate which checks whether a key is in a tree, by using a membership proof
  */
case class IsMember(tree: Value[SAvlTree.type],
                    key: Value[SByteArray.type],
                    proof: Value[SByteArray.type]) extends Relation3[SAvlTree.type, SByteArray.type, SByteArray.type] {
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