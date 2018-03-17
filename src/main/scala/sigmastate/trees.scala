package sigmastate

import java.math.BigInteger

import com.google.common.primitives.Longs
import scapi.sigma.DLogProtocol._
import scapi.sigma._
import scapi.sigma.{SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import scorex.crypto.hash.Blake2b256
import sigmastate.serialization.ValueSerializer
import sigmastate.serialization.ValueSerializer.OpCode
import sigmastate.utxo.Transformer
import sigmastate.utxo.CostTable.Cost
import sigmastate.Values._
import scala.annotation.tailrec
import scala.collection.mutable


case class CAND(sigmaBooleans: Seq[SigmaBoolean]) extends SigmaBoolean {
  override def cost: Int = sigmaBooleans.map(_.cost).sum + sigmaBooleans.length * Cost.AndPerChild + Cost.AndDeclaration
}

case class COR(sigmaBooleans: Seq[SigmaBoolean]) extends SigmaBoolean {
  override def cost: Int = sigmaBooleans.map(_.cost).sum + sigmaBooleans.length * Cost.OrPerChild + Cost.OrDeclaration
}

trait SigmaProofOfKnowledgeTree[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP, _]]
  extends SigmaBoolean with SigmaProtocolCommonInput[SP]


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
        if (reduced.forall(_.isInstanceOf[SigmaBoolean])) COR(reduced.map(_.asInstanceOf[SigmaBoolean]))
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
        if (reduced.forall(_.isInstanceOf[SigmaBoolean]))
          CAND(reduced.map(_.asInstanceOf[SigmaBoolean]))
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

case class Not(input: Value[SBoolean.type])
  extends Transformer[SBoolean.type, SBoolean.type] with NotReadyValueBoolean {

  override def function(bal: EvaluatedValue[SBoolean.type]): Value[SBoolean.type] =
    BooleanConstant.fromBoolean(!bal.value)

  override lazy val cost: Int = input.cost + 1 //todo: externalize cost
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
    with NotReadyValueGroupElement {

  override val cost = 5000
}

case class MultiplyGroup(override val left: Value[SGroupElement.type], override val right: Value[SGroupElement.type])
  extends TwoArgumentsOperation[SGroupElement.type, SGroupElement.type, SGroupElement.type]
    with NotReadyValueGroupElement{

  override val cost = 50
}


sealed trait Relation[LIV <: SType, RIV <: SType] extends Triple[LIV, RIV, SBoolean.type] with NotReadyValueBoolean


case class LT(override val left: Value[SInt.type],
              override val right: Value[SInt.type]) extends Relation[SInt.type, SInt.type]{
  override val opCode: OpCode = ValueSerializer.LtCode
}

case class LE(override val left: Value[SInt.type],
              override val right: Value[SInt.type]) extends Relation[SInt.type, SInt.type] {
  override val opCode: OpCode = ValueSerializer.LeCode
}

case class GT(override val left: Value[SInt.type],
              override val right: Value[SInt.type]) extends Relation[SInt.type, SInt.type] {
  override val opCode: OpCode = ValueSerializer.GtCode
}

case class GE(override val left: Value[SInt.type],
              override val right: Value[SInt.type]) extends Relation[SInt.type, SInt.type] {
  override val opCode: OpCode = ValueSerializer.GeCode
}

//todo: make EQ to really accept only values of the same type, now EQ(TrueLeaf, IntConstant(5)) is valid
case class EQ[S <: SType](override val left: Value[S], override val right: Value[S]) extends Relation[S, S] {
  override val opCode: OpCode = ValueSerializer.EqCode
}

object EQ {
  def applyNonTyped(left: Value[SType], right: Value[SType]): EQ[SType] = apply(left, right)
}

case class NEQ(override val left: Value[SType],
                                         override val right: Value[SType]) extends Relation[SType, SType] {
  override val opCode: OpCode = ValueSerializer.NeqCode
}


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


case class If[T <: SType](condition: Value[SBoolean.type], trueBranch: Value[T], falseBranch: Value[T])
  extends Quadruple[SBoolean.type, T, T, T] {
  override def tpe = trueBranch.tpe
  override lazy val first = condition
  override lazy val second = trueBranch
  override lazy val third = falseBranch
}





//Proof tree

trait ProofTree extends Product

sealed trait UnprovenTree extends ProofTree {
  val proposition: SigmaBoolean

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

sealed trait UncheckedSigmaTree[ST <: SigmaBoolean] extends UncheckedTree {
  val proposition: ST
}

trait UncheckedConjecture[ST <: SigmaBoolean] extends UncheckedSigmaTree[ST] {
  val challengeOpt: Option[Array[Byte]]
  val commitments: Seq[FirstProverMessage[_]]
}


case class SchnorrNode(override val proposition: ProveDlog,
                       firstMessageOpt: Option[FirstDLogProverMessage],
                       challenge: Array[Byte],
                       secondMessage: SecondDLogProverMessage)
  extends UncheckedSigmaTree[ProveDlog] {
}

case class DiffieHellmanTupleUncheckedNode(override val proposition: ProveDiffieHellmanTuple,
                                           firstMessageOpt: Option[FirstDiffieHellmanTupleProverMessage],
                                           challenge: Array[Byte],
                                           secondMessage: SecondDiffieHellmanTupleProverMessage)
  extends UncheckedSigmaTree[ProveDiffieHellmanTuple] {
}

case class CAndUncheckedNode(override val proposition: CAND,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             leafs: Seq[ProofTree])
  extends UncheckedConjecture[CAND] {
}


case class COr2UncheckedNode(override val proposition: COR,
                             override val challengeOpt: Option[Array[Byte]],
                             override val commitments: Seq[FirstProverMessage[_]],
                             children: Seq[ProofTree]) extends UncheckedConjecture[COR] {
}