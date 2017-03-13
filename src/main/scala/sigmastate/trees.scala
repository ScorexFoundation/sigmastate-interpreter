package sigmastate.experimental

import edu.biu.scapi.primitives.dlog.{DlogGroup, GroupElement}
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import scapi.sigma.rework.DLogProtocol.{DLogProverInput, DLogSigmaProtocol}
import scapi.sigma.rework.{SigmaProtocol, SigmaProtocolCommonInput, SigmaProtocolPrivateInput}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.ProofOfKnowledgeProposition


import org.bitbucket.inkytonik.kiama.relation._

trait SigmaStateTree extends Product

trait StateTree extends SigmaStateTree

trait SigmaTree extends SigmaStateTree

case class CAND(sigmaTrees: SigmaTree*) extends SigmaTree
case class COR(sigmaTrees: SigmaTree*) extends SigmaTree

trait SigmaProofOfKnowledgeTree[SP <: SigmaProtocol[SP], S <: SigmaProtocolPrivateInput[SP]]
  extends SigmaTree with ProofOfKnowledgeProposition[S]  with SigmaProtocolCommonInput[SP]

case class DLogNode(h: GroupElement)
  extends SigmaProtocolCommonInput[DLogSigmaProtocol]
    with SigmaProofOfKnowledgeTree[DLogSigmaProtocol, DLogProverInput]{

  override type M = this.type
  override def serializer: Serializer[DLogNode.this.type] = ???

  override val soundness: Int = 256
}



sealed abstract class BooleanConstantTree(val value: Boolean) extends SigmaStateTree

object BooleanConstantTree{
  def fromBoolean(v: Boolean) = v match{
    case true => TrueConstantTree
    case false => FalseConstantTree
  }
}

case object TrueConstantTree extends BooleanConstantTree(true)

case object FalseConstantTree extends BooleanConstantTree(false)


case class OR(left: SigmaStateTree, right: SigmaStateTree) extends SigmaStateTree

case class AND(left: SigmaStateTree, right: SigmaStateTree) extends SigmaStateTree


trait Relation extends StateTree

case class LT(left: SigmaStateTree, right: SigmaStateTree) extends Relation
case class LE(left: SigmaStateTree, right: SigmaStateTree) extends Relation

case class GT(left: SigmaStateTree, right: SigmaStateTree) extends Relation
case class GE(left: SigmaStateTree, right: SigmaStateTree) extends Relation

case class EQ(left: SigmaStateTree, right: SigmaStateTree) extends Relation


trait Value extends StateTree
case class IntLeaf(value: Int) extends Value

trait Variable extends Value
case object Height extends Variable



//Proof tree

trait ProofTree extends Product

case class SchnorrNode(signature:Array[Byte]) extends ProofTree
case class CAndProofNode(leafs: ProofTree*) extends ProofTree
case class COrProofNode(leafs: ProofTree*) extends ProofTree


object Rewriters extends App{
  def varSubst(h: Int) = rule[Value] {
    case Height => IntLeaf(h)
  }

  val rels = rule[SigmaStateTree] {
    case EQ(l, r) => BooleanConstantTree.fromBoolean(l == r)
    case GT(l: IntLeaf, r: IntLeaf) => BooleanConstantTree.fromBoolean(l.value > r.value)
    case GE(l: IntLeaf, r: IntLeaf) => BooleanConstantTree.fromBoolean(l.value >= r.value)
    case LT(l: IntLeaf, r: IntLeaf) => BooleanConstantTree.fromBoolean(l.value < r.value)
    case LE(l: IntLeaf, r: IntLeaf) => BooleanConstantTree.fromBoolean(l.value <= r.value)
  }

  val conjs = rule[SigmaStateTree] {
    case AND(l, r) if r.isInstanceOf[FalseConstantTree.type] => FalseConstantTree
    case AND(l, r) if r.isInstanceOf[TrueConstantTree.type] => l
    case AND(l, r) if l.isInstanceOf[FalseConstantTree.type] => FalseConstantTree
    case AND(l, r) if l.isInstanceOf[TrueConstantTree.type] => r
    case AND(l, r) if l.isInstanceOf[SigmaTree] && r.isInstanceOf[SigmaTree] =>
      CAND(l.asInstanceOf[SigmaTree],r.asInstanceOf[SigmaTree])


    case OR(l, r) if r.isInstanceOf[TrueConstantTree.type] => TrueConstantTree
    case OR(l, r) if r.isInstanceOf[FalseConstantTree.type] => l
    case OR(l, r) if l.isInstanceOf[TrueConstantTree.type] => TrueConstantTree
    case OR(l, r) if l.isInstanceOf[FalseConstantTree.type] => r
  }

  val h1: GroupElement = null
  val h2: GroupElement = null
  val h3: GroupElement = null

  val t = AND(OR(
    AND(DLogNode(h1), DLogNode(h2)),
    OR(EQ(IntLeaf(100), Height), GT(IntLeaf(100), Height))
  ), DLogNode(h3))

  val te = everywherebu(varSubst(1000) <+ rels <+ conjs)(t).get.asInstanceOf[SigmaStateTree]

  println(te)

  println("===")

  val tp = CAndProofNode(CAndProofNode(SchnorrNode(Array()), SchnorrNode(Array())), SchnorrNode(Array()))

  def corresponds(sst: SigmaStateTree, pt: ProofTree): Boolean = ???

  println(new Tree(te).childGraph)

  println(corresponds(te, tp))

}