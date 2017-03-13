package sigmastate

import edu.biu.scapi.primitives.dlog.DlogGroup
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import scapi.sigma.rework.DLogProtocol.DLogCommonInput
import scapi.sigma.rework.{DLogProtocol, NonInteractiveProver}
import sigmastate.ProofOfKnowledge.Challenge
import sigmastate.SigmaProposition.PropositionCode

import scala.util.Try

trait Context

/*
sealed trait Value[V] {
  def value: V
}

case class StringValue(override val value: String) extends Value[String]

case class IntValue(override val value: Int) extends Value[Int]

case class BooleanValue(override val value: Boolean) extends Value[Boolean]

case class NonNegValue(override val value: Int) extends Value[Int] {
  require(value >= 0)
}

//todo: types for merkle proofs


trait Context {
  def bind[V](name:String): Option[V]
}

trait Variable[V, VT <: Value[V], C <: Context] extends Value[V] {
  def name: String
  def context: C

  override def value = context.bind[V](name).get
}

trait Operation[V1, VT1 <: Value[V1], V2, VT2 <: Value[V2], V3, VT3 <: Value[V3]] extends VT3 {
  val v1: VT1
  val v2: VT2

  def eval(): VT3

  override def value: V3 = eval().value
}

trait SimpleOperation[V, VT <: Value[V]] extends Operation[V, VT, V, VT, V, VT]

trait Relation[V1, VT1 <: Value[V1], V2, VT2 <: Value[V2]] extends StateProposition {
  val v1: VT1
  val v2: VT2

  def holds: Boolean
}

trait SimpleRelation[V, VT <: Value[V]] extends Relation[V, VT, V, VT]

trait Pred[C <: Context]{
  def holds: Boolean
}


trait BlockchainContext extends Context {
  val height: Int
}

*/

trait Interpreter {
  type CTX <: Context
  type SProp <: StateProposition
  type CProp <: SigmaProposition
  type CProof <: Proof[CProp]

  val dlogGroup: DlogGroup = new BcDlogECFp()

  def maxDepth: Int

  def statefulReductions[SP <: SProp](proposition: SP, context: CTX): BooleanConstantProposition

  def reduceToCrypto(proposition: SigmaStateProposition, context: CTX): SigmaStateProposition =
    reduceToCrypto(proposition, context, depth = 0)

  def reduceToCrypto(proposition: SigmaStateProposition, context: CTX, depth: Int = 0): SigmaStateProposition = {
    require(depth < maxDepth)

    proposition match {
      case Or(statement1, statement2) =>
        (reduceToCrypto(statement1, context, depth + 1), reduceToCrypto(statement2, context, depth + 1)) match {
          case (TrueProposition, _) | (_, TrueProposition) => TrueProposition
          case (FalseProposition, st2r) => st2r
          case (st1r, FalseProposition) => st1r
          case (st1r: SigmaProofOfKnowledgeProposition[_, _], st2r: SigmaProofOfKnowledgeProposition[_, _]) => COr(st1r, st2r)
          case (_, _) => ???
        }
      case And(statement1, statement2) =>
        (reduceToCrypto(statement1, context, depth + 1), reduceToCrypto(statement2, context, depth + 1)) match {
          case (FalseProposition, _) | (_, FalseProposition) => FalseProposition
          case (TrueProposition, st2r) => st2r
          case (st1r, TrueProposition) => st1r
          case (st1r: SigmaProofOfKnowledgeProposition[_, _], st2r: SigmaProofOfKnowledgeProposition[_, _]) => CAnd(st1r, st2r)
          case (_, _) => ???
        }
      case cryptoProp: SigmaProofOfKnowledgeProposition[_, _] => cryptoProp
    }
  }

  def verifyCryptoStatement(cryptoStatement: CProp, proof: CProof, challenge: ProofOfKnowledge.Challenge): BooleanConstantProposition =
    BooleanConstantProposition.fromBoolean(proof.verify(cryptoStatement, challenge))

  def evaluate(proposition: SigmaStateProposition, context: CTX, proof: CProof, challenge: ProofOfKnowledge.Challenge): Try[Boolean] = Try {
    val cProp = reduceToCrypto(proposition, context)
    assert(cProp.isInstanceOf[CProp])
    println("cprop: " + cProp)
    verifyCryptoStatement(cProp.asInstanceOf[CProp], proof, challenge).value
  }
}

trait ProverInterpreter extends Interpreter {
  def prove(cryptoStatement: CProp, challenge: ProofOfKnowledge.Challenge): CProof

  def prove(proposition: SigmaStateProposition, context: CTX, challenge: ProofOfKnowledge.Challenge): Try[CProof] = Try {
    val cProp = reduceToCrypto(proposition, context)
    assert(cProp.isInstanceOf[CProp])
    prove(cProp.asInstanceOf[CProp], challenge)
  }
}


trait DLogProverInterpreter extends ProverInterpreter {
  override type CProp = SigmaProposition
  override type CProof = Proof[CProp]

  val secrets: Seq[DLogProtocol.DLogProverInput]

  val provers = new Provers {
    override val provers: Map[PropositionCode, Seq[NonInteractiveProver[_, _, _, _]]] =
      Map(DLogCommonInput.Code -> secrets.map(SchnorrSignatureSigner.apply))
  }

  override def prove(cryptoStatement: SigmaProposition, challenge: Challenge): Proof[SigmaProposition] =
    provers.prove(cryptoStatement, challenge)
}