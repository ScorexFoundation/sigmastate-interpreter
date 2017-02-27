package sigmastate

import edu.biu.scapi.primitives.dlog.DlogGroup
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import sigmastate.Proof.Challenge

import scala.util.Try

trait State

trait BlockchainState extends State {
  val height: Int
}

trait Interpreter {
  type Context <: State
  type SProp <: StateProposition
  type CProp <: SigmaProposition
  type CProof <: Proof[CProp]

  val dlogGroup: DlogGroup = new BcDlogECFp()

  def maxDepth: Int

  def statefulReductions[SP <: SProp](proposition: SP, context: Context): BooleanConstantProposition

  def reduceToCrypto(proposition: SigmaStateProposition, context: Context): SigmaStateProposition =
    reduceToCrypto(proposition, context, depth = 0)

  def reduceToCrypto(proposition: SigmaStateProposition, context: Context, depth: Int = 0): SigmaStateProposition = {
    require(depth < maxDepth)

    proposition match {
      case s: SProp => statefulReductions(s, context)

      case Or(statement1, statement2) =>
        (reduceToCrypto(statement1, context, depth + 1), reduceToCrypto(statement2, context, depth + 1)) match {
          case (TrueProposition, _) | (_, TrueProposition) => TrueProposition
          case (FalseProposition, st2r) => st2r
          case (st1r, FalseProposition) => st1r
          case (st1r: SigmaProofOfKnowledgeProposition[_], st2r: SigmaProofOfKnowledgeProposition[_]) => COr(st1r, st2r)
          case (_, _) => ???
        }
      case And(statement1, statement2) =>
        (reduceToCrypto(statement1, context, depth + 1), reduceToCrypto(statement2, context, depth + 1)) match {
          case (FalseProposition, _) | (_, FalseProposition) => FalseProposition
          case (TrueProposition, st2r) => st2r
          case (st1r, TrueProposition) => st1r
          case (st1r: SigmaProofOfKnowledgeProposition[_], st2r: SigmaProofOfKnowledgeProposition[_]) => CAnd(st1r, st2r)
          case (_, _) => ???
        }
      case cryptoProp: SigmaProofOfKnowledgeProposition[_] => cryptoProp
    }
  }

  def verifyCryptoStatement(cryptoStatement: CProp, proof: CProof, challenge: Proof.Challenge): BooleanConstantProposition =
    BooleanConstantProposition.fromBoolean(proof.verify(cryptoStatement, challenge))

  def evaluate(proposition: SigmaStateProposition, context: Context, proof: CProof, challenge: Proof.Challenge): Try[Boolean] = Try {
    val cProp = reduceToCrypto(proposition, context)
    assert(cProp.isInstanceOf[CProp])
    verifyCryptoStatement(cProp.asInstanceOf[CProp], proof, challenge).value
  }
}

trait ProverInterpreter extends Interpreter {

  def prove(cryptoStatement: CProp, challenge: Proof.Challenge): CProof

  def prove(proposition: SigmaStateProposition, context: Context, challenge: Proof.Challenge): Try[CProof] = Try {
    val cProp = reduceToCrypto(proposition, context)
    assert(cProp.isInstanceOf[CProp])
    prove(cProp.asInstanceOf[CProp], challenge)
  }
}


trait DLogProverInterpreter extends ProverInterpreter {
  override type CProp = SigmaProposition

  override def prove(cryptoStatement: SigmaProposition, challenge: Challenge): CProof = cryptoStatement match {
    case DLogProposition(_) => ???
    case CAnd() => ???
    case COr() => ???
  }
}