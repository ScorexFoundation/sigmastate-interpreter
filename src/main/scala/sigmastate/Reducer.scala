package sigmastate

import sigmastate.utxo.SigmaStateTransaction

trait State

trait BlockchainState extends State {
  val height: Int
}


case class ReducerInput(override val height: Int, transaction: SigmaStateTransaction) extends BlockchainState

trait Reducer {
  type Input <: State
  type SProp <: StateProposition
  type CProp <: SigmaProposition
  type CProof <: Proof[CProp]

  val maxDepth: Int

  def statefulReductions[SP <: SProp](proposition: SP, environment: Input): BooleanConstantProposition

  def reduce(proposition: SigmaStateProposition, environment: Input): SigmaStateProposition =
    reduce(proposition, environment, depth = 0)


  def reduce(proposition: SigmaStateProposition, environment: Input, depth: Int = 0): SigmaStateProposition = {
    assert(depth < maxDepth)

    proposition match {
      case s: SProp => statefulReductions(s, environment)

      case Or(statement1, statement2) =>
        (reduce(statement1, environment, depth + 1), reduce(statement2, environment, depth + 1)) match {
          case (TrueProposition, _) | (_, TrueProposition) => TrueProposition
          case (FalseProposition, st2r) => st2r
          case (st1r, FalseProposition) => st1r
          case (st1r: SigmaProofOfKnowledgeProposition[_], st2r: SigmaProofOfKnowledgeProposition[_]) => COr(st1r, st2r)
          case (_, _) => ???
        }
      case And(statement1, statement2) =>
        (reduce(statement1, environment, depth + 1), reduce(statement2, environment, depth + 1)) match {
          case (FalseProposition, _) | (_, FalseProposition) => FalseProposition
          case (TrueProposition, st2r) => st2r
          case (st1r, TrueProposition) => st1r
          case (st1r: SigmaProofOfKnowledgeProposition[_], st2r: SigmaProofOfKnowledgeProposition[_]) => CAnd(st1r, st2r)
          case (_, _) => ???
        }
      case cryptoProp: SigmaProofOfKnowledgeProposition[_] => cryptoProp
    }
  }

  def reduceCryptoStatement(cryptoStatement: CProp, proof: CProof): BooleanConstantProposition =
    BooleanConstantProposition.fromBoolean(proof.verify(cryptoStatement))
}


object ReducerExample extends Reducer with App {
  override type SProp = StateProposition
  override type Input = ReducerInput

  override val maxDepth = 50

  override def statefulReductions[SP <: StateProposition](proposition: SP, environment: ReducerInput): BooleanConstantProposition =
    proposition match {
      case HeightFromProposition(from) =>
        if (environment.height >= from) TrueProposition else FalseProposition
      case HeightBetweenProposition(from, until) =>
        if (environment.height >= from && environment.height < until) TrueProposition else FalseProposition
      case HeightUntilProposition(until) =>
        if (environment.height < until) TrueProposition else FalseProposition
    }

  def sign(proposition: SigmaStateProposition, message: Array[Byte]): Array[Byte] = ???

  def verify(proposition: SigmaStateProposition, message: Array[Byte], signature: Array[Byte]): Boolean = ???

  val dk1 = DLogPublic(Array.fill(32)(0: Byte))
  val dk2 = DLogPublic(Array.fill(32)(1: Byte))


  val env = ReducerInput(500, null)
  assert(reduce(And(HeightFromProposition(500), dk1), env).isInstanceOf[DLogPublic])

  println(reduce(Or(
    And(HeightUntilProposition(505), And(dk1, dk2)),
    And(HeightFromProposition(505), dk1)
  ), env))
}