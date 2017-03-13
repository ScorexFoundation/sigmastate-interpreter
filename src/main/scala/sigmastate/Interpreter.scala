package sigmastate

import edu.biu.scapi.primitives.dlog.DlogGroup
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scapi.sigma.rework.DLogProtocol.DLogCommonInput
import scapi.sigma.rework.{DLogProtocol, NonInteractiveProver}
import sigmastate.ProofOfKnowledge.Challenge
import sigmastate.SigmaProposition.PropositionCode
import sigmastate.experimental._

import scala.util.Try

trait Context

trait Interpreter {
  type CTX <: Context
  type SProp <: StateTree
  type CProp <: SigmaTree
  type CProof <: UnprovenTree[_]

  val dlogGroup: DlogGroup = new BcDlogECFp()

  def maxDepth: Int

  def varSubst(ctx: CTX): Strategy

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
      CAND(l.asInstanceOf[SigmaTree], r.asInstanceOf[SigmaTree])


    case OR(l, r) if r.isInstanceOf[TrueConstantTree.type] => TrueConstantTree
    case OR(l, r) if r.isInstanceOf[FalseConstantTree.type] => l
    case OR(l, r) if l.isInstanceOf[TrueConstantTree.type] => TrueConstantTree
    case OR(l, r) if l.isInstanceOf[FalseConstantTree.type] => r
  }

  val proofReduction = rule[ProofTree] {
    case s: UnprovenTree[_] => s.verify() match {
      case true => SuccessfulProof
      case false => FailedProof(s.proposition)
    }
  }

      //todo: check depth
      def reduceToCrypto(exp: SigmaStateTree, context: CTX): Try[SigmaStateTree] = Try({
        everywherebu(varSubst(context) <+ rels <+ conjs)(exp).get
      }.ensuring(res =>
        res.isInstanceOf[BooleanConstantTree] ||
          res.isInstanceOf[CAND] ||
          res.isInstanceOf[COR] ||
          res.isInstanceOf[DLogNode]
      ).asInstanceOf[SigmaStateTree])


      def verifyCryptoStatement(proof: CProof): Try[CheckedProof] =
        Try(everywherebu(proofReduction)(proof)
          .ensuring(_.get.isInstanceOf[CheckedProof])
          .asInstanceOf[CheckedProof])

      def evaluate(exp: SigmaStateTree, context: CTX, proof: CProof, challenge: ProofOfKnowledge.Challenge): Try[Boolean] = Try {
        val cProp = reduceToCrypto(exp, context).get
        println("cprop: " + cProp)

        verifyCryptoStatement(proof).get.isInstanceOf[SuccessfulProof.type]
      }
  }

  trait ProverInterpreter extends Interpreter {
    def prove(cryptoStatement: CProp, challenge: ProofOfKnowledge.Challenge): CProof

    def prove(exp: SigmaStateTree, context: CTX, challenge: ProofOfKnowledge.Challenge): Try[CProof] = Try {
      val cProp = reduceToCrypto(exp, context)
      assert(cProp.isInstanceOf[CProp])
      prove(cProp.asInstanceOf[CProp], challenge)
    }
  }


  trait DLogProverInterpreter extends ProverInterpreter {
    override type CProp = SigmaTree
    override type CProof = UnprovenTree[_]

    val secrets: Seq[DLogProtocol.DLogProverInput]

    val provers = new Provers {
      override val provers: Map[PropositionCode, Seq[NonInteractiveProver[_, _, _, _]]] =
        Map(DLogCommonInput.Code -> secrets.map(SchnorrSignatureSigner.apply))
    }

    override def prove(cryptoStatement: SigmaTree, challenge: Challenge): CProof =
      provers.prove(cryptoStatement, challenge)
  }
