package sigmastate

import edu.biu.scapi.primitives.dlog.DlogGroup
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter._
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scapi.sigma.rework.DLogProtocol.DLogProverInput
import scapi.sigma.rework.DLogProtocol

import scala.util.Try


trait Context

trait Interpreter {
  type CTX <: Context
  type StateT <: StateTree
  type SigmaT <: SigmaTree
  type ProofT <: UncheckedTree[_]

  val dlogGroup: DlogGroup = new BcDlogECFp()

  def maxDepth: Int

  /**
    * a custom strategy to replace variables with their respective values located in context
    *
    * @param ctx - a context
    * @return a strategy to rewrite a tree
    */
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
      CAND(Seq(l.asInstanceOf[SigmaTree], r.asInstanceOf[SigmaTree]))


    case OR(l, r) if r.isInstanceOf[TrueConstantTree.type] => TrueConstantTree
    case OR(l, r) if r.isInstanceOf[FalseConstantTree.type] => l
    case OR(l, r) if l.isInstanceOf[TrueConstantTree.type] => TrueConstantTree
    case OR(l, r) if l.isInstanceOf[FalseConstantTree.type] => r
  }

  val proofReduction = rule[ProofTree] {
    case s: UncheckedTree[_] => s.verify() match {
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


  def verifyCryptoStatement(proof: ProofT): Try[CheckedProof] =
    Try(everywherebu(proofReduction)(proof)
      .ensuring(_.get.isInstanceOf[CheckedProof])
      .asInstanceOf[CheckedProof])

  def evaluate(exp: SigmaStateTree, context: CTX, proof: ProofT, challenge: ProofOfKnowledge.Challenge): Try[Boolean] = Try {
    val cProp = reduceToCrypto(exp, context).get
    println("cprop: " + cProp)
    println("proof: " + proof)

    //verifyCryptoStatement(proof).get.isInstanceOf[SuccessfulProof.type]
    proof.proposition == cProp && proof.verify()
  }
}

trait ProverInterpreter extends Interpreter {
  protected def prove(unprovenTree: UnprovenTree): ProofT

  def normalizeUnprovenTree(unprovenTree: UnprovenTree): UnprovenTree

  def prove(exp: SigmaStateTree, context: CTX, challenge: ProofOfKnowledge.Challenge): Try[ProofT] = Try {
    val cProp = reduceToCrypto(exp, context).get
    println(cProp)
    assert(cProp.isInstanceOf[SigmaT])

    val ct = TreeConversion.convertToUnproven(cProp.asInstanceOf[SigmaT]).setChallenge(challenge)

    val toProve = normalizeUnprovenTree(ct)

    println(toProve)

    prove(toProve)
  }
}

object TreeConversion extends Attribution {
  //to be applied bottom up, converts SigmaTree => UnprovenTree
  val convertToUnproven: SigmaTree => UnprovenTree = attr {
    case CAND(sigmaTrees) => CAndUnproven(CAND(sigmaTrees), None, sigmaTrees.map(convertToUnproven))
    case COR(sigmaTrees) => COrUnproven(COR(sigmaTrees), None, sigmaTrees.map(convertToUnproven))
    case ci: DLogNode => SchnorrUnproven(None, simulated = false, ci.toCommonInput)
  }


  val proving: Seq[DLogProtocol.DLogProverInput] => UnprovenTree => UncheckedTree[_] = paramAttr{secrets => {
    case SchnorrUnproven(Some(challenge), simulated, proposition) =>
      simulated match {
        case true =>
          throw new Exception("simulation isn't done yet")
        case false =>
          val privKey = secrets.find(_.publicImage.h == proposition.h).get
          SchnorrSignatureSigner(privKey).sign(challenge)
      }
    case CAndUnproven(proposition, Some(challenge), children) =>
      val proven = children.map(proving(secrets))
      CAndUncheckedNode(proposition, challenge, proven)
  }}
}

trait DLogProverInterpreter extends ProverInterpreter {
  override type SigmaT = SigmaTree
  override type ProofT = UncheckedTree[_]

  val secrets: Seq[DLogProtocol.DLogProverInput]

  //to be applied bottom up, marks whether simulation is needed for a sigma-protocol
  val markSimulated = rule[UnprovenTree] {
    case su: SchnorrUnproven =>
      val secretKnown = secrets.exists(_.publicImage.h == su.proposition.h)
      su.copy(simulated = !secretKnown)
  }

  //to be applied down from the top node
  val challengeDisperse = rule[UnprovenTree] {
    case cand: CAndUnproven if cand.challengeOpt.isDefined =>
      val challenge = cand.challengeOpt.get
      cand.copy(children = cand.children.map(_.setChallenge(challenge)))

    case cor: COrUnproven if cor.challengeOpt.isDefined => ???
  }

  override def normalizeUnprovenTree(unprovenTree: UnprovenTree): UnprovenTree = {
    val t = everywherebu(markSimulated)(unprovenTree).get.asInstanceOf[UnprovenTree]
    everywheretd(challengeDisperse)(t).get.asInstanceOf[UnprovenTree]
  }

  override def prove(unproven: UnprovenTree): ProofT = TreeConversion.proving(secrets)(unproven)
}


case class TInput(height: Int) extends Context

object DLogProverInterpreterTest extends DLogProverInterpreter {
  override type StateT = StateTree
  override type CTX = TInput

  override val maxDepth = 50

  override def varSubst(context: TInput) = rule[Value] {
    case Height => IntLeaf(context.height)
  }

  override lazy val secrets: Seq[DLogProverInput] = {
    import SchnorrSignature._

    Seq(DLogProverInput.random()._1, DLogProverInput.random()._1)
  }

  val example = CAND(Seq(
    CAND(Seq(DLogNode(secrets.head.publicImage.h), DLogNode(secrets.tail.head.publicImage.h))),
    DLogNode(secrets.head.publicImage.h)))

  def main(args: Array[String]): Unit = {
    val challenge: Array[Byte] = Array.fill(32)(0: Byte)
    val proven = prove(example, TInput(100), challenge).get
    println(proven)


    println(proven.verify())
  }
}