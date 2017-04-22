package sigmastate

import edu.biu.scapi.primitives.dlog.DlogGroup
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywherebu, everywheretd, rule}
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import scapi.sigma.rework.DLogProtocol.DLogNode
import scapi.sigma.rework.DLogProtocol
import scorex.crypto.hash.Blake2b256

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try


trait Context {
  val extension: Map[Byte, _ <: Value]
}

trait Interpreter {
  type CTX <: Context
  type StateT <: StateTree
  type SigmaT <: SigmaTree
  type ProofT <: UncheckedTree

  val dlogGroup: DlogGroup = new BcDlogECFp()

  def maxDepth: Int

  /**
    * implementation-specific tree reductions, to be defined in descendants
    * @param tree - a tree to process-
    * @param ctx - context instance
    * @return - processed tree
    */
  def specificPhases(tree: SigmaStateTree, ctx: CTX): SigmaStateTree

  protected val rels: Strategy = everywherebu(rule[SigmaStateTree] {
    case EQ(l, r) => BooleanConstantTree.fromBoolean(l == r)
    case NEQ(l, r) => BooleanConstantTree.fromBoolean(l != r)
    case GT(l: IntLeaf, r: IntLeaf) => BooleanConstantTree.fromBoolean(l.value > r.value)
    case GE(l: IntLeaf, r: IntLeaf) => BooleanConstantTree.fromBoolean(l.value >= r.value)
    case LT(l: IntLeaf, r: IntLeaf) => BooleanConstantTree.fromBoolean(l.value < r.value)
    case LE(l: IntLeaf, r: IntLeaf) => BooleanConstantTree.fromBoolean(l.value <= r.value)
  })

  protected val ops: Strategy = everywherebu(rule[SigmaStateTree] {
    case Plus(l: IntLeaf, r: IntLeaf) => IntLeaf(l.value + r.value)
    case Minus(l: IntLeaf, r: IntLeaf) => IntLeaf(l.value - r.value)
    case CalcBlake2b256(l: ByteArrayLeaf) => ByteArrayLeaf(Blake2b256(l.value))
  })

  protected val conjs: Strategy = everywherebu(rule[SigmaStateTree] {

    case AND(children) =>

      @tailrec
      def iterChildren(children: Seq[SigmaStateTree],
                       currentBuffer: mutable.Buffer[SigmaStateTree]): mutable.Buffer[SigmaStateTree] = {
        if(children.isEmpty) currentBuffer else children.head match {
          case FalseConstantTree => mutable.Buffer(FalseConstantTree)
          case TrueConstantTree => iterChildren(children.tail, currentBuffer)
          case s: SigmaStateTree => iterChildren(children.tail, currentBuffer += s)
        }
      }

      val reduced = iterChildren(children, mutable.Buffer())

      reduced.size match {
        case i: Int if i == 0 => TrueConstantTree
        case i: Int if i == 1 => reduced.head
        case _ =>
          if(reduced.forall(_.isInstanceOf[SigmaTree]))
            CAND(reduced.map(_.asInstanceOf[SigmaTree]))
          else AND(reduced)
      }


    case OR(children) =>
      @tailrec
      def iterChildren(children: Seq[SigmaStateTree],
                       currentBuffer: mutable.Buffer[SigmaStateTree]): mutable.Buffer[SigmaStateTree] = {
        if(children.isEmpty) currentBuffer else children.head match {
          case TrueConstantTree => mutable.Buffer(TrueConstantTree)
          case FalseConstantTree => iterChildren(children.tail, currentBuffer)
          case s: SigmaStateTree => iterChildren(children.tail, currentBuffer += s)
        }
      }

      val reduced = iterChildren(children, mutable.Buffer())

      reduced.size match {
        case i: Int if i == 0 => FalseConstantTree
        case i: Int if i == 1 => reduced.head
        case _ =>
          if(reduced.forall(_.isInstanceOf[SigmaTree]))
            COR(reduced.map(_.asInstanceOf[SigmaTree]))
          else OR(reduced)
      }
  })

  //todo: cost analysis
  def reduceToCrypto(exp: SigmaStateTree, context: CTX): Try[SigmaStateTree] = Try({
    val afterSpecific = specificPhases(exp, context)
    val afterOps = ops(afterSpecific).get.asInstanceOf[SigmaStateTree]
    val afterRels = rels(afterOps).get.asInstanceOf[SigmaStateTree]
    conjs(afterRels).get
  }.ensuring(res =>
    res.isInstanceOf[BooleanConstantTree] ||
      res.isInstanceOf[CAND] ||
      res.isInstanceOf[COR] ||
      res.isInstanceOf[DLogNode]
  ).asInstanceOf[SigmaStateTree])


  def evaluate(exp: SigmaStateTree, context: CTX, proof: UncheckedTree, challenge: ProofOfKnowledge.Challenge): Try[Boolean] = Try {
    val cProp = reduceToCrypto(exp, context).get

    cProp match {
      case TrueConstantTree  =>  true
      case FalseConstantTree => false
      case _ =>
        proof match{
          case NoProof => false
          case sp: UncheckedSigmaTree[_] => sp.proposition == cProp && sp.verify()
        }
    }
  }
}

trait ProverInterpreter extends Interpreter {
  protected def enrichContext(ctx: CTX): CTX

  protected def prove(unprovenTree: UnprovenTree): ProofT

  def normalizeUnprovenTree(unprovenTree: UnprovenTree): UnprovenTree

  def prove(exp: SigmaStateTree, context: CTX, challenge: ProofOfKnowledge.Challenge): Try[ProofT] = Try {
    val cProp = reduceToCrypto(exp, context).get
    assert(cProp.isInstanceOf[SigmaT])
    val ct = TreeConversion.convertToUnproven(cProp.asInstanceOf[SigmaT]).setChallenge(challenge)
    val toProve = normalizeUnprovenTree(ct)
    prove(toProve)
  }
}

object TreeConversion extends Attribution {

  //to be applied bottom up, converts SigmaTree => UnprovenTree
  val convertToUnproven: SigmaTree => UnprovenTree = attr {
    case CAND(sigmaTrees) => CAndUnproven(CAND(sigmaTrees), None, sigmaTrees.map(convertToUnproven))
    case COR(sigmaTrees) => COrUnproven(COR(sigmaTrees), None, sigmaTrees.map(convertToUnproven))
    case ci: DLogNode => SchnorrUnproven(None, simulated = false, ci)
  }

  val proving: Seq[DLogProtocol.DLogProverInput] => UnprovenTree => UncheckedTree = paramAttr{secrets => {
    case SchnorrUnproven(Some(challenge), simulated, proposition) =>
      if (simulated) {
        throw new Exception("simulation isn't done yet")
      } else {
        val privKey = secrets.find(_.publicImage.h == proposition.h).get
        SchnorrSignatureSigner(privKey).prove(challenge)
      }
    case CAndUnproven(proposition, Some(challenge), children) =>
      val proven = children.map(proving(secrets))
      CAndUncheckedNode(proposition, challenge, proven)
  }}
}

trait DLogProverInterpreter extends ProverInterpreter {
  override type SigmaT = SigmaTree
  override type ProofT = UncheckedTree

  val secrets: Seq[DLogProtocol.DLogProverInput]

  //to be applied bottom up, marks whether simulation is needed for a sigma-protocol
  val markSimulated: Strategy = rule[UnprovenTree] {
    case su: SchnorrUnproven =>
      val secretKnown = secrets.exists(_.publicImage.h == su.proposition.h)
      su.copy(simulated = !secretKnown)
  }

  //to be applied down from the top node
  val challengeDisperse: Strategy = rule[UnprovenTree] {
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