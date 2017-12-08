package sigmastate.interpreter

import java.math.BigInteger

import edu.biu.scapi.primitives.dlog.{DlogGroup, ECElementSendableData}
import edu.biu.scapi.primitives.dlog.bc.BcDlogECFp
import org.bitbucket.inkytonik.kiama.relation.Tree
import scorex.crypto.hash.Blake2b256
import sigmastate._
import sigmastate.utils.Helpers

import scala.util.Try
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{and, everywherebu, log, rule}
import scapi.sigma.DLogProtocol.FirstDLogProverMessage
import scapi.sigma.FirstDiffieHellmanTupleProverMessage
import scapi.sigma.rework.FirstProverMessage
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.crypto.authds.avltree.batch.Lookup
import sigmastate.utxo.CostTable

import scala.annotation.tailrec


trait Interpreter {
  type CTX <: Context[CTX]
  type StateT <: StateTree
  type SigmaT <: SigmaTree

  type ProofT = UncheckedTree //todo:  ProofT <: UncheckedTree ?

  lazy val dlogGroup: DlogGroup = new BcDlogECFp()

  /**
    * Max cost of a script interpreter can accept
    */
  def maxCost: Int

  /**
    * Implementation-specific tree reductions, to be defined in descendants
    *
    * @param tree - a tree to process
    * @param ctx  - context instance
    * @return - processed tree
    */
  def specificPhases(tree: SigmaStateTree, ctx: CTX, cost: CostAccumulator): SigmaStateTree

  protected def contextSubst(ctx: CTX, cost: CostAccumulator): Strategy = {
    everywherebu(rule[SigmaStateTree] {
      case TaggedByteArray(id: Byte) if ctx.extension.values.contains(id) =>
        val value = ctx.extension.values(id)
        cost.addCost(value.cost).ensuring(_.isRight)
        value
    })
  }

  protected val relations: Strategy = everywherebu(rule[SigmaStateTree] {
    case EQ(l: Value, r: Value) if l.evaluated && r.evaluated =>
      BooleanLeafConstant.fromBoolean(l == r)
    case NEQ(l: Value, r: Value) if l.evaluated && r.evaluated =>
      BooleanLeafConstant.fromBoolean(l != r)
    case GT(l: IntLeafConstant, r: IntLeafConstant) =>
      BooleanLeafConstant.fromBoolean(l.value > r.value)
    case GE(l: IntLeafConstant, r: IntLeafConstant) =>
      BooleanLeafConstant.fromBoolean(l.value >= r.value)
    case LT(l: IntLeafConstant, r: IntLeafConstant) =>
      BooleanLeafConstant.fromBoolean(l.value < r.value)
    case LE(l: IntLeafConstant, r: IntLeafConstant) =>
      BooleanLeafConstant.fromBoolean(l.value <= r.value)

    //todo: cost
    case IsMember(tree: AvlTreeLeafConstant, key: ByteArrayLeafConstant, proof: ByteArrayLeafConstant) =>
      val bv = tree.createVerifier(SerializedAdProof @@ proof.value)
      BooleanLeafConstant.fromBoolean(bv.performOneOperation(Lookup(ADKey @@ key.value)).isSuccess)
  })

  protected val operations: Strategy = everywherebu(rule[SigmaStateTree] {
    case Plus(l: IntLeafConstant, r: IntLeafConstant) => IntLeafConstant(l.value + r.value)
    case Minus(l: IntLeafConstant, r: IntLeafConstant) => IntLeafConstant(l.value - r.value)
    case Xor(l: ByteArrayLeafConstant, r: ByteArrayLeafConstant) =>
      assert(l.value.length == r.value.length)
      ByteArrayLeafConstant(Helpers.xor(l.value, r.value))
    case Append(l: ByteArrayLeafConstant, r: ByteArrayLeafConstant) =>
      require(l.value.length + r.value.length < 10000) //todo: externalize this maximum intermediate value length limit
      ByteArrayLeafConstant(l.value ++ r.value)
    case c@CalcBlake2b256(l: ByteArrayLeafConstant) if l.evaluated => c.function(l)
  })

  protected val conjs: Strategy = everywherebu(rule[SigmaStateTree] {

    case a@AND(children) if a.transformationReady =>
      a.function(children.asInstanceOf[ConcreteCollection[BooleanLeaf]])

    case o@OR(children) if o.transformationReady =>
      o.function(children.asInstanceOf[ConcreteCollection[BooleanLeaf]])
  })


  // new reducer: 1 phase only which is constantly being repeated until non-reducible,
  // reduction state carried between reductions is about current cost and number of transformations done.
  // when the latter becomes zero, it means that it is time to stop (tree becomes irreducible)
  case class ReductionState(cost: CostAccumulator, numberOfTransformations: Int){
    def recordTransformation(transformationCost: Int): Try[ReductionState] = Try {
      cost.addCost(transformationCost).ensuring(_.isRight)
      ReductionState(cost, numberOfTransformations + 1)
    }
  }

  //todo: return final cost as well
  def reduceNew(exp: SigmaStateTree, context: CTX): Try[SigmaStateTree] = Try {
    require(new Tree(exp).nodes.length < CostTable.MaxExpressions)

    @tailrec
    def reductionStep(tree: SigmaStateTree, reductionState: ReductionState): (SigmaStateTree, ReductionState) = {
      var state = reductionState

      def statefulTransformation(rules: PartialFunction[SigmaStateTree, SigmaStateTree]):
      PartialFunction[SigmaStateTree, SigmaStateTree] = rules.andThen({newTree =>
        state = state.recordTransformation(newTree.cost).get //todo: replace .get with a nicer logic
        newTree
      })

      //todo: use and(s1, s2) strategy to combine rules below with specific phases
      val rules: Strategy = rule[SigmaStateTree](statefulTransformation({
        case TaggedByteArray(id: Byte) if context.extension.values.contains(id) =>
          context.extension.values(id)
      }))

      val newTree = everywherebu(rules)(tree).get.asInstanceOf[SigmaStateTree]

      if(state.numberOfTransformations == 0) (newTree, state) else reductionStep(newTree, state)
    }

    val initialCost = CostAccumulator(exp.cost, maxCost)
    val initialState = ReductionState(initialCost, 0)

    reductionStep(exp, initialState)._1
  }

  def reduceToCrypto(exp: SigmaStateTree, context: CTX): Try[SigmaStateTree] = Try({
    require(new Tree(exp).nodes.length < CostTable.MaxExpressions)

    val additionalCost = CostAccumulator(exp.cost, maxCost)

    //in these two phases a tree could be expanded, so additionalCost accumulator is passed to ensure
    // that cost of script is not exploding
    val afterContextSubst = contextSubst(context, additionalCost)(exp).get.asInstanceOf[SigmaStateTree]
    val afterSpecific = specificPhases(afterContextSubst, context, additionalCost)

    //in phases below, a tree could be reduced only
    val afterOps = operations(afterSpecific).get.asInstanceOf[SigmaStateTree]
    val afterRels = relations(afterOps).get.asInstanceOf[SigmaStateTree]
    conjs(afterRels).get
  }.asInstanceOf[SigmaStateTree])

  /**
    * Verifier steps:
    * 1. Place received challenges "e" and responses "z" into every leaf.
    * 2. Bottom-up: compute commitments at every leaf according to a = g^z/h^e. At every COR and CAND node, compute
    * the commitment as the union of the children's commitments. At every COR node, compute the challenge as the XOR of
    * the children's challenges. At every CAND node, verify that the children's challenges are all equal. (Note that
    * there is an opportunity for small savings here, because we don't need to send all the challenges for a CAND --
    * but let's save that optimization for later.)
    * 3. Check that the root challenge is equal to the hash of the root commitment and other inputs.
    */

  def verify(exp: SigmaStateTree,
             context: CTX,
             proof: UncheckedTree,
             message: Array[Byte]): Try[Boolean] = Try {
    val cProp = reduceToCrypto(exp, context).get
    cProp match {

      case TrueLeaf => true
      case FalseLeaf => false
      case b: BooleanLeaf if b.evaluated =>
        proof match {
          case NoProof => false
          case sp: UncheckedSigmaTree[_] =>
            assert(sp.proposition == cProp)

            val newRoot = checks(sp).get.asInstanceOf[UncheckedTree]
            val (challenge, rootCommitments) = newRoot match {
              case u: UncheckedConjecture[_] => (u.challengeOpt.get, u.commitments)
              case sn: SchnorrNode => (sn.challenge, sn.firstMessageOpt.toSeq)
              case dh: DiffieHellmanTupleUncheckedNode => (dh.challenge, dh.firstMessageOpt.toSeq)
            }

            val expectedChallenge = Blake2b256(rootCommitments.map(_.bytes).reduce(_ ++ _) ++ message)
            challenge.sameElements(expectedChallenge)
        }
      case _: SigmaStateTree => false
    }
  }

  /**
    * 2. Bottom-up: compute commitments at every leaf according to a = g^z/h^e. At every COR and CAND node, compute
    * the commitment as the union of the children's commitments. At every COR node, compute the challenge as the XOR of
    * the children's challenges. At every CAND node, verify that the children's challenges are all equal. (Note that
    * there is an opportunity for small savings here, because we don't need to send all the challenges for a CAND --
    * but let's save that optimization for later.)
    */
  val checks: Strategy = everywherebu(rule[UncheckedTree] {
    case and: CAndUncheckedNode =>
      //todo: reduce boilerplate below

      val challenges: Seq[Array[Byte]] = and.leafs.map {
        case u: UncheckedConjecture[_] => u.challengeOpt.get
        case sn: SchnorrNode => sn.challenge
        case dh: DiffieHellmanTupleUncheckedNode => dh.challenge
      }

      val commitments: Seq[FirstProverMessage[_]] = and.leafs.flatMap {
        case u: UncheckedConjecture[_] => u.commitments
        case sn: SchnorrNode => sn.firstMessageOpt.toSeq
        case dh: DiffieHellmanTupleUncheckedNode => dh.firstMessageOpt.toSeq
      }

      val challenge = challenges.head

      assert(challenges.tail.forall(_.sameElements(challenge)))

      and.copy(challengeOpt = Some(challenge), commitments = commitments)

    case or: COr2UncheckedNode =>
      val challenges = or.children map {
        case u: UncheckedConjecture[_] => u.challengeOpt.get
        case sn: SchnorrNode => sn.challenge
        case dh: DiffieHellmanTupleUncheckedNode => dh.challenge
        case a: Any => println(a); ???
      }

      val commitments = or.children flatMap {
        case u: UncheckedConjecture[_] => u.commitments
        case sn: SchnorrNode => sn.firstMessageOpt.toSeq
        case dh: DiffieHellmanTupleUncheckedNode => dh.firstMessageOpt.toSeq
        case _ => ???
      }

      or.copy(challengeOpt = Some(Helpers.xor(challenges: _*)), commitments = commitments)

    case sn: SchnorrNode =>

      val dlog = sn.proposition.dlogGroup
      val g = dlog.getGenerator
      val h = sn.proposition.h

      val a = dlog.multiplyGroupElements(
        dlog.exponentiate(g, sn.secondMessage.z.underlying()),
        dlog.getInverse(dlog.exponentiate(h, new BigInteger(1, sn.challenge))))

      sn.copy(firstMessageOpt = Some(FirstDLogProverMessage(a)))

    //todo: check that g,h belong to the group
    //g^z = a*u^e, h^z = b*v^e  => a = g^z/u^e, b = h^z/v^e
    case dh: DiffieHellmanTupleUncheckedNode =>
      val dlog = dh.proposition.dlogGroup

      val g = dh.proposition.g
      val h = dh.proposition.h
      val u = dh.proposition.u
      val v = dh.proposition.v

      val z = dh.secondMessage.z

      val e = new BigInteger(1, dh.challenge)

      val gToZ = dlog.exponentiate(g, z)
      val hToZ = dlog.exponentiate(h, z)

      val uToE = dlog.exponentiate(u, e)
      val vToE = dlog.exponentiate(v, e)

      val a = dlog.multiplyGroupElements(gToZ, dlog.getInverse(uToE)).generateSendableData().asInstanceOf[ECElementSendableData]
      val b = dlog.multiplyGroupElements(hToZ, dlog.getInverse(vToE)).generateSendableData().asInstanceOf[ECElementSendableData]
      dh.copy(firstMessageOpt = Some(FirstDiffieHellmanTupleProverMessage(a, b)))

    case _ => ???
  })

  def verify(exp: SigmaStateTree,
             context: CTX,
             proverResult: ProverResult[ProofT],
             message: Array[Byte]): Try[Boolean] = {
    val ctxv = context.withExtension(proverResult.extension)
    verify(exp, ctxv, proverResult.proof, message)
  }
}