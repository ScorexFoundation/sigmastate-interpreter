package sigmastate.interpreter

import java.math.BigInteger

import org.bitbucket.inkytonik.kiama.relation.Tree
import scorex.crypto.hash.Blake2b256
import sigmastate.{SType, _}
import sigmastate.utils.Helpers
import sigmastate.Values._

import scala.util.Try
import org.bitbucket.inkytonik.kiama.rewriting.Strategy
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{and, everywherebu, log, rule}
import org.bouncycastle.math.ec.custom.djb.Curve25519Point
import scapi.sigma.DLogProtocol.FirstDLogProverMessage
import scapi.sigma._
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.crypto.authds.avltree.batch.Lookup
import sigmastate.utxo.{CostTable, Transformer}

import scala.annotation.tailrec

object GroupSettings {
  type EcPointType = Curve25519Point
  val dlogGroup: BcDlogFp[EcPointType] = Curve25519

  implicit val soundness: Int = 256
}

trait Interpreter {

  import GroupSettings._

  type CTX <: Context[CTX]

  type ProofT = UncheckedTree //todo:  ProofT <: UncheckedTree ?

  /**
    * Max cost of a script interpreter can accept
    */
  def maxCost: Int

  /**
    * Implementation-specific tree reductions, to be defined in descendants
    *
    * @param context - context instance
    * @return - processed tree
    */
  def specificTransformations(context: CTX): PartialFunction[Value[_ <: SType], Value[_ <: SType]]


  // new reducer: 1 phase only which is constantly being repeated until non-reducible,
  // reduction state carried between reductions is number of transformations done.
  // when it becomes zero, it means that it is time to stop (tree becomes irreducible)
  case class ReductionState(numberOfTransformations: Int) {
    def recordTransformation(): ReductionState = ReductionState(numberOfTransformations + 1)
  }

  //todo: return cost as well
  /**
    * As the first step both prover and verifier are applying context-specific transformations and then estimating
    * cost of the intermediate expression. If cost is above limit, abort. Otherwise, both prover and verifier are
    * reducing the expression in the same way.
    *
    * @param exp
    * @param context
    * @return
    */
  def reduceToCrypto(exp: Value[SBoolean.type], context: CTX): Try[Value[SBoolean.type]] = Try {
    require(new Tree(exp).nodes.length < CostTable.MaxExpressions)

    @tailrec
    def reductionStep(tree: Value[SBoolean.type],
                      reductionState: ReductionState): (Value[SBoolean.type], ReductionState) = {
      var state = reductionState

      def statefulTransformation(rules: PartialFunction[Value[_ <: SType], Value[_ <: SType]]):
      PartialFunction[Value[_ <: SType], Value[_ <: SType]] = rules.andThen({ newTree =>
        state = state.recordTransformation()
        newTree
      })

      val transformations = ({
        case GroupGenerator =>
          GroupElementConstant(dlogGroup.generator) // TODO should we use GroupGenerator.value instead

        case t: Transformer[_, _] if t.transformationReady => t.function()

        //operations
        case Plus(l: IntConstant, r: IntConstant) => IntConstant(l.value + r.value)
        case Minus(l: IntConstant, r: IntConstant) => IntConstant(l.value - r.value)
        case Xor(l: ByteArrayConstant, r: ByteArrayConstant) =>
          assert(l.value.length == r.value.length)
          ByteArrayConstant(Helpers.xor(l.value, r.value))
        case AppendBytes(l: ByteArrayConstant, r: ByteArrayConstant) =>
          require(l.value.length + r.value.length < 10000) //todo: externalize this maximum intermediate value length limit
          ByteArrayConstant(l.value ++ r.value
          )

        case c:CalcHash if c.input.evaluated => c.function(c.input.asInstanceOf[EvaluatedValue[SByteArray.type]])

        case Exponentiate(l: GroupElementConstant, r: BigIntConstant) =>
          GroupElementConstant(dlogGroup.exponentiate(l.value, r.value))

        case MultiplyGroup(l: GroupElementConstant, r: GroupElementConstant) =>
          GroupElementConstant(dlogGroup.multiplyGroupElements(l.value, r.value))

        //relations
        case EQ(l: Value[_], r: Value[_]) if l.evaluated && r.evaluated =>
          BooleanConstant.fromBoolean(l == r)
        case NEQ(l: Value[_], r: Value[_]) if l.evaluated && r.evaluated =>
          BooleanConstant.fromBoolean(l != r)
        case GT(l: IntConstant, r: IntConstant) =>
          BooleanConstant.fromBoolean(l.value > r.value)
        case GE(l: IntConstant, r: IntConstant) =>
          BooleanConstant.fromBoolean(l.value >= r.value)
        case LT(l: IntConstant, r: IntConstant) =>
          BooleanConstant.fromBoolean(l.value < r.value)
        case LE(l: IntConstant, r: IntConstant) =>
          BooleanConstant.fromBoolean(l.value <= r.value)
        case IsMember(tree: AvlTreeConstant, key: ByteArrayConstant, proof: ByteArrayConstant) =>
          val bv = tree.createVerifier(SerializedAdProof @@ proof.value)
          BooleanConstant.fromBoolean(bv.performOneOperation(Lookup(ADKey @@ key.value)).isSuccess)
        case If(cond: EvaluatedValue[SBoolean.type], trueBranch, falseBranch) =>
          if (cond.value) trueBranch else falseBranch

        //conjectures
        case a@AND(children) if a.transformationReady =>
          a.function(children.asInstanceOf[EvaluatedValue[SCollection[SBoolean.type]]])

        case o@OR(children) if o.transformationReady =>
          o.function(children.asInstanceOf[EvaluatedValue[SCollection[SBoolean.type]]])
      }: PartialFunction[Value[_ <: SType], Value[_ <: SType]]).orElse(specificTransformations(context))

      //todo: use and(s1, s2) strategy to combine rules below with specific phases
      val rules: Strategy = rule[Value[_ <: SType]](statefulTransformation(transformations))

      val newTree = everywherebu(rules)(tree).get.asInstanceOf[Value[SBoolean.type]]

      if (state.numberOfTransformations == 0)
        (newTree, state)
      else
        reductionStep(newTree, state.copy(numberOfTransformations = 0))
    }

    // First, both the prover and the verifier are making context-dependent tree transformations
    // (usually, context variables substitutions).
    // We can estimate cost of the tree evaluation only after this step.
    val substRule = rule[Value[_ <: SType]](specificTransformations(context))

    //todo: controversial .asInstanceOf?
    val substTree = everywherebu(substRule)(exp) match {
      case Some(v: Value[SBoolean.type]@unchecked) if v.tpe == SBoolean => v
      case x => throw new Error(s"Context-dependent pre-processing should produce tree of type Boolean but was $x")
    }
    if (substTree.cost > maxCost) throw new Error("Estimated expression complexity exceeds the limit")

    // After performing context-dependent transformations and checking cost of the resulting tree, both the prover
    // and the verifier are evaluating the tree by applying rewriting rules, until no any rule triggers during tree
    // traversal (so interpreter checks whether no any nodes were rewritten during last rewriting, and aborts if so).
    // todo: any attacks possible on rewriting itself? Maybe, adversary can construct such a tree that its rewriting
    // todo: takes long time. Further investigation is needed.
    val initialState = ReductionState(0)
    reductionStep(substTree, initialState)._1
  }

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

  def verify(exp: Value[SBoolean.type],
             context: CTX,
             proof: UncheckedTree,
             message: Array[Byte]): Try[Boolean] = Try {
    val cProp = reduceToCrypto(exp, context).get

    cProp match {
      case TrueLeaf => true
      case FalseLeaf => false
      case b: Value[SBoolean.type] if b.evaluated =>
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
      case _: Value[_] => false
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

    case or: COrUncheckedNode =>
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

      val dlog = GroupSettings.dlogGroup
      val g = dlog.generator
      val h = sn.proposition.h

      val a = dlog.multiplyGroupElements(
        dlog.exponentiate(g, sn.secondMessage.z.underlying()),
        dlog.getInverse(dlog.exponentiate(h, new BigInteger(1, sn.challenge))))

      sn.copy(firstMessageOpt = Some(FirstDLogProverMessage(a)))

    //todo: check that g,h belong to the group
    //g^z = a*u^e, h^z = b*v^e  => a = g^z/u^e, b = h^z/v^e
    case dh: DiffieHellmanTupleUncheckedNode =>
      val dlog = GroupSettings.dlogGroup

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

      val a = dlog.multiplyGroupElements(gToZ, dlog.getInverse(uToE))
      val b = dlog.multiplyGroupElements(hToZ, dlog.getInverse(vToE))
      dh.copy(firstMessageOpt = Some(FirstDiffieHellmanTupleProverMessage(a, b)))

    case _ => ???
  })

  def verify(exp: Value[SBoolean.type],
             context: CTX,
             proverResult: ProverResult[ProofT],
             message: Array[Byte]): Try[Boolean] = {
    val ctxv = context.withExtension(proverResult.extension)
    verify(exp, ctxv, proverResult.proof, message)
  }
}

class InterpreterException(msg: String) extends Exception(msg)

object Interpreter {
  def error(msg: String) = throw new InterpreterException(msg)
}