package sigmastate.eval

import org.ergoplatform._
import scalan.{MutableLazy, Nullable, SigmaLibrary}
import sigmastate.{SBigInt, SGroupElement, SSigmaProp, SType}
import sigmastate.Values._
import sigmastate.interpreter.Interpreter.ScriptEnv

trait GraphBuilding extends SigmaLibrary { this: IRContext =>
  import Liftables._
  import Context._;
  import WOption._
  import Coll._
  import SigmaProp._
  import BigInt._
  import GroupElement._
  import Box._
  import AvlTree._

  def doBuild(env: ScriptEnv, typed: SValue, okRemoveIsProven: Boolean): Ref[Context => Any] = {
    val g = buildGraph[Any](env.map { case (k, v) => (k: Any, builder.liftAny(v).get) }, typed)
    g
  }

  def buildGraph[T](envVals: Map[Any, SValue], tree: SValue): Ref[Context => T] = {
    fun { ctxC: Ref[Context] =>
      val env = envVals.mapValues(v => buildNode(ctxC, Map.empty, v))
      val res = asRep[T](buildNode(ctxC, env, tree))
      res
    }
  }

  type CompilingEnv = Map[Any, Ref[_]]


  protected def buildNode[T <: SType](ctx: Ref[Context], env: CompilingEnv, node: Value[T]): Ref[T#WrappedType] = {
    def eval[T <: SType](node: Value[T]): Ref[T#WrappedType] = buildNode(ctx, env, node)
    object In { def unapply(v: SValue): Nullable[Ref[Any]] = Nullable(asRep[Any](buildNode(ctx, env, v))) }
    class InColl[T: Elem] {
      def unapply(v: SValue): Nullable[Ref[Coll[T]]] = {
        val res = asRep[Def[_]](buildNode(ctx, env, v))
        Nullable(tryCast[Coll[T]](res))
      }
    }
    val InCollByte = new InColl[Byte]; val InCollAny = new InColl[Any]()(AnyElement); val InCollInt = new InColl[Int]

    val InCollCollByte = new InColl[Coll[Byte]]()(eCollByte)
    val InPairCollByte = new InColl[(Coll[Byte], Coll[Byte])]()(ePairOfCollByte)

    object InSeq { def unapply(items: Seq[SValue]): Nullable[Seq[Ref[Any]]] = {
      val res = items.map { x: SValue =>
        val xC = eval(x)
        asRep[Any](xC)
      }
      Nullable(res)
    }}

    val res: Ref[Any] = node match {
      case c @ Constant(v, tpe) => v match {
        case p: SSigmaProp =>
          assert(tpe == SSigmaProp)
          val resV = liftConst(p)
          resV
        case bi: SBigInt =>
          assert(tpe == SBigInt)
          val resV = liftConst(bi)
          resV
        case p: SGroupElement =>
          assert(tpe == SGroupElement)
          val resV = liftConst(p)
          resV
        case coll: SColl[a] =>
          val tpeA = tpe.asCollection[SType].elemType
          stypeToElem(tpeA) match {
            case eWA: Elem[wa] =>
              implicit val l = liftableFromElem[wa](eWA).asInstanceOf[Liftable[a, wa]]
              val resVals = liftConst[SColl[a], Coll[wa]](coll)
              resVals
          }
        case box: SBox =>
          val boxV = liftConst(box)
          boxV
        case tree: special.sigma.AvlTree =>
          val treeV = liftConst(tree)
          treeV
        case s: String =>
          val resV = toRep(s)(stypeToElem(tpe).asInstanceOf[Elem[String]])
          resV
        case _ =>
          val resV = toRep(v)(stypeToElem(tpe))
          resV
      }
      case org.ergoplatform.Context => ctx
      case Global => sigmaDslBuilder
      case Height => ctx.HEIGHT
      case Inputs => ctx.INPUTS
      case Outputs => ctx.OUTPUTS
      case Self => ctx.SELF
      case LastBlockUtxoRootHash => ctx.LastBlockUtxoRootHash
      case MinerPubkey => ctx.minerPubKey

      case _ =>
        error(s"Don't know how to compileNode($node)", node.sourceContext.toOption)
    }
    val resC = asRep[T#WrappedType](res)
    resC
  }
}
