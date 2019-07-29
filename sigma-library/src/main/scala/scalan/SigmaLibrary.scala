package scalan

import special.sigma._

import scala.collection.mutable.ArrayBuffer

trait SigmaLibrary extends Library
    with special.sigma.wrappers.WrappersModule
    with SigmaDslModule
    with CostedObjectsModule
    with SigmaDslCostedModule
{
  import Coll._
  import CollBuilder._
  import SigmaProp._
  import SigmaContract._
  import SigmaDslBuilder._
  import WRType._
  import Size._

  implicit lazy val wRTypeAnyElement = wRTypeElement(AnyElement)
  implicit lazy val sizeAnyElement = sizeElement(AnyElement)

  private val CM = CollMethods
  private val CBM = CollBuilderMethods
  private val SM = SigmaPropMethods
  private val SCM = SigmaContractMethods
  private val SDBM = SigmaDslBuilderMethods

  def sigmaDslBuilder: Rep[SigmaDslBuilder]

  object AnyOf {
    def unapply(d: Def[_]): Option[(Rep[CollBuilder], Seq[Rep[A]], Elem[A]) forSome {type A}] = d match {
      case SDBM.anyOf(_, CBM.fromItems(b, items, e)) =>
        Some((b, items, e.asInstanceOf[Elem[Any]]))
      case _ => None
    }
  }
  object AllOf {
    def unapply(d: Def[_]): Option[(Rep[CollBuilder], Seq[Rep[A]], Elem[A]) forSome {type A}] = d match {
      case SDBM.allOf(_, CBM.fromItems(b, items, e)) =>
        Some((b, items, e.asInstanceOf[Elem[Any]]))
      case _ => None
    }
  }
  object AnyZk {
    def unapply(d: Def[_]): Option[(Rep[CollBuilder], Seq[Rep[SigmaProp]], Elem[SigmaProp])] = d match {
      case SDBM.anyZK(_, CBM.fromItems(b, items, e)) =>
        Some((b, items.asInstanceOf[Seq[Rep[SigmaProp]]], e.asInstanceOf[Elem[SigmaProp]]))
      case _ => None
    }
  }
  object AllZk {
    def unapply(d: Def[_]): Option[(Rep[CollBuilder], Seq[Rep[SigmaProp]], Elem[SigmaProp])] = d match {
      case SDBM.allZK(_, CBM.fromItems(b, items, e)) =>
        Some((b, items.asInstanceOf[Seq[Rep[SigmaProp]]], e.asInstanceOf[Elem[SigmaProp]]))
      case _ => None
    }
  }
  object HasSigmas {
    def unapply(items: Seq[Sym]): Option[(Seq[Rep[Boolean]], Seq[Rep[SigmaProp]])] = {
      val bs = ArrayBuffer.empty[Rep[Boolean]]
      val ss = ArrayBuffer.empty[Rep[SigmaProp]]
      for (i <- items) {
        i match {
          case SM.isValid(s) => ss += s
          case b => bs += asRep[Boolean](b)
        }
      }
      assert(items.length == bs.length + ss.length)
      if (ss.isEmpty) None
      else Some((bs,ss))
    }
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case AllOf(b, HasSigmas(bools, sigmas), _) =>
      val zkAll = sigmaDslBuilder.allZK(b.fromItems(sigmas:_*))
      if (bools.isEmpty)
        zkAll.isValid
      else
        (sigmaDslBuilder.sigmaProp(sigmaDslBuilder.allOf(b.fromItems(bools:_*))) && zkAll).isValid
    case AnyOf(b, HasSigmas(bs, ss), _) =>
      val zkAny = sigmaDslBuilder.anyZK(b.fromItems(ss:_*))
      if (bs.isEmpty)
        zkAny.isValid
      else
        (sigmaDslBuilder.sigmaProp(sigmaDslBuilder.anyOf(b.fromItems(bs:_*))) || zkAny).isValid
    case AllOf(_,items,_) if items.length == 1 => items(0)
    case AnyOf(_,items,_) if items.length == 1 => items(0)
    case AllZk(_,items,_) if items.length == 1 => items(0)
    case AnyZk(_,items,_) if items.length == 1 => items(0)

    case ApplyBinOp(op, lhs, rhs) =>
      op.asInstanceOf[BinOp[_, _]] match {
        case And =>
          sigmaDslBuilder.allOf(sigmaDslBuilder.Colls.fromItems(Seq(asRep[Boolean](lhs), asRep[Boolean](rhs)):_*))
        case Or =>
          sigmaDslBuilder.anyOf(sigmaDslBuilder.Colls.fromItems(Seq(asRep[Boolean](lhs), asRep[Boolean](rhs)):_*))
        case _ => super.rewriteDef(d)
      }

    case SDBM.sigmaProp(_, SM.isValid(p)) => p
    case SM.isValid(SDBM.sigmaProp(_, bool)) => bool

    case _ =>
      if (currentPass.config.constantPropagation) {
        // additional constant propagation rules (see other similar cases)
        d match {
          case AnyOf(_,items,_) if (items.forall(_.isConst)) =>
            val bs = items.map { case Def(Const(b: Boolean)) => b }
            toRep(bs.exists(_ == true))
          case AllOf(_,items,_) if (items.forall(_.isConst)) =>
            val bs = items.map { case Def(Const(b: Boolean)) => b }
            toRep(bs.forall(_ == true))
          case _ =>
            super.rewriteDef(d)
        }
      }
      else
        super.rewriteDef(d)
  }

}
