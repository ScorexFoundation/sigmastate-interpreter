package scalan

import scala.language.implicitConversions
import sigma.collection._
import special.wrappers.WrappersModule
import scalan.util.MemoizedFunc
import scalan.ExactIntegral._

trait Library extends Scalan
  with WrappersModule
  with CollsModule
{
  import WOption._
  import WRType._
  import Coll._; import CollBuilder._;
  import WSpecialPredef._

  type LazyRep[T] = MutableLazy[Ref[T]]

  private val _liftElemMemo = new MemoizedFunc({
    case eT: Elem[t] =>
      val lT = Liftables.asLiftable[Any, t](eT.liftable)
      liftableRType(lT).lift(eT.sourceType.asInstanceOf[RType[Any]])
  })
  implicit def liftElem[T](eT: Elem[T]): Ref[WRType[T]] = {
    _liftElemMemo(eT).asInstanceOf[Ref[WRType[T]]]  // asRep cannot be used for AnyRef
  }

  private val _specialPredef: LazyRep[WSpecialPredefCompanionCtor] = MutableLazy(RWSpecialPredef.value)
  def specialPredef: Ref[WSpecialPredefCompanionCtor] = _specialPredef.value

  override protected def onReset(): Unit = {
    _specialPredef.reset()
    _liftElemMemo.reset()
    super.onReset()
  }

  val CM = CollMethods
  private val CBM = CollBuilderMethods
  private val WOptionM = WOptionMethods
  private val SPCM = WSpecialPredefCompanionMethods

  def colBuilder: Ref[CollBuilder]

  object IsNumericToInt {
    def unapply(d: Def[_]): Nullable[Ref[A] forSome {type A}] = d match {
      case ApplyUnOp(_: NumericToInt[_], x) => Nullable(x.asInstanceOf[Ref[A] forSome {type A}])
      case _ => Nullable.None
    }
  }
  object IsNumericToLong {
    def unapply(d: Def[_]): Nullable[Ref[A] forSome {type A}] = d match {
      case ApplyUnOp(_: NumericToLong[_], x) => Nullable(x.asInstanceOf[Ref[A] forSome {type A}])
      case _ => Nullable.None
    }
  }

  override def rewriteDef[T](d: Def[T]) = d match {
    case CM.length(ys) => ys.node match {
      // Rule: xs.map(f).length  ==> xs.length
      case CM.map(xs, _) =>
        xs.length
      // Rule: replicate(len, v).length => len
      case CBM.replicate(_, len, _) =>
        len
      // Rule: Const[Coll[T]](coll).length =>
      case CollConst(coll, _) =>
        coll.length
      // Rule: Coll(items @ Seq(x1, x2, x3)).length => items.length
      case CBM.fromItems(_, items, _) =>
        items.length
      case _ => super.rewriteDef(d)
    }

    // Rule: replicate(l, x).zip(replicate(l, y)) ==> replicate(l, (x,y))
    case CM.zip(CBM.replicate(b1, l1, v1), CBM.replicate(b2, l2, v2)) if b1 == b2 && l1 == l2 =>
      b1.replicate(l1, Pair(v1, v2))

    case CM.map(xs, _f) => _f.node match {
      case IdentityLambda() => xs
      case _ => xs.node match {
        // Rule: replicate(l, v).map(f) ==> replicate(l, f(v))
        case CBM.replicate(b, l, v: Ref[a]) =>
          val f = asRep[a => Any](_f)
          b.replicate(l, Apply(f, v, false))

        // Rule: xs.map(f).map(g) ==> xs.map(x => g(f(x)))
        case CM.map(_xs, f: RFunc[a, b]) =>
          implicit val ea = f.elem.eDom
          val xs = asRep[Coll[a]](_xs)
          val g  = asRep[b => Any](_f)
          xs.map[Any](fun { x: Ref[a] => g(f(x)) })

        case _ => super.rewriteDef(d)
      }
    }

    case WOptionM.getOrElse(opt, _) => opt.node match {
      // Rule: Some(x).getOrElse(_) ==> x
      case SPCM.some(x) => x
      case WOptionConst(Some(x), lA) => lA.lift(x)
      case _ => super.rewriteDef(d)
    }

    case _ => super.rewriteDef(d)
  }

  override def invokeUnlifted(e: Elem[_], mc: MethodCall, dataEnv: DataEnv): Any = e match {
    case _: CollElem[_,_] => mc match {
      case CollMethods.map(xs, f) =>
        val newMC = mc.copy(args = mc.args :+ f.elem.eRange)(mc.resultType, mc.isAdapterCall)
        super.invokeUnlifted(e, newMC, dataEnv)
      case _ =>
        super.invokeUnlifted(e, mc, dataEnv)
    }
    case _ =>
      super.invokeUnlifted(e, mc, dataEnv)
  }

}
