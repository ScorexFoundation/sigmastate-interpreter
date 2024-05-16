package sigma.compiler.ir.wrappers.special

import scala.language.{existentials, implicitConversions}
import scalan._
import sigma.compiler.ir.IRContext

package impl {
  import sigma.compiler.ir.core.MutableLazy
  import sigma.compiler.ir.meta.ModuleInfo
  import sigma.compiler.ir.{Base, GraphIRReflection, IRContext}
  import sigma.data.Nullable
  import sigma.reflection.RClass

  // Abs -----------------------------------
trait WSpecialPredefsDefs extends Base with WSpecialPredefs {
  self: IRContext =>

  registerModule(WSpecialPredefsModule)

import WOption._
import WSpecialPredef._

object WSpecialPredef extends EntityObject("WSpecialPredef") {

  implicit case object WSpecialPredefCompanionElem extends CompanionElem[WSpecialPredefCompanionCtor]

  abstract class WSpecialPredefCompanionCtor extends CompanionDef[WSpecialPredefCompanionCtor] with WSpecialPredefCompanion {
    def resultType = WSpecialPredefCompanionElem
    override def toString = "WSpecialPredef"
  }
  implicit final def unrefWSpecialPredefCompanionCtor(p: Ref[WSpecialPredefCompanionCtor]): WSpecialPredefCompanionCtor =
    p.node.asInstanceOf[WSpecialPredefCompanionCtor]

  lazy val RWSpecialPredef: MutableLazy[WSpecialPredefCompanionCtor] = MutableLazy(new WSpecialPredefCompanionCtor {
    private val thisClass = RClass(classOf[WSpecialPredefCompanion])

    def some[A](x: Ref[A]): Ref[WOption[A]] = {
      implicit val eA = x.elem
      asRep[WOption[A]](mkMethodCall(self,
        thisClass.getMethod("some", classOf[Sym]),
        Array[AnyRef](x),
        true, false, element[WOption[A]]))
    }
  })

  object WSpecialPredefCompanionMethods {
    object some {
      def unapply(d: Def[_]): Nullable[Ref[A] forSome {type A}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "some" && receiver.elem == WSpecialPredefCompanionElem =>
          val res = args(0)
          Nullable(res).asInstanceOf[Nullable[Ref[A] forSome {type A}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[A] forSome {type A}] = unapply(exp.node)
    }
  }
} // of object WSpecialPredef
  registerEntityObject("WSpecialPredef", WSpecialPredef)

  override def resetContext(): Unit = {
    super.resetContext()
    RWSpecialPredef.reset()
  }

}

object WSpecialPredefsModule extends ModuleInfo("sigma.compiler.ir.wrappers.special", "WSpecialPredefs") {
  val reflection = GraphIRReflection
}
}

trait WSpecialPredefsModule extends sigma.compiler.ir.wrappers.special.impl.WSpecialPredefsDefs {self: IRContext =>}
