package special.wrappers

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait WrappersSpecDefs extends scalan.Scalan with WrappersSpec {
  self: Library =>
import WOption._
import WRType._
import WSpecialPredef._
import WrapSpecBase._
import OptionWrapSpec._
import RTypeWrapSpec._
import SpecialPredefWrapSpec._

object WrapSpecBase extends EntityObject("WrapSpecBase") {
  private val WrapSpecBaseClass = classOf[WrapSpecBase]

  // entityAdapter for WrapSpecBase trait
  case class WrapSpecBaseAdapter(source: Ref[WrapSpecBase])
      extends WrapSpecBase
      with Def[WrapSpecBase] {
    val resultType: Elem[WrapSpecBase] = element[WrapSpecBase]
    override def transform(t: Transformer) = WrapSpecBaseAdapter(t(source))
  }

  // entityUnref: single unref method for each type family
  implicit def unrefWrapSpecBase(p: Ref[WrapSpecBase]): WrapSpecBase = {
    if (p.node.isInstanceOf[WrapSpecBase]) p.node.asInstanceOf[WrapSpecBase]
    else
      WrapSpecBaseAdapter(p)
  }

  // familyElem
  class WrapSpecBaseElem[To <: WrapSpecBase]
    extends EntityElem[To] {
  }

  implicit lazy val wrapSpecBaseElement: Elem[WrapSpecBase] =
    new WrapSpecBaseElem[WrapSpecBase]

  implicit case object WrapSpecBaseCompanionElem extends CompanionElem[WrapSpecBaseCompanionCtor]

  abstract class WrapSpecBaseCompanionCtor extends CompanionDef[WrapSpecBaseCompanionCtor] with WrapSpecBaseCompanion {
    def resultType = WrapSpecBaseCompanionElem
    override def toString = "WrapSpecBase"
  }
  implicit def unrefWrapSpecBaseCompanionCtor(p: Ref[WrapSpecBaseCompanionCtor]): WrapSpecBaseCompanionCtor =
    p.node.asInstanceOf[WrapSpecBaseCompanionCtor]

  lazy val RWrapSpecBase: Ref[WrapSpecBaseCompanionCtor] = new WrapSpecBaseCompanionCtor {
    private val thisClass = classOf[WrapSpecBaseCompanion]
  }
} // of object WrapSpecBase
  registerEntityObject("WrapSpecBase", WrapSpecBase)

object OptionWrapSpec extends EntityObject("OptionWrapSpec") {
  private val OptionWrapSpecClass = classOf[OptionWrapSpec]

  // entityAdapter for OptionWrapSpec trait
  case class OptionWrapSpecAdapter(source: Ref[OptionWrapSpec])
      extends OptionWrapSpec
      with Def[OptionWrapSpec] {
    val resultType: Elem[OptionWrapSpec] = element[OptionWrapSpec]
    override def transform(t: Transformer) = OptionWrapSpecAdapter(t(source))

    override def getOrElse[A](xs: Ref[WOption[A]], default: Ref[Thunk[A]]): Ref[A] = {
      implicit val eA = xs.eA
      asRep[A](mkMethodCall(source,
        OptionWrapSpecClass.getMethod("getOrElse", classOf[Sym], classOf[Sym]),
        Array[AnyRef](xs, default),
        true, true, element[A]))
    }

    override def fold[A, B](xs: Ref[WOption[A]], ifEmpty: Ref[Thunk[B]], f: Ref[A => B]): Ref[B] = {
      implicit val eA = xs.eA
implicit val eB = ifEmpty.elem.eItem
      asRep[B](mkMethodCall(source,
        OptionWrapSpecClass.getMethod("fold", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](xs, ifEmpty, f),
        true, true, element[B]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefOptionWrapSpec(p: Ref[OptionWrapSpec]): OptionWrapSpec = {
    if (p.node.isInstanceOf[OptionWrapSpec]) p.node.asInstanceOf[OptionWrapSpec]
    else
      OptionWrapSpecAdapter(p)
  }

  // familyElem
  class OptionWrapSpecElem[To <: OptionWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
  }

  implicit lazy val optionWrapSpecElement: Elem[OptionWrapSpec] =
    new OptionWrapSpecElem[OptionWrapSpec]

  implicit case object OptionWrapSpecCompanionElem extends CompanionElem[OptionWrapSpecCompanionCtor]

  abstract class OptionWrapSpecCompanionCtor extends CompanionDef[OptionWrapSpecCompanionCtor] with OptionWrapSpecCompanion {
    def resultType = OptionWrapSpecCompanionElem
    override def toString = "OptionWrapSpec"
  }
  implicit def unrefOptionWrapSpecCompanionCtor(p: Ref[OptionWrapSpecCompanionCtor]): OptionWrapSpecCompanionCtor =
    p.node.asInstanceOf[OptionWrapSpecCompanionCtor]

  lazy val ROptionWrapSpec: Ref[OptionWrapSpecCompanionCtor] = new OptionWrapSpecCompanionCtor {
    private val thisClass = classOf[OptionWrapSpecCompanion]
  }
} // of object OptionWrapSpec
  registerEntityObject("OptionWrapSpec", OptionWrapSpec)

object SpecialPredefWrapSpec extends EntityObject("SpecialPredefWrapSpec") {
  private val SpecialPredefWrapSpecClass = classOf[SpecialPredefWrapSpec]

  // entityAdapter for SpecialPredefWrapSpec trait
  case class SpecialPredefWrapSpecAdapter(source: Ref[SpecialPredefWrapSpec])
      extends SpecialPredefWrapSpec
      with Def[SpecialPredefWrapSpec] {
    val resultType: Elem[SpecialPredefWrapSpec] = element[SpecialPredefWrapSpec]
    override def transform(t: Transformer) = SpecialPredefWrapSpecAdapter(t(source))
  }

  // entityUnref: single unref method for each type family
  implicit def unrefSpecialPredefWrapSpec(p: Ref[SpecialPredefWrapSpec]): SpecialPredefWrapSpec = {
    if (p.node.isInstanceOf[SpecialPredefWrapSpec]) p.node.asInstanceOf[SpecialPredefWrapSpec]
    else
      SpecialPredefWrapSpecAdapter(p)
  }

  // familyElem
  class SpecialPredefWrapSpecElem[To <: SpecialPredefWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
  }

  implicit lazy val specialPredefWrapSpecElement: Elem[SpecialPredefWrapSpec] =
    new SpecialPredefWrapSpecElem[SpecialPredefWrapSpec]

  implicit case object SpecialPredefWrapSpecCompanionElem extends CompanionElem[SpecialPredefWrapSpecCompanionCtor]

  abstract class SpecialPredefWrapSpecCompanionCtor extends CompanionDef[SpecialPredefWrapSpecCompanionCtor] with SpecialPredefWrapSpecCompanion {
    def resultType = SpecialPredefWrapSpecCompanionElem
    override def toString = "SpecialPredefWrapSpec"
  }
  implicit def unrefSpecialPredefWrapSpecCompanionCtor(p: Ref[SpecialPredefWrapSpecCompanionCtor]): SpecialPredefWrapSpecCompanionCtor =
    p.node.asInstanceOf[SpecialPredefWrapSpecCompanionCtor]

  lazy val RSpecialPredefWrapSpec: Ref[SpecialPredefWrapSpecCompanionCtor] = new SpecialPredefWrapSpecCompanionCtor {
    private val thisClass = classOf[SpecialPredefWrapSpecCompanion]
  }
} // of object SpecialPredefWrapSpec
  registerEntityObject("SpecialPredefWrapSpec", SpecialPredefWrapSpec)

object RTypeWrapSpec extends EntityObject("RTypeWrapSpec") {
  private val RTypeWrapSpecClass = classOf[RTypeWrapSpec]

  // entityAdapter for RTypeWrapSpec trait
  case class RTypeWrapSpecAdapter(source: Ref[RTypeWrapSpec])
      extends RTypeWrapSpec
      with Def[RTypeWrapSpec] {
    val resultType: Elem[RTypeWrapSpec] = element[RTypeWrapSpec]
    override def transform(t: Transformer) = RTypeWrapSpecAdapter(t(source))
  }

  // entityUnref: single unref method for each type family
  implicit def unrefRTypeWrapSpec(p: Ref[RTypeWrapSpec]): RTypeWrapSpec = {
    if (p.node.isInstanceOf[RTypeWrapSpec]) p.node.asInstanceOf[RTypeWrapSpec]
    else
      RTypeWrapSpecAdapter(p)
  }

  // familyElem
  class RTypeWrapSpecElem[To <: RTypeWrapSpec]
    extends WrapSpecBaseElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(wrapSpecBaseElement)
  }

  implicit lazy val rTypeWrapSpecElement: Elem[RTypeWrapSpec] =
    new RTypeWrapSpecElem[RTypeWrapSpec]

  implicit case object RTypeWrapSpecCompanionElem extends CompanionElem[RTypeWrapSpecCompanionCtor]

  abstract class RTypeWrapSpecCompanionCtor extends CompanionDef[RTypeWrapSpecCompanionCtor] with RTypeWrapSpecCompanion {
    def resultType = RTypeWrapSpecCompanionElem
    override def toString = "RTypeWrapSpec"
  }
  implicit def unrefRTypeWrapSpecCompanionCtor(p: Ref[RTypeWrapSpecCompanionCtor]): RTypeWrapSpecCompanionCtor =
    p.node.asInstanceOf[RTypeWrapSpecCompanionCtor]

  lazy val RRTypeWrapSpec: Ref[RTypeWrapSpecCompanionCtor] = new RTypeWrapSpecCompanionCtor {
    private val thisClass = classOf[RTypeWrapSpecCompanion]
  }
} // of object RTypeWrapSpec
  registerEntityObject("RTypeWrapSpec", RTypeWrapSpec)

  registerModule(WrappersSpecModule)
}

object WrappersSpecModule extends scalan.ModuleInfo("special.wrappers", "WrappersSpec")
}

trait WrappersSpecModule extends special.wrappers.impl.WrappersSpecDefs {self: Library =>}
