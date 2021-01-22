package special.collection

import scala.language.{existentials,implicitConversions}
import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait MonoidInstancesDefs extends scalan.Scalan with MonoidInstances {
  self: Library =>
import IntPlusMonoid._
import LongPlusMonoid._
import Monoid._
import MonoidBuilder._
import MonoidBuilderInst._

object MonoidBuilderInst extends EntityObject("MonoidBuilderInst") {
  case class MonoidBuilderInstCtor
      ()
    extends MonoidBuilderInst() with Def[MonoidBuilderInst] {
    lazy val resultType = element[MonoidBuilderInst]
    override def transform(t: Transformer) = MonoidBuilderInstCtor()
  }

  // state representation type
  type MonoidBuilderInstData = Unit

  // elem for concrete class
  class MonoidBuilderInstElem
    extends MonoidBuilderElem[MonoidBuilderInst]
    with ConcreteElem[MonoidBuilderInstData, MonoidBuilderInst] {
    override lazy val parent: Option[Elem[_]] = Some(monoidBuilderElement)
  }

  implicit lazy val monoidBuilderInstElement: Elem[MonoidBuilderInst] =
    new MonoidBuilderInstElem

  // 4) constructor and deconstructor
  class MonoidBuilderInstCompanionCtor extends CompanionDef[MonoidBuilderInstCompanionCtor] with MonoidBuilderInstCompanion {
    def resultType = MonoidBuilderInstCompanionElem
    override def toString = "MonoidBuilderInstCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[MonoidBuilderInstData]): Ref[MonoidBuilderInst] = {
      val unit = p
      mkMonoidBuilderInst()
    }

    @scalan.OverloadId("fromFields")
    def apply(): Ref[MonoidBuilderInst] =
      mkMonoidBuilderInst()

    def unapply(p: Ref[MonoidBuilder]) = unmkMonoidBuilderInst(p)
  }
  lazy val RMonoidBuilderInst: MutableLazy[MonoidBuilderInstCompanionCtor] = MutableLazy(new MonoidBuilderInstCompanionCtor)
  implicit final def unrefMonoidBuilderInstCompanion(p: Ref[MonoidBuilderInstCompanionCtor]): MonoidBuilderInstCompanionCtor = {
    if (p.node.isInstanceOf[MonoidBuilderInstCompanionCtor])
      p.node.asInstanceOf[MonoidBuilderInstCompanionCtor]
    else
      unrefDelegate[MonoidBuilderInstCompanionCtor](p)
  }

  implicit case object MonoidBuilderInstCompanionElem extends CompanionElem[MonoidBuilderInstCompanionCtor]

  implicit final def unrefMonoidBuilderInst(p: Ref[MonoidBuilderInst]): MonoidBuilderInst = {
    if (p.node.isInstanceOf[MonoidBuilderInst])
      p.node.asInstanceOf[MonoidBuilderInst]
    else
      unrefDelegate[MonoidBuilderInst](p)
  }

  def mkMonoidBuilderInst
    (): Ref[MonoidBuilderInst] = {
    new MonoidBuilderInstCtor()
  }
  def unmkMonoidBuilderInst(p: Ref[MonoidBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: MonoidBuilderInstElem @unchecked =>
      Some(())
    case _ =>
      None
  }
} // of object MonoidBuilderInst
  registerEntityObject("MonoidBuilderInst", MonoidBuilderInst)

object IntPlusMonoid extends EntityObject("IntPlusMonoid") {
  case class IntPlusMonoidCtor
      (override val zero: Ref[Int])
    extends IntPlusMonoid(zero) with Def[IntPlusMonoid] {
    override lazy val eT: Elem[Int] = implicitly[Elem[Int]]
    lazy val resultType = element[IntPlusMonoid]
    override def transform(t: Transformer) = IntPlusMonoidCtor(t(zero))
  }

  // state representation type
  type IntPlusMonoidData = Int

  // elem for concrete class
  class IntPlusMonoidElem
    extends MonoidElem[Int, IntPlusMonoid]
    with ConcreteElem[IntPlusMonoidData, IntPlusMonoid] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(IntElement))
  }

  implicit lazy val intPlusMonoidElement: Elem[IntPlusMonoid] =
    new IntPlusMonoidElem

  // 4) constructor and deconstructor
  class IntPlusMonoidCompanionCtor extends CompanionDef[IntPlusMonoidCompanionCtor] with IntPlusMonoidCompanion {
    def resultType = IntPlusMonoidCompanionElem
    override def toString = "IntPlusMonoidCompanion"

    @scalan.OverloadId("fromFields")
    def apply(zero: Ref[Int]): Ref[IntPlusMonoid] =
      mkIntPlusMonoid(zero)

    def unapply(p: Ref[Monoid[Int]]) = unmkIntPlusMonoid(p)
  }
  lazy val RIntPlusMonoid: MutableLazy[IntPlusMonoidCompanionCtor] = MutableLazy(new IntPlusMonoidCompanionCtor)
  implicit final def unrefIntPlusMonoidCompanion(p: Ref[IntPlusMonoidCompanionCtor]): IntPlusMonoidCompanionCtor = {
    if (p.node.isInstanceOf[IntPlusMonoidCompanionCtor])
      p.node.asInstanceOf[IntPlusMonoidCompanionCtor]
    else
      unrefDelegate[IntPlusMonoidCompanionCtor](p)
  }

  implicit case object IntPlusMonoidCompanionElem extends CompanionElem[IntPlusMonoidCompanionCtor]

  implicit final def unrefIntPlusMonoid(p: Ref[IntPlusMonoid]): IntPlusMonoid = {
    if (p.node.isInstanceOf[IntPlusMonoid])
      p.node.asInstanceOf[IntPlusMonoid]
    else
      unrefDelegate[IntPlusMonoid](p)
  }

  def mkIntPlusMonoid
    (zero: Ref[Int]): Ref[IntPlusMonoid] = {
    new IntPlusMonoidCtor(zero)
  }
  def unmkIntPlusMonoid(p: Ref[Monoid[Int]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IntPlusMonoidElem @unchecked =>
      Some((asRep[IntPlusMonoid](p).zero))
    case _ =>
      None
  }
} // of object IntPlusMonoid
  registerEntityObject("IntPlusMonoid", IntPlusMonoid)

object LongPlusMonoid extends EntityObject("LongPlusMonoid") {
  case class LongPlusMonoidCtor
      (override val zero: Ref[Long])
    extends LongPlusMonoid(zero) with Def[LongPlusMonoid] {
    override lazy val eT: Elem[Long] = implicitly[Elem[Long]]
    lazy val resultType = element[LongPlusMonoid]
    override def transform(t: Transformer) = LongPlusMonoidCtor(t(zero))
  }

  // state representation type
  type LongPlusMonoidData = Long

  // elem for concrete class
  class LongPlusMonoidElem
    extends MonoidElem[Long, LongPlusMonoid]
    with ConcreteElem[LongPlusMonoidData, LongPlusMonoid] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(LongElement))
  }

  implicit lazy val longPlusMonoidElement: Elem[LongPlusMonoid] =
    new LongPlusMonoidElem

  // 4) constructor and deconstructor
  class LongPlusMonoidCompanionCtor extends CompanionDef[LongPlusMonoidCompanionCtor] with LongPlusMonoidCompanion {
    def resultType = LongPlusMonoidCompanionElem
    override def toString = "LongPlusMonoidCompanion"

    @scalan.OverloadId("fromFields")
    def apply(zero: Ref[Long]): Ref[LongPlusMonoid] =
      mkLongPlusMonoid(zero)

    def unapply(p: Ref[Monoid[Long]]) = unmkLongPlusMonoid(p)
  }
  lazy val RLongPlusMonoid: MutableLazy[LongPlusMonoidCompanionCtor] = MutableLazy(new LongPlusMonoidCompanionCtor)
  implicit final def unrefLongPlusMonoidCompanion(p: Ref[LongPlusMonoidCompanionCtor]): LongPlusMonoidCompanionCtor = {
    if (p.node.isInstanceOf[LongPlusMonoidCompanionCtor])
      p.node.asInstanceOf[LongPlusMonoidCompanionCtor]
    else
      unrefDelegate[LongPlusMonoidCompanionCtor](p)
  }

  implicit case object LongPlusMonoidCompanionElem extends CompanionElem[LongPlusMonoidCompanionCtor]

  implicit final def unrefLongPlusMonoid(p: Ref[LongPlusMonoid]): LongPlusMonoid = {
    if (p.node.isInstanceOf[LongPlusMonoid])
      p.node.asInstanceOf[LongPlusMonoid]
    else
      unrefDelegate[LongPlusMonoid](p)
  }

  def mkLongPlusMonoid
    (zero: Ref[Long]): Ref[LongPlusMonoid] = {
    new LongPlusMonoidCtor(zero)
  }
  def unmkLongPlusMonoid(p: Ref[Monoid[Long]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: LongPlusMonoidElem @unchecked =>
      Some((asRep[LongPlusMonoid](p).zero))
    case _ =>
      None
  }
} // of object LongPlusMonoid
  registerEntityObject("LongPlusMonoid", LongPlusMonoid)

  override def resetContext(): Unit = {
    super.resetContext()

    RMonoidBuilderInst.reset()
    RIntPlusMonoid.reset()
    RLongPlusMonoid.reset()
  }

  registerModule(MonoidInstancesModule)
}

object MonoidInstancesModule extends scalan.ModuleInfo("special.collection", "MonoidInstances")
}

trait MonoidInstancesModule extends special.collection.impl.MonoidInstancesDefs {self: Library =>}
