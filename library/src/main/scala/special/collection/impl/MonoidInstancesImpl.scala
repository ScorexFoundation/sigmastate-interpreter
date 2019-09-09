package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait MonoidInstancesDefs extends scalan.Scalan with MonoidInstances {
  self: Library =>
import IntMaxMonoid._
import IntMinMonoid._
import IntPlusMonoid._
import LongMaxMonoid._
import LongMinMonoid._
import LongPlusMonoid._
import Monoid._
import MonoidBuilder._
import PairMonoid._
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

object IntMaxMonoid extends EntityObject("IntMaxMonoid") {
  case class IntMaxMonoidCtor
      (override val zero: Ref[Int])
    extends IntMaxMonoid(zero) with Def[IntMaxMonoid] {
    override lazy val eT: Elem[Int] = implicitly[Elem[Int]]
    lazy val resultType = element[IntMaxMonoid]
    override def transform(t: Transformer) = IntMaxMonoidCtor(t(zero))
    private val thisClass = classOf[Monoid[_]]

    override def plus(x: Ref[Int], y: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("plus", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, y),
        true, false, element[Int]))
    }

    override def power(x: Ref[Int], n: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("power", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, n),
        true, false, element[Int]))
    }
  }

  // state representation type
  type IntMaxMonoidData = Int

  // elem for concrete class
  class IntMaxMonoidElem
    extends MonoidElem[Int, IntMaxMonoid]
    with ConcreteElem[IntMaxMonoidData, IntMaxMonoid] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(IntElement))
  }

  implicit lazy val intMaxMonoidElement: Elem[IntMaxMonoid] =
    new IntMaxMonoidElem

  // 4) constructor and deconstructor
  class IntMaxMonoidCompanionCtor extends CompanionDef[IntMaxMonoidCompanionCtor] with IntMaxMonoidCompanion {
    def resultType = IntMaxMonoidCompanionElem
    override def toString = "IntMaxMonoidCompanion"

    @scalan.OverloadId("fromFields")
    def apply(zero: Ref[Int]): Ref[IntMaxMonoid] =
      mkIntMaxMonoid(zero)

    def unapply(p: Ref[Monoid[Int]]) = unmkIntMaxMonoid(p)
  }
  lazy val RIntMaxMonoid: MutableLazy[IntMaxMonoidCompanionCtor] = MutableLazy(new IntMaxMonoidCompanionCtor)
  implicit final def unrefIntMaxMonoidCompanion(p: Ref[IntMaxMonoidCompanionCtor]): IntMaxMonoidCompanionCtor = {
    if (p.node.isInstanceOf[IntMaxMonoidCompanionCtor])
      p.node.asInstanceOf[IntMaxMonoidCompanionCtor]
    else
      unrefDelegate[IntMaxMonoidCompanionCtor](p)
  }

  implicit case object IntMaxMonoidCompanionElem extends CompanionElem[IntMaxMonoidCompanionCtor]

  implicit final def unrefIntMaxMonoid(p: Ref[IntMaxMonoid]): IntMaxMonoid = {
    if (p.node.isInstanceOf[IntMaxMonoid])
      p.node.asInstanceOf[IntMaxMonoid]
    else
      unrefDelegate[IntMaxMonoid](p)
  }

  def mkIntMaxMonoid
    (zero: Ref[Int]): Ref[IntMaxMonoid] = {
    new IntMaxMonoidCtor(zero)
  }
  def unmkIntMaxMonoid(p: Ref[Monoid[Int]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IntMaxMonoidElem @unchecked =>
      Some((asRep[IntMaxMonoid](p).zero))
    case _ =>
      None
  }
} // of object IntMaxMonoid
  registerEntityObject("IntMaxMonoid", IntMaxMonoid)

object IntMinMonoid extends EntityObject("IntMinMonoid") {
  case class IntMinMonoidCtor
      (override val zero: Ref[Int])
    extends IntMinMonoid(zero) with Def[IntMinMonoid] {
    override lazy val eT: Elem[Int] = implicitly[Elem[Int]]
    lazy val resultType = element[IntMinMonoid]
    override def transform(t: Transformer) = IntMinMonoidCtor(t(zero))
    private val thisClass = classOf[Monoid[_]]

    override def plus(x: Ref[Int], y: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("plus", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, y),
        true, false, element[Int]))
    }

    override def power(x: Ref[Int], n: Ref[Int]): Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("power", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, n),
        true, false, element[Int]))
    }
  }

  // state representation type
  type IntMinMonoidData = Int

  // elem for concrete class
  class IntMinMonoidElem
    extends MonoidElem[Int, IntMinMonoid]
    with ConcreteElem[IntMinMonoidData, IntMinMonoid] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(IntElement))
  }

  implicit lazy val intMinMonoidElement: Elem[IntMinMonoid] =
    new IntMinMonoidElem

  // 4) constructor and deconstructor
  class IntMinMonoidCompanionCtor extends CompanionDef[IntMinMonoidCompanionCtor] with IntMinMonoidCompanion {
    def resultType = IntMinMonoidCompanionElem
    override def toString = "IntMinMonoidCompanion"

    @scalan.OverloadId("fromFields")
    def apply(zero: Ref[Int]): Ref[IntMinMonoid] =
      mkIntMinMonoid(zero)

    def unapply(p: Ref[Monoid[Int]]) = unmkIntMinMonoid(p)
  }
  lazy val RIntMinMonoid: MutableLazy[IntMinMonoidCompanionCtor] = MutableLazy(new IntMinMonoidCompanionCtor)
  implicit final def unrefIntMinMonoidCompanion(p: Ref[IntMinMonoidCompanionCtor]): IntMinMonoidCompanionCtor = {
    if (p.node.isInstanceOf[IntMinMonoidCompanionCtor])
      p.node.asInstanceOf[IntMinMonoidCompanionCtor]
    else
      unrefDelegate[IntMinMonoidCompanionCtor](p)
  }

  implicit case object IntMinMonoidCompanionElem extends CompanionElem[IntMinMonoidCompanionCtor]

  implicit final def unrefIntMinMonoid(p: Ref[IntMinMonoid]): IntMinMonoid = {
    if (p.node.isInstanceOf[IntMinMonoid])
      p.node.asInstanceOf[IntMinMonoid]
    else
      unrefDelegate[IntMinMonoid](p)
  }

  def mkIntMinMonoid
    (zero: Ref[Int]): Ref[IntMinMonoid] = {
    new IntMinMonoidCtor(zero)
  }
  def unmkIntMinMonoid(p: Ref[Monoid[Int]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IntMinMonoidElem @unchecked =>
      Some((asRep[IntMinMonoid](p).zero))
    case _ =>
      None
  }
} // of object IntMinMonoid
  registerEntityObject("IntMinMonoid", IntMinMonoid)

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

object LongMaxMonoid extends EntityObject("LongMaxMonoid") {
  case class LongMaxMonoidCtor
      (override val zero: Ref[Long])
    extends LongMaxMonoid(zero) with Def[LongMaxMonoid] {
    override lazy val eT: Elem[Long] = implicitly[Elem[Long]]
    lazy val resultType = element[LongMaxMonoid]
    override def transform(t: Transformer) = LongMaxMonoidCtor(t(zero))
    private val thisClass = classOf[Monoid[_]]

    override def plus(x: Ref[Long], y: Ref[Long]): Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("plus", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, y),
        true, false, element[Long]))
    }

    override def power(x: Ref[Long], n: Ref[Int]): Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("power", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, n),
        true, false, element[Long]))
    }
  }

  // state representation type
  type LongMaxMonoidData = Long

  // elem for concrete class
  class LongMaxMonoidElem
    extends MonoidElem[Long, LongMaxMonoid]
    with ConcreteElem[LongMaxMonoidData, LongMaxMonoid] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(LongElement))
  }

  implicit lazy val longMaxMonoidElement: Elem[LongMaxMonoid] =
    new LongMaxMonoidElem

  // 4) constructor and deconstructor
  class LongMaxMonoidCompanionCtor extends CompanionDef[LongMaxMonoidCompanionCtor] with LongMaxMonoidCompanion {
    def resultType = LongMaxMonoidCompanionElem
    override def toString = "LongMaxMonoidCompanion"

    @scalan.OverloadId("fromFields")
    def apply(zero: Ref[Long]): Ref[LongMaxMonoid] =
      mkLongMaxMonoid(zero)

    def unapply(p: Ref[Monoid[Long]]) = unmkLongMaxMonoid(p)
  }
  lazy val RLongMaxMonoid: MutableLazy[LongMaxMonoidCompanionCtor] = MutableLazy(new LongMaxMonoidCompanionCtor)
  implicit final def unrefLongMaxMonoidCompanion(p: Ref[LongMaxMonoidCompanionCtor]): LongMaxMonoidCompanionCtor = {
    if (p.node.isInstanceOf[LongMaxMonoidCompanionCtor])
      p.node.asInstanceOf[LongMaxMonoidCompanionCtor]
    else
      unrefDelegate[LongMaxMonoidCompanionCtor](p)
  }

  implicit case object LongMaxMonoidCompanionElem extends CompanionElem[LongMaxMonoidCompanionCtor]

  implicit final def unrefLongMaxMonoid(p: Ref[LongMaxMonoid]): LongMaxMonoid = {
    if (p.node.isInstanceOf[LongMaxMonoid])
      p.node.asInstanceOf[LongMaxMonoid]
    else
      unrefDelegate[LongMaxMonoid](p)
  }

  def mkLongMaxMonoid
    (zero: Ref[Long]): Ref[LongMaxMonoid] = {
    new LongMaxMonoidCtor(zero)
  }
  def unmkLongMaxMonoid(p: Ref[Monoid[Long]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: LongMaxMonoidElem @unchecked =>
      Some((asRep[LongMaxMonoid](p).zero))
    case _ =>
      None
  }
} // of object LongMaxMonoid
  registerEntityObject("LongMaxMonoid", LongMaxMonoid)

object LongMinMonoid extends EntityObject("LongMinMonoid") {
  case class LongMinMonoidCtor
      (override val zero: Ref[Long])
    extends LongMinMonoid(zero) with Def[LongMinMonoid] {
    override lazy val eT: Elem[Long] = implicitly[Elem[Long]]
    lazy val resultType = element[LongMinMonoid]
    override def transform(t: Transformer) = LongMinMonoidCtor(t(zero))
    private val thisClass = classOf[Monoid[_]]

    override def plus(x: Ref[Long], y: Ref[Long]): Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("plus", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, y),
        true, false, element[Long]))
    }

    override def power(x: Ref[Long], n: Ref[Int]): Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("power", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, n),
        true, false, element[Long]))
    }
  }

  // state representation type
  type LongMinMonoidData = Long

  // elem for concrete class
  class LongMinMonoidElem
    extends MonoidElem[Long, LongMinMonoid]
    with ConcreteElem[LongMinMonoidData, LongMinMonoid] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(LongElement))
  }

  implicit lazy val longMinMonoidElement: Elem[LongMinMonoid] =
    new LongMinMonoidElem

  // 4) constructor and deconstructor
  class LongMinMonoidCompanionCtor extends CompanionDef[LongMinMonoidCompanionCtor] with LongMinMonoidCompanion {
    def resultType = LongMinMonoidCompanionElem
    override def toString = "LongMinMonoidCompanion"

    @scalan.OverloadId("fromFields")
    def apply(zero: Ref[Long]): Ref[LongMinMonoid] =
      mkLongMinMonoid(zero)

    def unapply(p: Ref[Monoid[Long]]) = unmkLongMinMonoid(p)
  }
  lazy val RLongMinMonoid: MutableLazy[LongMinMonoidCompanionCtor] = MutableLazy(new LongMinMonoidCompanionCtor)
  implicit final def unrefLongMinMonoidCompanion(p: Ref[LongMinMonoidCompanionCtor]): LongMinMonoidCompanionCtor = {
    if (p.node.isInstanceOf[LongMinMonoidCompanionCtor])
      p.node.asInstanceOf[LongMinMonoidCompanionCtor]
    else
      unrefDelegate[LongMinMonoidCompanionCtor](p)
  }

  implicit case object LongMinMonoidCompanionElem extends CompanionElem[LongMinMonoidCompanionCtor]

  implicit final def unrefLongMinMonoid(p: Ref[LongMinMonoid]): LongMinMonoid = {
    if (p.node.isInstanceOf[LongMinMonoid])
      p.node.asInstanceOf[LongMinMonoid]
    else
      unrefDelegate[LongMinMonoid](p)
  }

  def mkLongMinMonoid
    (zero: Ref[Long]): Ref[LongMinMonoid] = {
    new LongMinMonoidCtor(zero)
  }
  def unmkLongMinMonoid(p: Ref[Monoid[Long]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: LongMinMonoidElem @unchecked =>
      Some((asRep[LongMinMonoid](p).zero))
    case _ =>
      None
  }
} // of object LongMinMonoid
  registerEntityObject("LongMinMonoid", LongMinMonoid)

object PairMonoid extends EntityObject("PairMonoid") {
  case class PairMonoidCtor[A, B]
      (override val m1: Ref[Monoid[A]], override val m2: Ref[Monoid[B]])
    extends PairMonoid[A, B](m1, m2) with Def[PairMonoid[A, B]] {
    implicit lazy val eA = m1.eT;
implicit lazy val eB = m2.eT
    override lazy val eT: Elem[(A, B)] = implicitly[Elem[(A, B)]]
    lazy val resultType = element[PairMonoid[A, B]]
    override def transform(t: Transformer) = PairMonoidCtor[A, B](t(m1), t(m2))
  }

  // state representation type
  type PairMonoidData[A, B] = (Monoid[A], Monoid[B])

  // elem for concrete class
  class PairMonoidElem[A, B](implicit val eA: Elem[A], val eB: Elem[B])
    extends MonoidElem[(A, B), PairMonoid[A, B]]
    with ConcreteElem[PairMonoidData[A, B], PairMonoid[A, B]] {
    override lazy val parent: Option[Elem[_]] = Some(monoidElement(pairElement(element[A],element[B])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("A" -> (eA -> scalan.util.Invariant), "B" -> (eB -> scalan.util.Invariant))
  }

  implicit final def pairMonoidElement[A, B](implicit eA: Elem[A], eB: Elem[B]): Elem[PairMonoid[A, B]] =
    cachedElemByClass(eA, eB)(classOf[PairMonoidElem[A, B]])

  // 4) constructor and deconstructor
  class PairMonoidCompanionCtor extends CompanionDef[PairMonoidCompanionCtor] with PairMonoidCompanion {
    def resultType = PairMonoidCompanionElem
    override def toString = "PairMonoidCompanion"
    @scalan.OverloadId("fromData")
    def apply[A, B](p: Ref[PairMonoidData[A, B]]): Ref[PairMonoid[A, B]] = {
      implicit val eA = p._1.eT;
implicit val eB = p._2.eT
      val Pair(m1, m2) = p
      mkPairMonoid(m1, m2)
    }

    @scalan.OverloadId("fromFields")
    def apply[A, B](m1: Ref[Monoid[A]], m2: Ref[Monoid[B]]): Ref[PairMonoid[A, B]] =
      mkPairMonoid(m1, m2)

    def unapply[A, B](p: Ref[Monoid[(A, B)]]) = unmkPairMonoid(p)
  }
  lazy val RPairMonoid: MutableLazy[PairMonoidCompanionCtor] = MutableLazy(new PairMonoidCompanionCtor)
  implicit final def unrefPairMonoidCompanion(p: Ref[PairMonoidCompanionCtor]): PairMonoidCompanionCtor = {
    if (p.node.isInstanceOf[PairMonoidCompanionCtor])
      p.node.asInstanceOf[PairMonoidCompanionCtor]
    else
      unrefDelegate[PairMonoidCompanionCtor](p)
  }

  implicit case object PairMonoidCompanionElem extends CompanionElem[PairMonoidCompanionCtor]

  implicit final def unrefPairMonoid[A, B](p: Ref[PairMonoid[A, B]]): PairMonoid[A, B] = {
    if (p.node.isInstanceOf[PairMonoid[A, B]@unchecked])
      p.node.asInstanceOf[PairMonoid[A, B]]
    else
      unrefDelegate[PairMonoid[A, B]](p)
  }

  def mkPairMonoid[A, B]
    (m1: Ref[Monoid[A]], m2: Ref[Monoid[B]]): Ref[PairMonoid[A, B]] = {
    new PairMonoidCtor[A, B](m1, m2)
  }
  def unmkPairMonoid[A, B](p: Ref[Monoid[(A, B)]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: PairMonoidElem[A, B] @unchecked =>
      Some((asRep[PairMonoid[A, B]](p).m1, asRep[PairMonoid[A, B]](p).m2))
    case _ =>
      None
  }
} // of object PairMonoid
  registerEntityObject("PairMonoid", PairMonoid)

  override def resetContext(): Unit = {
    super.resetContext()

    RMonoidBuilderInst.reset()
    RIntPlusMonoid.reset()
    RIntMaxMonoid.reset()
    RIntMinMonoid.reset()
    RLongPlusMonoid.reset()
    RLongMaxMonoid.reset()
    RLongMinMonoid.reset()
    RPairMonoid.reset()
  }

  registerModule(MonoidInstancesModule)
}

object MonoidInstancesModule extends scalan.ModuleInfo("special.collection", "MonoidInstances")
}

trait MonoidInstancesModule extends special.collection.impl.MonoidInstancesDefs {self: Library =>}
