package special.collection

import scala.language.{existentials,implicitConversions}
import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
import scalan.util.MemoizedFunc // manual fix

// Abs -----------------------------------
trait ConcreteCalcsDefs extends scalan.Scalan with ConcreteCalcs {
  self: Library =>
import CCostedBuilder._
import CCostedColl._
import CCostedFunc._
import CCostedOption._
import CCostedPair._

import CCalcPrim._

import CSizeColl._
import CSizeFunc._
import CSizeOption._
import CSizePair._
import CSizePrim._
import Coll._
import Costed._
import CostedBuilder._
import CostedColl._
import CostedFunc._
import CostedOption._
import CostedPair._
// import CostedPrim._
import CalcPrim._
import MonoidBuilder._
import MonoidBuilderInst._
import Size._
import SizeColl._
import SizeFunc._
import SizeOption._
import SizePair._
import SizePrim._
import WOption._
import WRType._
import WSpecialPredef._

object CCalcPrim extends EntityObject("CCalcPrim") {
  case class CCalcPrimCtor[Val](
    override val value: Ref[Val],
    override val size: Ref[Size[Val]]
  ) extends CCalcPrim[Val](value, size)
    with Def[CCalcPrim[Val]]
  {
    implicit lazy val eVal = value.elem

    // lazy val resultType = element[CCalcPrim[Val]]
    lazy val resultType = ???
    override def transform(t: Transformer) = CCalcPrimCtor[Val](t(value), t(size))

    override def cost: Ref[Int] = ???
  }

  // state representation type (value, size)
  type CCalcPrimData[Val] = (Val, Size[Val])

  // elem for concrete class
  class CCalcPrimElem[Val](implicit override val eVal: Elem[Val])
    extends CalcPrimElem[Val, CCalcPrim[Val]]
    with ConcreteElem[CCalcPrimData[Val], CCalcPrim[Val]] {
    override lazy val parent: Option[Elem[_]] = Some(calcPrimElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }

  implicit final def cCalcPrimElement[Val](implicit eVal: Elem[Val]): Elem[CCalcPrim[Val]] =
    cachedElemByClass(eVal)(classOf[CCalcPrimElem[Val]])

  // 4) constructor and deconstructor
  class CCalcPrimCompanionCtor extends CompanionDef[CCalcPrimCompanionCtor] with CCalcPrimCompanion {
    def resultType = CCalcPrimCompanionElem

    override def toString = "CCalcPrimCompanion"
    
    @scalan.OverloadId("fromData")
    def apply[Val](p: Ref[CCalcPrimData[Val]]): Ref[CCalcPrim[Val]] = {
      implicit val eVal = p._1.elem
      val Pair(value, size) = p
      mkCCalcPrim(value, size)
    }

    // manual fix
    @scalan.OverloadId("fromFields")
    def apply[Val](value: Ref[Val], size: Ref[Size[Val]]): Ref[CCalcPrim[Val]] = {
      // assertValueIdForOpCost(value, cost)
      mkCCalcPrim(value, size)
    }

    def unapply[Val](p: Ref[CalcPrim[Val]]) = unmkCCalcPrim(p)
  }

  lazy val RCCalcPrim: MutableLazy[CCalcPrimCompanionCtor] = MutableLazy(new CCalcPrimCompanionCtor)

  implicit final def unrefCCalcPrimCompanion(p: Ref[CCalcPrimCompanionCtor]): CCalcPrimCompanionCtor = {
    if (p.node.isInstanceOf[CCalcPrimCompanionCtor])
      p.node.asInstanceOf[CCalcPrimCompanionCtor]
    else
      unrefDelegate[CCalcPrimCompanionCtor](p)
  }

  implicit case object CCalcPrimCompanionElem extends CompanionElem[CCalcPrimCompanionCtor]

  implicit final def unrefCCalcPrim[Val](p: Ref[CCalcPrim[Val]]): CCalcPrim[Val] = {
    if (p.node.isInstanceOf[CCalcPrim[Val]@unchecked])
      p.node.asInstanceOf[CCalcPrim[Val]]
    else
      unrefDelegate[CCalcPrim[Val]](p)
  }

  def mkCCalcPrim[Val](value: Ref[Val], size: Ref[Size[Val]]): Ref[CCalcPrim[Val]] = {
    new CCalcPrimCtor[Val](value, size)
  }
  def unmkCCalcPrim[Val](p: Ref[CalcPrim[Val]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCalcPrimElem[Val] @unchecked =>
      Some((asRep[CCalcPrim[Val]](p).value, asRep[CCalcPrim[Val]](p).size))
    case _ =>
      None
  }
} // of object CCalcPrim
  registerEntityObject("CCalcPrim", CCalcPrim)

object CCostedPair extends EntityObject("CCostedPair") {
  case class CCostedPairCtor[L, R]
      (override val l: Ref[Costed[L]], override val r: Ref[Costed[R]], override val accCost: Ref[Int])
    extends CCostedPair[L, R](l, r, accCost) with Def[CCostedPair[L, R]] {
    implicit lazy val eL = l.eVal;
implicit lazy val eR = r.eVal
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    lazy val resultType = element[CCostedPair[L, R]]
    override def transform(t: Transformer) = CCostedPairCtor[L, R](t(l), t(r), t(accCost))
    private val thisClass = classOf[CostedPair[_, _]]

    override def cost: Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        WrappedArray.empty,
        true, false, element[Int]))
    }
  }

  // state representation type
  type CCostedPairData[L, R] = (Costed[L], (Costed[R], Int))

  // elem for concrete class
  class CCostedPairElem[L, R](implicit override val eL: Elem[L], override val eR: Elem[R])
    extends CostedPairElem[L, R, CCostedPair[L, R]]
    with ConcreteElem[CCostedPairData[L, R], CCostedPair[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(costedPairElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }

  implicit final def cCostedPairElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[CCostedPair[L, R]] =
    cachedElemByClass(eL, eR)(classOf[CCostedPairElem[L, R]])

  // 4) constructor and deconstructor
  class CCostedPairCompanionCtor extends CompanionDef[CCostedPairCompanionCtor] with CCostedPairCompanion {
    def resultType = CCostedPairCompanionElem
    override def toString = "CCostedPairCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Ref[CCostedPairData[L, R]]): Ref[CCostedPair[L, R]] = {
      implicit val eL = p._1.eVal;
implicit val eR = p._2.eVal
      val Pair(l, Pair(r, accCost)) = p
      mkCCostedPair(l, r, accCost)
    }

    // manual fix
    @scalan.OverloadId("fromFields")
    def apply[L, R](l: Ref[Costed[L]], r: Ref[Costed[R]], accCost: Ref[Int]): Ref[CCostedPair[L, R]] = {
      assertValueIdForOpCost(Pair(l, r), accCost)
      mkCCostedPair(l, r, accCost)
    }

    def unapply[L, R](p: Ref[CostedPair[L, R]]) = unmkCCostedPair(p)
  }
  lazy val RCCostedPair: MutableLazy[CCostedPairCompanionCtor] = MutableLazy(new CCostedPairCompanionCtor)
  implicit final def unrefCCostedPairCompanion(p: Ref[CCostedPairCompanionCtor]): CCostedPairCompanionCtor = {
    if (p.node.isInstanceOf[CCostedPairCompanionCtor])
      p.node.asInstanceOf[CCostedPairCompanionCtor]
    else
      unrefDelegate[CCostedPairCompanionCtor](p)
  }

  implicit case object CCostedPairCompanionElem extends CompanionElem[CCostedPairCompanionCtor]

  implicit final def unrefCCostedPair[L, R](p: Ref[CCostedPair[L, R]]): CCostedPair[L, R] = {
    if (p.node.isInstanceOf[CCostedPair[L, R]@unchecked])
      p.node.asInstanceOf[CCostedPair[L, R]]
    else
      unrefDelegate[CCostedPair[L, R]](p)
  }

  def mkCCostedPair[L, R]
    (l: Ref[Costed[L]], r: Ref[Costed[R]], accCost: Ref[Int]): Ref[CCostedPair[L, R]] = {
    new CCostedPairCtor[L, R](l, r, accCost)
  }
  def unmkCCostedPair[L, R](p: Ref[CostedPair[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedPairElem[L, R] @unchecked =>
      Some((asRep[CCostedPair[L, R]](p).l, asRep[CCostedPair[L, R]](p).r, asRep[CCostedPair[L, R]](p).accCost))
    case _ =>
      None
  }
} // of object CCostedPair
  registerEntityObject("CCostedPair", CCostedPair)

object CCostedFunc extends EntityObject("CCostedFunc") {
  case class CCostedFuncCtor[Env, Arg, Res]
      (override val envCosted: Ref[Costed[Env]], override val func: Ref[Costed[Arg] => Costed[Res]], override val cost: Ref[Int], override val size: Ref[Size[Arg => Res]])
    extends CCostedFunc[Env, Arg, Res](envCosted, func, cost, size) with Def[CCostedFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = envCosted.eVal;
implicit lazy val eArg = func.elem.eDom.typeArgs("Val")._1.asInstanceOf[Elem[Arg]];
implicit lazy val eRes = func.elem.eRange.typeArgs("Val")._1.asInstanceOf[Elem[Res]]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    lazy val resultType = element[CCostedFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = CCostedFuncCtor[Env, Arg, Res](t(envCosted), t(func), t(cost), t(size))
    private val thisClass = classOf[CostedFunc[_, _, _]]

    override def value: Ref[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(self,
        thisClass.getMethod("value"),
        WrappedArray.empty,
        true, false, element[Arg => Res]))
    }
  }

  // state representation type
  type CCostedFuncData[Env, Arg, Res] = (Costed[Env], (Costed[Arg] => Costed[Res], (Int, Size[Arg => Res])))

  // elem for concrete class
  class CCostedFuncElem[Env, Arg, Res](implicit override val eEnv: Elem[Env], override val eArg: Elem[Arg], override val eRes: Elem[Res])
    extends CostedFuncElem[Env, Arg, Res, CCostedFunc[Env, Arg, Res]]
    with ConcreteElem[CCostedFuncData[Env, Arg, Res], CCostedFunc[Env, Arg, Res]] {
    override lazy val parent: Option[Elem[_]] = Some(costedFuncElement(element[Env], element[Arg], element[Res]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
  }

  implicit final def cCostedFuncElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[CCostedFunc[Env, Arg, Res]] =
    cachedElemByClass(eEnv, eArg, eRes)(classOf[CCostedFuncElem[Env, Arg, Res]])

  // 4) constructor and deconstructor
  class CCostedFuncCompanionCtor extends CompanionDef[CCostedFuncCompanionCtor] with CCostedFuncCompanion {
    def resultType = CCostedFuncCompanionElem
    override def toString = "CCostedFuncCompanion"
    @scalan.OverloadId("fromData")
    def apply[Env, Arg, Res](p: Ref[CCostedFuncData[Env, Arg, Res]]): Ref[CCostedFunc[Env, Arg, Res]] = {
      implicit val eEnv = p._1.eVal;
implicit val eArg = p._2.elem.eDom.typeArgs("Val")._1.asInstanceOf[Elem[Arg]];
implicit val eRes = p._2.elem.eRange.typeArgs("Val")._1.asInstanceOf[Elem[Res]]
      val Pair(envCosted, Pair(func, Pair(cost, size))) = p
      mkCCostedFunc(envCosted, func, cost, size)
    }

    @scalan.OverloadId("fromFields")
    def apply[Env, Arg, Res](envCosted: Ref[Costed[Env]], func: Ref[Costed[Arg] => Costed[Res]], cost: Ref[Int], size: Ref[Size[Arg => Res]]): Ref[CCostedFunc[Env, Arg, Res]] =
      mkCCostedFunc(envCosted, func, cost, size)

    def unapply[Env, Arg, Res](p: Ref[CostedFunc[Env, Arg, Res]]) = unmkCCostedFunc(p)
  }
  lazy val RCCostedFunc: MutableLazy[CCostedFuncCompanionCtor] = MutableLazy(new CCostedFuncCompanionCtor)
  implicit final def unrefCCostedFuncCompanion(p: Ref[CCostedFuncCompanionCtor]): CCostedFuncCompanionCtor = {
    if (p.node.isInstanceOf[CCostedFuncCompanionCtor])
      p.node.asInstanceOf[CCostedFuncCompanionCtor]
    else
      unrefDelegate[CCostedFuncCompanionCtor](p)
  }

  implicit case object CCostedFuncCompanionElem extends CompanionElem[CCostedFuncCompanionCtor]

  implicit final def unrefCCostedFunc[Env, Arg, Res](p: Ref[CCostedFunc[Env, Arg, Res]]): CCostedFunc[Env, Arg, Res] = {
    if (p.node.isInstanceOf[CCostedFunc[Env, Arg, Res]@unchecked])
      p.node.asInstanceOf[CCostedFunc[Env, Arg, Res]]
    else
      unrefDelegate[CCostedFunc[Env, Arg, Res]](p)
  }

  def mkCCostedFunc[Env, Arg, Res]
    (envCosted: Ref[Costed[Env]], func: Ref[Costed[Arg] => Costed[Res]], cost: Ref[Int], size: Ref[Size[Arg => Res]]): Ref[CCostedFunc[Env, Arg, Res]] = {
    new CCostedFuncCtor[Env, Arg, Res](envCosted, func, cost, size)
  }
  def unmkCCostedFunc[Env, Arg, Res](p: Ref[CostedFunc[Env, Arg, Res]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedFuncElem[Env, Arg, Res] @unchecked =>
      Some((asRep[CCostedFunc[Env, Arg, Res]](p).envCosted, asRep[CCostedFunc[Env, Arg, Res]](p).func, asRep[CCostedFunc[Env, Arg, Res]](p).cost, asRep[CCostedFunc[Env, Arg, Res]](p).size))
    case _ =>
      None
  }
} // of object CCostedFunc
  registerEntityObject("CCostedFunc", CCostedFunc)

object CCostedColl extends EntityObject("CCostedColl") {
  case class CCostedCollCtor[Item]
      (override val values: Ref[Coll[Item]], override val costs: Ref[Coll[Int]], override val sizes: Ref[Coll[Size[Item]]], override val valuesCost: Ref[Int])
    extends CCostedColl[Item](values, costs, sizes, valuesCost) with Def[CCostedColl[Item]] {
    implicit lazy val eItem = values.eA
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    lazy val resultType = element[CCostedColl[Item]]
    override def transform(t: Transformer) = CCostedCollCtor[Item](t(values), t(costs), t(sizes), t(valuesCost))
    private val thisClass = classOf[CostedColl[_]]

    override def cost: Ref[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        WrappedArray.empty,
        true, false, element[Int]))
    }

    override def mapCosted[Res](f: Ref[Costed[Item] => Costed[Res]]): Ref[CostedColl[Res]] = {
      implicit val eRes = f.elem.eRange.typeArgs("Val")._1.asInstanceOf[Elem[Res]]
      asRep[CostedColl[Res]](mkMethodCall(self,
        thisClass.getMethod("mapCosted", classOf[Sym]),
        Array[AnyRef](f),
        true, false, element[CostedColl[Res]]))
    }

    override def filterCosted(f: Ref[Costed[Item] => Costed[Boolean]]): Ref[CostedColl[Item]] = {
      asRep[CostedColl[Item]](mkMethodCall(self,
        thisClass.getMethod("filterCosted", classOf[Sym]),
        Array[AnyRef](f),
        true, false, element[CostedColl[Item]]))
    }

    override def foldCosted[B](zero: Ref[Costed[B]], op: Ref[Costed[(B, Item)] => Costed[B]]): Ref[Costed[B]] = {
      implicit val eB = zero.eVal
      asRep[Costed[B]](mkMethodCall(self,
        thisClass.getMethod("foldCosted", classOf[Sym], classOf[Sym]),
        Array[AnyRef](zero, op),
        true, false, element[Costed[B]]))
    }
  }

  // state representation type
  type CCostedCollData[Item] = (Coll[Item], (Coll[Int], (Coll[Size[Item]], Int)))

  // elem for concrete class
  class CCostedCollElem[Item](implicit override val eItem: Elem[Item])
    extends CostedCollElem[Item, CCostedColl[Item]]
    with ConcreteElem[CCostedCollData[Item], CCostedColl[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(costedCollElement(element[Item]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }

  implicit final def cCostedCollElement[Item](implicit eItem: Elem[Item]): Elem[CCostedColl[Item]] =
    cachedElemByClass(eItem)(classOf[CCostedCollElem[Item]])

  // 4) constructor and deconstructor
  class CCostedCollCompanionCtor extends CompanionDef[CCostedCollCompanionCtor] with CCostedCollCompanion {
    def resultType = CCostedCollCompanionElem
    override def toString = "CCostedCollCompanion"
    @scalan.OverloadId("fromData")
    def apply[Item](p: Ref[CCostedCollData[Item]]): Ref[CCostedColl[Item]] = {
      implicit val eItem = p._1.eA
      val Pair(values, Pair(costs, Pair(sizes, valuesCost))) = p
      mkCCostedColl(values, costs, sizes, valuesCost)
    }

    @scalan.OverloadId("fromFields")
    def apply[Item](values: Ref[Coll[Item]], costs: Ref[Coll[Int]], sizes: Ref[Coll[Size[Item]]], valuesCost: Ref[Int]): Ref[CCostedColl[Item]] =
      mkCCostedColl(values, costs, sizes, valuesCost)

    def unapply[Item](p: Ref[CostedColl[Item]]) = unmkCCostedColl(p)
  }
  lazy val RCCostedColl: MutableLazy[CCostedCollCompanionCtor] = MutableLazy(new CCostedCollCompanionCtor)
  implicit final def unrefCCostedCollCompanion(p: Ref[CCostedCollCompanionCtor]): CCostedCollCompanionCtor = {
    if (p.node.isInstanceOf[CCostedCollCompanionCtor])
      p.node.asInstanceOf[CCostedCollCompanionCtor]
    else
      unrefDelegate[CCostedCollCompanionCtor](p)
  }

  implicit case object CCostedCollCompanionElem extends CompanionElem[CCostedCollCompanionCtor]

  implicit final def unrefCCostedColl[Item](p: Ref[CCostedColl[Item]]): CCostedColl[Item] = {
    if (p.node.isInstanceOf[CCostedColl[Item]@unchecked])
      p.node.asInstanceOf[CCostedColl[Item]]
    else
      unrefDelegate[CCostedColl[Item]](p)
  }

  def mkCCostedColl[Item]
    (values: Ref[Coll[Item]], costs: Ref[Coll[Int]], sizes: Ref[Coll[Size[Item]]], valuesCost: Ref[Int]): Ref[CCostedColl[Item]] = {
    new CCostedCollCtor[Item](values, costs, sizes, valuesCost)
  }
  def unmkCCostedColl[Item](p: Ref[CostedColl[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedCollElem[Item] @unchecked =>
      Some((asRep[CCostedColl[Item]](p).values, asRep[CCostedColl[Item]](p).costs, asRep[CCostedColl[Item]](p).sizes, asRep[CCostedColl[Item]](p).valuesCost))
    case _ =>
      None
  }
} // of object CCostedColl
  registerEntityObject("CCostedColl", CCostedColl)

object CCalcBuilder extends EntityObject("CCalcBuilder") {
  case class CCostedBuilderCtor()
    extends CCostedBuilder()
    with Def[CCostedBuilder]
  {
    lazy val resultType = element[CCostedBuilder]
    override def transform(t: Transformer) = CCostedBuilderCtor()
    private val thisClass = classOf[CostedBuilder]

    override def costedValue[T](x: Ref[T], optCost: Ref[WOption[Int]]): Ref[Costed[T]] = {
      implicit val eT = x.elem
      asRep[Costed[T]](mkMethodCall(self,
        thisClass.getMethod("costedValue", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, optCost),
        true, false, element[Costed[T]]))
    }

    override def defaultValue[T](valueType: Ref[WRType[T]]): Ref[T] = {
      implicit val eT = valueType.eA
      asRep[T](mkMethodCall(self,
        thisClass.getMethod("defaultValue", classOf[Sym]),
        Array[AnyRef](valueType),
        true, false, element[T]))
    }
  }

  // state representation type
  type CCostedBuilderData = Unit

  // elem for concrete class
  class CCostedBuilderElem
    extends CostedBuilderElem[CCostedBuilder]
    with ConcreteElem[CCostedBuilderData, CCostedBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(costedBuilderElement)
  }

  implicit lazy val cCostedBuilderElement: Elem[CCostedBuilder] =
    new CCostedBuilderElem

  // 4) constructor and deconstructor
  class CCalcBuilderCompanionCtor extends CompanionDef[CCalcBuilderCompanionCtor] with CCalcBuilderCompanion {
    def resultType = CCalcBuilderCompanionElem
    override def toString = "CCostedBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CCostedBuilderData]): Ref[CCostedBuilder] = {
      val unit = p
      mkCCostedBuilder()
    }

    @scalan.OverloadId("fromFields")
    def apply(): Ref[CCostedBuilder] =
      mkCCostedBuilder()

    def unapply(p: Ref[CostedBuilder]) = unmkCCostedBuilder(p)
  }

  lazy val RCCalcBuilder: MutableLazy[CCalcBuilderCompanionCtor] = MutableLazy(new CCalcBuilderCompanionCtor)

  implicit final def unrefCCostedBuilderCompanion(p: Ref[CCostedBuilderCompanionCtor]): CCostedBuilderCompanionCtor = {
    if (p.node.isInstanceOf[CCostedBuilderCompanionCtor])
      p.node.asInstanceOf[CCostedBuilderCompanionCtor]
    else
      unrefDelegate[CCostedBuilderCompanionCtor](p)
  }

  implicit case object CCalcBuilderCompanionElem extends CompanionElem[CCalcBuilderCompanionCtor]

  implicit final def unrefCCostedBuilder(p: Ref[CCostedBuilder]): CCostedBuilder = {
    if (p.node.isInstanceOf[CCostedBuilder])
      p.node.asInstanceOf[CCostedBuilder]
    else
      unrefDelegate[CCostedBuilder](p)
  }

  def mkCCostedBuilder
    (): Ref[CCostedBuilder] = {
    new CCostedBuilderCtor()
  }
  def unmkCCostedBuilder(p: Ref[CostedBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CCostedBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }
} // of object CCostedBuilder
  registerEntityObject("CCostedBuilder", CCostedBuilder)

  override def resetContext(): Unit = {
    super.resetContext()

    RCCalcPrim.reset()
    RCCostedPair.reset()
    RCCostedFunc.reset()
    RCCostedColl.reset()
  }

  registerModule(ConcreteCalcsModule)
}

object ConcreteCalcsModule extends scalan.ModuleInfo("special.collection", "ConcreteCalcs")
}

trait ConcreteCalcsModule extends special.collection.impl.ConcreteCalcsDefs {self: Library =>}
