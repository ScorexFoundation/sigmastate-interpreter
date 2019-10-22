package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
  import scalan.util.MemoizedFunc // manual fix

  // Abs -----------------------------------
trait ConcreteSizesDefs extends scalan.Scalan with ConcreteSizes {
  self: Library =>
import Size._  // manual fix
import Coll._  // manual fix
import WOption._  // manual fix
import SizeColl._
import SizeFunc._
import SizeOption._
import SizePair._
import SizePrim._
import CSizeColl._
import CSizeFunc._
import CSizeOption._
import CSizePair._
import CSizePrim._
import WRType._  // manual fix

object CSizePrim extends EntityObject("CSizePrim") {
  case class CSizePrimCtor[Val]
      (override val dataSize: Ref[Long], override val tVal: Ref[WRType[Val]])
    extends CSizePrim[Val](dataSize, tVal) with Def[CSizePrim[Val]] {
    implicit lazy val eVal = tVal.eA

    lazy val resultType = element[CSizePrim[Val]]
    override def transform(t: Transformer) = CSizePrimCtor[Val](t(dataSize), t(tVal))
  }

  // state representation type
  type CSizePrimData[Val] = (Long, WRType[Val])

  // elem for concrete class
  class CSizePrimElem[Val](implicit override val eVal: Elem[Val])
    extends SizePrimElem[Val, CSizePrim[Val]]
    with ConcreteElem[CSizePrimData[Val], CSizePrim[Val]] {
    override lazy val parent: Option[Elem[_]] = Some(sizePrimElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }

  implicit final def cSizePrimElement[Val](implicit eVal: Elem[Val]): Elem[CSizePrim[Val]] =
    cachedElemByClass(eVal)(classOf[CSizePrimElem[Val]])

  // 4) constructor and deconstructor
  class CSizePrimCompanionCtor extends CompanionDef[CSizePrimCompanionCtor] with CSizePrimCompanion {
    def resultType = CSizePrimCompanionElem
    override def toString = "CSizePrimCompanion"
    @scalan.OverloadId("fromData")
    def apply[Val](p: Ref[CSizePrimData[Val]]): Ref[CSizePrim[Val]] = {
      implicit val eVal = p._2.eA
      val Pair(dataSize, tVal) = p
      mkCSizePrim(dataSize, tVal)
    }

    @scalan.OverloadId("fromFields")
    def apply[Val](dataSize: Ref[Long], tVal: Ref[WRType[Val]]): Ref[CSizePrim[Val]] =
      mkCSizePrim(dataSize, tVal)

    def unapply[Val](p: Ref[SizePrim[Val]]) = unmkCSizePrim(p)
  }
  lazy val RCSizePrim: MutableLazy[CSizePrimCompanionCtor] = MutableLazy(new CSizePrimCompanionCtor)
  implicit final def unrefCSizePrimCompanion(p: Ref[CSizePrimCompanionCtor]): CSizePrimCompanionCtor = {
    if (p.node.isInstanceOf[CSizePrimCompanionCtor])
      p.node.asInstanceOf[CSizePrimCompanionCtor]
    else
      unrefDelegate[CSizePrimCompanionCtor](p)
  }

  implicit case object CSizePrimCompanionElem extends CompanionElem[CSizePrimCompanionCtor]

  implicit final def unrefCSizePrim[Val](p: Ref[CSizePrim[Val]]): CSizePrim[Val] = {
    if (p.node.isInstanceOf[CSizePrim[Val]@unchecked])
      p.node.asInstanceOf[CSizePrim[Val]]
    else
      unrefDelegate[CSizePrim[Val]](p)
  }

  def mkCSizePrim[Val]
    (dataSize: Ref[Long], tVal: Ref[WRType[Val]]): Ref[CSizePrim[Val]] = {
    new CSizePrimCtor[Val](dataSize, tVal)
  }
  def unmkCSizePrim[Val](p: Ref[SizePrim[Val]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizePrimElem[Val] @unchecked =>
      Some((asRep[CSizePrim[Val]](p).dataSize, asRep[CSizePrim[Val]](p).tVal))
    case _ =>
      None
  }
} // of object CSizePrim
  registerEntityObject("CSizePrim", CSizePrim)

object CSizePair extends EntityObject("CSizePair") {
  case class CSizePairCtor[L, R]
      (override val l: Ref[Size[L]], override val r: Ref[Size[R]])
    extends CSizePair[L, R](l, r) with Def[CSizePair[L, R]] {
    implicit lazy val eL = l.eVal;
implicit lazy val eR = r.eVal
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    lazy val resultType = element[CSizePair[L, R]]
    override def transform(t: Transformer) = CSizePairCtor[L, R](t(l), t(r))
    private val thisClass = classOf[SizePair[_, _]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }

  // state representation type
  type CSizePairData[L, R] = (Size[L], Size[R])

  // elem for concrete class
  class CSizePairElem[L, R](implicit override val eL: Elem[L], override val eR: Elem[R])
    extends SizePairElem[L, R, CSizePair[L, R]]
    with ConcreteElem[CSizePairData[L, R], CSizePair[L, R]] {
    override lazy val parent: Option[Elem[_]] = Some(sizePairElement(element[L], element[R]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }

  implicit final def cSizePairElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[CSizePair[L, R]] =
    cachedElemByClass(eL, eR)(classOf[CSizePairElem[L, R]])

  // 4) constructor and deconstructor
  class CSizePairCompanionCtor extends CompanionDef[CSizePairCompanionCtor] with CSizePairCompanion {
    def resultType = CSizePairCompanionElem
    override def toString = "CSizePairCompanion"
    @scalan.OverloadId("fromData")
    def apply[L, R](p: Ref[CSizePairData[L, R]]): Ref[CSizePair[L, R]] = {
      implicit val eL = p._1.eVal;
implicit val eR = p._2.eVal
      val Pair(l, r) = p
      mkCSizePair(l, r)
    }

    @scalan.OverloadId("fromFields")
    def apply[L, R](l: Ref[Size[L]], r: Ref[Size[R]]): Ref[CSizePair[L, R]] =
      mkCSizePair(l, r)

    def unapply[L, R](p: Ref[SizePair[L, R]]) = unmkCSizePair(p)
  }
  lazy val RCSizePair: MutableLazy[CSizePairCompanionCtor] = MutableLazy(new CSizePairCompanionCtor)
  implicit final def unrefCSizePairCompanion(p: Ref[CSizePairCompanionCtor]): CSizePairCompanionCtor = {
    if (p.node.isInstanceOf[CSizePairCompanionCtor])
      p.node.asInstanceOf[CSizePairCompanionCtor]
    else
      unrefDelegate[CSizePairCompanionCtor](p)
  }

  implicit case object CSizePairCompanionElem extends CompanionElem[CSizePairCompanionCtor]

  implicit final def unrefCSizePair[L, R](p: Ref[CSizePair[L, R]]): CSizePair[L, R] = {
    if (p.node.isInstanceOf[CSizePair[L, R]@unchecked])
      p.node.asInstanceOf[CSizePair[L, R]]
    else
      unrefDelegate[CSizePair[L, R]](p)
  }

  def mkCSizePair[L, R]
    (l: Ref[Size[L]], r: Ref[Size[R]]): Ref[CSizePair[L, R]] = {
    new CSizePairCtor[L, R](l, r)
  }
  def unmkCSizePair[L, R](p: Ref[SizePair[L, R]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizePairElem[L, R] @unchecked =>
      Some((asRep[CSizePair[L, R]](p).l, asRep[CSizePair[L, R]](p).r))
    case _ =>
      None
  }
} // of object CSizePair
  registerEntityObject("CSizePair", CSizePair)

object CSizeColl extends EntityObject("CSizeColl") {
  case class CSizeCollCtor[Item]
      (override val sizes: Ref[Coll[Size[Item]]])
    extends CSizeColl[Item](sizes) with Def[CSizeColl[Item]] {
    implicit lazy val eItem = sizes.eA.typeArgs("Val")._1.asInstanceOf[Elem[Item]]
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    lazy val resultType = element[CSizeColl[Item]]
    override def transform(t: Transformer) = CSizeCollCtor[Item](t(sizes))
    private val thisClass = classOf[SizeColl[_]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }

  // state representation type
  type CSizeCollData[Item] = Coll[Size[Item]]

  // elem for concrete class
  class CSizeCollElem[Item](implicit override val eItem: Elem[Item])
    extends SizeCollElem[Item, CSizeColl[Item]]
    with ConcreteElem[CSizeCollData[Item], CSizeColl[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(sizeCollElement(element[Item]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }

  implicit final def cSizeCollElement[Item](implicit eItem: Elem[Item]): Elem[CSizeColl[Item]] =
    cachedElemByClass(eItem)(classOf[CSizeCollElem[Item]])

  // 4) constructor and deconstructor
  class CSizeCollCompanionCtor extends CompanionDef[CSizeCollCompanionCtor] with CSizeCollCompanion {
    def resultType = CSizeCollCompanionElem
    override def toString = "CSizeCollCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Item](sizes: Ref[Coll[Size[Item]]]): Ref[CSizeColl[Item]] =
      mkCSizeColl(sizes)

    def unapply[Item](p: Ref[SizeColl[Item]]) = unmkCSizeColl(p)
  }
  lazy val RCSizeColl: MutableLazy[CSizeCollCompanionCtor] = MutableLazy(new CSizeCollCompanionCtor)
  implicit final def unrefCSizeCollCompanion(p: Ref[CSizeCollCompanionCtor]): CSizeCollCompanionCtor = {
    if (p.node.isInstanceOf[CSizeCollCompanionCtor])
      p.node.asInstanceOf[CSizeCollCompanionCtor]
    else
      unrefDelegate[CSizeCollCompanionCtor](p)
  }

  implicit case object CSizeCollCompanionElem extends CompanionElem[CSizeCollCompanionCtor]

  implicit final def unrefCSizeColl[Item](p: Ref[CSizeColl[Item]]): CSizeColl[Item] = {
    if (p.node.isInstanceOf[CSizeColl[Item]@unchecked])
      p.node.asInstanceOf[CSizeColl[Item]]
    else
      unrefDelegate[CSizeColl[Item]](p)
  }

  def mkCSizeColl[Item]
    (sizes: Ref[Coll[Size[Item]]]): Ref[CSizeColl[Item]] = {
    new CSizeCollCtor[Item](sizes)
  }
  def unmkCSizeColl[Item](p: Ref[SizeColl[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeCollElem[Item] @unchecked =>
      Some((asRep[CSizeColl[Item]](p).sizes))
    case _ =>
      None
  }
} // of object CSizeColl
  registerEntityObject("CSizeColl", CSizeColl)

object CSizeFunc extends EntityObject("CSizeFunc") {
  case class CSizeFuncCtor[Env, Arg, Res]
      (override val sizeEnv: Ref[Size[Env]], override val sizeFunc: Ref[Long], override val tArg: Ref[WRType[Arg]], override val tRes: Ref[WRType[Res]])
    extends CSizeFunc[Env, Arg, Res](sizeEnv, sizeFunc, tArg, tRes) with Def[CSizeFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = sizeEnv.eVal;
implicit lazy val eArg = tArg.eA;
implicit lazy val eRes = tRes.eA
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    lazy val resultType = element[CSizeFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = CSizeFuncCtor[Env, Arg, Res](t(sizeEnv), t(sizeFunc), t(tArg), t(tRes))
    private val thisClass = classOf[SizeFunc[_, _, _]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }

  // state representation type
  type CSizeFuncData[Env, Arg, Res] = (Size[Env], (Long, (WRType[Arg], WRType[Res])))

  // elem for concrete class
  class CSizeFuncElem[Env, Arg, Res](implicit override val eEnv: Elem[Env], override val eArg: Elem[Arg], override val eRes: Elem[Res])
    extends SizeFuncElem[Env, Arg, Res, CSizeFunc[Env, Arg, Res]]
    with ConcreteElem[CSizeFuncData[Env, Arg, Res], CSizeFunc[Env, Arg, Res]] {
    override lazy val parent: Option[Elem[_]] = Some(sizeFuncElement(element[Env], element[Arg], element[Res]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
  }

  implicit final def cSizeFuncElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[CSizeFunc[Env, Arg, Res]] =
    cachedElemByClass(eEnv, eArg, eRes)(classOf[CSizeFuncElem[Env, Arg, Res]])

  // 4) constructor and deconstructor
  class CSizeFuncCompanionCtor extends CompanionDef[CSizeFuncCompanionCtor] with CSizeFuncCompanion {
    def resultType = CSizeFuncCompanionElem
    override def toString = "CSizeFuncCompanion"
    @scalan.OverloadId("fromData")
    def apply[Env, Arg, Res](p: Ref[CSizeFuncData[Env, Arg, Res]]): Ref[CSizeFunc[Env, Arg, Res]] = {
      implicit val eEnv = p._1.eVal;
implicit val eArg = p._3.eA;
implicit val eRes = p._4.eA
      val Pair(sizeEnv, Pair(sizeFunc, Pair(tArg, tRes))) = p
      mkCSizeFunc(sizeEnv, sizeFunc, tArg, tRes)
    }

    @scalan.OverloadId("fromFields")
    def apply[Env, Arg, Res](sizeEnv: Ref[Size[Env]], sizeFunc: Ref[Long], tArg: Ref[WRType[Arg]], tRes: Ref[WRType[Res]]): Ref[CSizeFunc[Env, Arg, Res]] =
      mkCSizeFunc(sizeEnv, sizeFunc, tArg, tRes)

    def unapply[Env, Arg, Res](p: Ref[SizeFunc[Env, Arg, Res]]) = unmkCSizeFunc(p)
  }
  lazy val RCSizeFunc: MutableLazy[CSizeFuncCompanionCtor] = MutableLazy(new CSizeFuncCompanionCtor)
  implicit final def unrefCSizeFuncCompanion(p: Ref[CSizeFuncCompanionCtor]): CSizeFuncCompanionCtor = {
    if (p.node.isInstanceOf[CSizeFuncCompanionCtor])
      p.node.asInstanceOf[CSizeFuncCompanionCtor]
    else
      unrefDelegate[CSizeFuncCompanionCtor](p)
  }

  implicit case object CSizeFuncCompanionElem extends CompanionElem[CSizeFuncCompanionCtor]

  implicit final def unrefCSizeFunc[Env, Arg, Res](p: Ref[CSizeFunc[Env, Arg, Res]]): CSizeFunc[Env, Arg, Res] = {
    if (p.node.isInstanceOf[CSizeFunc[Env, Arg, Res]@unchecked])
      p.node.asInstanceOf[CSizeFunc[Env, Arg, Res]]
    else
      unrefDelegate[CSizeFunc[Env, Arg, Res]](p)
  }

  def mkCSizeFunc[Env, Arg, Res]
    (sizeEnv: Ref[Size[Env]], sizeFunc: Ref[Long], tArg: Ref[WRType[Arg]], tRes: Ref[WRType[Res]]): Ref[CSizeFunc[Env, Arg, Res]] = {
    new CSizeFuncCtor[Env, Arg, Res](sizeEnv, sizeFunc, tArg, tRes)
  }
  def unmkCSizeFunc[Env, Arg, Res](p: Ref[SizeFunc[Env, Arg, Res]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeFuncElem[Env, Arg, Res] @unchecked =>
      Some((asRep[CSizeFunc[Env, Arg, Res]](p).sizeEnv, asRep[CSizeFunc[Env, Arg, Res]](p).sizeFunc, asRep[CSizeFunc[Env, Arg, Res]](p).tArg, asRep[CSizeFunc[Env, Arg, Res]](p).tRes))
    case _ =>
      None
  }
} // of object CSizeFunc
  registerEntityObject("CSizeFunc", CSizeFunc)

object CSizeOption extends EntityObject("CSizeOption") {
  case class CSizeOptionCtor[Item]
      (override val sizeOpt: Ref[WOption[Size[Item]]])
    extends CSizeOption[Item](sizeOpt) with Def[CSizeOption[Item]] {
    implicit lazy val eItem = sizeOpt.eA.typeArgs("Val")._1.asInstanceOf[Elem[Item]]
    override lazy val eT: Elem[Item] = eItem
override lazy val eVal: Elem[WOption[Item]] = implicitly[Elem[WOption[Item]]]
    lazy val resultType = element[CSizeOption[Item]]
    override def transform(t: Transformer) = CSizeOptionCtor[Item](t(sizeOpt))
    private val thisClass = classOf[SizeOption[_]]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }

  // state representation type
  type CSizeOptionData[Item] = WOption[Size[Item]]

  // elem for concrete class
  class CSizeOptionElem[Item](implicit val eItem: Elem[Item])
    extends SizeOptionElem[Item, CSizeOption[Item]]
    with ConcreteElem[CSizeOptionData[Item], CSizeOption[Item]] {
    override lazy val parent: Option[Elem[_]] = Some(sizeOptionElement(element[Item]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }

  implicit final def cSizeOptionElement[Item](implicit eItem: Elem[Item]): Elem[CSizeOption[Item]] =
    cachedElemByClass(eItem)(classOf[CSizeOptionElem[Item]])

  // 4) constructor and deconstructor
  class CSizeOptionCompanionCtor extends CompanionDef[CSizeOptionCompanionCtor] with CSizeOptionCompanion {
    def resultType = CSizeOptionCompanionElem
    override def toString = "CSizeOptionCompanion"

    @scalan.OverloadId("fromFields")
    def apply[Item](sizeOpt: Ref[WOption[Size[Item]]]): Ref[CSizeOption[Item]] =
      mkCSizeOption(sizeOpt)

    def unapply[Item](p: Ref[SizeOption[Item]]) = unmkCSizeOption(p)
  }
  lazy val RCSizeOption: MutableLazy[CSizeOptionCompanionCtor] = MutableLazy(new CSizeOptionCompanionCtor)
  implicit final def unrefCSizeOptionCompanion(p: Ref[CSizeOptionCompanionCtor]): CSizeOptionCompanionCtor = {
    if (p.node.isInstanceOf[CSizeOptionCompanionCtor])
      p.node.asInstanceOf[CSizeOptionCompanionCtor]
    else
      unrefDelegate[CSizeOptionCompanionCtor](p)
  }

  implicit case object CSizeOptionCompanionElem extends CompanionElem[CSizeOptionCompanionCtor]

  implicit final def unrefCSizeOption[Item](p: Ref[CSizeOption[Item]]): CSizeOption[Item] = {
    if (p.node.isInstanceOf[CSizeOption[Item]@unchecked])
      p.node.asInstanceOf[CSizeOption[Item]]
    else
      unrefDelegate[CSizeOption[Item]](p)
  }

  def mkCSizeOption[Item]
    (sizeOpt: Ref[WOption[Size[Item]]]): Ref[CSizeOption[Item]] = {
    new CSizeOptionCtor[Item](sizeOpt)
  }
  def unmkCSizeOption[Item](p: Ref[SizeOption[Item]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeOptionElem[Item] @unchecked =>
      Some((asRep[CSizeOption[Item]](p).sizeOpt))
    case _ =>
      None
  }
} // of object CSizeOption
  registerEntityObject("CSizeOption", CSizeOption)

  override def resetContext(): Unit = {
    super.resetContext()

    RCSizePrim.reset()
    RCSizePair.reset()
    RCSizeColl.reset()
    RCSizeFunc.reset()
    RCSizeOption.reset()
  }

  registerModule(ConcreteSizesModule)
}

object ConcreteSizesModule extends scalan.ModuleInfo("special.collection", "ConcreteSizes")
}

trait ConcreteSizesModule extends special.collection.impl.ConcreteSizesDefs {self: Library =>}
