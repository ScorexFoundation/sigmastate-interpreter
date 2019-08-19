package special.collection

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

package impl {
// Abs -----------------------------------
trait CostsDefs extends scalan.Scalan with Costs {
  self: Library =>
import Coll._
import Costed._
import CostedBuilder._
import CostedColl._
import CostedFunc._
import CostedOption._
import CostedPair._
import CostedPrim._
import MonoidBuilder._
import Size._
import SizeColl._
import SizeFunc._
import SizeOption._
import SizePair._
import SizePrim._
import WOption._
import WRType._

object Costed extends EntityObject("Costed") {
  private val CostedClass = classOf[Costed[_]]

  // entityAdapter for Costed trait
  case class CostedAdapter[Val](source: Ref[Costed[Val]])
      extends Node with Costed[Val]
      with Def[Costed[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asInstanceOf[Elem[Val]]

    val resultType: Elem[Costed[Val]] = element[Costed[Val]]
    override def transform(t: Transformer) = CostedAdapter[Val](t(source))

    def builder: Ref[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Ref[Val] = {
      asRep[Val](mkMethodCall(source,
        CostedClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Val]))
    }

    def cost: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Ref[Size[Val]] = {
      asRep[Size[Val]](mkMethodCall(source,
        CostedClass.getMethod("size"),
        WrappedArray.empty,
        true, true, element[Size[Val]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefCosted[Val](p: Ref[Costed[Val]]): Costed[Val] = {
    if (p.node.isInstanceOf[Costed[Val]@unchecked]) p.node.asInstanceOf[Costed[Val]]
    else
      CostedAdapter(p)
  }

  // familyElem
  class CostedElem[Val, To <: Costed[Val]](implicit _eVal: Elem[Val])
    extends EntityElem[To] {
    def eVal = _eVal

    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }

  implicit def costedElement[Val](implicit eVal: Elem[Val]): Elem[Costed[Val]] =
    cachedElemByClass(eVal)(classOf[CostedElem[Val, Costed[Val]]])

  implicit case object CostedCompanionElem extends CompanionElem[CostedCompanionCtor]

  abstract class CostedCompanionCtor extends CompanionDef[CostedCompanionCtor] with CostedCompanion {
    def resultType = CostedCompanionElem
    override def toString = "Costed"
  }
  implicit def unrefCostedCompanionCtor(p: Ref[CostedCompanionCtor]): CostedCompanionCtor =
    p.node.asInstanceOf[CostedCompanionCtor]

  lazy val RCosted: Ref[CostedCompanionCtor] = new CostedCompanionCtor {
    private val thisClass = classOf[CostedCompanion]
  }

  object CostedMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Ref[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "builder" && receiver.elem.isInstanceOf[CostedElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Costed[Val]] forSome {type Val}] = unapply(exp.node)
    }

    object value {
      def unapply(d: Def[_]): Nullable[Ref[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "value" && receiver.elem.isInstanceOf[CostedElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Costed[Val]] forSome {type Val}] = unapply(exp.node)
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Ref[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "cost" && receiver.elem.isInstanceOf[CostedElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Costed[Val]] forSome {type Val}] = unapply(exp.node)
    }

    object size {
      def unapply(d: Def[_]): Nullable[Ref[Costed[Val]] forSome {type Val}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "size" && receiver.elem.isInstanceOf[CostedElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[Costed[Val]] forSome {type Val}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[Costed[Val]] forSome {type Val}] = unapply(exp.node)
    }
  }

  object CostedCompanionMethods {
  }
} // of object Costed
  registerEntityObject("Costed", Costed)

object CostedPrim extends EntityObject("CostedPrim") {
  private val CostedPrimClass = classOf[CostedPrim[_]]

  // entityAdapter for CostedPrim trait
  case class CostedPrimAdapter[Val](source: Ref[CostedPrim[Val]])
      extends Node with CostedPrim[Val]
      with Def[CostedPrim[Val]] {
    implicit lazy val eVal = source.elem.typeArgs("Val")._1.asInstanceOf[Elem[Val]]

    val resultType: Elem[CostedPrim[Val]] = element[CostedPrim[Val]]
    override def transform(t: Transformer) = CostedPrimAdapter[Val](t(source))

    def value: Ref[Val] = {
      asRep[Val](mkMethodCall(source,
        CostedPrimClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Val]))
    }

    def cost: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedPrimClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Ref[Size[Val]] = {
      asRep[Size[Val]](mkMethodCall(source,
        CostedPrimClass.getMethod("size"),
        WrappedArray.empty,
        true, true, element[Size[Val]]))
    }

    def builder: Ref[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedPrimClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefCostedPrim[Val](p: Ref[CostedPrim[Val]]): CostedPrim[Val] = {
    if (p.node.isInstanceOf[CostedPrim[Val]@unchecked]) p.node.asInstanceOf[CostedPrim[Val]]
    else
      CostedPrimAdapter(p)
  }

  // familyElem
  class CostedPrimElem[Val, To <: CostedPrim[Val]](implicit _eVal: Elem[Val])
    extends CostedElem[Val, To] {
    override def eVal = _eVal

    override lazy val parent: Option[Elem[_]] = Some(costedElement(element[Val]))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Val" -> (eVal -> scalan.util.Invariant))
  }

  implicit def costedPrimElement[Val](implicit eVal: Elem[Val]): Elem[CostedPrim[Val]] =
    cachedElemByClass(eVal)(classOf[CostedPrimElem[Val, CostedPrim[Val]]])

  implicit case object CostedPrimCompanionElem extends CompanionElem[CostedPrimCompanionCtor]

  abstract class CostedPrimCompanionCtor extends CompanionDef[CostedPrimCompanionCtor] with CostedPrimCompanion {
    def resultType = CostedPrimCompanionElem
    override def toString = "CostedPrim"
  }
  implicit def unrefCostedPrimCompanionCtor(p: Ref[CostedPrimCompanionCtor]): CostedPrimCompanionCtor =
    p.node.asInstanceOf[CostedPrimCompanionCtor]

  lazy val RCostedPrim: Ref[CostedPrimCompanionCtor] = new CostedPrimCompanionCtor {
    private val thisClass = classOf[CostedPrimCompanion]
  }
} // of object CostedPrim
  registerEntityObject("CostedPrim", CostedPrim)

object CostedPair extends EntityObject("CostedPair") {
  private val CostedPairClass = classOf[CostedPair[_, _]]

  // entityAdapter for CostedPair trait
  case class CostedPairAdapter[L, R](source: Ref[CostedPair[L, R]])
      extends Node with CostedPair[L, R]
      with Def[CostedPair[L, R]] {
    implicit lazy val eL = source.elem.typeArgs("L")._1.asInstanceOf[Elem[L]];
implicit lazy val eR = source.elem.typeArgs("R")._1.asInstanceOf[Elem[R]]
    override lazy val eVal: Elem[(L, R)] = implicitly[Elem[(L, R)]]
    val resultType: Elem[CostedPair[L, R]] = element[CostedPair[L, R]]
    override def transform(t: Transformer) = CostedPairAdapter[L, R](t(source))

    def l: Ref[Costed[L]] = {
      asRep[Costed[L]](mkMethodCall(source,
        CostedPairClass.getMethod("l"),
        WrappedArray.empty,
        true, true, element[Costed[L]]))
    }

    def r: Ref[Costed[R]] = {
      asRep[Costed[R]](mkMethodCall(source,
        CostedPairClass.getMethod("r"),
        WrappedArray.empty,
        true, true, element[Costed[R]]))
    }

    def accCost: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedPairClass.getMethod("accCost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def builder: Ref[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedPairClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Ref[(L, R)] = {
      asRep[(L, R)](mkMethodCall(source,
        CostedPairClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[(L, R)]))
    }

    def cost: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedPairClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Ref[Size[(L, R)]] = {
      asRep[Size[(L, R)]](mkMethodCall(source,
        CostedPairClass.getMethod("size"),
        WrappedArray.empty,
        true, true, element[Size[(L, R)]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefCostedPair[L, R](p: Ref[CostedPair[L, R]]): CostedPair[L, R] = {
    if (p.node.isInstanceOf[CostedPair[L, R]@unchecked]) p.node.asInstanceOf[CostedPair[L, R]]
    else
      CostedPairAdapter(p)
  }

  // familyElem
  class CostedPairElem[L, R, To <: CostedPair[L, R]](implicit _eL: Elem[L], _eR: Elem[R])
    extends CostedElem[(L, R), To] {
    def eL = _eL
    def eR = _eR

    override lazy val parent: Option[Elem[_]] = Some(costedElement(pairElement(element[L],element[R])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("L" -> (eL -> scalan.util.Invariant), "R" -> (eR -> scalan.util.Invariant))
  }

  implicit def costedPairElement[L, R](implicit eL: Elem[L], eR: Elem[R]): Elem[CostedPair[L, R]] =
    cachedElemByClass(eL, eR)(classOf[CostedPairElem[L, R, CostedPair[L, R]]])

  implicit case object CostedPairCompanionElem extends CompanionElem[CostedPairCompanionCtor]

  abstract class CostedPairCompanionCtor extends CompanionDef[CostedPairCompanionCtor] with CostedPairCompanion {
    def resultType = CostedPairCompanionElem
    override def toString = "CostedPair"
  }
  implicit def unrefCostedPairCompanionCtor(p: Ref[CostedPairCompanionCtor]): CostedPairCompanionCtor =
    p.node.asInstanceOf[CostedPairCompanionCtor]

  lazy val RCostedPair: Ref[CostedPairCompanionCtor] = new CostedPairCompanionCtor {
    private val thisClass = classOf[CostedPairCompanion]
  }
} // of object CostedPair
  registerEntityObject("CostedPair", CostedPair)

object CostedFunc extends EntityObject("CostedFunc") {
  private val CostedFuncClass = classOf[CostedFunc[_, _, _]]

  // entityAdapter for CostedFunc trait
  case class CostedFuncAdapter[Env, Arg, Res](source: Ref[CostedFunc[Env, Arg, Res]])
      extends Node with CostedFunc[Env, Arg, Res]
      with Def[CostedFunc[Env, Arg, Res]] {
    implicit lazy val eEnv = source.elem.typeArgs("Env")._1.asInstanceOf[Elem[Env]];
implicit lazy val eArg = source.elem.typeArgs("Arg")._1.asInstanceOf[Elem[Arg]];
implicit lazy val eRes = source.elem.typeArgs("Res")._1.asInstanceOf[Elem[Res]]
    override lazy val eVal: Elem[Arg => Res] = implicitly[Elem[Arg => Res]]
    val resultType: Elem[CostedFunc[Env, Arg, Res]] = element[CostedFunc[Env, Arg, Res]]
    override def transform(t: Transformer) = CostedFuncAdapter[Env, Arg, Res](t(source))

    def envCosted: Ref[Costed[Env]] = {
      asRep[Costed[Env]](mkMethodCall(source,
        CostedFuncClass.getMethod("envCosted"),
        WrappedArray.empty,
        true, true, element[Costed[Env]]))
    }

    def func: Ref[Costed[Arg] => Costed[Res]] = {
      asRep[Costed[Arg] => Costed[Res]](mkMethodCall(source,
        CostedFuncClass.getMethod("func"),
        WrappedArray.empty,
        true, true, element[Costed[Arg] => Costed[Res]]))
    }

    def cost: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedFuncClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def sliceCalc: Ref[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(source,
        CostedFuncClass.getMethod("sliceCalc"),
        WrappedArray.empty,
        true, true, element[Arg => Res]))
    }

    def sliceCost: Ref[((Int, Size[Arg])) => Int] = {
      asRep[((Int, Size[Arg])) => Int](mkMethodCall(source,
        CostedFuncClass.getMethod("sliceCost"),
        WrappedArray.empty,
        true, true, element[((Int, Size[Arg])) => Int]))
    }

    def sliceCostEx: Ref[((Arg, (Int, Size[Arg]))) => Int] = {
      asRep[((Arg, (Int, Size[Arg]))) => Int](mkMethodCall(source,
        CostedFuncClass.getMethod("sliceCostEx"),
        WrappedArray.empty,
        true, true, element[((Arg, (Int, Size[Arg]))) => Int]))
    }

    def sliceSize: Ref[Size[Arg] => Size[Res]] = {
      asRep[Size[Arg] => Size[Res]](mkMethodCall(source,
        CostedFuncClass.getMethod("sliceSize"),
        WrappedArray.empty,
        true, true, element[Size[Arg] => Size[Res]]))
    }

    def builder: Ref[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedFuncClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Ref[Arg => Res] = {
      asRep[Arg => Res](mkMethodCall(source,
        CostedFuncClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Arg => Res]))
    }

    def size: Ref[Size[Arg => Res]] = {
      asRep[Size[Arg => Res]](mkMethodCall(source,
        CostedFuncClass.getMethod("size"),
        WrappedArray.empty,
        true, true, element[Size[Arg => Res]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefCostedFunc[Env, Arg, Res](p: Ref[CostedFunc[Env, Arg, Res]]): CostedFunc[Env, Arg, Res] = {
    if (p.node.isInstanceOf[CostedFunc[Env, Arg, Res]@unchecked]) p.node.asInstanceOf[CostedFunc[Env, Arg, Res]]
    else
      CostedFuncAdapter(p)
  }

  // familyElem
  class CostedFuncElem[Env, Arg, Res, To <: CostedFunc[Env, Arg, Res]](implicit _eEnv: Elem[Env], _eArg: Elem[Arg], _eRes: Elem[Res])
    extends CostedElem[Arg => Res, To] {
    def eEnv = _eEnv
    def eArg = _eArg
    def eRes = _eRes

    override lazy val parent: Option[Elem[_]] = Some(costedElement(funcElement(element[Arg],element[Res])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Env" -> (eEnv -> scalan.util.Invariant), "Arg" -> (eArg -> scalan.util.Invariant), "Res" -> (eRes -> scalan.util.Invariant))
  }

  implicit def costedFuncElement[Env, Arg, Res](implicit eEnv: Elem[Env], eArg: Elem[Arg], eRes: Elem[Res]): Elem[CostedFunc[Env, Arg, Res]] =
    cachedElemByClass(eEnv, eArg, eRes)(classOf[CostedFuncElem[Env, Arg, Res, CostedFunc[Env, Arg, Res]]])

  implicit case object CostedFuncCompanionElem extends CompanionElem[CostedFuncCompanionCtor]

  abstract class CostedFuncCompanionCtor extends CompanionDef[CostedFuncCompanionCtor] with CostedFuncCompanion {
    def resultType = CostedFuncCompanionElem
    override def toString = "CostedFunc"
  }
  implicit def unrefCostedFuncCompanionCtor(p: Ref[CostedFuncCompanionCtor]): CostedFuncCompanionCtor =
    p.node.asInstanceOf[CostedFuncCompanionCtor]

  lazy val RCostedFunc: Ref[CostedFuncCompanionCtor] = new CostedFuncCompanionCtor {
    private val thisClass = classOf[CostedFuncCompanion]
  }
} // of object CostedFunc
  registerEntityObject("CostedFunc", CostedFunc)

object CostedColl extends EntityObject("CostedColl") {
  private val CostedCollClass = classOf[CostedColl[_]]

  // entityAdapter for CostedColl trait
  case class CostedCollAdapter[Item](source: Ref[CostedColl[Item]])
      extends Node with CostedColl[Item]
      with Def[CostedColl[Item]] {
    implicit lazy val eItem = source.elem.typeArgs("Item")._1.asInstanceOf[Elem[Item]]
    override lazy val eVal: Elem[Coll[Item]] = implicitly[Elem[Coll[Item]]]
    val resultType: Elem[CostedColl[Item]] = element[CostedColl[Item]]
    override def transform(t: Transformer) = CostedCollAdapter[Item](t(source))

    def values: Ref[Coll[Item]] = {
      asRep[Coll[Item]](mkMethodCall(source,
        CostedCollClass.getMethod("values"),
        WrappedArray.empty,
        true, true, element[Coll[Item]]))
    }

    def costs: Ref[Coll[Int]] = {
      asRep[Coll[Int]](mkMethodCall(source,
        CostedCollClass.getMethod("costs"),
        WrappedArray.empty,
        true, true, element[Coll[Int]]))
    }

    def sizes: Ref[Coll[Size[Item]]] = {
      asRep[Coll[Size[Item]]](mkMethodCall(source,
        CostedCollClass.getMethod("sizes"),
        WrappedArray.empty,
        true, true, element[Coll[Size[Item]]]))
    }

    def valuesCost: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedCollClass.getMethod("valuesCost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def mapCosted[Res](f: Ref[Costed[Item] => Costed[Res]]): Ref[CostedColl[Res]] = {
      implicit val eRes = f.elem.eRange.typeArgs("Val")._1.asInstanceOf[Elem[Res]]
      asRep[CostedColl[Res]](mkMethodCall(source,
        CostedCollClass.getMethod("mapCosted", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[CostedColl[Res]]))
    }

    def filterCosted(f: Ref[Costed[Item] => Costed[Boolean]]): Ref[CostedColl[Item]] = {
      asRep[CostedColl[Item]](mkMethodCall(source,
        CostedCollClass.getMethod("filterCosted", classOf[Sym]),
        Array[AnyRef](f),
        true, true, element[CostedColl[Item]]))
    }

    def foldCosted[B](zero: Ref[Costed[B]], op: Ref[Costed[(B, Item)] => Costed[B]]): Ref[Costed[B]] = {
      implicit val eB = zero.eVal
      asRep[Costed[B]](mkMethodCall(source,
        CostedCollClass.getMethod("foldCosted", classOf[Sym], classOf[Sym]),
        Array[AnyRef](zero, op),
        true, true, element[Costed[B]]))
    }

    def builder: Ref[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedCollClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Ref[Coll[Item]] = {
      asRep[Coll[Item]](mkMethodCall(source,
        CostedCollClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[Coll[Item]]))
    }

    def cost: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedCollClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Ref[Size[Coll[Item]]] = {
      asRep[Size[Coll[Item]]](mkMethodCall(source,
        CostedCollClass.getMethod("size"),
        WrappedArray.empty,
        true, true, element[Size[Coll[Item]]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefCostedColl[Item](p: Ref[CostedColl[Item]]): CostedColl[Item] = {
    if (p.node.isInstanceOf[CostedColl[Item]@unchecked]) p.node.asInstanceOf[CostedColl[Item]]
    else
      CostedCollAdapter(p)
  }

  // familyElem
  class CostedCollElem[Item, To <: CostedColl[Item]](implicit _eItem: Elem[Item])
    extends CostedElem[Coll[Item], To] {
    def eItem = _eItem

    override lazy val parent: Option[Elem[_]] = Some(costedElement(collElement(element[Item])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("Item" -> (eItem -> scalan.util.Invariant))
  }

  implicit def costedCollElement[Item](implicit eItem: Elem[Item]): Elem[CostedColl[Item]] =
    cachedElemByClass(eItem)(classOf[CostedCollElem[Item, CostedColl[Item]]])

  implicit case object CostedCollCompanionElem extends CompanionElem[CostedCollCompanionCtor]

  abstract class CostedCollCompanionCtor extends CompanionDef[CostedCollCompanionCtor] with CostedCollCompanion {
    def resultType = CostedCollCompanionElem
    override def toString = "CostedColl"
  }
  implicit def unrefCostedCollCompanionCtor(p: Ref[CostedCollCompanionCtor]): CostedCollCompanionCtor =
    p.node.asInstanceOf[CostedCollCompanionCtor]

  lazy val RCostedColl: Ref[CostedCollCompanionCtor] = new CostedCollCompanionCtor {
    private val thisClass = classOf[CostedCollCompanion]
  }

  object CostedCollMethods {
    object values {
      def unapply(d: Def[_]): Nullable[Ref[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "values" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CostedColl[Item]] forSome {type Item}] = unapply(exp.node)
    }

    object costs {
      def unapply(d: Def[_]): Nullable[Ref[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "costs" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CostedColl[Item]] forSome {type Item}] = unapply(exp.node)
    }

    object sizes {
      def unapply(d: Def[_]): Nullable[Ref[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "sizes" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CostedColl[Item]] forSome {type Item}] = unapply(exp.node)
    }

    object valuesCost {
      def unapply(d: Def[_]): Nullable[Ref[CostedColl[Item]] forSome {type Item}] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "valuesCost" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CostedColl[Item]] forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CostedColl[Item]] forSome {type Item}] = unapply(exp.node)
    }

    object mapCosted {
      def unapply(d: Def[_]): Nullable[(Ref[CostedColl[Item]], Ref[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mapCosted" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedColl[Item]], Ref[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedColl[Item]], Ref[Costed[Item] => Costed[Res]]) forSome {type Item; type Res}] = unapply(exp.node)
    }

    object filterCosted {
      def unapply(d: Def[_]): Nullable[(Ref[CostedColl[Item]], Ref[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "filterCosted" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedColl[Item]], Ref[Costed[Item] => Costed[Boolean]]) forSome {type Item}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedColl[Item]], Ref[Costed[Item] => Costed[Boolean]]) forSome {type Item}] = unapply(exp.node)
    }

    object foldCosted {
      def unapply(d: Def[_]): Nullable[(Ref[CostedColl[Item]], Ref[Costed[B]], Ref[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "foldCosted" && receiver.elem.isInstanceOf[CostedCollElem[_, _]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedColl[Item]], Ref[Costed[B]], Ref[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedColl[Item]], Ref[Costed[B]], Ref[Costed[(B, Item)] => Costed[B]]) forSome {type Item; type B}] = unapply(exp.node)
    }
  }

  object CostedCollCompanionMethods {
  }
} // of object CostedColl
  registerEntityObject("CostedColl", CostedColl)

object CostedOption extends EntityObject("CostedOption") {
  private val CostedOptionClass = classOf[CostedOption[_]]

  // entityAdapter for CostedOption trait
  case class CostedOptionAdapter[T](source: Ref[CostedOption[T]])
      extends Node with CostedOption[T]
      with Def[CostedOption[T]] {
    implicit lazy val eT = source.elem.typeArgs("T")._1.asInstanceOf[Elem[T]]
    override lazy val eVal: Elem[WOption[T]] = implicitly[Elem[WOption[T]]]
    val resultType: Elem[CostedOption[T]] = element[CostedOption[T]]
    override def transform(t: Transformer) = CostedOptionAdapter[T](t(source))

    def costOpt: Ref[WOption[Int]] = {
      asRep[WOption[Int]](mkMethodCall(source,
        CostedOptionClass.getMethod("costOpt"),
        WrappedArray.empty,
        true, true, element[WOption[Int]]))
    }

    def sizeOpt: Ref[WOption[Size[T]]] = {
      asRep[WOption[Size[T]]](mkMethodCall(source,
        CostedOptionClass.getMethod("sizeOpt"),
        WrappedArray.empty,
        true, true, element[WOption[Size[T]]]))
    }

    def accumulatedCost: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedOptionClass.getMethod("accumulatedCost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def builder: Ref[CostedBuilder] = {
      asRep[CostedBuilder](mkMethodCall(source,
        CostedOptionClass.getMethod("builder"),
        WrappedArray.empty,
        true, true, element[CostedBuilder]))
    }

    def value: Ref[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(source,
        CostedOptionClass.getMethod("value"),
        WrappedArray.empty,
        true, true, element[WOption[T]]))
    }

    def cost: Ref[Int] = {
      asRep[Int](mkMethodCall(source,
        CostedOptionClass.getMethod("cost"),
        WrappedArray.empty,
        true, true, element[Int]))
    }

    def size: Ref[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(source,
        CostedOptionClass.getMethod("size"),
        WrappedArray.empty,
        true, true, element[Size[WOption[T]]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefCostedOption[T](p: Ref[CostedOption[T]]): CostedOption[T] = {
    if (p.node.isInstanceOf[CostedOption[T]@unchecked]) p.node.asInstanceOf[CostedOption[T]]
    else
      CostedOptionAdapter(p)
  }

  // familyElem
  class CostedOptionElem[T, To <: CostedOption[T]](implicit _eT: Elem[T])
    extends CostedElem[WOption[T], To] {
    def eT = _eT

    override lazy val parent: Option[Elem[_]] = Some(costedElement(wOptionElement(element[T])))
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }

  implicit def costedOptionElement[T](implicit eT: Elem[T]): Elem[CostedOption[T]] =
    cachedElemByClass(eT)(classOf[CostedOptionElem[T, CostedOption[T]]])

  implicit case object CostedOptionCompanionElem extends CompanionElem[CostedOptionCompanionCtor]

  abstract class CostedOptionCompanionCtor extends CompanionDef[CostedOptionCompanionCtor] with CostedOptionCompanion {
    def resultType = CostedOptionCompanionElem
    override def toString = "CostedOption"
  }
  implicit def unrefCostedOptionCompanionCtor(p: Ref[CostedOptionCompanionCtor]): CostedOptionCompanionCtor =
    p.node.asInstanceOf[CostedOptionCompanionCtor]

  lazy val RCostedOption: Ref[CostedOptionCompanionCtor] = new CostedOptionCompanionCtor {
    private val thisClass = classOf[CostedOptionCompanion]
  }
} // of object CostedOption
  registerEntityObject("CostedOption", CostedOption)

object CostedBuilder extends EntityObject("CostedBuilder") {
  private val CostedBuilderClass = classOf[CostedBuilder]

  // entityAdapter for CostedBuilder trait
  case class CostedBuilderAdapter(source: Ref[CostedBuilder])
      extends Node with CostedBuilder
      with Def[CostedBuilder] {
    val resultType: Elem[CostedBuilder] = element[CostedBuilder]
    override def transform(t: Transformer) = CostedBuilderAdapter(t(source))

    def costedValue[T](x: Ref[T], optCost: Ref[WOption[Int]]): Ref[Costed[T]] = {
      implicit val eT = x.elem
      asRep[Costed[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("costedValue", classOf[Sym], classOf[Sym]),
        Array[AnyRef](x, optCost),
        true, true, element[Costed[T]]))
    }

    def defaultValue[T](valueType: Ref[WRType[T]]): Ref[T] = {
      implicit val eT = valueType.eA
      asRep[T](mkMethodCall(source,
        CostedBuilderClass.getMethod("defaultValue", classOf[Sym]),
        Array[AnyRef](valueType),
        true, true, element[T]))
    }

    def monoidBuilder: Ref[MonoidBuilder] = {
      asRep[MonoidBuilder](mkMethodCall(source,
        CostedBuilderClass.getMethod("monoidBuilder"),
        WrappedArray.empty,
        true, true, element[MonoidBuilder]))
    }

    def mkSizePrim[T](dataSize: Ref[Long], tT: Ref[WRType[T]]): Ref[SizePrim[T]] = {
      implicit val eT = tT.eA
      asRep[SizePrim[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizePrim", classOf[Sym], classOf[Sym]),
        Array[AnyRef](dataSize, tT),
        true, true, element[SizePrim[T]]))
    }

    def mkSizePair[L, R](l: Ref[Size[L]], r: Ref[Size[R]]): Ref[SizePair[L, R]] = {
      implicit val eL = l.eVal
implicit val eR = r.eVal
      asRep[SizePair[L, R]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizePair", classOf[Sym], classOf[Sym]),
        Array[AnyRef](l, r),
        true, true, element[SizePair[L, R]]))
    }

    def mkSizeColl[T](sizes: Ref[Coll[Size[T]]]): Ref[SizeColl[T]] = {
      implicit val eT = sizes.eA.typeArgs("Val")._1.asInstanceOf[Elem[T]]
      asRep[SizeColl[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizeColl", classOf[Sym]),
        Array[AnyRef](sizes),
        true, true, element[SizeColl[T]]))
    }

    def mkSizeFunc[E, A, R](sizeEnv: Ref[Size[E]], sizeFunc: Ref[Long], tA: Ref[WRType[A]], tR: Ref[WRType[R]]): Ref[SizeFunc[E, A, R]] = {
      implicit val eE = sizeEnv.eVal
implicit val eA = tA.eA
implicit val eR = tR.eA
      asRep[SizeFunc[E, A, R]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizeFunc", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](sizeEnv, sizeFunc, tA, tR),
        true, true, element[SizeFunc[E, A, R]]))
    }

    def mkSizeOption[T](sizeOpt: Ref[WOption[Size[T]]]): Ref[SizeOption[T]] = {
      implicit val eT = sizeOpt.eA.typeArgs("Val")._1.asInstanceOf[Elem[T]]
      asRep[SizeOption[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkSizeOption", classOf[Sym]),
        Array[AnyRef](sizeOpt),
        true, true, element[SizeOption[T]]))
    }

    def mkCostedPrim[T](value: Ref[T], cost: Ref[Int], size: Ref[Size[T]]): Ref[CostedPrim[T]] = {
      implicit val eT = value.elem
      asRep[CostedPrim[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedPrim", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](value, cost, size),
        true, true, element[CostedPrim[T]]))
    }

    def mkCostedPair[L, R](first: Ref[Costed[L]], second: Ref[Costed[R]], accCost: Ref[Int]): Ref[CostedPair[L, R]] = {
      implicit val eL = first.eVal
implicit val eR = second.eVal
      asRep[CostedPair[L, R]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedPair", classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](first, second, accCost),
        true, true, element[CostedPair[L, R]]))
    }

    def mkCostedFunc[Env, Arg, Res](envCosted: Ref[Costed[Env]], func: Ref[Costed[Arg] => Costed[Res]], cost: Ref[Int], size: Ref[Size[Arg => Res]]): Ref[CostedFunc[Env, Arg, Res]] = {
      implicit val eEnv = envCosted.eVal
implicit val eArg = func.elem.eDom.typeArgs("Val")._1.asInstanceOf[Elem[Arg]]
implicit val eRes = func.elem.eRange.typeArgs("Val")._1.asInstanceOf[Elem[Res]]
      asRep[CostedFunc[Env, Arg, Res]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedFunc", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](envCosted, func, cost, size),
        true, true, element[CostedFunc[Env, Arg, Res]]))
    }

    def mkCostedColl[T](values: Ref[Coll[T]], costs: Ref[Coll[Int]], sizes: Ref[Coll[Size[T]]], valuesCost: Ref[Int]): Ref[CostedColl[T]] = {
      implicit val eT = values.eA
      asRep[CostedColl[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedColl", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](values, costs, sizes, valuesCost),
        true, true, element[CostedColl[T]]))
    }

    def mkCostedOption[T](value: Ref[WOption[T]], costOpt: Ref[WOption[Int]], sizeOpt: Ref[WOption[Size[T]]], accumulatedCost: Ref[Int]): Ref[CostedOption[T]] = {
      implicit val eT = value.eA
      asRep[CostedOption[T]](mkMethodCall(source,
        CostedBuilderClass.getMethod("mkCostedOption", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        Array[AnyRef](value, costOpt, sizeOpt, accumulatedCost),
        true, true, element[CostedOption[T]]))
    }
  }

  // entityUnref: single unref method for each type family
  implicit def unrefCostedBuilder(p: Ref[CostedBuilder]): CostedBuilder = {
    if (p.node.isInstanceOf[CostedBuilder]) p.node.asInstanceOf[CostedBuilder]
    else
      CostedBuilderAdapter(p)
  }

  // familyElem
  class CostedBuilderElem[To <: CostedBuilder]
    extends EntityElem[To] {
  }

  implicit lazy val costedBuilderElement: Elem[CostedBuilder] =
    new CostedBuilderElem[CostedBuilder]

  implicit case object CostedBuilderCompanionElem extends CompanionElem[CostedBuilderCompanionCtor]

  abstract class CostedBuilderCompanionCtor extends CompanionDef[CostedBuilderCompanionCtor] with CostedBuilderCompanion {
    def resultType = CostedBuilderCompanionElem
    override def toString = "CostedBuilder"
  }
  implicit def unrefCostedBuilderCompanionCtor(p: Ref[CostedBuilderCompanionCtor]): CostedBuilderCompanionCtor =
    p.node.asInstanceOf[CostedBuilderCompanionCtor]

  lazy val RCostedBuilder: Ref[CostedBuilderCompanionCtor] = new CostedBuilderCompanionCtor {
    private val thisClass = classOf[CostedBuilderCompanion]
  }

  object CostedBuilderMethods {
    object ConstructTupleCost {
      def unapply(d: Def[_]): Nullable[Ref[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "ConstructTupleCost" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CostedBuilder]] = unapply(exp.node)
    }

    object ConstructSumCost {
      def unapply(d: Def[_]): Nullable[Ref[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "ConstructSumCost" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CostedBuilder]] = unapply(exp.node)
    }

    object SelectFieldCost {
      def unapply(d: Def[_]): Nullable[Ref[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "SelectFieldCost" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CostedBuilder]] = unapply(exp.node)
    }

    object SumTagSize {
      def unapply(d: Def[_]): Nullable[Ref[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "SumTagSize" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CostedBuilder]] = unapply(exp.node)
    }

    object costedValue {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[T], Ref[WOption[Int]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "costedValue" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[T], Ref[WOption[Int]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[T], Ref[WOption[Int]]) forSome {type T}] = unapply(exp.node)
    }

    object defaultValue {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "defaultValue" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[WRType[T]]) forSome {type T}] = unapply(exp.node)
    }

    object monoidBuilder {
      def unapply(d: Def[_]): Nullable[Ref[CostedBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if method.getName == "monoidBuilder" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Ref[CostedBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Ref[CostedBuilder]] = unapply(exp.node)
    }

    object mkSizePrim {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[Long], Ref[WRType[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizePrim" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[Long], Ref[WRType[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[Long], Ref[WRType[T]]) forSome {type T}] = unapply(exp.node)
    }

    object mkSizePair {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[Size[L]], Ref[Size[R]]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizePair" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[Size[L]], Ref[Size[R]]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[Size[L]], Ref[Size[R]]) forSome {type L; type R}] = unapply(exp.node)
    }

    object mkSizeColl {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[Coll[Size[T]]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizeColl" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[Coll[Size[T]]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[Coll[Size[T]]]) forSome {type T}] = unapply(exp.node)
    }

    object mkSizeFunc {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[Size[E]], Ref[Long], Ref[WRType[A]], Ref[WRType[R]]) forSome {type E; type A; type R}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizeFunc" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[Size[E]], Ref[Long], Ref[WRType[A]], Ref[WRType[R]]) forSome {type E; type A; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[Size[E]], Ref[Long], Ref[WRType[A]], Ref[WRType[R]]) forSome {type E; type A; type R}] = unapply(exp.node)
    }

    object mkSizeOption {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[WOption[Size[T]]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkSizeOption" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[WOption[Size[T]]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[WOption[Size[T]]]) forSome {type T}] = unapply(exp.node)
    }

    object mkCostedPrim {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[T], Ref[Int], Ref[Size[T]]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedPrim" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[T], Ref[Int], Ref[Size[T]]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[T], Ref[Int], Ref[Size[T]]) forSome {type T}] = unapply(exp.node)
    }

    object mkCostedPair {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[Costed[L]], Ref[Costed[R]], Ref[Int]) forSome {type L; type R}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedPair" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[Costed[L]], Ref[Costed[R]], Ref[Int]) forSome {type L; type R}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[Costed[L]], Ref[Costed[R]], Ref[Int]) forSome {type L; type R}] = unapply(exp.node)
    }

    object mkCostedFunc {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[Costed[Env]], Ref[Costed[Arg] => Costed[Res]], Ref[Int], Ref[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedFunc" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[Costed[Env]], Ref[Costed[Arg] => Costed[Res]], Ref[Int], Ref[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[Costed[Env]], Ref[Costed[Arg] => Costed[Res]], Ref[Int], Ref[Size[Arg => Res]]) forSome {type Env; type Arg; type Res}] = unapply(exp.node)
    }

    object mkCostedColl {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[Coll[T]], Ref[Coll[Int]], Ref[Coll[Size[T]]], Ref[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedColl" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[Coll[T]], Ref[Coll[Int]], Ref[Coll[Size[T]]], Ref[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[Coll[T]], Ref[Coll[Int]], Ref[Coll[Size[T]]], Ref[Int]) forSome {type T}] = unapply(exp.node)
    }

    object mkCostedOption {
      def unapply(d: Def[_]): Nullable[(Ref[CostedBuilder], Ref[WOption[T]], Ref[WOption[Int]], Ref[WOption[Size[T]]], Ref[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if method.getName == "mkCostedOption" && receiver.elem.isInstanceOf[CostedBuilderElem[_]] =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Ref[CostedBuilder], Ref[WOption[T]], Ref[WOption[Int]], Ref[WOption[Size[T]]], Ref[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Ref[CostedBuilder], Ref[WOption[T]], Ref[WOption[Int]], Ref[WOption[Size[T]]], Ref[Int]) forSome {type T}] = unapply(exp.node)
    }
  }

  object CostedBuilderCompanionMethods {
  }
} // of object CostedBuilder
  registerEntityObject("CostedBuilder", CostedBuilder)

  registerModule(CostsModule)
}

object CostsModule extends scalan.ModuleInfo("special.collection", "Costs")
}

trait CostsModule extends special.collection.impl.CostsDefs {self: Library =>}
