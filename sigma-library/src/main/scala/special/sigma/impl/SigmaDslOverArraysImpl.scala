package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
  // manual fix
  import scalan.OverloadHack.Overloaded1

  // Abs -----------------------------------
trait SigmaDslOverArraysDefs extends scalan.Scalan with SigmaDslOverArrays {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import AnyValue._
import AvlTree._
import Box._
import CCostedBuilder._
import Coll._
import CollBuilder._
import CollOverArrayBuilder._
import Context._
import CostModel._
import Costed._
import CostedBuilder._
import CostedColl._
import CostedOption._
import DefaultSigma._
import MonoidBuilder._
import MonoidBuilderInst._
import SigmaContract._
import SigmaDslBuilder._
import SigmaProp._
import TestSigmaDslBuilder._
import WBigInteger._
import WECPoint._
import WOption._
import WArray._
import WSpecialPredef._
import DefaultContract._
import ProveDHTEvidence._
import ProveDlogEvidence._
import TestAvlTree._
import TestBox._
import TestContext._
import TestValue._
import TrivialSigma._

object DefaultSigma extends EntityObject("DefaultSigma") {
  // entityAdapter for DefaultSigma trait
  case class DefaultSigmaAdapter(source: Rep[DefaultSigma])
      extends DefaultSigma with Def[DefaultSigma] {
    val selfType: Elem[DefaultSigma] = element[DefaultSigma]
    override def transform(t: Transformer) = DefaultSigmaAdapter(t(source))
    private val thisClass = classOf[DefaultSigma]

    // manual fix (removed && method)

    // manual fix (removed || method)

    def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("isValid"),
        List(),
        true, true, element[Boolean]))
    }

    def propBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(source,
        thisClass.getMethod("propBytes"),
        List(),
        true, true, element[Coll[Byte]]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyDefaultSigma(p: Rep[DefaultSigma]): DefaultSigma = {
    if (p.rhs.isInstanceOf[DefaultSigma@unchecked]) p.rhs.asInstanceOf[DefaultSigma]
    else
      DefaultSigmaAdapter(p)
  }

  // familyElem
  class DefaultSigmaElem[To <: DefaultSigma]
    extends SigmaPropElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaPropElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DefaultSigma].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DefaultSigma] => convertDefaultSigma(x) }
      tryConvert(element[DefaultSigma], this, x, conv)
    }

    def convertDefaultSigma(x: Rep[DefaultSigma]): Rep[To] = {
      x.elem match {
        case _: DefaultSigmaElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have DefaultSigmaElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val defaultSigmaElement: Elem[DefaultSigma] =
    new DefaultSigmaElem[DefaultSigma]

  implicit case object DefaultSigmaCompanionElem extends CompanionElem[DefaultSigmaCompanionCtor] {
    lazy val tag = weakTypeTag[DefaultSigmaCompanionCtor]
    protected def getDefaultRep = RDefaultSigma
  }

  abstract class DefaultSigmaCompanionCtor extends CompanionDef[DefaultSigmaCompanionCtor] with DefaultSigmaCompanion {
    def selfType = DefaultSigmaCompanionElem
    override def toString = "DefaultSigma"
  }
  implicit def proxyDefaultSigmaCompanionCtor(p: Rep[DefaultSigmaCompanionCtor]): DefaultSigmaCompanionCtor =
    proxyOps[DefaultSigmaCompanionCtor](p)

  lazy val RDefaultSigma: Rep[DefaultSigmaCompanionCtor] = new DefaultSigmaCompanionCtor {
    private val thisClass = classOf[DefaultSigmaCompanion]
  }

  object DefaultSigmaMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[DefaultSigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[DefaultSigma]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[DefaultSigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Nullable[(Rep[DefaultSigma], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[DefaultSigma], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[DefaultSigma], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Nullable[(Rep[DefaultSigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[DefaultSigma], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[DefaultSigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Nullable[(Rep[DefaultSigma], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[DefaultSigma], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[DefaultSigma], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Nullable[(Rep[DefaultSigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[DefaultSigma], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[DefaultSigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyAnd {
      def unapply(d: Def[_]): Nullable[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "lazyAnd" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyOr {
      def unapply(d: Def[_]): Nullable[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[DefaultSigmaElem[_]] && method.getName == "lazyOr" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[DefaultSigma], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object DefaultSigmaCompanionMethods {
  }
} // of object DefaultSigma
  registerEntityObject("DefaultSigma", DefaultSigma)

object DefaultContract extends EntityObject("DefaultContract") {
  // entityAdapter for DefaultContract trait
  case class DefaultContractAdapter(source: Rep[DefaultContract])
      extends DefaultContract with Def[DefaultContract] {
    val selfType: Elem[DefaultContract] = element[DefaultContract]
    override def transform(t: Transformer) = DefaultContractAdapter(t(source))
    private val thisClass = classOf[DefaultContract]

    def canOpen(ctx: Rep[Context]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(source,
        thisClass.getMethod("canOpen", classOf[Sym]),
        List(ctx),
        true, true, element[Boolean]))
    }
  }

  // entityProxy: single proxy for each type family
  implicit def proxyDefaultContract(p: Rep[DefaultContract]): DefaultContract = {
    if (p.rhs.isInstanceOf[DefaultContract@unchecked]) p.rhs.asInstanceOf[DefaultContract]
    else
      DefaultContractAdapter(p)
  }

  // familyElem
  class DefaultContractElem[To <: DefaultContract]
    extends SigmaContractElem[To] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaContractElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override lazy val tag = {
      weakTypeTag[DefaultContract].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      val conv = fun {x: Rep[DefaultContract] => convertDefaultContract(x) }
      tryConvert(element[DefaultContract], this, x, conv)
    }

    def convertDefaultContract(x: Rep[DefaultContract]): Rep[To] = {
      x.elem match {
        case _: DefaultContractElem[_] => asRep[To](x)
        case e => !!!(s"Expected $x to have DefaultContractElem[_], but got $e", x)
      }
    }
    override def getDefaultRep: Rep[To] = ???
  }

  implicit lazy val defaultContractElement: Elem[DefaultContract] =
    new DefaultContractElem[DefaultContract]

  implicit case object DefaultContractCompanionElem extends CompanionElem[DefaultContractCompanionCtor] {
    lazy val tag = weakTypeTag[DefaultContractCompanionCtor]
    protected def getDefaultRep = RDefaultContract
  }

  abstract class DefaultContractCompanionCtor extends CompanionDef[DefaultContractCompanionCtor] with DefaultContractCompanion {
    def selfType = DefaultContractCompanionElem
    override def toString = "DefaultContract"
  }
  implicit def proxyDefaultContractCompanionCtor(p: Rep[DefaultContractCompanionCtor]): DefaultContractCompanionCtor =
    proxyOps[DefaultContractCompanionCtor](p)

  lazy val RDefaultContract: Rep[DefaultContractCompanionCtor] = new DefaultContractCompanionCtor {
    private val thisClass = classOf[DefaultContractCompanion]
  }

  object DefaultContractMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[DefaultContract]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[DefaultContractElem[_]] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[DefaultContract]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[DefaultContract]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object DefaultContractCompanionMethods {
  }
} // of object DefaultContract
  registerEntityObject("DefaultContract", DefaultContract)

object TestBox extends EntityObject("TestBox") {
  case class TestBoxCtor
      (override val id: Rep[Coll[Byte]], override val value: Rep[Long], override val bytes: Rep[Coll[Byte]], override val bytesWithoutRef: Rep[Coll[Byte]], override val propositionBytes: Rep[Coll[Byte]], override val registers: Rep[Coll[AnyValue]])
    extends TestBox(id, value, bytes, bytesWithoutRef, propositionBytes, registers) with Def[TestBox] {
    lazy val selfType = element[TestBox]
    override def transform(t: Transformer) = TestBoxCtor(t(id), t(value), t(bytes), t(bytesWithoutRef), t(propositionBytes), t(registers))
    private val thisClass = classOf[Box]

    override def getReg[T](id: Rep[Int])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        thisClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, false, element[WOption[T]]))
    }

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, false, element[Int]))
    }

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class TestBoxElem(val iso: Iso[TestBoxData, TestBox])
    extends BoxElem[TestBox]
    with ConcreteElem[TestBoxData, TestBox] {
    override lazy val parent: Option[Elem[_]] = Some(boxElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertBox(x: Rep[Box]) = RTestBox(x.id, x.value, x.bytes, x.bytesWithoutRef, x.propositionBytes, x.registers)
    override def getDefaultRep = RTestBox(element[Coll[Byte]].defaultRepValue, 0l, element[Coll[Byte]].defaultRepValue, element[Coll[Byte]].defaultRepValue, element[Coll[Byte]].defaultRepValue, element[Coll[AnyValue]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[TestBox]
    }
  }

  // state representation type
  type TestBoxData = (Coll[Byte], (Long, (Coll[Byte], (Coll[Byte], (Coll[Byte], Coll[AnyValue])))))

  // 3) Iso for concrete class
  class TestBoxIso
    extends EntityIso[TestBoxData, TestBox] with Def[TestBoxIso] {
    override def transform(t: Transformer) = new TestBoxIso()
    private lazy val _safeFrom = fun { p: Rep[TestBox] => (p.id, p.value, p.bytes, p.bytesWithoutRef, p.propositionBytes, p.registers) }
    override def from(p: Rep[TestBox]) =
      tryConvert[TestBox, (Coll[Byte], (Long, (Coll[Byte], (Coll[Byte], (Coll[Byte], Coll[AnyValue])))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Coll[Byte], (Long, (Coll[Byte], (Coll[Byte], (Coll[Byte], Coll[AnyValue])))))]) = {
      val Pair(id, Pair(value, Pair(bytes, Pair(bytesWithoutRef, Pair(propositionBytes, registers))))) = p
      RTestBox(id, value, bytes, bytesWithoutRef, propositionBytes, registers)
    }
    lazy val eFrom = pairElement(element[Coll[Byte]], pairElement(element[Long], pairElement(element[Coll[Byte]], pairElement(element[Coll[Byte]], pairElement(element[Coll[Byte]], element[Coll[AnyValue]])))))
    lazy val eTo = new TestBoxElem(self)
    lazy val selfType = new TestBoxIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestBoxIsoElem() extends Elem[TestBoxIso] {
    def getDefaultRep = reifyObject(new TestBoxIso())
    lazy val tag = {
      weakTypeTag[TestBoxIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestBoxCompanionCtor extends CompanionDef[TestBoxCompanionCtor] with TestBoxCompanion {
    def selfType = TestBoxCompanionElem
    override def toString = "TestBoxCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestBoxData]): Rep[TestBox] = {
      isoTestBox.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(id: Rep[Coll[Byte]], value: Rep[Long], bytes: Rep[Coll[Byte]], bytesWithoutRef: Rep[Coll[Byte]], propositionBytes: Rep[Coll[Byte]], registers: Rep[Coll[AnyValue]]): Rep[TestBox] =
      mkTestBox(id, value, bytes, bytesWithoutRef, propositionBytes, registers)

    def unapply(p: Rep[Box]) = unmkTestBox(p)
  }
  lazy val TestBoxRep: Rep[TestBoxCompanionCtor] = new TestBoxCompanionCtor
  lazy val RTestBox: TestBoxCompanionCtor = proxyTestBoxCompanion(TestBoxRep)
  implicit def proxyTestBoxCompanion(p: Rep[TestBoxCompanionCtor]): TestBoxCompanionCtor = {
    if (p.rhs.isInstanceOf[TestBoxCompanionCtor])
      p.rhs.asInstanceOf[TestBoxCompanionCtor]
    else
      proxyOps[TestBoxCompanionCtor](p)
  }

  implicit case object TestBoxCompanionElem extends CompanionElem[TestBoxCompanionCtor] {
    lazy val tag = weakTypeTag[TestBoxCompanionCtor]
    protected def getDefaultRep = TestBoxRep
  }

  implicit def proxyTestBox(p: Rep[TestBox]): TestBox =
    proxyOps[TestBox](p)

  implicit class ExtendedTestBox(p: Rep[TestBox]) {
    def toData: Rep[TestBoxData] = {
      isoTestBox.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestBox: Iso[TestBoxData, TestBox] =
    reifyObject(new TestBoxIso())

  def mkTestBox
    (id: Rep[Coll[Byte]], value: Rep[Long], bytes: Rep[Coll[Byte]], bytesWithoutRef: Rep[Coll[Byte]], propositionBytes: Rep[Coll[Byte]], registers: Rep[Coll[AnyValue]]): Rep[TestBox] = {
    new TestBoxCtor(id, value, bytes, bytesWithoutRef, propositionBytes, registers)
  }
  def unmkTestBox(p: Rep[Box]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestBoxElem @unchecked =>
      Some((asRep[TestBox](p).id, asRep[TestBox](p).value, asRep[TestBox](p).bytes, asRep[TestBox](p).bytesWithoutRef, asRep[TestBox](p).propositionBytes, asRep[TestBox](p).registers))
    case _ =>
      None
  }

    object TestBoxMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getReg {
      def unapply(d: Def[_]): Nullable[(Rep[TestBox], Rep[Int], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "getReg" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestBox], Rep[Int], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestBox], Rep[Int], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object creationInfo {
      def unapply(d: Def[_]): Nullable[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "creationInfo" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object tokens {
      def unapply(d: Def[_]): Nullable[Rep[TestBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestBoxElem] && method.getName == "tokens" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object TestBoxCompanionMethods {
  }
} // of object TestBox
  registerEntityObject("TestBox", TestBox)

object TestAvlTree extends EntityObject("TestAvlTree") {
  case class TestAvlTreeCtor
      (override val startingDigest: Rep[Coll[Byte]], override val keyLength: Rep[Int], override val valueLengthOpt: Rep[WOption[Int]], override val maxNumOperations: Rep[WOption[Int]], override val maxDeletes: Rep[WOption[Int]])
    extends TestAvlTree(startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes) with Def[TestAvlTree] {
    lazy val selfType = element[TestAvlTree]
    override def transform(t: Transformer) = TestAvlTreeCtor(t(startingDigest), t(keyLength), t(valueLengthOpt), t(maxNumOperations), t(maxDeletes))
    private val thisClass = classOf[AvlTree]

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, false, element[Int]))
    }
  }
  // elem for concrete class
  class TestAvlTreeElem(val iso: Iso[TestAvlTreeData, TestAvlTree])
    extends AvlTreeElem[TestAvlTree]
    with ConcreteElem[TestAvlTreeData, TestAvlTree] {
    override lazy val parent: Option[Elem[_]] = Some(avlTreeElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertAvlTree(x: Rep[AvlTree]) = RTestAvlTree(x.startingDigest, x.keyLength, x.valueLengthOpt, x.maxNumOperations, x.maxDeletes)
    override def getDefaultRep = RTestAvlTree(element[Coll[Byte]].defaultRepValue, 0, element[WOption[Int]].defaultRepValue, element[WOption[Int]].defaultRepValue, element[WOption[Int]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[TestAvlTree]
    }
  }

  // state representation type
  type TestAvlTreeData = (Coll[Byte], (Int, (WOption[Int], (WOption[Int], WOption[Int]))))

  // 3) Iso for concrete class
  class TestAvlTreeIso
    extends EntityIso[TestAvlTreeData, TestAvlTree] with Def[TestAvlTreeIso] {
    override def transform(t: Transformer) = new TestAvlTreeIso()
    private lazy val _safeFrom = fun { p: Rep[TestAvlTree] => (p.startingDigest, p.keyLength, p.valueLengthOpt, p.maxNumOperations, p.maxDeletes) }
    override def from(p: Rep[TestAvlTree]) =
      tryConvert[TestAvlTree, (Coll[Byte], (Int, (WOption[Int], (WOption[Int], WOption[Int]))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Coll[Byte], (Int, (WOption[Int], (WOption[Int], WOption[Int]))))]) = {
      val Pair(startingDigest, Pair(keyLength, Pair(valueLengthOpt, Pair(maxNumOperations, maxDeletes)))) = p
      RTestAvlTree(startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes)
    }
    lazy val eFrom = pairElement(element[Coll[Byte]], pairElement(element[Int], pairElement(element[WOption[Int]], pairElement(element[WOption[Int]], element[WOption[Int]]))))
    lazy val eTo = new TestAvlTreeElem(self)
    lazy val selfType = new TestAvlTreeIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestAvlTreeIsoElem() extends Elem[TestAvlTreeIso] {
    def getDefaultRep = reifyObject(new TestAvlTreeIso())
    lazy val tag = {
      weakTypeTag[TestAvlTreeIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestAvlTreeCompanionCtor extends CompanionDef[TestAvlTreeCompanionCtor] with TestAvlTreeCompanion {
    def selfType = TestAvlTreeCompanionElem
    override def toString = "TestAvlTreeCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestAvlTreeData]): Rep[TestAvlTree] = {
      isoTestAvlTree.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(startingDigest: Rep[Coll[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]], maxNumOperations: Rep[WOption[Int]], maxDeletes: Rep[WOption[Int]]): Rep[TestAvlTree] =
      mkTestAvlTree(startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes)

    def unapply(p: Rep[AvlTree]) = unmkTestAvlTree(p)
  }
  lazy val TestAvlTreeRep: Rep[TestAvlTreeCompanionCtor] = new TestAvlTreeCompanionCtor
  lazy val RTestAvlTree: TestAvlTreeCompanionCtor = proxyTestAvlTreeCompanion(TestAvlTreeRep)
  implicit def proxyTestAvlTreeCompanion(p: Rep[TestAvlTreeCompanionCtor]): TestAvlTreeCompanionCtor = {
    if (p.rhs.isInstanceOf[TestAvlTreeCompanionCtor])
      p.rhs.asInstanceOf[TestAvlTreeCompanionCtor]
    else
      proxyOps[TestAvlTreeCompanionCtor](p)
  }

  implicit case object TestAvlTreeCompanionElem extends CompanionElem[TestAvlTreeCompanionCtor] {
    lazy val tag = weakTypeTag[TestAvlTreeCompanionCtor]
    protected def getDefaultRep = TestAvlTreeRep
  }

  implicit def proxyTestAvlTree(p: Rep[TestAvlTree]): TestAvlTree =
    proxyOps[TestAvlTree](p)

  implicit class ExtendedTestAvlTree(p: Rep[TestAvlTree]) {
    def toData: Rep[TestAvlTreeData] = {
      isoTestAvlTree.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestAvlTree: Iso[TestAvlTreeData, TestAvlTree] =
    reifyObject(new TestAvlTreeIso())

  def mkTestAvlTree
    (startingDigest: Rep[Coll[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]], maxNumOperations: Rep[WOption[Int]], maxDeletes: Rep[WOption[Int]]): Rep[TestAvlTree] = {
    new TestAvlTreeCtor(startingDigest, keyLength, valueLengthOpt, maxNumOperations, maxDeletes)
  }
  def unmkTestAvlTree(p: Rep[AvlTree]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestAvlTreeElem @unchecked =>
      Some((asRep[TestAvlTree](p).startingDigest, asRep[TestAvlTree](p).keyLength, asRep[TestAvlTree](p).valueLengthOpt, asRep[TestAvlTree](p).maxNumOperations, asRep[TestAvlTree](p).maxDeletes))
    case _ =>
      None
  }

    object TestAvlTreeMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[TestAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestAvlTreeElem] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[TestAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestAvlTreeElem] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[TestAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestAvlTreeElem] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestAvlTree]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestAvlTree]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object TestAvlTreeCompanionMethods {
  }
} // of object TestAvlTree
  registerEntityObject("TestAvlTree", TestAvlTree)

object TestValue extends EntityObject("TestValue") {
  case class TestValueCtor[T]
      (override val value: Rep[T])
    extends TestValue[T](value) with Def[TestValue[T]] {
    implicit lazy val eT = value.elem

    lazy val selfType = element[TestValue[T]]
    override def transform(t: Transformer) = TestValueCtor[T](t(value))
    private val thisClass = classOf[AnyValue]

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class TestValueElem[T](val iso: Iso[TestValueData[T], TestValue[T]])(implicit val eT: Elem[T])
    extends AnyValueElem[TestValue[T]]
    with ConcreteElem[TestValueData[T], TestValue[T]] {
    override lazy val parent: Option[Elem[_]] = Some(anyValueElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
    override def convertAnyValue(x: Rep[AnyValue]) = // Converter is not generated by meta
!!!("Cannot convert from AnyValue to TestValue: missing fields List(value)")
    override def getDefaultRep = RTestValue(element[T].defaultRepValue)
    override lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[TestValue[T]]
    }
  }

  // state representation type
  type TestValueData[T] = T

  // 3) Iso for concrete class
  class TestValueIso[T](implicit eT: Elem[T])
    extends EntityIso[TestValueData[T], TestValue[T]] with Def[TestValueIso[T]] {
    override def transform(t: Transformer) = new TestValueIso[T]()(eT)
    private lazy val _safeFrom = fun { p: Rep[TestValue[T]] => p.value }
    override def from(p: Rep[TestValue[T]]) =
      tryConvert[TestValue[T], T](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[T]) = {
      val value = p
      RTestValue(value)
    }
    lazy val eFrom = element[T]
    lazy val eTo = new TestValueElem[T](self)
    lazy val selfType = new TestValueIsoElem[T](eT)
    def productArity = 1
    def productElement(n: Int) = eT
  }
  case class TestValueIsoElem[T](eT: Elem[T]) extends Elem[TestValueIso[T]] {
    def getDefaultRep = reifyObject(new TestValueIso[T]()(eT))
    lazy val tag = {
      implicit val tagT = eT.tag
      weakTypeTag[TestValueIso[T]]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs("T" -> (eT -> scalan.util.Invariant))
  }
  // 4) constructor and deconstructor
  class TestValueCompanionCtor extends CompanionDef[TestValueCompanionCtor] with TestValueCompanion {
    def selfType = TestValueCompanionElem
    override def toString = "TestValueCompanion"

    @scalan.OverloadId("fromFields")
    def apply[T](value: Rep[T]): Rep[TestValue[T]] =
      mkTestValue(value)

    def unapply[T](p: Rep[AnyValue]) = unmkTestValue(p)
  }
  lazy val TestValueRep: Rep[TestValueCompanionCtor] = new TestValueCompanionCtor
  lazy val RTestValue: TestValueCompanionCtor = proxyTestValueCompanion(TestValueRep)
  implicit def proxyTestValueCompanion(p: Rep[TestValueCompanionCtor]): TestValueCompanionCtor = {
    if (p.rhs.isInstanceOf[TestValueCompanionCtor])
      p.rhs.asInstanceOf[TestValueCompanionCtor]
    else
      proxyOps[TestValueCompanionCtor](p)
  }

  implicit case object TestValueCompanionElem extends CompanionElem[TestValueCompanionCtor] {
    lazy val tag = weakTypeTag[TestValueCompanionCtor]
    protected def getDefaultRep = TestValueRep
  }

  implicit def proxyTestValue[T](p: Rep[TestValue[T]]): TestValue[T] =
    proxyOps[TestValue[T]](p)

  implicit class ExtendedTestValue[T](p: Rep[TestValue[T]]) {
    def toData: Rep[TestValueData[T]] = {
      implicit val eT = p.value.elem
      isoTestValue(eT).from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestValue[T](implicit eT: Elem[T]): Iso[TestValueData[T], TestValue[T]] =
    reifyObject(new TestValueIso[T]()(eT))

  def mkTestValue[T]
    (value: Rep[T]): Rep[TestValue[T]] = {
    new TestValueCtor[T](value)
  }
  def unmkTestValue[T](p: Rep[AnyValue]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestValueElem[T] @unchecked =>
      Some((asRep[TestValue[T]](p).value))
    case _ =>
      None
  }

    object TestValueMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[TestValue[T]] forSome {type T}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestValueElem[_]] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestValue[T]] forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestValue[T]] forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object TestValueCompanionMethods {
  }
} // of object TestValue
  registerEntityObject("TestValue", TestValue)

object TestContext extends EntityObject("TestContext") {
  case class TestContextCtor
      (override val inputs: Rep[WArray[Box]], override val outputs: Rep[WArray[Box]], override val height: Rep[Int], override val selfBox: Rep[Box], override val lastBlockUtxoRootHash: Rep[AvlTree], override val minerPubKey: Rep[WArray[Byte]], override val vars: Rep[WArray[AnyValue]])
    extends TestContext(inputs, outputs, height, selfBox, lastBlockUtxoRootHash, minerPubKey, vars) with Def[TestContext] {
    lazy val selfType = element[TestContext]
    override def transform(t: Transformer) = TestContextCtor(t(inputs), t(outputs), t(height), t(selfBox), t(lastBlockUtxoRootHash), t(minerPubKey), t(vars))
    private val thisClass = classOf[Context]

    override def HEIGHT: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("HEIGHT"),
        List(),
        true, false, element[Int]))
    }

    override def SELF: Rep[Box] = {
      asRep[Box](mkMethodCall(self,
        thisClass.getMethod("SELF"),
        List(),
        true, false, element[Box]))
    }

    override def INPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(self,
        thisClass.getMethod("INPUTS"),
        List(),
        true, false, element[Coll[Box]]))
    }

    override def OUTPUTS: Rep[Coll[Box]] = {
      asRep[Coll[Box]](mkMethodCall(self,
        thisClass.getMethod("OUTPUTS"),
        List(),
        true, false, element[Coll[Box]]))
    }

    override def LastBlockUtxoRootHash: Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        thisClass.getMethod("LastBlockUtxoRootHash"),
        List(),
        true, false, element[AvlTree]))
    }

    override def MinerPubKey: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("MinerPubKey"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def getVar[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[WOption[T]] = {
      asRep[WOption[T]](mkMethodCall(self,
        thisClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, false, element[WOption[T]]))
    }

    // manual fix
    override def getConstant[T](id: Rep[Byte])(implicit cT: Elem[T]): Rep[T] = {
      asRep[Nothing](mkMethodCall(self,
        thisClass.getMethod("getConstant", classOf[Sym], classOf[Elem[_]]),
        List(id, cT),
        true, false, cT))
    }

    override def cost: Rep[Int] = {
      asRep[Int](mkMethodCall(self,
        thisClass.getMethod("cost"),
        List(),
        true, false, element[Int]))
    }

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class TestContextElem(val iso: Iso[TestContextData, TestContext])
    extends ContextElem[TestContext]
    with ConcreteElem[TestContextData, TestContext] {
    override lazy val parent: Option[Elem[_]] = Some(contextElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertContext(x: Rep[Context]) = // Converter is not generated by meta
!!!("Cannot convert from Context to TestContext: missing fields List(inputs, outputs, height, selfBox, lastBlockUtxoRootHash, minerPubKey, vars)")
    override def getDefaultRep = RTestContext(element[WArray[Box]].defaultRepValue, element[WArray[Box]].defaultRepValue, 0, element[Box].defaultRepValue, element[AvlTree].defaultRepValue, element[WArray[Byte]].defaultRepValue, element[WArray[AnyValue]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[TestContext]
    }
  }

  // state representation type
  type TestContextData = (WArray[Box], (WArray[Box], (Int, (Box, (AvlTree, (WArray[Byte], WArray[AnyValue]))))))

  // 3) Iso for concrete class
  class TestContextIso
    extends EntityIso[TestContextData, TestContext] with Def[TestContextIso] {
    override def transform(t: Transformer) = new TestContextIso()
    private lazy val _safeFrom = fun { p: Rep[TestContext] => (p.inputs, p.outputs, p.height, p.selfBox, p.lastBlockUtxoRootHash, p.minerPubKey, p.vars) }
    override def from(p: Rep[TestContext]) =
      tryConvert[TestContext, (WArray[Box], (WArray[Box], (Int, (Box, (AvlTree, (WArray[Byte], WArray[AnyValue]))))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(WArray[Box], (WArray[Box], (Int, (Box, (AvlTree, (WArray[Byte], WArray[AnyValue]))))))]) = {
      val Pair(inputs, Pair(outputs, Pair(height, Pair(selfBox, Pair(lastBlockUtxoRootHash, Pair(minerPubKey, vars)))))) = p
      RTestContext(inputs, outputs, height, selfBox, lastBlockUtxoRootHash, minerPubKey, vars)
    }
    lazy val eFrom = pairElement(element[WArray[Box]], pairElement(element[WArray[Box]], pairElement(element[Int], pairElement(element[Box], pairElement(element[AvlTree], pairElement(element[WArray[Byte]], element[WArray[AnyValue]]))))))
    lazy val eTo = new TestContextElem(self)
    lazy val selfType = new TestContextIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestContextIsoElem() extends Elem[TestContextIso] {
    def getDefaultRep = reifyObject(new TestContextIso())
    lazy val tag = {
      weakTypeTag[TestContextIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestContextCompanionCtor extends CompanionDef[TestContextCompanionCtor] with TestContextCompanion {
    def selfType = TestContextCompanionElem
    override def toString = "TestContextCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestContextData]): Rep[TestContext] = {
      isoTestContext.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(inputs: Rep[WArray[Box]], outputs: Rep[WArray[Box]], height: Rep[Int], selfBox: Rep[Box], lastBlockUtxoRootHash: Rep[AvlTree], minerPubKey: Rep[WArray[Byte]], vars: Rep[WArray[AnyValue]]): Rep[TestContext] =
      mkTestContext(inputs, outputs, height, selfBox, lastBlockUtxoRootHash, minerPubKey, vars)

    def unapply(p: Rep[Context]) = unmkTestContext(p)
  }
  lazy val TestContextRep: Rep[TestContextCompanionCtor] = new TestContextCompanionCtor
  lazy val RTestContext: TestContextCompanionCtor = proxyTestContextCompanion(TestContextRep)
  implicit def proxyTestContextCompanion(p: Rep[TestContextCompanionCtor]): TestContextCompanionCtor = {
    if (p.rhs.isInstanceOf[TestContextCompanionCtor])
      p.rhs.asInstanceOf[TestContextCompanionCtor]
    else
      proxyOps[TestContextCompanionCtor](p)
  }

  implicit case object TestContextCompanionElem extends CompanionElem[TestContextCompanionCtor] {
    lazy val tag = weakTypeTag[TestContextCompanionCtor]
    protected def getDefaultRep = TestContextRep
  }

  implicit def proxyTestContext(p: Rep[TestContext]): TestContext =
    proxyOps[TestContext](p)

  implicit class ExtendedTestContext(p: Rep[TestContext]) {
    def toData: Rep[TestContextData] = {
      isoTestContext.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestContext: Iso[TestContextData, TestContext] =
    reifyObject(new TestContextIso())

  def mkTestContext
    (inputs: Rep[WArray[Box]], outputs: Rep[WArray[Box]], height: Rep[Int], selfBox: Rep[Box], lastBlockUtxoRootHash: Rep[AvlTree], minerPubKey: Rep[WArray[Byte]], vars: Rep[WArray[AnyValue]]): Rep[TestContext] = {
    new TestContextCtor(inputs, outputs, height, selfBox, lastBlockUtxoRootHash, minerPubKey, vars)
  }
  def unmkTestContext(p: Rep[Context]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestContextElem @unchecked =>
      Some((asRep[TestContext](p).inputs, asRep[TestContext](p).outputs, asRep[TestContext](p).height, asRep[TestContext](p).selfBox, asRep[TestContext](p).lastBlockUtxoRootHash, asRep[TestContext](p).minerPubKey, asRep[TestContext](p).vars))
    case _ =>
      None
  }

    object TestContextMethods {
    object builder {
      def unapply(d: Def[_]): Nullable[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "builder" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object HEIGHT {
      def unapply(d: Def[_]): Nullable[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "HEIGHT" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object SELF {
      def unapply(d: Def[_]): Nullable[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "SELF" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object INPUTS {
      def unapply(d: Def[_]): Nullable[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "INPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object OUTPUTS {
      def unapply(d: Def[_]): Nullable[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "OUTPUTS" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object LastBlockUtxoRootHash {
      def unapply(d: Def[_]): Nullable[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "LastBlockUtxoRootHash" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object MinerPubKey {
      def unapply(d: Def[_]): Nullable[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "MinerPubKey" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getVar {
      def unapply(d: Def[_]): Nullable[(Rep[TestContext], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "getVar" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestContext], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestContext], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object getConstant {
      def unapply(d: Def[_]): Nullable[(Rep[TestContext], Rep[Byte], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "getConstant" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestContext], Rep[Byte], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestContext], Rep[Byte], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object cost {
      def unapply(d: Def[_]): Nullable[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "cost" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[TestContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestContextElem] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object TestContextCompanionMethods {
  }
} // of object TestContext
  registerEntityObject("TestContext", TestContext)

object TestSigmaDslBuilder extends EntityObject("TestSigmaDslBuilder") {
  case class TestSigmaDslBuilderCtor
      ()
    extends TestSigmaDslBuilder() with Def[TestSigmaDslBuilder] {
    lazy val selfType = element[TestSigmaDslBuilder]
    override def transform(t: Transformer) = TestSigmaDslBuilderCtor()
    private val thisClass = classOf[SigmaDslBuilder]

    override def CostModel: Rep[CostModel] = {
      asRep[CostModel](mkMethodCall(self,
        thisClass.getMethod("CostModel"),
        List(),
        true, false, element[CostModel]))
    }

    override def verifyZK(proof: Rep[Thunk[SigmaProp]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("verifyZK", classOf[Sym]),
        List(proof),
        true, false, element[Boolean]))
    }

    override def atLeast(bound: Rep[Int], props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("atLeast", classOf[Sym], classOf[Sym]),
        List(bound, props),
        true, false, element[SigmaProp]))
    }

    override def allOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("allOf", classOf[Sym]),
        List(conditions),
        true, false, element[Boolean]))
    }

    override def anyOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("anyOf", classOf[Sym]),
        List(conditions),
        true, false, element[Boolean]))
    }

    override def allZK(proofs: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("allZK", classOf[Sym]),
        List(proofs),
        true, false, element[SigmaProp]))
    }

    override def anyZK(proofs: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("anyZK", classOf[Sym]),
        List(proofs),
        true, false, element[SigmaProp]))
    }

    override def sigmaProp(b: Rep[Boolean]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("sigmaProp", classOf[Sym]),
        List(b),
        true, false, element[SigmaProp]))
    }

    override def blake2b256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("blake2b256", classOf[Sym]),
        List(bytes),
        true, false, element[Coll[Byte]]))
    }

    override def sha256(bytes: Rep[Coll[Byte]]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("sha256", classOf[Sym]),
        List(bytes),
        true, false, element[Coll[Byte]]))
    }

    override def PubKey(base64String: Rep[String]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("PubKey", classOf[Sym]),
        List(base64String),
        true, false, element[SigmaProp]))
    }

    override def byteArrayToBigInt(bytes: Rep[Coll[Byte]]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        List(bytes),
        true, false, element[WBigInteger]))
    }

    override def longToByteArray(l: Rep[Long]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("longToByteArray", classOf[Sym]),
        List(l),
        true, false, element[Coll[Byte]]))
    }

    override def proveDlog(g: Rep[WECPoint]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("proveDlog", classOf[Sym]),
        List(g),
        true, false, element[SigmaProp]))
    }

    override def proveDHTuple(g: Rep[WECPoint], h: Rep[WECPoint], u: Rep[WECPoint], v: Rep[WECPoint]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(g, h, u, v),
        true, false, element[SigmaProp]))
    }

    override def isMember(tree: Rep[AvlTree], key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("isMember", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, key, proof),
        true, false, element[Boolean]))
    }

    override def treeLookup(tree: Rep[AvlTree], key: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[WOption[Coll[Byte]]] = {
      asRep[WOption[Coll[Byte]]](mkMethodCall(self,
        thisClass.getMethod("treeLookup", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, key, proof),
        true, false, element[WOption[Coll[Byte]]]))
    }

    override def treeModifications(tree: Rep[AvlTree], operations: Rep[Coll[Byte]], proof: Rep[Coll[Byte]]): Rep[WOption[Coll[Byte]]] = {
      asRep[WOption[Coll[Byte]]](mkMethodCall(self,
        thisClass.getMethod("treeModifications", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, operations, proof),
        true, false, element[WOption[Coll[Byte]]]))
    }

    override def treeInserts(tree: Rep[AvlTree], operations: Rep[Coll[(Coll[Byte], Coll[Byte])]], proof: Rep[Coll[Byte]]): Rep[WOption[Coll[Byte]]] = {
      asRep[WOption[Coll[Byte]]](mkMethodCall(self,
        thisClass.getMethod("treeInserts", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, operations, proof),
        true, false, element[WOption[Coll[Byte]]]))
    }

    override def treeRemovals(tree: Rep[AvlTree], operations: Rep[Coll[Coll[Byte]]], proof: Rep[Coll[Byte]]): Rep[WOption[Coll[Byte]]] = {
      asRep[WOption[Coll[Byte]]](mkMethodCall(self,
        thisClass.getMethod("treeRemovals", classOf[Sym], classOf[Sym], classOf[Sym]),
        List(tree, operations, proof),
        true, false, element[WOption[Coll[Byte]]]))
    }

    override def groupGenerator: Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(self,
        thisClass.getMethod("groupGenerator"),
        List(),
        true, false, element[WECPoint]))
    }

    override def exponentiate(base: Rep[WECPoint], exponent: Rep[WBigInteger]): Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(self,
        thisClass.getMethod("exponentiate", classOf[Sym], classOf[Sym]),
        List(base, exponent),
        true, false, element[WECPoint]))
    }

    override def substConstants[T](scriptBytes: Rep[Coll[Byte]], positions: Rep[Coll[Int]], newValues: Rep[Coll[T]])(implicit cT: Elem[T]): Rep[Coll[Byte]] = {
      implicit val eT = newValues.eA
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Elem[_]]),
        List(scriptBytes, positions, newValues, cT),
        true, false, element[Coll[Byte]]))
    }

    override def decodePoint(encoded: Rep[Coll[Byte]]): Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(self,
        thisClass.getMethod("decodePoint", classOf[Sym]),
        List(encoded),
        true, false, element[WECPoint]))
    }
  }
  // elem for concrete class
  class TestSigmaDslBuilderElem(val iso: Iso[TestSigmaDslBuilderData, TestSigmaDslBuilder])
    extends SigmaDslBuilderElem[TestSigmaDslBuilder]
    with ConcreteElem[TestSigmaDslBuilderData, TestSigmaDslBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaDslBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSigmaDslBuilder(x: Rep[SigmaDslBuilder]) = RTestSigmaDslBuilder()
    override def getDefaultRep = RTestSigmaDslBuilder()
    override lazy val tag = {
      weakTypeTag[TestSigmaDslBuilder]
    }
  }

  // state representation type
  type TestSigmaDslBuilderData = Unit

  // 3) Iso for concrete class
  class TestSigmaDslBuilderIso
    extends EntityIso[TestSigmaDslBuilderData, TestSigmaDslBuilder] with Def[TestSigmaDslBuilderIso] {
    override def transform(t: Transformer) = new TestSigmaDslBuilderIso()
    private lazy val _safeFrom = fun { p: Rep[TestSigmaDslBuilder] => () }
    override def from(p: Rep[TestSigmaDslBuilder]) =
      tryConvert[TestSigmaDslBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RTestSigmaDslBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new TestSigmaDslBuilderElem(self)
    lazy val selfType = new TestSigmaDslBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TestSigmaDslBuilderIsoElem() extends Elem[TestSigmaDslBuilderIso] {
    def getDefaultRep = reifyObject(new TestSigmaDslBuilderIso())
    lazy val tag = {
      weakTypeTag[TestSigmaDslBuilderIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TestSigmaDslBuilderCompanionCtor extends CompanionDef[TestSigmaDslBuilderCompanionCtor] with TestSigmaDslBuilderCompanion {
    def selfType = TestSigmaDslBuilderCompanionElem
    override def toString = "TestSigmaDslBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[TestSigmaDslBuilderData]): Rep[TestSigmaDslBuilder] = {
      isoTestSigmaDslBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[TestSigmaDslBuilder] =
      mkTestSigmaDslBuilder()

    def unapply(p: Rep[SigmaDslBuilder]) = unmkTestSigmaDslBuilder(p)
  }
  lazy val TestSigmaDslBuilderRep: Rep[TestSigmaDslBuilderCompanionCtor] = new TestSigmaDslBuilderCompanionCtor
  lazy val RTestSigmaDslBuilder: TestSigmaDslBuilderCompanionCtor = proxyTestSigmaDslBuilderCompanion(TestSigmaDslBuilderRep)
  implicit def proxyTestSigmaDslBuilderCompanion(p: Rep[TestSigmaDslBuilderCompanionCtor]): TestSigmaDslBuilderCompanionCtor = {
    if (p.rhs.isInstanceOf[TestSigmaDslBuilderCompanionCtor])
      p.rhs.asInstanceOf[TestSigmaDslBuilderCompanionCtor]
    else
      proxyOps[TestSigmaDslBuilderCompanionCtor](p)
  }

  implicit case object TestSigmaDslBuilderCompanionElem extends CompanionElem[TestSigmaDslBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[TestSigmaDslBuilderCompanionCtor]
    protected def getDefaultRep = TestSigmaDslBuilderRep
  }

  implicit def proxyTestSigmaDslBuilder(p: Rep[TestSigmaDslBuilder]): TestSigmaDslBuilder =
    proxyOps[TestSigmaDslBuilder](p)

  implicit class ExtendedTestSigmaDslBuilder(p: Rep[TestSigmaDslBuilder]) {
    def toData: Rep[TestSigmaDslBuilderData] = {
      isoTestSigmaDslBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTestSigmaDslBuilder: Iso[TestSigmaDslBuilderData, TestSigmaDslBuilder] =
    reifyObject(new TestSigmaDslBuilderIso())

  def mkTestSigmaDslBuilder
    (): Rep[TestSigmaDslBuilder] = {
    new TestSigmaDslBuilderCtor()
  }
  def unmkTestSigmaDslBuilder(p: Rep[SigmaDslBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TestSigmaDslBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object TestSigmaDslBuilderMethods {
    object Colls {
      def unapply(d: Def[_]): Nullable[Rep[TestSigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "Colls" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestSigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestSigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object Monoids {
      def unapply(d: Def[_]): Nullable[Rep[TestSigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "Monoids" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestSigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestSigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object Costing {
      def unapply(d: Def[_]): Nullable[Rep[TestSigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "Costing" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestSigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestSigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object CostModel {
      def unapply(d: Def[_]): Nullable[Rep[TestSigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "CostModel" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestSigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestSigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costBoxes {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Box]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "costBoxes" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Box]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Box]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costColWithConstSizedItem {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[T]], Rep[Int], Rep[Long]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "costColWithConstSizedItem" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[T]], Rep[Int], Rep[Long]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[T]], Rep[Int], Rep[Long]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object costOption {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[WOption[T]], Rep[Int]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "costOption" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[WOption[T]], Rep[Int]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[WOption[T]], Rep[Int]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object verifyZK {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "verifyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object atLeast {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Int], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "atLeast" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Int], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Int], Rep[Coll[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allOf {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "allOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyOf {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "anyOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Boolean]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object allZK {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "allZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object anyZK {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "anyZK" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sigmaProp {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "sigmaProp" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object blake2b256 {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "blake2b256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object sha256 {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "sha256" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object PubKey {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[String])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "PubKey" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[String])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[String])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object byteArrayToBigInt {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "byteArrayToBigInt" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object longToByteArray {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Long])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "longToByteArray" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Long])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Long])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDlog {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "proveDlog" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "proveDHTuple" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isMember {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "isMember" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object treeLookup {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "treeLookup" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object treeModifications {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "treeModifications" =>
          val res = (receiver, args(0), args(1), args(2))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[AvlTree], Rep[Coll[Byte]], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object groupGenerator {
      def unapply(d: Def[_]): Nullable[Rep[TestSigmaDslBuilder]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "groupGenerator" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TestSigmaDslBuilder]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TestSigmaDslBuilder]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object exponentiate {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "exponentiate" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object substConstants {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Int]], Rep[Coll[T]], Elem[T]) forSome {type T}] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "substConstants" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Int]], Rep[Coll[T]], Elem[T]) forSome {type T}]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]], Rep[Coll[Int]], Rep[Coll[T]], Elem[T]) forSome {type T}] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object decodePoint {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "decodePoint" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object TestSigmaDslBuilderCompanionMethods {
  }
} // of object TestSigmaDslBuilder
  registerEntityObject("TestSigmaDslBuilder", TestSigmaDslBuilder)

object TrivialSigma extends EntityObject("TrivialSigma") {
  case class TrivialSigmaCtor
      (override val _isValid: Rep[Boolean])
    extends TrivialSigma(_isValid) with Def[TrivialSigma] {
    lazy val selfType = element[TrivialSigma]
    override def transform(t: Transformer) = TrivialSigmaCtor(t(_isValid))
    private val thisClass = classOf[SigmaProp]

    override def propBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("propBytes"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("isValid"),
        List(),
        true, false, element[Boolean]))
    }

    override def $amp$amp(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$amp$amp", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    // manual fix
    override def &&(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$amp$amp", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, false, element[SigmaProp]))
    }

    override def $bar$bar(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$bar$bar", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    // manual fix
    override def ||(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$bar$bar", classOf[Sym], classOf[Overloaded1]),
        List(other, o),
        true, false, element[SigmaProp]))
    }

    override def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("lazyAnd", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    override def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("lazyOr", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }
  }
  // elem for concrete class
  class TrivialSigmaElem(val iso: Iso[TrivialSigmaData, TrivialSigma])
    extends SigmaPropElem[TrivialSigma]
    with ConcreteElem[TrivialSigmaData, TrivialSigma] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaPropElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    // manual fix
    override def convertSigmaProp(x: Rep[SigmaProp]) = RTrivialSigma(x.isValid)
    override def getDefaultRep = RTrivialSigma(false)
    override lazy val tag = {
      weakTypeTag[TrivialSigma]
    }
  }

  // state representation type
  type TrivialSigmaData = Boolean

  // 3) Iso for concrete class
  class TrivialSigmaIso
    extends EntityIso[TrivialSigmaData, TrivialSigma] with Def[TrivialSigmaIso] {
    override def transform(t: Transformer) = new TrivialSigmaIso()
    private lazy val _safeFrom = fun { p: Rep[TrivialSigma] => p._isValid }
    override def from(p: Rep[TrivialSigma]) =
      tryConvert[TrivialSigma, Boolean](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Boolean]) = {
      val _isValid = p
      RTrivialSigma(_isValid)
    }
    lazy val eFrom = element[Boolean]
    lazy val eTo = new TrivialSigmaElem(self)
    lazy val selfType = new TrivialSigmaIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class TrivialSigmaIsoElem() extends Elem[TrivialSigmaIso] {
    def getDefaultRep = reifyObject(new TrivialSigmaIso())
    lazy val tag = {
      weakTypeTag[TrivialSigmaIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class TrivialSigmaCompanionCtor extends CompanionDef[TrivialSigmaCompanionCtor] with TrivialSigmaCompanion {
    def selfType = TrivialSigmaCompanionElem
    override def toString = "TrivialSigmaCompanion"

    @scalan.OverloadId("fromFields")
    def apply(_isValid: Rep[Boolean]): Rep[TrivialSigma] =
      mkTrivialSigma(_isValid)

    def unapply(p: Rep[SigmaProp]) = unmkTrivialSigma(p)
  }
  lazy val TrivialSigmaRep: Rep[TrivialSigmaCompanionCtor] = new TrivialSigmaCompanionCtor
  lazy val RTrivialSigma: TrivialSigmaCompanionCtor = proxyTrivialSigmaCompanion(TrivialSigmaRep)
  implicit def proxyTrivialSigmaCompanion(p: Rep[TrivialSigmaCompanionCtor]): TrivialSigmaCompanionCtor = {
    if (p.rhs.isInstanceOf[TrivialSigmaCompanionCtor])
      p.rhs.asInstanceOf[TrivialSigmaCompanionCtor]
    else
      proxyOps[TrivialSigmaCompanionCtor](p)
  }

  implicit case object TrivialSigmaCompanionElem extends CompanionElem[TrivialSigmaCompanionCtor] {
    lazy val tag = weakTypeTag[TrivialSigmaCompanionCtor]
    protected def getDefaultRep = TrivialSigmaRep
  }

  implicit def proxyTrivialSigma(p: Rep[TrivialSigma]): TrivialSigma =
    proxyOps[TrivialSigma](p)

  implicit class ExtendedTrivialSigma(p: Rep[TrivialSigma]) {
    def toData: Rep[TrivialSigmaData] = {
      isoTrivialSigma.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoTrivialSigma: Iso[TrivialSigmaData, TrivialSigma] =
    reifyObject(new TrivialSigmaIso())

  def mkTrivialSigma
    (_isValid: Rep[Boolean]): Rep[TrivialSigma] = {
    new TrivialSigmaCtor(_isValid)
  }
  def unmkTrivialSigma(p: Rep[SigmaProp]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TrivialSigmaElem @unchecked =>
      Some((asRep[TrivialSigma](p)._isValid))
    case _ =>
      None
  }

    object TrivialSigmaMethods {
    object propBytes {
      def unapply(d: Def[_]): Nullable[Rep[TrivialSigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "propBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TrivialSigma]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TrivialSigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isValid {
      def unapply(d: Def[_]): Nullable[Rep[TrivialSigma]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "isValid" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[TrivialSigma]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[TrivialSigma]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Nullable[(Rep[TrivialSigma], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TrivialSigma], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TrivialSigma], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Nullable[(Rep[TrivialSigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TrivialSigma], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TrivialSigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Nullable[(Rep[TrivialSigma], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TrivialSigma], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TrivialSigma], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Nullable[(Rep[TrivialSigma], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TrivialSigma], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TrivialSigma], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyAnd {
      def unapply(d: Def[_]): Nullable[(Rep[TrivialSigma], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "lazyAnd" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TrivialSigma], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TrivialSigma], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyOr {
      def unapply(d: Def[_]): Nullable[(Rep[TrivialSigma], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TrivialSigmaElem] && method.getName == "lazyOr" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TrivialSigma], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TrivialSigma], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object TrivialSigmaCompanionMethods {
  }
} // of object TrivialSigma
  registerEntityObject("TrivialSigma", TrivialSigma)

object ProveDlogEvidence extends EntityObject("ProveDlogEvidence") {
  case class ProveDlogEvidenceCtor
      (override val value: Rep[WECPoint])
    extends ProveDlogEvidence(value) with Def[ProveDlogEvidence] {
    lazy val selfType = element[ProveDlogEvidence]
    override def transform(t: Transformer) = ProveDlogEvidenceCtor(t(value))
    private val thisClass = classOf[SigmaProp]

    override def propBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("propBytes"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("isValid"),
        List(),
        true, false, element[Boolean]))
    }

    override def $amp$amp(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$amp$amp", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    override def $amp$amp(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$amp$amp", classOf[Sym], classOf[Sym]),
        List(other, o),
        true, false, element[SigmaProp]))
    }

    override def $bar$bar(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$bar$bar", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    override def $bar$bar(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$bar$bar", classOf[Sym], classOf[Sym]),
        List(other, o),
        true, false, element[SigmaProp]))
    }

    override def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("lazyAnd", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    override def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("lazyOr", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }
  }
  // elem for concrete class
  class ProveDlogEvidenceElem(val iso: Iso[ProveDlogEvidenceData, ProveDlogEvidence])
    extends SigmaPropElem[ProveDlogEvidence]
    with ConcreteElem[ProveDlogEvidenceData, ProveDlogEvidence] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaPropElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSigmaProp(x: Rep[SigmaProp]) = // Converter is not generated by meta
!!!("Cannot convert from SigmaProp to ProveDlogEvidence: missing fields List(value)")
    override def getDefaultRep = RProveDlogEvidence(element[WECPoint].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[ProveDlogEvidence]
    }
  }

  // state representation type
  type ProveDlogEvidenceData = WECPoint

  // 3) Iso for concrete class
  class ProveDlogEvidenceIso
    extends EntityIso[ProveDlogEvidenceData, ProveDlogEvidence] with Def[ProveDlogEvidenceIso] {
    override def transform(t: Transformer) = new ProveDlogEvidenceIso()
    private lazy val _safeFrom = fun { p: Rep[ProveDlogEvidence] => p.value }
    override def from(p: Rep[ProveDlogEvidence]) =
      tryConvert[ProveDlogEvidence, WECPoint](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[WECPoint]) = {
      val value = p
      RProveDlogEvidence(value)
    }
    lazy val eFrom = element[WECPoint]
    lazy val eTo = new ProveDlogEvidenceElem(self)
    lazy val selfType = new ProveDlogEvidenceIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ProveDlogEvidenceIsoElem() extends Elem[ProveDlogEvidenceIso] {
    def getDefaultRep = reifyObject(new ProveDlogEvidenceIso())
    lazy val tag = {
      weakTypeTag[ProveDlogEvidenceIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ProveDlogEvidenceCompanionCtor extends CompanionDef[ProveDlogEvidenceCompanionCtor] with ProveDlogEvidenceCompanion {
    def selfType = ProveDlogEvidenceCompanionElem
    override def toString = "ProveDlogEvidenceCompanion"

    @scalan.OverloadId("fromFields")
    def apply(value: Rep[WECPoint]): Rep[ProveDlogEvidence] =
      mkProveDlogEvidence(value)

    def unapply(p: Rep[SigmaProp]) = unmkProveDlogEvidence(p)
  }
  lazy val ProveDlogEvidenceRep: Rep[ProveDlogEvidenceCompanionCtor] = new ProveDlogEvidenceCompanionCtor
  lazy val RProveDlogEvidence: ProveDlogEvidenceCompanionCtor = proxyProveDlogEvidenceCompanion(ProveDlogEvidenceRep)
  implicit def proxyProveDlogEvidenceCompanion(p: Rep[ProveDlogEvidenceCompanionCtor]): ProveDlogEvidenceCompanionCtor = {
    if (p.rhs.isInstanceOf[ProveDlogEvidenceCompanionCtor])
      p.rhs.asInstanceOf[ProveDlogEvidenceCompanionCtor]
    else
      proxyOps[ProveDlogEvidenceCompanionCtor](p)
  }

  implicit case object ProveDlogEvidenceCompanionElem extends CompanionElem[ProveDlogEvidenceCompanionCtor] {
    lazy val tag = weakTypeTag[ProveDlogEvidenceCompanionCtor]
    protected def getDefaultRep = ProveDlogEvidenceRep
  }

  implicit def proxyProveDlogEvidence(p: Rep[ProveDlogEvidence]): ProveDlogEvidence =
    proxyOps[ProveDlogEvidence](p)

  implicit class ExtendedProveDlogEvidence(p: Rep[ProveDlogEvidence]) {
    def toData: Rep[ProveDlogEvidenceData] = {
      isoProveDlogEvidence.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoProveDlogEvidence: Iso[ProveDlogEvidenceData, ProveDlogEvidence] =
    reifyObject(new ProveDlogEvidenceIso())

  def mkProveDlogEvidence
    (value: Rep[WECPoint]): Rep[ProveDlogEvidence] = {
    new ProveDlogEvidenceCtor(value)
  }
  def unmkProveDlogEvidence(p: Rep[SigmaProp]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ProveDlogEvidenceElem @unchecked =>
      Some((asRep[ProveDlogEvidence](p).value))
    case _ =>
      None
  }

    object ProveDlogEvidenceMethods {
    object propBytes {
      def unapply(d: Def[_]): Nullable[Rep[ProveDlogEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "propBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ProveDlogEvidence]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ProveDlogEvidence]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isValid {
      def unapply(d: Def[_]): Nullable[Rep[ProveDlogEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "isValid" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ProveDlogEvidence]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ProveDlogEvidence]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDlogEvidence], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDlogEvidence], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDlogEvidence], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDlogEvidence], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDlogEvidence], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDlogEvidence], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDlogEvidence], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDlogEvidence], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDlogEvidence], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDlogEvidence], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDlogEvidence], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDlogEvidence], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyAnd {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDlogEvidence], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "lazyAnd" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDlogEvidence], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDlogEvidence], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyOr {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDlogEvidence], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDlogEvidenceElem] && method.getName == "lazyOr" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDlogEvidence], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDlogEvidence], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ProveDlogEvidenceCompanionMethods {
  }
} // of object ProveDlogEvidence
  registerEntityObject("ProveDlogEvidence", ProveDlogEvidence)

object ProveDHTEvidence extends EntityObject("ProveDHTEvidence") {
  case class ProveDHTEvidenceCtor
      (override val gv: Rep[WECPoint], override val hv: Rep[WECPoint], override val uv: Rep[WECPoint], override val vv: Rep[WECPoint])
    extends ProveDHTEvidence(gv, hv, uv, vv) with Def[ProveDHTEvidence] {
    lazy val selfType = element[ProveDHTEvidence]
    override def transform(t: Transformer) = ProveDHTEvidenceCtor(t(gv), t(hv), t(uv), t(vv))
    private val thisClass = classOf[SigmaProp]

    override def propBytes: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("propBytes"),
        List(),
        true, false, element[Coll[Byte]]))
    }

    override def isValid: Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("isValid"),
        List(),
        true, false, element[Boolean]))
    }

    override def $amp$amp(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$amp$amp", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    override def $amp$amp(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$amp$amp", classOf[Sym], classOf[Sym]),
        List(other, o),
        true, false, element[SigmaProp]))
    }

    override def $bar$bar(other: Rep[SigmaProp]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$bar$bar", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    override def $bar$bar(other: Rep[Boolean])(implicit o: Overloaded1): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("$bar$bar", classOf[Sym], classOf[Sym]),
        List(other, o),
        true, false, element[SigmaProp]))
    }

    override def lazyAnd(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("lazyAnd", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }

    override def lazyOr(other: Rep[Thunk[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("lazyOr", classOf[Sym]),
        List(other),
        true, false, element[SigmaProp]))
    }
  }
  // elem for concrete class
  class ProveDHTEvidenceElem(val iso: Iso[ProveDHTEvidenceData, ProveDHTEvidence])
    extends SigmaPropElem[ProveDHTEvidence]
    with ConcreteElem[ProveDHTEvidenceData, ProveDHTEvidence] {
    override lazy val parent: Option[Elem[_]] = Some(sigmaPropElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSigmaProp(x: Rep[SigmaProp]) = // Converter is not generated by meta
!!!("Cannot convert from SigmaProp to ProveDHTEvidence: missing fields List(gv, hv, uv, vv)")
    override def getDefaultRep = RProveDHTEvidence(element[WECPoint].defaultRepValue, element[WECPoint].defaultRepValue, element[WECPoint].defaultRepValue, element[WECPoint].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[ProveDHTEvidence]
    }
  }

  // state representation type
  type ProveDHTEvidenceData = (WECPoint, (WECPoint, (WECPoint, WECPoint)))

  // 3) Iso for concrete class
  class ProveDHTEvidenceIso
    extends EntityIso[ProveDHTEvidenceData, ProveDHTEvidence] with Def[ProveDHTEvidenceIso] {
    override def transform(t: Transformer) = new ProveDHTEvidenceIso()
    private lazy val _safeFrom = fun { p: Rep[ProveDHTEvidence] => (p.gv, p.hv, p.uv, p.vv) }
    override def from(p: Rep[ProveDHTEvidence]) =
      tryConvert[ProveDHTEvidence, (WECPoint, (WECPoint, (WECPoint, WECPoint)))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(WECPoint, (WECPoint, (WECPoint, WECPoint)))]) = {
      val Pair(gv, Pair(hv, Pair(uv, vv))) = p
      RProveDHTEvidence(gv, hv, uv, vv)
    }
    lazy val eFrom = pairElement(element[WECPoint], pairElement(element[WECPoint], pairElement(element[WECPoint], element[WECPoint])))
    lazy val eTo = new ProveDHTEvidenceElem(self)
    lazy val selfType = new ProveDHTEvidenceIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class ProveDHTEvidenceIsoElem() extends Elem[ProveDHTEvidenceIso] {
    def getDefaultRep = reifyObject(new ProveDHTEvidenceIso())
    lazy val tag = {
      weakTypeTag[ProveDHTEvidenceIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class ProveDHTEvidenceCompanionCtor extends CompanionDef[ProveDHTEvidenceCompanionCtor] with ProveDHTEvidenceCompanion {
    def selfType = ProveDHTEvidenceCompanionElem
    override def toString = "ProveDHTEvidenceCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[ProveDHTEvidenceData]): Rep[ProveDHTEvidence] = {
      isoProveDHTEvidence.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(gv: Rep[WECPoint], hv: Rep[WECPoint], uv: Rep[WECPoint], vv: Rep[WECPoint]): Rep[ProveDHTEvidence] =
      mkProveDHTEvidence(gv, hv, uv, vv)

    def unapply(p: Rep[SigmaProp]) = unmkProveDHTEvidence(p)
  }
  lazy val ProveDHTEvidenceRep: Rep[ProveDHTEvidenceCompanionCtor] = new ProveDHTEvidenceCompanionCtor
  lazy val RProveDHTEvidence: ProveDHTEvidenceCompanionCtor = proxyProveDHTEvidenceCompanion(ProveDHTEvidenceRep)
  implicit def proxyProveDHTEvidenceCompanion(p: Rep[ProveDHTEvidenceCompanionCtor]): ProveDHTEvidenceCompanionCtor = {
    if (p.rhs.isInstanceOf[ProveDHTEvidenceCompanionCtor])
      p.rhs.asInstanceOf[ProveDHTEvidenceCompanionCtor]
    else
      proxyOps[ProveDHTEvidenceCompanionCtor](p)
  }

  implicit case object ProveDHTEvidenceCompanionElem extends CompanionElem[ProveDHTEvidenceCompanionCtor] {
    lazy val tag = weakTypeTag[ProveDHTEvidenceCompanionCtor]
    protected def getDefaultRep = ProveDHTEvidenceRep
  }

  implicit def proxyProveDHTEvidence(p: Rep[ProveDHTEvidence]): ProveDHTEvidence =
    proxyOps[ProveDHTEvidence](p)

  implicit class ExtendedProveDHTEvidence(p: Rep[ProveDHTEvidence]) {
    def toData: Rep[ProveDHTEvidenceData] = {
      isoProveDHTEvidence.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoProveDHTEvidence: Iso[ProveDHTEvidenceData, ProveDHTEvidence] =
    reifyObject(new ProveDHTEvidenceIso())

  def mkProveDHTEvidence
    (gv: Rep[WECPoint], hv: Rep[WECPoint], uv: Rep[WECPoint], vv: Rep[WECPoint]): Rep[ProveDHTEvidence] = {
    new ProveDHTEvidenceCtor(gv, hv, uv, vv)
  }
  def unmkProveDHTEvidence(p: Rep[SigmaProp]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: ProveDHTEvidenceElem @unchecked =>
      Some((asRep[ProveDHTEvidence](p).gv, asRep[ProveDHTEvidence](p).hv, asRep[ProveDHTEvidence](p).uv, asRep[ProveDHTEvidence](p).vv))
    case _ =>
      None
  }

    object ProveDHTEvidenceMethods {
    object propBytes {
      def unapply(d: Def[_]): Nullable[Rep[ProveDHTEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "propBytes" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ProveDHTEvidence]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ProveDHTEvidence]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object isValid {
      def unapply(d: Def[_]): Nullable[Rep[ProveDHTEvidence]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "isValid" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[ProveDHTEvidence]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[ProveDHTEvidence]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_sigma_&& {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDHTEvidence], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDHTEvidence], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDHTEvidence], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object and_bool_&& {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDHTEvidence], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "$amp$amp" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "and_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDHTEvidence], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDHTEvidence], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_sigma_|| {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDHTEvidence], Rep[SigmaProp])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_sigma" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDHTEvidence], Rep[SigmaProp])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDHTEvidence], Rep[SigmaProp])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object or_bool_|| {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDHTEvidence], Rep[Boolean])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "$bar$bar" && { val ann = method.getAnnotation(classOf[scalan.OverloadId]); ann != null && ann.value == "or_bool" } =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDHTEvidence], Rep[Boolean])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDHTEvidence], Rep[Boolean])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyAnd {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDHTEvidence], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "lazyAnd" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDHTEvidence], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDHTEvidence], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object lazyOr {
      def unapply(d: Def[_]): Nullable[(Rep[ProveDHTEvidence], Rep[Thunk[SigmaProp]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[ProveDHTEvidenceElem] && method.getName == "lazyOr" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[ProveDHTEvidence], Rep[Thunk[SigmaProp]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[ProveDHTEvidence], Rep[Thunk[SigmaProp]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object ProveDHTEvidenceCompanionMethods {
  }
} // of object ProveDHTEvidence
  registerEntityObject("ProveDHTEvidence", ProveDHTEvidence)

  registerModule(SigmaDslOverArraysModule)
}

object SigmaDslOverArraysModule extends scalan.ModuleInfo("special.sigma", "SigmaDslOverArrays")
}

trait SigmaDslOverArraysModule extends special.sigma.impl.SigmaDslOverArraysDefs {self: SigmaLibrary =>}
