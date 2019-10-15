package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SigmaDslOverArraysDefs extends scalan.Scalan with SigmaDslOverArrays {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import AvlTree._
import BigInt._
import CCostedBuilder._
import Coll._
import CollBuilder._
import CollOverArrayBuilder._
import CostModel._
import CostedBuilder._
import GroupElement._
import MonoidBuilder._
import MonoidBuilderInst._
import SigmaDslBuilder._
import SigmaProp._
import WOption._
import TestSigmaDslBuilder._

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

    override def allZK(props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("allZK", classOf[Sym]),
        List(props),
        true, false, element[SigmaProp]))
    }

    override def anyZK(props: Rep[Coll[SigmaProp]]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("anyZK", classOf[Sym]),
        List(props),
        true, false, element[SigmaProp]))
    }

    override def xorOf(conditions: Rep[Coll[Boolean]]): Rep[Boolean] = {
      asRep[Boolean](mkMethodCall(self,
        thisClass.getMethod("xorOf", classOf[Sym]),
        List(conditions),
        true, false, element[Boolean]))
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

    override def byteArrayToBigInt(bytes: Rep[Coll[Byte]]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        thisClass.getMethod("byteArrayToBigInt", classOf[Sym]),
        List(bytes),
        true, false, element[BigInt]))
    }

    override def longToByteArray(l: Rep[Long]): Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("longToByteArray", classOf[Sym]),
        List(l),
        true, false, element[Coll[Byte]]))
    }

    override def byteArrayToLong(bytes: Rep[Coll[Byte]]): Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("byteArrayToLong", classOf[Sym]),
        List(bytes),
        true, false, element[Long]))
    }

    override def proveDlog(g: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("proveDlog", classOf[Sym]),
        List(g),
        true, false, element[SigmaProp]))
    }

    override def proveDHTuple(g: Rep[GroupElement], h: Rep[GroupElement], u: Rep[GroupElement], v: Rep[GroupElement]): Rep[SigmaProp] = {
      asRep[SigmaProp](mkMethodCall(self,
        thisClass.getMethod("proveDHTuple", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(g, h, u, v),
        true, false, element[SigmaProp]))
    }

    override def groupGenerator: Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        thisClass.getMethod("groupGenerator"),
        List(),
        true, false, element[GroupElement]))
    }

    override def substConstants[T](scriptBytes: Rep[Coll[Byte]], positions: Rep[Coll[Int]], newValues: Rep[Coll[T]])(implicit cT: Elem[T]): Rep[Coll[Byte]] = {
      implicit val eT = newValues.eA
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("substConstants", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Elem[_]]),
        List(scriptBytes, positions, newValues, cT),
        true, false, element[Coll[Byte]]))
    }

    override def decodePoint(encoded: Rep[Coll[Byte]]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        thisClass.getMethod("decodePoint", classOf[Sym]),
        List(encoded),
        true, false, element[GroupElement]))
    }

    override def avlTree(operationFlags: Rep[Byte], digest: Rep[Coll[Byte]], keyLength: Rep[Int], valueLengthOpt: Rep[WOption[Int]]): Rep[AvlTree] = {
      asRep[AvlTree](mkMethodCall(self,
        thisClass.getMethod("avlTree", classOf[Sym], classOf[Sym], classOf[Sym], classOf[Sym]),
        List(operationFlags, digest, keyLength, valueLengthOpt),
        true, false, element[AvlTree]))
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

    object xorOf {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Boolean]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "xorOf" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Boolean]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Boolean]])] = exp match {
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

    object byteArrayToLong {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "byteArrayToLong" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Coll[Byte]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDlog {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "proveDlog" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[GroupElement])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object proveDHTuple {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "proveDHTuple" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement], Rep[GroupElement])] = exp match {
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

    object avlTree {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[Byte], Rep[Coll[Byte]], Rep[Int], Rep[WOption[Int]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "avlTree" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[Byte], Rep[Coll[Byte]], Rep[Int], Rep[WOption[Int]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[Byte], Rep[Coll[Byte]], Rep[Int], Rep[WOption[Int]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object TestSigmaDslBuilderCompanionMethods {
  }
} // of object TestSigmaDslBuilder
  registerEntityObject("TestSigmaDslBuilder", TestSigmaDslBuilder)

  registerModule(SigmaDslOverArraysModule)
}

object SigmaDslOverArraysModule extends scalan.ModuleInfo("special.sigma", "SigmaDslOverArrays")
}

trait SigmaDslOverArraysModule extends special.sigma.impl.SigmaDslOverArraysDefs {self: SigmaLibrary =>}
