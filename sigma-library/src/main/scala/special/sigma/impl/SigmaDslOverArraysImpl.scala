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
import Box._
import CCostedBuilder._
import Coll._
import CollBuilder._
import CollOverArrayBuilder._
import CostModel._
import Costed._
import CostedBuilder._
import CostedColl._
import CostedOption._
import GroupElement._
import MonoidBuilder._
import MonoidBuilderInst._
import SigmaDslBuilder._
import SigmaProp._
import TestSigmaDslBuilder._
import WBigInteger._
import WECPoint._
import WOption._
import WSpecialPredef._
import TestAvlTree._

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

    override def digest: Rep[Coll[Byte]] = {
      asRep[Coll[Byte]](mkMethodCall(self,
        thisClass.getMethod("digest"),
        List(),
        true, false, element[Coll[Byte]]))
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

    object digest {
      def unapply(d: Def[_]): Nullable[Rep[TestAvlTree]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[TestAvlTreeElem] && method.getName == "digest" =>
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

    override def BigInt(n: Rep[WBigInteger]): Rep[BigInt] = {
      asRep[BigInt](mkMethodCall(self,
        thisClass.getMethod("BigInt", classOf[Sym]),
        List(n),
        true, false, element[BigInt]))
    }

    override def toBigInteger(n: Rep[BigInt]): Rep[WBigInteger] = {
      asRep[WBigInteger](mkMethodCall(self,
        thisClass.getMethod("toBigInteger", classOf[Sym]),
        List(n),
        true, false, element[WBigInteger]))
    }

    override def GroupElement(p: Rep[WECPoint]): Rep[GroupElement] = {
      asRep[GroupElement](mkMethodCall(self,
        thisClass.getMethod("GroupElement", classOf[Sym]),
        List(p),
        true, false, element[GroupElement]))
    }

    override def toECPoint(ge: Rep[GroupElement]): Rep[WECPoint] = {
      asRep[WECPoint](mkMethodCall(self,
        thisClass.getMethod("toECPoint", classOf[Sym]),
        List(ge),
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

    object BigInt {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[WBigInteger])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "BigInt" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[WBigInteger])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[WBigInteger])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toBigInteger {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[BigInt])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "toBigInteger" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[BigInt])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[BigInt])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object GroupElement {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "GroupElement" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[WECPoint])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object toECPoint {
      def unapply(d: Def[_]): Nullable[(Rep[TestSigmaDslBuilder], Rep[GroupElement])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[TestSigmaDslBuilderElem] && method.getName == "toECPoint" =>
          val res = (receiver, args(0))
          Nullable(res).asInstanceOf[Nullable[(Rep[TestSigmaDslBuilder], Rep[GroupElement])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[TestSigmaDslBuilder], Rep[GroupElement])] = exp match {
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
