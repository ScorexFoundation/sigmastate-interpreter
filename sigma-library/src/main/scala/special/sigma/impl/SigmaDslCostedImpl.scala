package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._

package impl {
// Abs -----------------------------------
trait SigmaDslCostedDefs extends scalan.Scalan with SigmaDslCosted {
  self: SigmaLibrary =>
import IsoUR._
import Converter._
import AnyValue._
import AvlTree._
import Box._
import CSizeAnyValue._
import CSizeBox._
import CSizeContext._
import Coll._
import Header._
import PreHeader._
import Size._
import SizeAnyValue._
import SizeBox._
import SizeBuilder._
import SizeContext._
import WOption._
import WRType._
import CSizeBuilder._
import Context._  // manual fix
object CSizeAnyValue extends EntityObject("CSizeAnyValue") {
  case class CSizeAnyValueCtor
      (override val tVal: Rep[WRType[Any]], override val valueSize: Rep[Size[Any]])
    extends CSizeAnyValue(tVal, valueSize) with Def[CSizeAnyValue] {
    override lazy val eVal: Elem[AnyValue] = implicitly[Elem[AnyValue]]
    lazy val selfType = element[CSizeAnyValue]
    override def transform(t: Transformer) = CSizeAnyValueCtor(t(tVal), t(valueSize))
    private val thisClass = classOf[SizeAnyValue]

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class CSizeAnyValueElem(val iso: Iso[CSizeAnyValueData, CSizeAnyValue])
    extends SizeAnyValueElem[CSizeAnyValue]
    with ConcreteElem[CSizeAnyValueData, CSizeAnyValue] {
    override lazy val parent: Option[Elem[_]] = Some(sizeAnyValueElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSizeAnyValue(x: Rep[SizeAnyValue]) = RCSizeAnyValue(x.tVal, x.valueSize)
    override def getDefaultRep = RCSizeAnyValue(element[WRType[Any]].defaultRepValue, element[Size[Any]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[CSizeAnyValue]
    }
  }

  // state representation type
  type CSizeAnyValueData = (WRType[Any], Size[Any])

  // 3) Iso for concrete class
  class CSizeAnyValueIso
    extends EntityIso[CSizeAnyValueData, CSizeAnyValue] with Def[CSizeAnyValueIso] {
    override def transform(t: Transformer) = new CSizeAnyValueIso()
    private lazy val _safeFrom = fun { p: Rep[CSizeAnyValue] => (p.tVal, p.valueSize) }
    override def from(p: Rep[CSizeAnyValue]) =
      tryConvert[CSizeAnyValue, (WRType[Any], Size[Any])](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(WRType[Any], Size[Any])]) = {
      val Pair(tVal, valueSize) = p
      RCSizeAnyValue(tVal, valueSize)
    }
    lazy val eFrom = pairElement(element[WRType[Any]], element[Size[Any]])
    lazy val eTo = new CSizeAnyValueElem(self)
    lazy val selfType = new CSizeAnyValueIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CSizeAnyValueIsoElem() extends Elem[CSizeAnyValueIso] {
    def getDefaultRep = reifyObject(new CSizeAnyValueIso())
    lazy val tag = {
      weakTypeTag[CSizeAnyValueIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CSizeAnyValueCompanionCtor extends CompanionDef[CSizeAnyValueCompanionCtor] with CSizeAnyValueCompanion {
    def selfType = CSizeAnyValueCompanionElem
    override def toString = "CSizeAnyValueCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CSizeAnyValueData]): Rep[CSizeAnyValue] = {
      isoCSizeAnyValue.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(tVal: Rep[WRType[Any]], valueSize: Rep[Size[Any]]): Rep[CSizeAnyValue] =
      mkCSizeAnyValue(tVal, valueSize)

    def unapply(p: Rep[SizeAnyValue]) = unmkCSizeAnyValue(p)
  }
  lazy val CSizeAnyValueRep: Rep[CSizeAnyValueCompanionCtor] = new CSizeAnyValueCompanionCtor
  lazy val RCSizeAnyValue: CSizeAnyValueCompanionCtor = proxyCSizeAnyValueCompanion(CSizeAnyValueRep)
  implicit def proxyCSizeAnyValueCompanion(p: Rep[CSizeAnyValueCompanionCtor]): CSizeAnyValueCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeAnyValueCompanionCtor])
      p.rhs.asInstanceOf[CSizeAnyValueCompanionCtor]
    else
      proxyOps[CSizeAnyValueCompanionCtor](p)
  }

  implicit case object CSizeAnyValueCompanionElem extends CompanionElem[CSizeAnyValueCompanionCtor] {
    lazy val tag = weakTypeTag[CSizeAnyValueCompanionCtor]
    protected def getDefaultRep = CSizeAnyValueRep
  }

  implicit def proxyCSizeAnyValue(p: Rep[CSizeAnyValue]): CSizeAnyValue =
    proxyOps[CSizeAnyValue](p)

  implicit class ExtendedCSizeAnyValue(p: Rep[CSizeAnyValue]) {
    def toData: Rep[CSizeAnyValueData] = {
      isoCSizeAnyValue.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeAnyValue: Iso[CSizeAnyValueData, CSizeAnyValue] =
    reifyObject(new CSizeAnyValueIso())

  def mkCSizeAnyValue
    (tVal: Rep[WRType[Any]], valueSize: Rep[Size[Any]]): Rep[CSizeAnyValue] = {
    new CSizeAnyValueCtor(tVal, valueSize)
  }
  def unmkCSizeAnyValue(p: Rep[SizeAnyValue]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeAnyValueElem @unchecked =>
      Some((asRep[CSizeAnyValue](p).tVal, asRep[CSizeAnyValue](p).valueSize))
    case _ =>
      None
  }

    object CSizeAnyValueMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CSizeAnyValue]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CSizeAnyValueElem] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CSizeAnyValue]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CSizeAnyValue]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CSizeAnyValueCompanionMethods {
  }
} // of object CSizeAnyValue
  registerEntityObject("CSizeAnyValue", CSizeAnyValue)

object CSizeBox extends EntityObject("CSizeBox") {
  case class CSizeBoxCtor
      (override val propositionBytes: Rep[Size[Coll[Byte]]], override val bytes: Rep[Size[Coll[Byte]]], override val bytesWithoutRef: Rep[Size[Coll[Byte]]], override val registers: Rep[Size[Coll[WOption[AnyValue]]]])
    extends CSizeBox(propositionBytes, bytes, bytesWithoutRef, registers) with Def[CSizeBox] {
    override lazy val eVal: Elem[Box] = implicitly[Elem[Box]]
    lazy val selfType = element[CSizeBox]
    override def transform(t: Transformer) = CSizeBoxCtor(t(propositionBytes), t(bytes), t(bytesWithoutRef), t(registers))
    private val thisClass = classOf[SizeBox]

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class CSizeBoxElem(val iso: Iso[CSizeBoxData, CSizeBox])
    extends SizeBoxElem[CSizeBox]
    with ConcreteElem[CSizeBoxData, CSizeBox] {
    override lazy val parent: Option[Elem[_]] = Some(sizeBoxElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSizeBox(x: Rep[SizeBox]) = RCSizeBox(x.propositionBytes, x.bytes, x.bytesWithoutRef, x.registers)
    override def getDefaultRep = RCSizeBox(element[Size[Coll[Byte]]].defaultRepValue, element[Size[Coll[Byte]]].defaultRepValue, element[Size[Coll[Byte]]].defaultRepValue, element[Size[Coll[WOption[AnyValue]]]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[CSizeBox]
    }
  }

  // state representation type
  type CSizeBoxData = (Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[Byte]], Size[Coll[WOption[AnyValue]]])))

  // 3) Iso for concrete class
  class CSizeBoxIso
    extends EntityIso[CSizeBoxData, CSizeBox] with Def[CSizeBoxIso] {
    override def transform(t: Transformer) = new CSizeBoxIso()
    private lazy val _safeFrom = fun { p: Rep[CSizeBox] => (p.propositionBytes, p.bytes, p.bytesWithoutRef, p.registers) }
    override def from(p: Rep[CSizeBox]) =
      tryConvert[CSizeBox, (Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[Byte]], Size[Coll[WOption[AnyValue]]])))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[Byte]], Size[Coll[WOption[AnyValue]]])))]) = {
      val Pair(propositionBytes, Pair(bytes, Pair(bytesWithoutRef, registers))) = p
      RCSizeBox(propositionBytes, bytes, bytesWithoutRef, registers)
    }
    lazy val eFrom = pairElement(element[Size[Coll[Byte]]], pairElement(element[Size[Coll[Byte]]], pairElement(element[Size[Coll[Byte]]], element[Size[Coll[WOption[AnyValue]]]])))
    lazy val eTo = new CSizeBoxElem(self)
    lazy val selfType = new CSizeBoxIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CSizeBoxIsoElem() extends Elem[CSizeBoxIso] {
    def getDefaultRep = reifyObject(new CSizeBoxIso())
    lazy val tag = {
      weakTypeTag[CSizeBoxIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CSizeBoxCompanionCtor extends CompanionDef[CSizeBoxCompanionCtor] with CSizeBoxCompanion {
    def selfType = CSizeBoxCompanionElem
    override def toString = "CSizeBoxCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CSizeBoxData]): Rep[CSizeBox] = {
      isoCSizeBox.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(propositionBytes: Rep[Size[Coll[Byte]]], bytes: Rep[Size[Coll[Byte]]], bytesWithoutRef: Rep[Size[Coll[Byte]]], registers: Rep[Size[Coll[WOption[AnyValue]]]]): Rep[CSizeBox] =
      mkCSizeBox(propositionBytes, bytes, bytesWithoutRef, registers)

    def unapply(p: Rep[SizeBox]) = unmkCSizeBox(p)
  }
  lazy val CSizeBoxRep: Rep[CSizeBoxCompanionCtor] = new CSizeBoxCompanionCtor
  lazy val RCSizeBox: CSizeBoxCompanionCtor = proxyCSizeBoxCompanion(CSizeBoxRep)
  implicit def proxyCSizeBoxCompanion(p: Rep[CSizeBoxCompanionCtor]): CSizeBoxCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeBoxCompanionCtor])
      p.rhs.asInstanceOf[CSizeBoxCompanionCtor]
    else
      proxyOps[CSizeBoxCompanionCtor](p)
  }

  implicit case object CSizeBoxCompanionElem extends CompanionElem[CSizeBoxCompanionCtor] {
    lazy val tag = weakTypeTag[CSizeBoxCompanionCtor]
    protected def getDefaultRep = CSizeBoxRep
  }

  implicit def proxyCSizeBox(p: Rep[CSizeBox]): CSizeBox =
    proxyOps[CSizeBox](p)

  implicit class ExtendedCSizeBox(p: Rep[CSizeBox]) {
    def toData: Rep[CSizeBoxData] = {
      isoCSizeBox.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeBox: Iso[CSizeBoxData, CSizeBox] =
    reifyObject(new CSizeBoxIso())

  def mkCSizeBox
    (propositionBytes: Rep[Size[Coll[Byte]]], bytes: Rep[Size[Coll[Byte]]], bytesWithoutRef: Rep[Size[Coll[Byte]]], registers: Rep[Size[Coll[WOption[AnyValue]]]]): Rep[CSizeBox] = {
    new CSizeBoxCtor(propositionBytes, bytes, bytesWithoutRef, registers)
  }
  def unmkCSizeBox(p: Rep[SizeBox]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeBoxElem @unchecked =>
      Some((asRep[CSizeBox](p).propositionBytes, asRep[CSizeBox](p).bytes, asRep[CSizeBox](p).bytesWithoutRef, asRep[CSizeBox](p).registers))
    case _ =>
      None
  }

    object CSizeBoxMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CSizeBox]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CSizeBoxElem] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CSizeBox]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CSizeBox]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CSizeBoxCompanionMethods {
  }
} // of object CSizeBox
  registerEntityObject("CSizeBox", CSizeBox)

object CSizeContext extends EntityObject("CSizeContext") {
  case class CSizeContextCtor
      (override val outputs: Rep[Size[Coll[Box]]], override val inputs: Rep[Size[Coll[Box]]], override val dataInputs: Rep[Size[Coll[Box]]], override val selfBox: Rep[Size[Box]], override val lastBlockUtxoRootHash: Rep[Size[AvlTree]], override val headers: Rep[Size[Coll[Header]]], override val preHeader: Rep[Size[PreHeader]])
    extends CSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader) with Def[CSizeContext] {
    override lazy val eVal: Elem[Context] = implicitly[Elem[Context]]
    lazy val selfType = element[CSizeContext]
    override def transform(t: Transformer) = CSizeContextCtor(t(outputs), t(inputs), t(dataInputs), t(selfBox), t(lastBlockUtxoRootHash), t(headers), t(preHeader))
    private val thisClass = classOf[SizeContext]

    override def dataSize: Rep[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        List(),
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class CSizeContextElem(val iso: Iso[CSizeContextData, CSizeContext])
    extends SizeContextElem[CSizeContext]
    with ConcreteElem[CSizeContextData, CSizeContext] {
    override lazy val parent: Option[Elem[_]] = Some(sizeContextElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSizeContext(x: Rep[SizeContext]) = RCSizeContext(x.outputs, x.inputs, x.dataInputs, x.selfBox, x.lastBlockUtxoRootHash, x.headers, x.preHeader)
    override def getDefaultRep = RCSizeContext(element[Size[Coll[Box]]].defaultRepValue, element[Size[Coll[Box]]].defaultRepValue, element[Size[Coll[Box]]].defaultRepValue, element[Size[Box]].defaultRepValue, element[Size[AvlTree]].defaultRepValue, element[Size[Coll[Header]]].defaultRepValue, element[Size[PreHeader]].defaultRepValue)
    override lazy val tag = {
      weakTypeTag[CSizeContext]
    }
  }

  // state representation type
  type CSizeContextData = (Size[Coll[Box]], (Size[Coll[Box]], (Size[Coll[Box]], (Size[Box], (Size[AvlTree], (Size[Coll[Header]], Size[PreHeader]))))))

  // 3) Iso for concrete class
  class CSizeContextIso
    extends EntityIso[CSizeContextData, CSizeContext] with Def[CSizeContextIso] {
    override def transform(t: Transformer) = new CSizeContextIso()
    private lazy val _safeFrom = fun { p: Rep[CSizeContext] => (p.outputs, p.inputs, p.dataInputs, p.selfBox, p.lastBlockUtxoRootHash, p.headers, p.preHeader) }
    override def from(p: Rep[CSizeContext]) =
      tryConvert[CSizeContext, (Size[Coll[Box]], (Size[Coll[Box]], (Size[Coll[Box]], (Size[Box], (Size[AvlTree], (Size[Coll[Header]], Size[PreHeader]))))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[(Size[Coll[Box]], (Size[Coll[Box]], (Size[Coll[Box]], (Size[Box], (Size[AvlTree], (Size[Coll[Header]], Size[PreHeader]))))))]) = {
      val Pair(outputs, Pair(inputs, Pair(dataInputs, Pair(selfBox, Pair(lastBlockUtxoRootHash, Pair(headers, preHeader)))))) = p
      RCSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader)
    }
    lazy val eFrom = pairElement(element[Size[Coll[Box]]], pairElement(element[Size[Coll[Box]]], pairElement(element[Size[Coll[Box]]], pairElement(element[Size[Box]], pairElement(element[Size[AvlTree]], pairElement(element[Size[Coll[Header]]], element[Size[PreHeader]]))))))
    lazy val eTo = new CSizeContextElem(self)
    lazy val selfType = new CSizeContextIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CSizeContextIsoElem() extends Elem[CSizeContextIso] {
    def getDefaultRep = reifyObject(new CSizeContextIso())
    lazy val tag = {
      weakTypeTag[CSizeContextIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CSizeContextCompanionCtor extends CompanionDef[CSizeContextCompanionCtor] with CSizeContextCompanion {
    def selfType = CSizeContextCompanionElem
    override def toString = "CSizeContextCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CSizeContextData]): Rep[CSizeContext] = {
      isoCSizeContext.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(outputs: Rep[Size[Coll[Box]]], inputs: Rep[Size[Coll[Box]]], dataInputs: Rep[Size[Coll[Box]]], selfBox: Rep[Size[Box]], lastBlockUtxoRootHash: Rep[Size[AvlTree]], headers: Rep[Size[Coll[Header]]], preHeader: Rep[Size[PreHeader]]): Rep[CSizeContext] =
      mkCSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader)

    def unapply(p: Rep[SizeContext]) = unmkCSizeContext(p)
  }
  lazy val CSizeContextRep: Rep[CSizeContextCompanionCtor] = new CSizeContextCompanionCtor
  lazy val RCSizeContext: CSizeContextCompanionCtor = proxyCSizeContextCompanion(CSizeContextRep)
  implicit def proxyCSizeContextCompanion(p: Rep[CSizeContextCompanionCtor]): CSizeContextCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeContextCompanionCtor])
      p.rhs.asInstanceOf[CSizeContextCompanionCtor]
    else
      proxyOps[CSizeContextCompanionCtor](p)
  }

  implicit case object CSizeContextCompanionElem extends CompanionElem[CSizeContextCompanionCtor] {
    lazy val tag = weakTypeTag[CSizeContextCompanionCtor]
    protected def getDefaultRep = CSizeContextRep
  }

  implicit def proxyCSizeContext(p: Rep[CSizeContext]): CSizeContext =
    proxyOps[CSizeContext](p)

  implicit class ExtendedCSizeContext(p: Rep[CSizeContext]) {
    def toData: Rep[CSizeContextData] = {
      isoCSizeContext.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeContext: Iso[CSizeContextData, CSizeContext] =
    reifyObject(new CSizeContextIso())

  def mkCSizeContext
    (outputs: Rep[Size[Coll[Box]]], inputs: Rep[Size[Coll[Box]]], dataInputs: Rep[Size[Coll[Box]]], selfBox: Rep[Size[Box]], lastBlockUtxoRootHash: Rep[Size[AvlTree]], headers: Rep[Size[Coll[Header]]], preHeader: Rep[Size[PreHeader]]): Rep[CSizeContext] = {
    new CSizeContextCtor(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader)
  }
  def unmkCSizeContext(p: Rep[SizeContext]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeContextElem @unchecked =>
      Some((asRep[CSizeContext](p).outputs, asRep[CSizeContext](p).inputs, asRep[CSizeContext](p).dataInputs, asRep[CSizeContext](p).selfBox, asRep[CSizeContext](p).lastBlockUtxoRootHash, asRep[CSizeContext](p).headers, asRep[CSizeContext](p).preHeader))
    case _ =>
      None
  }

    object CSizeContextMethods {
    object dataSize {
      def unapply(d: Def[_]): Nullable[Rep[CSizeContext]] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[CSizeContextElem] && method.getName == "dataSize" =>
          val res = receiver
          Nullable(res).asInstanceOf[Nullable[Rep[CSizeContext]]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[Rep[CSizeContext]] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CSizeContextCompanionMethods {
  }
} // of object CSizeContext
  registerEntityObject("CSizeContext", CSizeContext)

object CSizeBuilder extends EntityObject("CSizeBuilder") {
  case class CSizeBuilderCtor
      ()
    extends CSizeBuilder() with Def[CSizeBuilder] {
    lazy val selfType = element[CSizeBuilder]
    override def transform(t: Transformer) = CSizeBuilderCtor()
  }
  // elem for concrete class
  class CSizeBuilderElem(val iso: Iso[CSizeBuilderData, CSizeBuilder])
    extends SizeBuilderElem[CSizeBuilder]
    with ConcreteElem[CSizeBuilderData, CSizeBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(sizeBuilderElement)
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
    override def convertSizeBuilder(x: Rep[SizeBuilder]) = RCSizeBuilder()
    override def getDefaultRep = RCSizeBuilder()
    override lazy val tag = {
      weakTypeTag[CSizeBuilder]
    }
  }

  // state representation type
  type CSizeBuilderData = Unit

  // 3) Iso for concrete class
  class CSizeBuilderIso
    extends EntityIso[CSizeBuilderData, CSizeBuilder] with Def[CSizeBuilderIso] {
    override def transform(t: Transformer) = new CSizeBuilderIso()
    private lazy val _safeFrom = fun { p: Rep[CSizeBuilder] => () }
    override def from(p: Rep[CSizeBuilder]) =
      tryConvert[CSizeBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Rep[Unit]) = {
      val unit = p
      RCSizeBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new CSizeBuilderElem(self)
    lazy val selfType = new CSizeBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CSizeBuilderIsoElem() extends Elem[CSizeBuilderIso] {
    def getDefaultRep = reifyObject(new CSizeBuilderIso())
    lazy val tag = {
      weakTypeTag[CSizeBuilderIso]
    }
    override def buildTypeArgs = super.buildTypeArgs ++ TypeArgs()
  }
  // 4) constructor and deconstructor
  class CSizeBuilderCompanionCtor extends CompanionDef[CSizeBuilderCompanionCtor] with CSizeBuilderCompanion {
    def selfType = CSizeBuilderCompanionElem
    override def toString = "CSizeBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Rep[CSizeBuilderData]): Rep[CSizeBuilder] = {
      isoCSizeBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Rep[CSizeBuilder] =
      mkCSizeBuilder()

    def unapply(p: Rep[SizeBuilder]) = unmkCSizeBuilder(p)
  }
  lazy val CSizeBuilderRep: Rep[CSizeBuilderCompanionCtor] = new CSizeBuilderCompanionCtor
  lazy val RCSizeBuilder: CSizeBuilderCompanionCtor = proxyCSizeBuilderCompanion(CSizeBuilderRep)
  implicit def proxyCSizeBuilderCompanion(p: Rep[CSizeBuilderCompanionCtor]): CSizeBuilderCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeBuilderCompanionCtor])
      p.rhs.asInstanceOf[CSizeBuilderCompanionCtor]
    else
      proxyOps[CSizeBuilderCompanionCtor](p)
  }

  implicit case object CSizeBuilderCompanionElem extends CompanionElem[CSizeBuilderCompanionCtor] {
    lazy val tag = weakTypeTag[CSizeBuilderCompanionCtor]
    protected def getDefaultRep = CSizeBuilderRep
  }

  implicit def proxyCSizeBuilder(p: Rep[CSizeBuilder]): CSizeBuilder =
    proxyOps[CSizeBuilder](p)

  implicit class ExtendedCSizeBuilder(p: Rep[CSizeBuilder]) {
    def toData: Rep[CSizeBuilderData] = {
      isoCSizeBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeBuilder: Iso[CSizeBuilderData, CSizeBuilder] =
    reifyObject(new CSizeBuilderIso())

  def mkCSizeBuilder
    (): Rep[CSizeBuilder] = {
    new CSizeBuilderCtor()
  }
  def unmkCSizeBuilder(p: Rep[SizeBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }

    object CSizeBuilderMethods {
    object mkSizeAnyValue {
      def unapply(d: Def[_]): Nullable[(Rep[CSizeBuilder], Rep[WRType[Any]], Rep[Size[Any]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CSizeBuilderElem] && method.getName == "mkSizeAnyValue" =>
          val res = (receiver, args(0), args(1))
          Nullable(res).asInstanceOf[Nullable[(Rep[CSizeBuilder], Rep[WRType[Any]], Rep[Size[Any]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CSizeBuilder], Rep[WRType[Any]], Rep[Size[Any]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeBox {
      def unapply(d: Def[_]): Nullable[(Rep[CSizeBuilder], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[WOption[AnyValue]]]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CSizeBuilderElem] && method.getName == "mkSizeBox" =>
          val res = (receiver, args(0), args(1), args(2), args(3))
          Nullable(res).asInstanceOf[Nullable[(Rep[CSizeBuilder], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[WOption[AnyValue]]]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CSizeBuilder], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[Byte]]], Rep[Size[Coll[WOption[AnyValue]]]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }

    object mkSizeContext {
      def unapply(d: Def[_]): Nullable[(Rep[CSizeBuilder], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Box]], Rep[Size[AvlTree]], Rep[Size[Coll[Header]]], Rep[Size[PreHeader]])] = d match {
        case MethodCall(receiver, method, args, _) if receiver.elem.isInstanceOf[CSizeBuilderElem] && method.getName == "mkSizeContext" =>
          val res = (receiver, args(0), args(1), args(2), args(3), args(4), args(5), args(6))
          Nullable(res).asInstanceOf[Nullable[(Rep[CSizeBuilder], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Box]], Rep[Size[AvlTree]], Rep[Size[Coll[Header]]], Rep[Size[PreHeader]])]]
        case _ => Nullable.None
      }
      def unapply(exp: Sym): Nullable[(Rep[CSizeBuilder], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Coll[Box]]], Rep[Size[Box]], Rep[Size[AvlTree]], Rep[Size[Coll[Header]]], Rep[Size[PreHeader]])] = exp match {
        case Def(d) => unapply(d)
        case _ => Nullable.None
      }
    }
  }

  object CSizeBuilderCompanionMethods {
  }
} // of object CSizeBuilder
  registerEntityObject("CSizeBuilder", CSizeBuilder)

  registerModule(SigmaDslCostedModule)
}

object SigmaDslCostedModule extends scalan.ModuleInfo("special.sigma", "SigmaDslCosted")
}

trait SigmaDslCostedModule extends special.sigma.impl.SigmaDslCostedDefs {self: SigmaLibrary =>}
