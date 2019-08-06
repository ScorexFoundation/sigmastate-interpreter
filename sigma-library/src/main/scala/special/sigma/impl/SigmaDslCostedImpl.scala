package special.sigma

import scalan._
import scala.reflect.runtime.universe._
import scala.reflect._
import scala.collection.mutable.WrappedArray

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
import SizeSigmaProp._
import WOption._
import WRType._
import CSizeBuilder._
import CSizeSigmaProp._
import Context._  // manual fix
import SigmaProp._  // manual fix

object CSizeAnyValue extends EntityObject("CSizeAnyValue") {
  case class CSizeAnyValueCtor
      (override val tVal: Ref[WRType[Any]], override val valueSize: Ref[Size[Any]])
    extends CSizeAnyValue(tVal, valueSize) with Def[CSizeAnyValue] {
    override lazy val eVal: Elem[AnyValue] = implicitly[Elem[AnyValue]]
    lazy val resultType = element[CSizeAnyValue]
    override def transform(t: Transformer) = CSizeAnyValueCtor(t(tVal), t(valueSize))
    private val thisClass = classOf[SizeAnyValue]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class CSizeAnyValueElem(val iso: Iso[CSizeAnyValueData, CSizeAnyValue])
    extends SizeAnyValueElem[CSizeAnyValue]
    with ConcreteElem[CSizeAnyValueData, CSizeAnyValue] {
    override lazy val parent: Option[Elem[_]] = Some(sizeAnyValueElement)
  }

  // state representation type
  type CSizeAnyValueData = (WRType[Any], Size[Any])

  // 3) Iso for concrete class
  class CSizeAnyValueIso
    extends EntityIso[CSizeAnyValueData, CSizeAnyValue] with Def[CSizeAnyValueIso] {
    override def transform(t: Transformer) = new CSizeAnyValueIso()
    private lazy val _safeFrom = fun { p: Ref[CSizeAnyValue] => (p.tVal, p.valueSize) }
    override def from(p: Ref[CSizeAnyValue]) =
      tryConvert[CSizeAnyValue, (WRType[Any], Size[Any])](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(WRType[Any], Size[Any])]) = {
      val Pair(tVal, valueSize) = p
      RCSizeAnyValue(tVal, valueSize)
    }
    lazy val eFrom = pairElement(element[WRType[Any]], element[Size[Any]])
    lazy val eTo = new CSizeAnyValueElem(self)
    lazy val resultType = new CSizeAnyValueIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CSizeAnyValueIsoElem() extends Elem[CSizeAnyValueIso] {
  }
  // 4) constructor and deconstructor
  class CSizeAnyValueCompanionCtor extends CompanionDef[CSizeAnyValueCompanionCtor] with CSizeAnyValueCompanion {
    def resultType = CSizeAnyValueCompanionElem
    override def toString = "CSizeAnyValueCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CSizeAnyValueData]): Ref[CSizeAnyValue] = {
      isoCSizeAnyValue.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(tVal: Ref[WRType[Any]], valueSize: Ref[Size[Any]]): Ref[CSizeAnyValue] =
      mkCSizeAnyValue(tVal, valueSize)

    def unapply(p: Ref[SizeAnyValue]) = unmkCSizeAnyValue(p)
  }
  lazy val CSizeAnyValueRep: Ref[CSizeAnyValueCompanionCtor] = new CSizeAnyValueCompanionCtor
  lazy val RCSizeAnyValue: CSizeAnyValueCompanionCtor = proxyCSizeAnyValueCompanion(CSizeAnyValueRep)
  implicit def proxyCSizeAnyValueCompanion(p: Ref[CSizeAnyValueCompanionCtor]): CSizeAnyValueCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeAnyValueCompanionCtor])
      p.rhs.asInstanceOf[CSizeAnyValueCompanionCtor]
    else
      unrefDelegate[CSizeAnyValueCompanionCtor](p)
  }

  implicit case object CSizeAnyValueCompanionElem extends CompanionElem[CSizeAnyValueCompanionCtor]

  implicit def proxyCSizeAnyValue(p: Ref[CSizeAnyValue]): CSizeAnyValue = {
    if (p.rhs.isInstanceOf[CSizeAnyValue])
      p.rhs.asInstanceOf[CSizeAnyValue]
    else
      unrefDelegate[CSizeAnyValue](p)
  }

  implicit class ExtendedCSizeAnyValue(p: Ref[CSizeAnyValue]) {
    def toData: Ref[CSizeAnyValueData] = {
      isoCSizeAnyValue.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeAnyValue: Iso[CSizeAnyValueData, CSizeAnyValue] =
    reifyObject(new CSizeAnyValueIso())

  def mkCSizeAnyValue
    (tVal: Ref[WRType[Any]], valueSize: Ref[Size[Any]]): Ref[CSizeAnyValue] = {
    new CSizeAnyValueCtor(tVal, valueSize)
  }
  def unmkCSizeAnyValue(p: Ref[SizeAnyValue]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeAnyValueElem @unchecked =>
      Some((asRep[CSizeAnyValue](p).tVal, asRep[CSizeAnyValue](p).valueSize))
    case _ =>
      None
  }
} // of object CSizeAnyValue
  registerEntityObject("CSizeAnyValue", CSizeAnyValue)

object CSizeSigmaProp extends EntityObject("CSizeSigmaProp") {
  case class CSizeSigmaPropCtor
      (override val propBytes: Ref[Size[Coll[Byte]]])
    extends CSizeSigmaProp(propBytes) with Def[CSizeSigmaProp] {
    override lazy val eVal: Elem[SigmaProp] = implicitly[Elem[SigmaProp]]
    lazy val resultType = element[CSizeSigmaProp]
    override def transform(t: Transformer) = CSizeSigmaPropCtor(t(propBytes))
    private val thisClass = classOf[SizeSigmaProp]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }
  }
  // elem for concrete class
  class CSizeSigmaPropElem(val iso: Iso[CSizeSigmaPropData, CSizeSigmaProp])
    extends SizeSigmaPropElem[CSizeSigmaProp]
    with ConcreteElem[CSizeSigmaPropData, CSizeSigmaProp] {
    override lazy val parent: Option[Elem[_]] = Some(sizeSigmaPropElement)
  }

  // state representation type
  type CSizeSigmaPropData = Size[Coll[Byte]]

  // 3) Iso for concrete class
  class CSizeSigmaPropIso
    extends EntityIso[CSizeSigmaPropData, CSizeSigmaProp] with Def[CSizeSigmaPropIso] {
    override def transform(t: Transformer) = new CSizeSigmaPropIso()
    private lazy val _safeFrom = fun { p: Ref[CSizeSigmaProp] => p.propBytes }
    override def from(p: Ref[CSizeSigmaProp]) =
      tryConvert[CSizeSigmaProp, Size[Coll[Byte]]](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[Size[Coll[Byte]]]) = {
      val propBytes = p
      RCSizeSigmaProp(propBytes)
    }
    lazy val eFrom = element[Size[Coll[Byte]]]
    lazy val eTo = new CSizeSigmaPropElem(self)
    lazy val resultType = new CSizeSigmaPropIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CSizeSigmaPropIsoElem() extends Elem[CSizeSigmaPropIso] {
  }
  // 4) constructor and deconstructor
  class CSizeSigmaPropCompanionCtor extends CompanionDef[CSizeSigmaPropCompanionCtor] with CSizeSigmaPropCompanion {
    def resultType = CSizeSigmaPropCompanionElem
    override def toString = "CSizeSigmaPropCompanion"

    @scalan.OverloadId("fromFields")
    def apply(propBytes: Ref[Size[Coll[Byte]]]): Ref[CSizeSigmaProp] =
      mkCSizeSigmaProp(propBytes)

    def unapply(p: Ref[SizeSigmaProp]) = unmkCSizeSigmaProp(p)
  }
  lazy val CSizeSigmaPropRep: Ref[CSizeSigmaPropCompanionCtor] = new CSizeSigmaPropCompanionCtor
  lazy val RCSizeSigmaProp: CSizeSigmaPropCompanionCtor = proxyCSizeSigmaPropCompanion(CSizeSigmaPropRep)
  implicit def proxyCSizeSigmaPropCompanion(p: Ref[CSizeSigmaPropCompanionCtor]): CSizeSigmaPropCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeSigmaPropCompanionCtor])
      p.rhs.asInstanceOf[CSizeSigmaPropCompanionCtor]
    else
      unrefDelegate[CSizeSigmaPropCompanionCtor](p)
  }

  implicit case object CSizeSigmaPropCompanionElem extends CompanionElem[CSizeSigmaPropCompanionCtor]

  implicit def proxyCSizeSigmaProp(p: Ref[CSizeSigmaProp]): CSizeSigmaProp = {
    if (p.rhs.isInstanceOf[CSizeSigmaProp])
      p.rhs.asInstanceOf[CSizeSigmaProp]
    else
      unrefDelegate[CSizeSigmaProp](p)
  }

  implicit class ExtendedCSizeSigmaProp(p: Ref[CSizeSigmaProp]) {
    def toData: Ref[CSizeSigmaPropData] = {
      isoCSizeSigmaProp.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeSigmaProp: Iso[CSizeSigmaPropData, CSizeSigmaProp] =
    reifyObject(new CSizeSigmaPropIso())

  def mkCSizeSigmaProp
    (propBytes: Ref[Size[Coll[Byte]]]): Ref[CSizeSigmaProp] = {
    new CSizeSigmaPropCtor(propBytes)
  }
  def unmkCSizeSigmaProp(p: Ref[SizeSigmaProp]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeSigmaPropElem @unchecked =>
      Some((asRep[CSizeSigmaProp](p).propBytes))
    case _ =>
      None
  }
} // of object CSizeSigmaProp
  registerEntityObject("CSizeSigmaProp", CSizeSigmaProp)

object CSizeBox extends EntityObject("CSizeBox") {
  case class CSizeBoxCtor
      (override val propositionBytes: Ref[Size[Coll[Byte]]], override val bytes: Ref[Size[Coll[Byte]]], override val bytesWithoutRef: Ref[Size[Coll[Byte]]], override val registers: Ref[Size[Coll[WOption[AnyValue]]]], override val tokens: Ref[Size[Coll[(Coll[Byte], Long)]]])
    extends CSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens) with Def[CSizeBox] {
    override lazy val eVal: Elem[Box] = implicitly[Elem[Box]]
    lazy val resultType = element[CSizeBox]
    override def transform(t: Transformer) = CSizeBoxCtor(t(propositionBytes), t(bytes), t(bytesWithoutRef), t(registers), t(tokens))
    private val thisClass = classOf[SizeBox]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }

    override def getReg[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(self,
        thisClass.getMethod("getReg", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, tT),
        true, false, element[Size[WOption[T]]]))
    }
  }
  // elem for concrete class
  class CSizeBoxElem(val iso: Iso[CSizeBoxData, CSizeBox])
    extends SizeBoxElem[CSizeBox]
    with ConcreteElem[CSizeBoxData, CSizeBox] {
    override lazy val parent: Option[Elem[_]] = Some(sizeBoxElement)
  }

  // state representation type
  type CSizeBoxData = (Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[WOption[AnyValue]]], Size[Coll[(Coll[Byte], Long)]]))))

  // 3) Iso for concrete class
  class CSizeBoxIso
    extends EntityIso[CSizeBoxData, CSizeBox] with Def[CSizeBoxIso] {
    override def transform(t: Transformer) = new CSizeBoxIso()
    private lazy val _safeFrom = fun { p: Ref[CSizeBox] => (p.propositionBytes, p.bytes, p.bytesWithoutRef, p.registers, p.tokens) }
    override def from(p: Ref[CSizeBox]) =
      tryConvert[CSizeBox, (Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[WOption[AnyValue]]], Size[Coll[(Coll[Byte], Long)]]))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[WOption[AnyValue]]], Size[Coll[(Coll[Byte], Long)]]))))]) = {
      val Pair(propositionBytes, Pair(bytes, Pair(bytesWithoutRef, Pair(registers, tokens)))) = p
      RCSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens)
    }
    lazy val eFrom = pairElement(element[Size[Coll[Byte]]], pairElement(element[Size[Coll[Byte]]], pairElement(element[Size[Coll[Byte]]], pairElement(element[Size[Coll[WOption[AnyValue]]]], element[Size[Coll[(Coll[Byte], Long)]]]))))
    lazy val eTo = new CSizeBoxElem(self)
    lazy val resultType = new CSizeBoxIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CSizeBoxIsoElem() extends Elem[CSizeBoxIso] {
  }
  // 4) constructor and deconstructor
  class CSizeBoxCompanionCtor extends CompanionDef[CSizeBoxCompanionCtor] with CSizeBoxCompanion {
    def resultType = CSizeBoxCompanionElem
    override def toString = "CSizeBoxCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CSizeBoxData]): Ref[CSizeBox] = {
      isoCSizeBox.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(propositionBytes: Ref[Size[Coll[Byte]]], bytes: Ref[Size[Coll[Byte]]], bytesWithoutRef: Ref[Size[Coll[Byte]]], registers: Ref[Size[Coll[WOption[AnyValue]]]], tokens: Ref[Size[Coll[(Coll[Byte], Long)]]]): Ref[CSizeBox] =
      mkCSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens)

    def unapply(p: Ref[SizeBox]) = unmkCSizeBox(p)
  }
  lazy val CSizeBoxRep: Ref[CSizeBoxCompanionCtor] = new CSizeBoxCompanionCtor
  lazy val RCSizeBox: CSizeBoxCompanionCtor = proxyCSizeBoxCompanion(CSizeBoxRep)
  implicit def proxyCSizeBoxCompanion(p: Ref[CSizeBoxCompanionCtor]): CSizeBoxCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeBoxCompanionCtor])
      p.rhs.asInstanceOf[CSizeBoxCompanionCtor]
    else
      unrefDelegate[CSizeBoxCompanionCtor](p)
  }

  implicit case object CSizeBoxCompanionElem extends CompanionElem[CSizeBoxCompanionCtor]

  implicit def proxyCSizeBox(p: Ref[CSizeBox]): CSizeBox = {
    if (p.rhs.isInstanceOf[CSizeBox])
      p.rhs.asInstanceOf[CSizeBox]
    else
      unrefDelegate[CSizeBox](p)
  }

  implicit class ExtendedCSizeBox(p: Ref[CSizeBox]) {
    def toData: Ref[CSizeBoxData] = {
      isoCSizeBox.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeBox: Iso[CSizeBoxData, CSizeBox] =
    reifyObject(new CSizeBoxIso())

  def mkCSizeBox
    (propositionBytes: Ref[Size[Coll[Byte]]], bytes: Ref[Size[Coll[Byte]]], bytesWithoutRef: Ref[Size[Coll[Byte]]], registers: Ref[Size[Coll[WOption[AnyValue]]]], tokens: Ref[Size[Coll[(Coll[Byte], Long)]]]): Ref[CSizeBox] = {
    new CSizeBoxCtor(propositionBytes, bytes, bytesWithoutRef, registers, tokens)
  }
  def unmkCSizeBox(p: Ref[SizeBox]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeBoxElem @unchecked =>
      Some((asRep[CSizeBox](p).propositionBytes, asRep[CSizeBox](p).bytes, asRep[CSizeBox](p).bytesWithoutRef, asRep[CSizeBox](p).registers, asRep[CSizeBox](p).tokens))
    case _ =>
      None
  }
} // of object CSizeBox
  registerEntityObject("CSizeBox", CSizeBox)

object CSizeContext extends EntityObject("CSizeContext") {
  case class CSizeContextCtor
      (override val outputs: Ref[Size[Coll[Box]]], override val inputs: Ref[Size[Coll[Box]]], override val dataInputs: Ref[Size[Coll[Box]]], override val selfBox: Ref[Size[Box]], override val lastBlockUtxoRootHash: Ref[Size[AvlTree]], override val headers: Ref[Size[Coll[Header]]], override val preHeader: Ref[Size[PreHeader]], override val vars: Ref[Coll[Size[AnyValue]]])
    extends CSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars) with Def[CSizeContext] {
    override lazy val eVal: Elem[Context] = implicitly[Elem[Context]]
    lazy val resultType = element[CSizeContext]
    override def transform(t: Transformer) = CSizeContextCtor(t(outputs), t(inputs), t(dataInputs), t(selfBox), t(lastBlockUtxoRootHash), t(headers), t(preHeader), t(vars))
    private val thisClass = classOf[SizeContext]

    override def dataSize: Ref[Long] = {
      asRep[Long](mkMethodCall(self,
        thisClass.getMethod("dataSize"),
        WrappedArray.empty,
        true, false, element[Long]))
    }

    override def getVar[T](id: Ref[Byte])(implicit tT: Elem[T]): Ref[Size[WOption[T]]] = {
      asRep[Size[WOption[T]]](mkMethodCall(self,
        thisClass.getMethod("getVar", classOf[Sym], classOf[Elem[_]]),
        Array[AnyRef](id, tT),
        true, false, element[Size[WOption[T]]]))
    }
  }
  // elem for concrete class
  class CSizeContextElem(val iso: Iso[CSizeContextData, CSizeContext])
    extends SizeContextElem[CSizeContext]
    with ConcreteElem[CSizeContextData, CSizeContext] {
    override lazy val parent: Option[Elem[_]] = Some(sizeContextElement)
  }

  // state representation type
  type CSizeContextData = (Size[Coll[Box]], (Size[Coll[Box]], (Size[Coll[Box]], (Size[Box], (Size[AvlTree], (Size[Coll[Header]], (Size[PreHeader], Coll[Size[AnyValue]])))))))

  // 3) Iso for concrete class
  class CSizeContextIso
    extends EntityIso[CSizeContextData, CSizeContext] with Def[CSizeContextIso] {
    override def transform(t: Transformer) = new CSizeContextIso()
    private lazy val _safeFrom = fun { p: Ref[CSizeContext] => (p.outputs, p.inputs, p.dataInputs, p.selfBox, p.lastBlockUtxoRootHash, p.headers, p.preHeader, p.vars) }
    override def from(p: Ref[CSizeContext]) =
      tryConvert[CSizeContext, (Size[Coll[Box]], (Size[Coll[Box]], (Size[Coll[Box]], (Size[Box], (Size[AvlTree], (Size[Coll[Header]], (Size[PreHeader], Coll[Size[AnyValue]])))))))](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[(Size[Coll[Box]], (Size[Coll[Box]], (Size[Coll[Box]], (Size[Box], (Size[AvlTree], (Size[Coll[Header]], (Size[PreHeader], Coll[Size[AnyValue]])))))))]) = {
      val Pair(outputs, Pair(inputs, Pair(dataInputs, Pair(selfBox, Pair(lastBlockUtxoRootHash, Pair(headers, Pair(preHeader, vars))))))) = p
      RCSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars)
    }
    lazy val eFrom = pairElement(element[Size[Coll[Box]]], pairElement(element[Size[Coll[Box]]], pairElement(element[Size[Coll[Box]]], pairElement(element[Size[Box]], pairElement(element[Size[AvlTree]], pairElement(element[Size[Coll[Header]]], pairElement(element[Size[PreHeader]], element[Coll[Size[AnyValue]]])))))))
    lazy val eTo = new CSizeContextElem(self)
    lazy val resultType = new CSizeContextIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CSizeContextIsoElem() extends Elem[CSizeContextIso] {
  }
  // 4) constructor and deconstructor
  class CSizeContextCompanionCtor extends CompanionDef[CSizeContextCompanionCtor] with CSizeContextCompanion {
    def resultType = CSizeContextCompanionElem
    override def toString = "CSizeContextCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CSizeContextData]): Ref[CSizeContext] = {
      isoCSizeContext.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(outputs: Ref[Size[Coll[Box]]], inputs: Ref[Size[Coll[Box]]], dataInputs: Ref[Size[Coll[Box]]], selfBox: Ref[Size[Box]], lastBlockUtxoRootHash: Ref[Size[AvlTree]], headers: Ref[Size[Coll[Header]]], preHeader: Ref[Size[PreHeader]], vars: Ref[Coll[Size[AnyValue]]]): Ref[CSizeContext] =
      mkCSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars)

    def unapply(p: Ref[SizeContext]) = unmkCSizeContext(p)
  }
  lazy val CSizeContextRep: Ref[CSizeContextCompanionCtor] = new CSizeContextCompanionCtor
  lazy val RCSizeContext: CSizeContextCompanionCtor = proxyCSizeContextCompanion(CSizeContextRep)
  implicit def proxyCSizeContextCompanion(p: Ref[CSizeContextCompanionCtor]): CSizeContextCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeContextCompanionCtor])
      p.rhs.asInstanceOf[CSizeContextCompanionCtor]
    else
      unrefDelegate[CSizeContextCompanionCtor](p)
  }

  implicit case object CSizeContextCompanionElem extends CompanionElem[CSizeContextCompanionCtor]

  implicit def proxyCSizeContext(p: Ref[CSizeContext]): CSizeContext = {
    if (p.rhs.isInstanceOf[CSizeContext])
      p.rhs.asInstanceOf[CSizeContext]
    else
      unrefDelegate[CSizeContext](p)
  }

  implicit class ExtendedCSizeContext(p: Ref[CSizeContext]) {
    def toData: Ref[CSizeContextData] = {
      isoCSizeContext.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeContext: Iso[CSizeContextData, CSizeContext] =
    reifyObject(new CSizeContextIso())

  def mkCSizeContext
    (outputs: Ref[Size[Coll[Box]]], inputs: Ref[Size[Coll[Box]]], dataInputs: Ref[Size[Coll[Box]]], selfBox: Ref[Size[Box]], lastBlockUtxoRootHash: Ref[Size[AvlTree]], headers: Ref[Size[Coll[Header]]], preHeader: Ref[Size[PreHeader]], vars: Ref[Coll[Size[AnyValue]]]): Ref[CSizeContext] = {
    new CSizeContextCtor(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars)
  }
  def unmkCSizeContext(p: Ref[SizeContext]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeContextElem @unchecked =>
      Some((asRep[CSizeContext](p).outputs, asRep[CSizeContext](p).inputs, asRep[CSizeContext](p).dataInputs, asRep[CSizeContext](p).selfBox, asRep[CSizeContext](p).lastBlockUtxoRootHash, asRep[CSizeContext](p).headers, asRep[CSizeContext](p).preHeader, asRep[CSizeContext](p).vars))
    case _ =>
      None
  }
} // of object CSizeContext
  registerEntityObject("CSizeContext", CSizeContext)

object CSizeBuilder extends EntityObject("CSizeBuilder") {
  case class CSizeBuilderCtor
      ()
    extends CSizeBuilder() with Def[CSizeBuilder] {
    lazy val resultType = element[CSizeBuilder]
    override def transform(t: Transformer) = CSizeBuilderCtor()
  }
  // elem for concrete class
  class CSizeBuilderElem(val iso: Iso[CSizeBuilderData, CSizeBuilder])
    extends SizeBuilderElem[CSizeBuilder]
    with ConcreteElem[CSizeBuilderData, CSizeBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(sizeBuilderElement)
  }

  // state representation type
  type CSizeBuilderData = Unit

  // 3) Iso for concrete class
  class CSizeBuilderIso
    extends EntityIso[CSizeBuilderData, CSizeBuilder] with Def[CSizeBuilderIso] {
    override def transform(t: Transformer) = new CSizeBuilderIso()
    private lazy val _safeFrom = fun { p: Ref[CSizeBuilder] => () }
    override def from(p: Ref[CSizeBuilder]) =
      tryConvert[CSizeBuilder, Unit](eTo, eFrom, p, _safeFrom)
    override def to(p: Ref[Unit]) = {
      val unit = p
      RCSizeBuilder()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new CSizeBuilderElem(self)
    lazy val resultType = new CSizeBuilderIsoElem
    def productArity = 0
    def productElement(n: Int) = ???
  }
  case class CSizeBuilderIsoElem() extends Elem[CSizeBuilderIso] {
  }
  // 4) constructor and deconstructor
  class CSizeBuilderCompanionCtor extends CompanionDef[CSizeBuilderCompanionCtor] with CSizeBuilderCompanion {
    def resultType = CSizeBuilderCompanionElem
    override def toString = "CSizeBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CSizeBuilderData]): Ref[CSizeBuilder] = {
      isoCSizeBuilder.to(p)
    }

    @scalan.OverloadId("fromFields")
    def apply(): Ref[CSizeBuilder] =
      mkCSizeBuilder()

    def unapply(p: Ref[SizeBuilder]) = unmkCSizeBuilder(p)
  }
  lazy val CSizeBuilderRep: Ref[CSizeBuilderCompanionCtor] = new CSizeBuilderCompanionCtor
  lazy val RCSizeBuilder: CSizeBuilderCompanionCtor = proxyCSizeBuilderCompanion(CSizeBuilderRep)
  implicit def proxyCSizeBuilderCompanion(p: Ref[CSizeBuilderCompanionCtor]): CSizeBuilderCompanionCtor = {
    if (p.rhs.isInstanceOf[CSizeBuilderCompanionCtor])
      p.rhs.asInstanceOf[CSizeBuilderCompanionCtor]
    else
      unrefDelegate[CSizeBuilderCompanionCtor](p)
  }

  implicit case object CSizeBuilderCompanionElem extends CompanionElem[CSizeBuilderCompanionCtor]

  implicit def proxyCSizeBuilder(p: Ref[CSizeBuilder]): CSizeBuilder = {
    if (p.rhs.isInstanceOf[CSizeBuilder])
      p.rhs.asInstanceOf[CSizeBuilder]
    else
      unrefDelegate[CSizeBuilder](p)
  }

  implicit class ExtendedCSizeBuilder(p: Ref[CSizeBuilder]) {
    def toData: Ref[CSizeBuilderData] = {
      isoCSizeBuilder.from(p)
    }
  }

  // 5) implicit resolution of Iso
  implicit def isoCSizeBuilder: Iso[CSizeBuilderData, CSizeBuilder] =
    reifyObject(new CSizeBuilderIso())

  def mkCSizeBuilder
    (): Ref[CSizeBuilder] = {
    new CSizeBuilderCtor()
  }
  def unmkCSizeBuilder(p: Ref[SizeBuilder]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: CSizeBuilderElem @unchecked =>
      Some(())
    case _ =>
      None
  }
} // of object CSizeBuilder
  registerEntityObject("CSizeBuilder", CSizeBuilder)

  registerModule(SigmaDslCostedModule)
}

object SigmaDslCostedModule extends scalan.ModuleInfo("special.sigma", "SigmaDslCosted")
}

trait SigmaDslCostedModule extends special.sigma.impl.SigmaDslCostedDefs {self: SigmaLibrary =>}
