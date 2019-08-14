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

  implicit lazy val cSizeAnyValueElement: Elem[CSizeAnyValue] =
    new CSizeAnyValueElem

  // elem for concrete class
  class CSizeAnyValueElem
    extends SizeAnyValueElem[CSizeAnyValue]
    with ConcreteElem[CSizeAnyValueData, CSizeAnyValue] {
    override lazy val parent: Option[Elem[_]] = Some(sizeAnyValueElement)
    override def iso = ???
  }

  // state representation type
  type CSizeAnyValueData = (WRType[Any], Size[Any])

  // 4) constructor and deconstructor
  class CSizeAnyValueCompanionCtor extends CompanionDef[CSizeAnyValueCompanionCtor] with CSizeAnyValueCompanion {
    def resultType = CSizeAnyValueCompanionElem
    override def toString = "CSizeAnyValueCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CSizeAnyValueData]): Ref[CSizeAnyValue] = {
      val Pair(tVal, valueSize) = p
      mkCSizeAnyValue(tVal, valueSize)
    }

    @scalan.OverloadId("fromFields")
    def apply(tVal: Ref[WRType[Any]], valueSize: Ref[Size[Any]]): Ref[CSizeAnyValue] =
      mkCSizeAnyValue(tVal, valueSize)

    def unapply(p: Ref[SizeAnyValue]) = unmkCSizeAnyValue(p)
  }
  lazy val CSizeAnyValueRef: Ref[CSizeAnyValueCompanionCtor] = new CSizeAnyValueCompanionCtor
  lazy val RCSizeAnyValue: CSizeAnyValueCompanionCtor = unrefCSizeAnyValueCompanion(CSizeAnyValueRef)
  implicit def unrefCSizeAnyValueCompanion(p: Ref[CSizeAnyValueCompanionCtor]): CSizeAnyValueCompanionCtor = {
    if (p.node.isInstanceOf[CSizeAnyValueCompanionCtor])
      p.node.asInstanceOf[CSizeAnyValueCompanionCtor]
    else
      unrefDelegate[CSizeAnyValueCompanionCtor](p)
  }

  implicit case object CSizeAnyValueCompanionElem extends CompanionElem[CSizeAnyValueCompanionCtor]

  implicit def unrefCSizeAnyValue(p: Ref[CSizeAnyValue]): CSizeAnyValue = {
    if (p.node.isInstanceOf[CSizeAnyValue])
      p.node.asInstanceOf[CSizeAnyValue]
    else
      unrefDelegate[CSizeAnyValue](p)
  }

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
  implicit lazy val cSizeSigmaPropElement: Elem[CSizeSigmaProp] =
    new CSizeSigmaPropElem

  class CSizeSigmaPropElem
    extends SizeSigmaPropElem[CSizeSigmaProp]
    with ConcreteElem[CSizeSigmaPropData, CSizeSigmaProp] {
    override lazy val parent: Option[Elem[_]] = Some(sizeSigmaPropElement)
    override def iso = ???
  }

  // state representation type
  type CSizeSigmaPropData = Size[Coll[Byte]]

  // 4) constructor and deconstructor
  class CSizeSigmaPropCompanionCtor extends CompanionDef[CSizeSigmaPropCompanionCtor] with CSizeSigmaPropCompanion {
    def resultType = CSizeSigmaPropCompanionElem
    override def toString = "CSizeSigmaPropCompanion"

    @scalan.OverloadId("fromFields")
    def apply(propBytes: Ref[Size[Coll[Byte]]]): Ref[CSizeSigmaProp] =
      mkCSizeSigmaProp(propBytes)

    def unapply(p: Ref[SizeSigmaProp]) = unmkCSizeSigmaProp(p)
  }
  lazy val CSizeSigmaPropRef: Ref[CSizeSigmaPropCompanionCtor] = new CSizeSigmaPropCompanionCtor
  lazy val RCSizeSigmaProp: CSizeSigmaPropCompanionCtor = unrefCSizeSigmaPropCompanion(CSizeSigmaPropRef)
  implicit def unrefCSizeSigmaPropCompanion(p: Ref[CSizeSigmaPropCompanionCtor]): CSizeSigmaPropCompanionCtor = {
    if (p.node.isInstanceOf[CSizeSigmaPropCompanionCtor])
      p.node.asInstanceOf[CSizeSigmaPropCompanionCtor]
    else
      unrefDelegate[CSizeSigmaPropCompanionCtor](p)
  }

  implicit case object CSizeSigmaPropCompanionElem extends CompanionElem[CSizeSigmaPropCompanionCtor]

  implicit def unrefCSizeSigmaProp(p: Ref[CSizeSigmaProp]): CSizeSigmaProp = {
    if (p.node.isInstanceOf[CSizeSigmaProp])
      p.node.asInstanceOf[CSizeSigmaProp]
    else
      unrefDelegate[CSizeSigmaProp](p)
  }

  // 5) implicit resolution of Iso
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
  implicit lazy val cSizeBoxElement: Elem[CSizeBox] =
    new CSizeBoxElem

  class CSizeBoxElem
    extends SizeBoxElem[CSizeBox]
    with ConcreteElem[CSizeBoxData, CSizeBox] {
    override lazy val parent: Option[Elem[_]] = Some(sizeBoxElement)
    def iso = ???
  }

  // state representation type
  type CSizeBoxData = (Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[Byte]], (Size[Coll[WOption[AnyValue]]], Size[Coll[(Coll[Byte], Long)]]))))

  // 4) constructor and deconstructor
  class CSizeBoxCompanionCtor extends CompanionDef[CSizeBoxCompanionCtor] with CSizeBoxCompanion {
    def resultType = CSizeBoxCompanionElem
    override def toString = "CSizeBoxCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CSizeBoxData]): Ref[CSizeBox] = {
      val Pair(propositionBytes, Pair(bytes, Pair(bytesWithoutRef, Pair(registers, tokens)))) = p
      mkCSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens)
    }

    @scalan.OverloadId("fromFields")
    def apply(propositionBytes: Ref[Size[Coll[Byte]]], bytes: Ref[Size[Coll[Byte]]], bytesWithoutRef: Ref[Size[Coll[Byte]]], registers: Ref[Size[Coll[WOption[AnyValue]]]], tokens: Ref[Size[Coll[(Coll[Byte], Long)]]]): Ref[CSizeBox] =
      mkCSizeBox(propositionBytes, bytes, bytesWithoutRef, registers, tokens)

    def unapply(p: Ref[SizeBox]) = unmkCSizeBox(p)
  }
  lazy val CSizeBoxRef: Ref[CSizeBoxCompanionCtor] = new CSizeBoxCompanionCtor
  lazy val RCSizeBox: CSizeBoxCompanionCtor = unrefCSizeBoxCompanion(CSizeBoxRef)
  implicit def unrefCSizeBoxCompanion(p: Ref[CSizeBoxCompanionCtor]): CSizeBoxCompanionCtor = {
    if (p.node.isInstanceOf[CSizeBoxCompanionCtor])
      p.node.asInstanceOf[CSizeBoxCompanionCtor]
    else
      unrefDelegate[CSizeBoxCompanionCtor](p)
  }

  implicit case object CSizeBoxCompanionElem extends CompanionElem[CSizeBoxCompanionCtor]

  implicit def unrefCSizeBox(p: Ref[CSizeBox]): CSizeBox = {
    if (p.node.isInstanceOf[CSizeBox])
      p.node.asInstanceOf[CSizeBox]
    else
      unrefDelegate[CSizeBox](p)
  }

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
  implicit lazy val cSizeContextElement: Elem[CSizeContext] =
    new CSizeContextElem

  class CSizeContextElem
    extends SizeContextElem[CSizeContext]
    with ConcreteElem[CSizeContextData, CSizeContext] {
    override lazy val parent: Option[Elem[_]] = Some(sizeContextElement)
    def iso = ???
  }

  // state representation type
  type CSizeContextData = (Size[Coll[Box]], (Size[Coll[Box]], (Size[Coll[Box]], (Size[Box], (Size[AvlTree], (Size[Coll[Header]], (Size[PreHeader], Coll[Size[AnyValue]])))))))

  // 4) constructor and deconstructor
  class CSizeContextCompanionCtor extends CompanionDef[CSizeContextCompanionCtor] with CSizeContextCompanion {
    def resultType = CSizeContextCompanionElem
    override def toString = "CSizeContextCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CSizeContextData]): Ref[CSizeContext] = {
      val Pair(outputs, Pair(inputs, Pair(dataInputs, Pair(selfBox, Pair(lastBlockUtxoRootHash, Pair(headers, Pair(preHeader, vars))))))) = p
      mkCSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars)
    }

    @scalan.OverloadId("fromFields")
    def apply(outputs: Ref[Size[Coll[Box]]], inputs: Ref[Size[Coll[Box]]], dataInputs: Ref[Size[Coll[Box]]], selfBox: Ref[Size[Box]], lastBlockUtxoRootHash: Ref[Size[AvlTree]], headers: Ref[Size[Coll[Header]]], preHeader: Ref[Size[PreHeader]], vars: Ref[Coll[Size[AnyValue]]]): Ref[CSizeContext] =
      mkCSizeContext(outputs, inputs, dataInputs, selfBox, lastBlockUtxoRootHash, headers, preHeader, vars)

    def unapply(p: Ref[SizeContext]) = unmkCSizeContext(p)
  }
  lazy val CSizeContextRef: Ref[CSizeContextCompanionCtor] = new CSizeContextCompanionCtor
  lazy val RCSizeContext: CSizeContextCompanionCtor = unrefCSizeContextCompanion(CSizeContextRef)
  implicit def unrefCSizeContextCompanion(p: Ref[CSizeContextCompanionCtor]): CSizeContextCompanionCtor = {
    if (p.node.isInstanceOf[CSizeContextCompanionCtor])
      p.node.asInstanceOf[CSizeContextCompanionCtor]
    else
      unrefDelegate[CSizeContextCompanionCtor](p)
  }

  implicit case object CSizeContextCompanionElem extends CompanionElem[CSizeContextCompanionCtor]

  implicit def unrefCSizeContext(p: Ref[CSizeContext]): CSizeContext = {
    if (p.node.isInstanceOf[CSizeContext])
      p.node.asInstanceOf[CSizeContext]
    else
      unrefDelegate[CSizeContext](p)
  }

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
  implicit lazy val cSizeBuilderElement: Elem[CSizeBuilder] =
    new CSizeBuilderElem

  class CSizeBuilderElem
    extends SizeBuilderElem[CSizeBuilder]
    with ConcreteElem[CSizeBuilderData, CSizeBuilder] {
    override lazy val parent: Option[Elem[_]] = Some(sizeBuilderElement)
    def iso = ???
  }

  // state representation type
  type CSizeBuilderData = Unit

  // 4) constructor and deconstructor
  class CSizeBuilderCompanionCtor extends CompanionDef[CSizeBuilderCompanionCtor] with CSizeBuilderCompanion {
    def resultType = CSizeBuilderCompanionElem
    override def toString = "CSizeBuilderCompanion"
    @scalan.OverloadId("fromData")
    def apply(p: Ref[CSizeBuilderData]): Ref[CSizeBuilder] = {
      mkCSizeBuilder()
    }

    @scalan.OverloadId("fromFields")
    def apply(): Ref[CSizeBuilder] =
      mkCSizeBuilder()

    def unapply(p: Ref[SizeBuilder]) = unmkCSizeBuilder(p)
  }
  lazy val CSizeBuilderRef: Ref[CSizeBuilderCompanionCtor] = new CSizeBuilderCompanionCtor
  lazy val RCSizeBuilder: CSizeBuilderCompanionCtor = unrefCSizeBuilderCompanion(CSizeBuilderRef)
  implicit def unrefCSizeBuilderCompanion(p: Ref[CSizeBuilderCompanionCtor]): CSizeBuilderCompanionCtor = {
    if (p.node.isInstanceOf[CSizeBuilderCompanionCtor])
      p.node.asInstanceOf[CSizeBuilderCompanionCtor]
    else
      unrefDelegate[CSizeBuilderCompanionCtor](p)
  }

  implicit case object CSizeBuilderCompanionElem extends CompanionElem[CSizeBuilderCompanionCtor]

  implicit def unrefCSizeBuilder(p: Ref[CSizeBuilder]): CSizeBuilder = {
    if (p.node.isInstanceOf[CSizeBuilder])
      p.node.asInstanceOf[CSizeBuilder]
    else
      unrefDelegate[CSizeBuilder](p)
  }

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
