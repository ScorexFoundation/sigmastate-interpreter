package scalan.primitives

import scalan._
import scala.reflect.runtime.universe._
import scalan.util.CollectionUtil
import OverloadHack._
import scalan.compilation.{GraphVizConfig, GraphVizExport}
import special.Types

/**
 The code is inspired by LMS structs and is used in Scalan with the same semantics
 in order to easily translate operations to the equivalents via LmsBridge.
 Their usage in Scalan is limited to be consistent with functional semantics of Scalan.
 Don't expect everything possible in LMS to be also possible in Scalan in the same way.
 There are changes in the code:
 - Sym -> Exp
 - Manifest -> Elem
 - infix -> implicit class
 - no SourceContext, withPos
 - mirroring implemented in Scalan way (though consistent with LMS)
 */
trait Structs extends TypeDescs with GraphVizExport { self: Scalan =>
  // TODO consider if T type parameter is needed here and for AbstractStruct
  // It's only useful if we'll have some static typing on structs later (Shapeless' records?)
  abstract class StructTag[T <: Struct](implicit val typeTag: TypeTag[T]) extends Product {
    override def equals(other: Any): Boolean =
      !!!("StructTag.equals must be overridden so that the outer instances aren't compared")
  }
  case class SimpleTag[T <: Struct](name: String)(implicit typeTag: TypeTag[T]) extends StructTag[T] {
    override def equals(other: Any) = other match {
      case tag: Structs#SimpleTag[_] => name == tag.name && typeTag == tag.typeTag
      case _ => false
    }
  }
  object SimpleTag {
    def apply[T <: Struct](implicit tag: TypeTag[T]): SimpleTag[T] = SimpleTag[T](tag.tpe.typeSymbol.name.toString)
  }
  lazy val defaultStructTag = SimpleTag[Struct]

  protected def baseStructName(tag: StructTag[_]) = tag match {
    case `defaultStructTag` => ""
    case SimpleTag(name) => s"$name "
    // Intentionally no case _, add something here or override when extending StructTag!
  }

  type StructField = (String, Ref[Any])
  trait Struct {
    def tag: StructTag[_]
    def fields: Seq[StructField]
  }
  type RStruct = Ref[Struct]

  import Liftables._
  import scala.reflect.{ClassTag, classTag}

  type SStruct = special.collection.Coll[Any]

  case class StructConst[T <: Struct](constValue: SStruct, _selfType: StructElem[T])
        extends AbstractStruct[T] with LiftedConst[SStruct, T] {
    override lazy val resultType = _selfType
    def tag = _selfType.structTag
    val fields: Seq[(String, Ref[Any])] =
      constValue.toArray.zip(_selfType.fields).map { case (v, (fn, e)) => (fn, toRep(v)(asElem[Any](e))) }
    def liftable = liftableStruct(_selfType)
  }

  case class LiftableStruct[T <: Struct](eW: Elem[T]) extends Liftable[SStruct, T] {
    val sourceClassTag = classTag[SStruct]
    val sourceType = eW match {
      case se: StructElem[_] =>
        val (names, elems) = se.fields.unzip
        val types = elems.map(e => e.liftable.sourceType)
        Types.structRType(names.toArray, types.toArray)
      case _ =>
        !!!(s"Invalid argument of LiftableStruct($eW)")
    }
    def lift(x: SStruct): Ref[T] = StructConst(x, eW)
    def unlift(w: Ref[T]): SStruct = w.node match {
      case StructConst(x: SStruct, _) => x
      case _ => unliftError(w)
    }
  }

  implicit def liftableStruct[T <: Struct](implicit e: Elem[T]): Liftable[SStruct, T] =
    LiftableStruct(e)

  def liftStruct[T <: Struct](x: SStruct)(implicit eT: Elem[T]): Ref[T] = StructConst(x, eT)

  case class StructElem[T <: Struct](structTag: StructTag[T], fields: Seq[(String, Elem[_])]) extends Elem[T] {
    override def liftable: Liftables.Liftable[_, T] = asLiftable[SStruct, T](liftableStruct(this))

    def get(fieldName: String): Option[Elem[_]] = fields.find(_._1 == fieldName).map(_._2)
    def apply(fieldIndex: Int): Elem[_] = fields(fieldIndex)._2
    def apply(fieldName: String): Elem[_] = get(fieldName).getOrElse {
      !!!(s"""Field $fieldName not found in struct $fieldsString""")
    }
    def fieldNames = fields.map(_._1)
    def fieldElems: Seq[Elem[_]] = fields.map(_._2)
    def isEqualType(tuple: Seq[Elem[_]]) = {
      fields.length == tuple.length && fields.zip(tuple).forall { case ((fn,fe), e) => fe == e }
    }
    override def getName(f: TypeDesc => String) =
      baseStructName(structTag) + fieldsString(f)
    def fieldsString(f: TypeDesc => String): String =
      "{" + fields.map { case (fn,fe) => s"$fn: ${f(fe)}" }.mkString("; ") + "}"
    lazy val fieldsString: String = fieldsString(_.name)
    def findFieldIndex(fieldName: String): Int = fields.iterator.map(_._1).indexOf(fieldName)

    override def buildTypeArgs = EmptyTypeArgs
  }
  implicit def StructElemExtensions[T <: Struct](e: Elem[T]): StructElem[T] = e.asInstanceOf[StructElem[T]]

  /** Replaces a root tree of `PairElem`s in the given element `e` with `StructElem`s.
    * All other types are considered as leaves.
    * @return new StructElem if `e` is `PairElem` otherwise returns `e`.
    */
  def toStructElemShallow[T](e: Elem[T]): Elem[_] = e match {
    case pe: PairElem[a,b] =>
      tupleStructElement(toStructElemShallow(pe.eFst), toStructElemShallow(pe.eSnd))
    case _ => e
  }

  def structElement[T <: Struct](tag: StructTag[T], fields: Seq[(String, Elem[_])]): StructElem[T] =
    if (cacheElems)
      cachedElemByClass(tag, fields)(classOf[StructElem[T]])
    else
      StructElem(tag, fields)

  def structElement(fields: Seq[(String, Elem[_])]): StructElem[Struct] =
    structElement(defaultStructTag, fields)

  def structElementFor[T <: Struct : TypeTag](fields: Seq[(String, Elem[_])]): StructElem[T] =
    structElement(SimpleTag[T], fields)
  /**
    * Get tuple field name by index
    */
  def tupleFN(fieldIndex: Int) = s"_${fieldIndex + 1}"

  def tupleStructElement(fieldElems: Elem[_]*)(implicit o: Overloaded1): StructElem[Struct] = {
    val fields = fieldElems.zipWithIndex.map { case (f, i) => tupleFN(i) -> f }
    // TODO refactor: add tupleTag(n)
    structElement(defaultStructTag, fields)
  }

  def tuple2StructElement[A:Elem, B:Elem]: StructElem[Struct] =
    tupleStructElement(element[A], element[B])

  def tuple3StructElement[A:Elem, B:Elem, C:Elem]: StructElem[Struct] =
    tupleStructElement(element[A], element[B], element[C])


  implicit class StructOps(s: Ref[Struct]) {
    def getUntyped(index: Int): Ref[_] = field(s, index)
    def getUntyped(fieldName: String): Ref[_] = field(s, fieldName)
    def getUnchecked[A](fieldName: String): Ref[A] = asRep[A](field(s, fieldName))
    def get[A: Elem](fieldName: String): Ref[A] = {
      val value = getUnchecked[A](fieldName)
      assertElem(value, element[A])
      value
    }
    def tag: StructTag[_ <: Struct] = s.elem.asInstanceOf[StructElem[_ <: Struct]].structTag
    def fieldNames = s.elem.asInstanceOf[StructElem[_ <: Struct]].fieldNames
    def fields: Seq[StructField] = {
      fieldNames.map(name => (name, field(s, name)))
    }
    def mapFields(f: Ref[_] => Ref[_]) = {
      val newFields = fieldNames.map { name =>
        val fieldValue = field(s, name)
        val newValue = f(fieldValue)
        (name, newValue)
      }
      struct(tag, newFields)
    }
  }

  def struct(fields: StructField*)(implicit o: Overloaded1): Ref[Struct] = struct(fields)
  def struct(fields: Seq[StructField]): Ref[Struct] = struct(defaultStructTag, fields)
  def struct[T <: Struct](tag: StructTag[T], fields: StructField*)(implicit o: Overloaded1): Ref[T] =
    struct(tag, fields)
  def tupleStruct(items: Ref[_]*): Ref[Struct] = {
    val fields = items.zipWithIndex.map { case (f, i) => tupleFN(i) -> f }
    struct(defaultStructTag, fields)
  }

  abstract class AbstractStruct[T <: Struct] extends Def[T] {
    def tag: StructTag[T]
    def fields: Seq[StructField]
    lazy val resultType = structElement(tag, fields.map { case (name, value) => (name, value.elem) })
  }

  object Struct {
    def unapply[T <: Struct](d: Def[T]): Option[(StructTag[T], Seq[StructField])] = d match {
      case s: AbstractStruct[T] => Some((s.tag, s.fields))
      case _ => None
    }
  }

  object Field {
    def unapply[T](d: Def[T]): Option[(Ref[Struct], String)] = d match {
      case FieldApply(struct, fieldName) => Some((struct, fieldName))
      case _ => None
    }
  }

  case class SimpleStruct[T <: Struct](tag: StructTag[T], fields: Seq[StructField]) extends AbstractStruct[T] {
    override def transform(t: Transformer): Def[T] = SimpleStruct(tag, fields.map { case (n, s) => (n, t(s)) })
  }
  case class FieldApply[T](struct: Ref[Struct], fieldName: String)
    extends BaseDef[T]()(asElem[T](struct.elem(fieldName))) {
    override def transform(t: Transformer): Def[T] = FieldApply(t(struct), fieldName)
  }

  case class FieldUpdate[S <: Struct, T](struct: Ref[S], fieldName: String, value: Ref[T]) extends AbstractStruct[S] {
    val tag = struct.elem.structTag
    val fields = struct.elem.fields.map { case (fn, _) =>
      if (fn == fieldName)
        (fieldName, value)
      else
        (fn, field(struct, fn))
    }
    override def transform(t: Transformer): Def[S] = FieldUpdate(t(struct), fieldName, t(value))
  }

  case class ProjectionStruct(struct: Ref[Struct], outFields: Seq[String]) extends AbstractStruct[Struct] {
    def tag = defaultStructTag
    val fields = outFields.map(fn => (fn, field(struct, fn)))
    override def transform(t: Transformer): Def[Struct] = ProjectionStruct(t(struct), outFields)
  }

  def struct[T <: Struct](tag: StructTag[T], fields: Seq[StructField]): Ref[T] = {
    val names = fields.map(_._1)
    assert(names.distinct.lengthCompare(fields.length) == 0, s"Fields of struct should be distinct but $names")
    SimpleStruct(tag, fields)
  }
  def field(struct: Ref[Struct], field: String): Ref[_] = {
    struct.elem match {
      case se: StructElem[a] =>
        //        val fieldElem = se(field)
        FieldApply[a](struct, field)
      case _ =>
        !!!(s"Attempt to get field $field from a non-struct ${struct.varNameWithType}", struct)
    }
  }
  def field(struct: Ref[Struct], fieldIndex: Int): Ref[_] = {
    val fieldName = struct.elem.fields(fieldIndex)._1
    field(struct, fieldName)
  }

  def updateField[S <: Struct](struct: Ref[S], fieldName: String, v: Ref[_]): Ref[S] = FieldUpdate[S,Any](struct, fieldName, v)
  def fields(struct: Ref[Struct], fields: Seq[String]): Ref[Struct] = ProjectionStruct(struct, fields)

  override protected def formatDef(d: Def[_])(implicit config: GraphVizConfig): String = d match {
    case SimpleStruct(tag, fields) =>
      s"${baseStructName(tag)}{${fields.map { case (fn, s) => s"$fn:$s" }.mkString("; ")}}"
    case ProjectionStruct(struct, outs) => s"$struct.{${outs.mkString(",")}}"
    case FieldUpdate(s, fn, v) => s"$s.$fn := $v"
    case FieldApply(struct, fn) => s"$struct.$fn"
    case _ => super.formatDef(d)
  }

}
