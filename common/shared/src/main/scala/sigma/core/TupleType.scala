package sigma.core

import sigma.TupleData
import sigma.core.RType.SomeType
import sigma.util.CollectionUtil

import scala.reflect.ClassTag

/** Descriptor (in RType family) of tuple types.
  *
  * @param items types of tuple items
  */
case class TupleType(items: Array[SomeType]) extends RType[TupleData] {
  override val classTag: ClassTag[TupleData] = scala.reflect.classTag[TupleData]

  override def name: String = items.map(_.name).mkString("(", ", ", ")")

  override def hashCode(): Int = CollectionUtil.deepHashCode(items)

  override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
    case that: TupleType => java.util.Arrays.equals(items.asInstanceOf[Array[AnyRef]], that.items.asInstanceOf[Array[AnyRef]])
    case _ => false
  })
}
