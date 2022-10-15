package special

import scalan.util.CollectionUtil
import scalan.RType
import scala.reflect.ClassTag
import special.collection.Coll
import scalan.RType.SomeType

object Types {
  type TupleData = Coll[Any]

  def tupleRType(types: Array[SomeType]): RType[TupleData] = TupleType(types)

  case class TupleType(items: Array[SomeType]) extends RType[TupleData] {
    val classTag: ClassTag[TupleData] = scala.reflect.classTag[TupleData]

    override def name: String = items.map(_.name).mkString("(", ", ", ")")

    override def hashCode(): Int = CollectionUtil.deepHashCode(items)

    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
      case that: TupleType => java.util.Arrays.equals(items.asInstanceOf[Array[AnyRef]], that.items.asInstanceOf[Array[AnyRef]])
      case _ => false
    })
  }
}
