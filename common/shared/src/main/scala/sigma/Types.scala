package sigma

import scalan.RType
import scalan.RType.SomeType
import scalan.util.CollectionUtil

import scala.reflect.ClassTag

object Types {
  /** Generic representation of tuples values. */
  type TupleData = Coll[Any]

  /** Returns an RType object representing a tuple type with the given SomeType array types.
    * @param types An array of SomeType representing the types of each item in the tuple.
    * @return An RType object for the tuple type.
    */
  def tupleRType(types: Array[SomeType]): RType[TupleData] = TupleType(types)

  /** Descriptor (in RType family) of tuple types.
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
}
