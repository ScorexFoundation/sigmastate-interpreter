package special

import scalan.RType
import scalan.util.CollectionUtil

import scala.reflect.ClassTag
import scalan.RType.SomeType
import special.collection.Coll

object Types {

  type StructData = Coll[Any]

  def structRType(names: Array[String], types: Array[SomeType]): RType[StructData] = StructType(names, types)

  case class StructType(fieldNames: Array[String], fieldTypes: Array[SomeType]) extends RType[StructData] {
    val classTag: ClassTag[StructData] = scala.reflect.classTag[StructData]
    override def isConstantSize: Boolean = fieldTypes.forall(_.isConstantSize)
    override def hashCode(): Int = {
      var h = CollectionUtil.deepHashCode(fieldNames)
      h += h * 31 + CollectionUtil.deepHashCode(fieldTypes)
      h
    }
    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
      case that: StructType =>
        java.util.Arrays.equals(fieldNames.asInstanceOf[Array[AnyRef]], that.fieldNames.asInstanceOf[Array[AnyRef]]) &&
            java.util.Arrays.equals(fieldTypes.asInstanceOf[Array[AnyRef]], that.fieldTypes.asInstanceOf[Array[AnyRef]])
      case _ => false
    })
  }

  type TupleData = Coll[Any]

  def tupleRType(types: Array[SomeType]): RType[TupleData] = TupleType(types)

  case class TupleType(items: Array[SomeType]) extends RType[StructData] {
    val classTag: ClassTag[TupleData] = scala.reflect.classTag[TupleData]
    override def name: String = items.map(_.name).mkString("(", ", ", ")")
    override def isConstantSize: Boolean = items.forall(_.isConstantSize)
    override def hashCode(): Int = CollectionUtil.deepHashCode(items)
    override def equals(obj: Any): Boolean = (this eq obj.asInstanceOf[AnyRef]) || (obj match {
      case that: TupleType => java.util.Arrays.equals(items.asInstanceOf[Array[AnyRef]], that.items.asInstanceOf[Array[AnyRef]])
      case _ => false
    })
  }

}

