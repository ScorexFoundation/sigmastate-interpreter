package scalan.reflection

import scala.collection.compat.immutable.ArraySeq

class SRField(val name: String, tpe: Class[_]) extends RField {
  override def getType: Class[_] = tpe

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: SRField => name == that.name
    case _ => false
  })
  override def hashCode(): Int = name.hashCode()
}

abstract class SRConstructor[T](parameterTypes: Array[Class[_]]) extends RConstructor[T] {
  override def getParameterTypes(): Array[RClass[_]] = parameterTypes.map(RClass(_))
}

class SRClass[T](val clazz: Class[_],
                 constructors: Seq[SRConstructor[_]],
                 fields: Map[String, SRField],
                 methods: Map[(String, Seq[Class[_]]), RMethod]) extends RClass[T] {

  override def getField(fieldName: String): RField = fields(fieldName)

  override def getMethod(name: String,
                         parameterTypes: Class[_]*): RMethod = {
    methods.get((name, parameterTypes)) match {
      case Some(m) => m
      case _ => throw new NoSuchMethodException(name)
    }
  }

  override def getSimpleName: String = clazz.getSimpleName

  override def getName: String = clazz.getName

  override def isPrimitive(): Boolean = clazz.isPrimitive

  override def getConstructors(): Array[RConstructor[_]] = ArraySeq.empty.toArray

  override def getSuperclass(): RClass[_ >: T] = clazz.getSuperclass.asInstanceOf[RClass[_ >: T]]

  override def isAssignableFrom(cls: Class[_]): Boolean = clazz.isAssignableFrom(cls)

  override def getDeclaredMethods(): Array[RMethod] = methods.values.toArray

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: SRClass[_] => clazz == that.clazz
    case _ => false
  })
  override def hashCode(): Int = clazz.hashCode()
}
