package scalan.reflection

import scala.collection.mutable

class SRField(val name: String, tpe: Class[_]) extends RField {
  override def getType: Class[_] = tpe

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: SRField => name == that.name
    case _ => false
  })
  override def hashCode(): Int = name.hashCode()
}

case class SRClass[T](clazz: Class[_]) extends RClass[T] {
  val fields = mutable.HashMap.empty[String, SRField]
  val methods = mutable.HashMap.empty[(String, Seq[Class[_]]), RMethod]

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

  override def getConstructors(): Array[RConstructor[_]] = ???

  override def isPrimitive(): Boolean = ???

  override def getSuperclass(): RClass[_ >: T] = ???

  override def isAssignableFrom(cls: Class[_]): Boolean = ???

  override def getDeclaredMethods(): Array[RMethod] = ???
}
