package scalan.reflection

import java.lang.reflect.{Constructor, Field, Method}

class RClass[T](val value: Class[T]) {
  def getMethod(name: String, parameterTypes: Class[_]*): RMethod = {
    RMethod(value.getMethod(name, parameterTypes:_*))
  }

  def getSimpleName: String = value.getSimpleName
  def getName: String = value.getName
  def getConstructors(): Array[RConstructor[_]] = value.getConstructors.map(x => RConstructor.apply(x))

  def getDeclaredConstructors(): Array[RConstructor[_]] = value.getDeclaredConstructors.map(RConstructor(_))

  def isPrimitive(): Boolean = value.isPrimitive
  def getField(name: String): RField = RField(value.getField(name))

  def getSuperclass(): RClass[_ >: T] = RClass(value.getSuperclass)

  def isAssignableFrom(cls: Class[_]): Boolean = value.isAssignableFrom(cls)

  def getDeclaredMethods(): Array[RMethod] = value.getDeclaredMethods.map(RMethod(_))

  def canEqual(other: Any): Boolean = other.isInstanceOf[RClass[_]]

  override def equals(other: Any): Boolean = other match {
    case that: RClass[_] =>
      (that canEqual this) && value == that.value
    case _ => false
  }

  override def hashCode(): Int = {
    value.hashCode()
  }
}
object RClass {
  def apply[T](clazz: Class[T]): RClass[T] = new RClass[T](clazz)
}

class RField(val value: Field) {
  def canEqual(other: Any): Boolean = other.isInstanceOf[RField]

  override def equals(other: Any): Boolean = other match {
    case that: RField =>
      (that canEqual this) && value == that.value
    case _ => false
  }

  override def hashCode(): Int = {
    value.hashCode()
  }
}
object RField {
  def apply(field: Field): RField = new RField(field)
}

class RConstructor[T](val value: Constructor[T]) {
  def newInstance(initargs: AnyRef*): T = value.newInstance(initargs:_*)
  def getParameterTypes(): Array[RClass[_]] = value.getParameterTypes.map(RClass(_))

  def canEqual(other: Any): Boolean = other.isInstanceOf[RConstructor[_]]

  override def equals(other: Any): Boolean = other match {
    case that: RConstructor[_] =>
      (that canEqual this) && value == that.value
    case _ => false
  }

  override def hashCode(): Int = {
    value.hashCode()
  }
}
object RConstructor {
  def apply[T](value: Constructor[T]): RConstructor[T]  = new RConstructor[T](value)
}

class RMethod(val value: Method) {
  def invoke(obj: AnyRef, args: AnyRef*): AnyRef = value.invoke(obj, args:_*)

  def getName: String = value.getName

  def getDeclaringClass(): RClass[_] = RClass(value.getDeclaringClass)

  def canEqual(other: Any): Boolean = other.isInstanceOf[RMethod]

  override def equals(other: Any): Boolean = other match {
    case that: RMethod =>
      (that canEqual this) && value == that.value
    case _ => false
  }

  override def hashCode(): Int = {
    value.hashCode()
  }
}
object RMethod {
  def apply(value: Method): RMethod = new RMethod(value)
}

class RInvocationException() extends Exception