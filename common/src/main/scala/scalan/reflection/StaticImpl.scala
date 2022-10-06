package scalan.reflection

class SRClass[T](name: String) extends RClass[T] {
  override def getField(name: String): RField = ???

  override def getMethod(name: String,
                         parameterTypes: Class[_]*): RMethod = ???

  override def getSimpleName: String = ???

  override def getName: String = name

  override def getConstructors(): Array[RConstructor[_]] = ???

  override def getDeclaredConstructors(): Array[RConstructor[_]] = ???

  override def isPrimitive(): Boolean = ???

  override def getSuperclass(): RClass[_ >: T] = ???

  override def isAssignableFrom(cls: Class[_]): Boolean = ???

  override def getDeclaredMethods(): Array[RMethod] = ???
}
