package scalan.reflection

import scala.collection.mutable

abstract class RField {
  def getType: Class[_]
}

trait RConstructor[T] {
  def newInstance(args: AnyRef*): T
  def getParameterTypes(): Array[Class[_]]
}

abstract class RMethod {
  def invoke(obj: Any, args: AnyRef*): Any
  def getName: String
  def getDeclaringClass(): Class[_]
  def getParameterTypes(): Seq[Class[_]]
}

abstract class RClass[T] {
  def getField(name: String): RField

  def getMethod(name: String, parameterTypes: Class[_]*): RMethod

  def getSimpleName: String

  def getName: String

  def getConstructors(): Seq[RConstructor[_]]

  def isPrimitive(): Boolean

  def getSuperclass(): RClass[_ >: T]

  def isAssignableFrom(cls: Class[_]): Boolean

  def getDeclaredMethods(): Array[RMethod]
}

object RClass {
  val classes = mutable.HashMap.empty[Class[_], JRClass[_]]

  def memoize[K, V](map: mutable.HashMap[K, V])
                   (key: K, value: => V): V = map.synchronized {
    map.get(key) match {
      case Some(v) => v
      case None =>
        val v = value
        map.put(key, v)
        v
    }
  }

  def apply[T](clazz: Class[T]): RClass[T] = {
    val res = CommonReflection.classes.get(clazz) match {
      case Some(c) =>
        assert(c.clazz == clazz)
        c
      case _ =>
//        sys.error(s"Cannot find RClass data for $clazz")
        memoize(classes)(clazz, new JRClass[T](clazz))
    }
    res.asInstanceOf[RClass[T]]
  }

}