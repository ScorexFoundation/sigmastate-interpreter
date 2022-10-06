package scalan.reflection

import scala.collection.mutable
import scalan.util.PrintExtensions.IterableExtensions

abstract class RField {
  def getType: Class[_]
}

trait RConstructor[T] {
  def newInstance(initargs: AnyRef*): T
  def getParameterTypes(): Array[RClass[_]]
}

abstract class RClass[T] {
  def getField(name: String): RField

  def getMethod(name: String, parameterTypes: Class[_]*): RMethod

  def getSimpleName: String

  def getName: String

  def getConstructors(): Array[RConstructor[_]]

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
    val res = ReflectionData.classes.get(clazz) match {
      case Some(c) => c
      case _ =>
        memoize(classes)(clazz, new JRClass[T](clazz))
    }
    res
        .asInstanceOf[RClass[T]]
  }

  def generateReport(): String = {
    val b = new mutable.StringBuilder(100)
    for ( e <- classes ) {
      val clazz = e._1
      b.append(s"$clazz {\n")
      for ( (n, f) <- e._2.fields ) {
        b.append(s"  val $n: ${f.getType.getName}\n")
      }
      for ( ((n, args), m) <- e._2.methods ) {
        b.append(s"  def $n($args) -> obj.asInstanceOf[${clazz.getName}].$n(${args.zipWithIndex.rep { case (c, i) => s"args($i).asInstanceOf[${c.getName}]" }})\n")
      }
      b.append(s"}\n")
    }
    b.result()
  }
}