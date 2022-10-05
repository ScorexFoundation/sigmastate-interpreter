package scalan.reflection

import scalan.reflection.RClass.memoize
import scalan.util.PrintExtensions.IterableExtensions

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.mutable

class RClass[T](val value: Class[T]) {
  val fields = mutable.HashMap.empty[String, RField]

  def getField(name: String): RField =
    memoize(fields)(name, RField(value.getField(name)))

  val methods = mutable.HashMap.empty[(String, Seq[Class[_]]), RMethod]

  def getMethod(name: String, parameterTypes: Class[_]*): RMethod = {
    memoize(methods)((name, parameterTypes), RMethod(value.getMethod(name, parameterTypes:_*)))
  }

  def getSimpleName: String = value.getSimpleName
  def getName: String = value.getName
  def getConstructors(): Array[RConstructor[_]] = value.getConstructors.map(x => RConstructor.apply(x))

  def getDeclaredConstructors(): Array[RConstructor[_]] = value.getDeclaredConstructors.map(RConstructor(_))

  def isPrimitive(): Boolean = value.isPrimitive

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
  val classes = mutable.HashMap.empty[Class[_], RClass[_]]

  def memoize[K, V](map: mutable.HashMap[K, V])(key: K, value: => V): V = map.synchronized {
    map.get(key) match {
      case Some(v) => v
      case None =>
        val v = value
        map.put(key, v)
        v
    }
  }

  def apply[T](clazz: Class[T]): RClass[T] = memoize(classes)(clazz, new RClass[T](clazz)).asInstanceOf[RClass[T]]

  def generateReport(): String = {
    val b = new mutable.StringBuilder(100)
    for (e <- classes) {
      val clazz = e._1
      b.append(s"$clazz {\n")
      for ((n, f) <- e._2.fields) {
        b.append(s"  val $n: ${f.value.getType.getName}\n")
      }
      for (((n, args), m) <- e._2.methods) {
        b.append(s"  def $n($args) -> obj.asInstanceOf[${clazz.getName}].$n(${args.zipWithIndex.rep{case (c, i) => s"args($i).asInstanceOf[${c.getName}]"}})\n")
      }
      b.append(s"}\n")
    }
    b.result()
  }
}

class RField private (val value: Field) {
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
  private[reflection] def apply(field: Field): RField = new RField(field)
}

class RConstructor[T] private (val value: Constructor[T]) {
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
  private[reflection] def apply[T](value: Constructor[T]): RConstructor[T]  = new RConstructor[T](value)
}

class RMethod private (val value: Method) {
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
  private[reflection] def apply(value: Method): RMethod = new RMethod(value)
}

class RInvocationException() extends Exception