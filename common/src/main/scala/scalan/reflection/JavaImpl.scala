package scalan.reflection

import debox.cfor
import scalan.reflection.RClass.memoize

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.mutable

class JRClass[T](val value: Class[T]) extends RClass[T] {
  val fields = mutable.HashMap.empty[String, RField]

  override def getField(name: String): RField =
    memoize(fields)(name, JRField(value.getField(name)))

  val methods = mutable.HashMap.empty[(String, Seq[Class[_]]), RMethod]

  override def getMethod(name: String, parameterTypes: Class[_]*): RMethod = {
    memoize(methods)((name, parameterTypes), JRMethod(value.getMethod(name, parameterTypes:_*)))
  }

  def getSimpleName: String = value.getSimpleName
  def getName: String = value.getName

  var constructors: Array[RConstructor[_]] = _

  def getConstructors(): Array[RConstructor[_]] = {
    if (constructors == null) {
      synchronized {
        if (constructors == null) {
          val cs = value.getConstructors.asInstanceOf[Array[Constructor[Any]]]
          val buf = mutable.ArrayBuilder.make[RConstructor[Any]]()
          cfor(0)(_ < cs.length, _ + 1) { i =>
            val c = cs(i)
            buf += JRConstructor[Any](i, c)
          }
          constructors = buf.result().asInstanceOf[Array[RConstructor[_]]]
        }
      }
    }
    constructors
  }

  def isPrimitive(): Boolean = value.isPrimitive

  def getSuperclass(): RClass[_ >: T] = RClass(value.getSuperclass)

  def isAssignableFrom(cls: Class[_]): Boolean = value.isAssignableFrom(cls)

  def getDeclaredMethods(): Array[RMethod] = value.getDeclaredMethods.map(JRMethod(_))

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: JRClass[_] =>
      val eq = value == that.value
      if (!eq)
        assert(this.getName != that.getName) // sanity check
      eq
    case _ => false
  })

  override def hashCode(): Int = value.hashCode()
}


class JRField private (val value: Field) extends RField {
  override def getType: Class[_] = value.getType

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: JRField => value == that.value
    case _ => false
  })
  override def hashCode(): Int = value.hashCode()
}
object JRField {
  private[reflection] def apply(field: Field): RField = new JRField(field)
}

class JRConstructor[T] private (val index: Int, val value: Constructor[T]) extends RConstructor[T] {
  override def newInstance(initargs: AnyRef*): T = value.newInstance(initargs:_*)
  override def getParameterTypes(): Array[RClass[_]] = value.getParameterTypes.map(RClass(_))

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: JRConstructor[_] => value == that.value
    case _ => false
  })

  override def hashCode(): Int = value.hashCode()
}
object JRConstructor {
  private[reflection] def apply[T](index: Int, value: Constructor[T]): RConstructor[T]  = new JRConstructor[T](index, value)
}

abstract class RMethod {
  def invoke(obj: AnyRef, args: AnyRef*): AnyRef
  def getName: String
  def getDeclaringClass(): RClass[_]
}

class JRMethod private (val value: Method) extends RMethod {
  def invoke(obj: AnyRef, args: AnyRef*): AnyRef = value.invoke(obj, args:_*)

  def getName: String = value.getName

  def getDeclaringClass(): RClass[_] = RClass(value.getDeclaringClass)

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: JRMethod => value == that.value
    case _ => false
  })

  override def hashCode(): Int = value.hashCode()
}
object JRMethod {
  private[reflection] def apply(value: Method): RMethod = new JRMethod(value)
}

class RInvocationException() extends Exception