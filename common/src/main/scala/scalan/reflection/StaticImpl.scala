package scalan.reflection

import scalan.reflection.ReflectionData.SRClassBuilder

import java.util
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
  override def getParameterTypes(): Array[Class[_]] = parameterTypes
}

abstract class SRMethod(declaringClass: Class[_], name: String, parameterTypes: Seq[Class[_]]) extends RMethod {
  override def getName: String = name
  override def getDeclaringClass(): Class[_] = declaringClass
  override def getParameterTypes(): Seq[Class[_]] = parameterTypes

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: SRMethod
      if getDeclaringClass == that.getDeclaringClass && getName == that.getName =>
      parameterTypes == that.getParameterTypes()
    case _ => false
  })
}

class SRClass[T](val clazz: Class[T],
                 constructors: Seq[SRConstructor[_]],
                 fields: Map[String, SRField],
                 methods: Map[(String, Seq[Class[_]]), RMethod]) extends RClass[T] {

  override def getField(fieldName: String): RField = fields.get(fieldName) match {
    case Some(f) => f
    case _ => throw new NoSuchFieldException(s"${clazz.getName}.$fieldName")
  }

  override def getMethod(name: String,
                         parameterTypes: Class[_]*): RMethod = {
    methods.get((name, parameterTypes)) match {
      case Some(m) => m
      case _ => throw new NoSuchMethodException(s"${clazz.getName}.$name(${parameterTypes.map(_.getName).mkString(",")})")
    }
  }

  override def getSimpleName: String = clazz.getSimpleName

  override def getName: String = clazz.getName

  override def isPrimitive(): Boolean = clazz.isPrimitive

  override def getConstructors(): Seq[RConstructor[_]] = constructors

  override def getSuperclass(): RClass[_ >: T] = clazz.getSuperclass.asInstanceOf[RClass[_ >: T]]

  override def isAssignableFrom(cls: Class[_]): Boolean = clazz.isAssignableFrom(cls)

  override def getDeclaredMethods(): Array[RMethod] = methods.values.toArray

  override def equals(other: Any): Boolean = (this eq other.asInstanceOf[AnyRef]) || (other match {
    case that: SRClass[_] => clazz == that.clazz
    case _ => false
  })
  override def hashCode(): Int = clazz.hashCode()
}

object SRClass {
  def newBuilder[T](clazz: Class[T]): SRClassBuilder[T] = new SRClassBuilder(clazz)
}
