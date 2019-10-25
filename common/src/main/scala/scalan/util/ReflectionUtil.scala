package scalan.util

import java.lang.reflect.{Method, AnnotatedElement}

import scala.reflect.{classTag, ClassTag}
import scalan.OverloadId

object ReflectionUtil {
  def jAnnotation[A <: java.lang.annotation.Annotation : ClassTag](element: AnnotatedElement) =
    Option(element.getAnnotation(classTag[A].runtimeClass.asInstanceOf[Class[A]]))

  def overloadId(method: Method) = jAnnotation[OverloadId](method).map(_.value)

  /** Returns the superclass for an anonymous class produced by mixing in traits; the argument otherwise. */
  def namedSuperclass(clazz: Class[_]) = {
    if (clazz.getSimpleName.contains("$anon$")) {
      val superclass = clazz.getSuperclass
      if (superclass == classOf[Object]) {
        // clazz is composed of traits only, return the first one
        clazz.getInterfaces.head
      } else
        superclass
    } else
      clazz
  }

  implicit class ClassOps(val cl: Class[_]) extends AnyVal {
    private def isSpecialChar(c: Char): Boolean = {
      ('0' <= c && c <= '9') || c == '$'
    }
    def safeSimpleName: String = {
      if (cl.getEnclosingClass == null) return cl.getSimpleName
      val simpleName = cl.getName.substring(cl.getEnclosingClass.getName.length)
      val length = simpleName.length
      var index = 0
      while (index < length && isSpecialChar(simpleName.charAt(index))) { index += 1 }
      // Eventually, this is the empty string iff this is an anonymous class
      simpleName.substring(index)
    }
  }
}
