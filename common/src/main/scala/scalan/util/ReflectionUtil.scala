package scalan.util

import scala.language.existentials

object ReflectionUtil {

  implicit class ClassOps(val cl: Class[_]) extends AnyVal {
    /** Special character in the name. */
    private def isSpecialChar(c: Char): Boolean = {
      ('0' <= c && c <= '9') || c == '$'
    }

    /** Safe version of `getSimpleName` that works around a bug in Scala compilers 2.11, 2.12.
      * This method is only used for debugging purposes.
      * @see https://github.com/scala/bug/issues/5425
      */
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
