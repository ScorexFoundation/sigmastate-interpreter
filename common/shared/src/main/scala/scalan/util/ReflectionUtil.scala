package scalan.util

import scala.language.existentials

object ReflectionUtil {
  /** Special character in the name. */
  def isSpecialChar(c: Char): Boolean = {
    ('0' <= c && c <= '9') || c == '$'
  }

}
