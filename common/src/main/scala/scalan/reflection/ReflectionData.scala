package scalan.reflection

import scala.collection.mutable

object ReflectionData {
  def cls[T](clazz: Class[T]): (Class[T], SRClass[T]) = clazz -> SRClass(clazz)

  val classes:  mutable.HashMap[Class[_], SRClass[_]] = mutable.HashMap(
    cls(classOf[java.lang.String])
  )

}
