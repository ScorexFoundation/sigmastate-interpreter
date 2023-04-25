package scalan.reflection

/** JS Platform dependent implementation of reflection methods. */
object Platform {
  /** Returns an RClass instance for the given class.
    *
    * @param clazz The class for which to retrieve an RClass instance.
    * @tparam T The type of the class.
    * @return An RClass instance for the given class.
    * @throws java.lang.RuntimeException if RClass metadata for the given class cannot be
    *                                    found.
    */
  def resolveClass[T](clazz: Class[T]): RClass[T] = {
    val res = CommonReflection.classes.get(clazz) match {
      case Some(c) =>
        assert(c.clazz == clazz)
        c
      case _ =>
        sys.error(s"Cannot find RClass data for $clazz")
      // Uncomment the following line to collect missing reflection data and generate Scala code for it
      //        memoize(classes)(clazz, new JRClass[T](clazz))
    }
    res.asInstanceOf[RClass[T]]
  }
}
