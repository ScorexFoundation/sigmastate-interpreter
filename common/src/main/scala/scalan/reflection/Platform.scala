package scalan.reflection

import scala.collection.concurrent.TrieMap

/** Platform dependent implementation of reflection methods. */
object Platform {
  /** Stores all classes which were accessed during runtime and whose metadata was not
    * registered.
    * This should be used only for debugging and never in production.
    */
  val unknownClasses = TrieMap.empty[Class[_], JRClass[_]]

  /** Thread-safe storage of class information. */
  private val classes = TrieMap.empty[Class[_], JRClass[_]]

  /** Check class registration. Should be used only for debugging. */
  private def checkRegisteredClass[T](clazz: Class[T]): Unit = {
    CommonReflection.classes.get(clazz) match {
      case Some(c) =>
        assert(c.clazz == clazz)
      case _ =>
        sys.error(s"Cannot find RClass data for $clazz")
    }
  }

  /** Returns an RClass instance for the given class.
    *
    * @param clazz The class for which to retrieve an RClass instance.
    * @tparam T The type of the class.
    * @return An RClass instance for the given class.
    * @throws java.lang.RuntimeException if RClass metadata for the given class cannot be
    *                                    found.
    */
  def resolveClass[T](clazz: Class[T]): RClass[T] = {
    val cls = memoize(classes)(clazz, new JRClass[T](clazz)).asInstanceOf[JRClass[T]]
    // Uncomment the following lines to collect missing reflection data and generate Scala code for it
    // Should be used only for debugging and never in production.
    try {
      checkRegisteredClass(clazz)
    } catch {
      case e: RuntimeException =>
        memoize(unknownClasses)(clazz, cls)
    }
    cls
  }
}
