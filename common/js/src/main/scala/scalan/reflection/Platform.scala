package scalan.reflection

import scala.collection.mutable

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

  /** A cache that stores key-value pairs using a synchronized HashMap.
    *
    * @tparam K the type of keys used in the cache
    * @tparam V the type of values stored in the cache
    */
  class Cache[K, V] {

    /** key-value pairs. */
    private val map = mutable.HashMap.empty[K, V]

    /** Retrieves the value associated with the given key from the cache or
      * computes and stores the value if the key is not present in the cache.
      * This method is thread-safe using the synchronized block.
      *
      * @param key the key to look up or store in the cache
      * @param value a by-name parameter that computes the value to be stored if the key is not present
      * @return the value associated with the key, either retrieved or computed
      */
    def getOrElseUpdate(key: K, value: => V): V = synchronized {
      map.getOrElseUpdate(key, value)
    }
  }
}
