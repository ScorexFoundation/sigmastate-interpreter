package scalan

import java.util.HashMap

/** Allocation free alternative to scala.Option with similar interface.
  * Using this in recognizers allows:
  * 1) to avoid allocation of Some(x)
  * 2) reading random memory location (where Some is stored) to access x
  **/
class Nullable[+T](val x: T) extends AnyVal {
  @inline final def isEmpty = x == null
  @inline final def get: T = x
  @inline final def isDefined = x != null
  @inline final def getOrElse[B >: T](default: =>B): B = if (x != null) x else default
  @inline final def toList: List[T] = if (x == null) Nil else x :: Nil
  @inline final def toOption: Option[T] = if (x == null) None else Some(x)
}
object Nullable {
  final val None: Nullable[Null] = new Nullable(null)
  final def apply[T](x: T): Nullable[T] = new Nullable(x)
  final def unapply[T](opt: Nullable[T]): Nullable[T] = opt
}

/** Allocation free alternative to scala.collection.mutable.Map with similar interface.
  * This simplifies optimization of performance critical code. */
class AVHashMap[K,V](val hashMap: HashMap[K,V]) extends AnyVal {
  @inline final def isEmpty: Boolean = hashMap.isEmpty
  @inline final def get(key: K): Nullable[V] = Nullable(hashMap.get(key))
  @inline final def apply(key: K): V = hashMap.get(key)
  @inline final def containsKey(key: K): Boolean = hashMap.containsKey(key)
  @inline final def put(key: K, value: V): V = hashMap.put(key, value)
  @inline final def clear(): Unit = {
    hashMap.clear()
  }
  final def getOrElseUpdate(key: K, op: => V): V = {
    var v = hashMap.get(key)
    if (v == null) {
      v = op
      hashMap.put(key, v)
    }
    v
  }
  @inline final def keySet: java.util.Set[K] = hashMap.keySet()
}
object AVHashMap {
  def apply[K,V](initialCapacity: Int) = new AVHashMap[K,V](new HashMap[K,V](initialCapacity))
}

