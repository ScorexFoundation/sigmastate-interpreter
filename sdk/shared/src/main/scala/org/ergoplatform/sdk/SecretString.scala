package org.ergoplatform.sdk

import debox.cfor

import java.util

/**
  * Encapsulates secret array of characters (char[]) with proper equality.
  * The secret data can be {@link SecretString# erase ( ) erased} in memory and not leaked to GC.
  * Using this class is more secure and safe than using char[] directly.
  */
object SecretString {
  /**
    * Creates a new instance wrapping the given characters. The given array is not copied.
    */
  def create(data: Array[Char]) = new SecretString(data)

  /**
    * Creates a new instance by copying characters from the given String.
    */
  def create(s: String) = new SecretString(s.toCharArray)

  /**
    * Create a new instance with empty sequence of characters.
    */
  def empty() = new SecretString(new Array[Char](0))
}

/**
  * Secret data, should not be copied outside of this instance.
  * Use static methods to construct new instances.
  *
  */
final class SecretString private[sdk](val _data: Array[Char]) extends CharSequence {
  /**
    * Returns true if the string doesn't have characters.
    */
  override def isEmpty(): Boolean = {
    _data == null || _data.length == 0
  }

  /**
    * Extracts secret characters as an array.
    */
  def getData(): Array[Char] = {_data }

  /**
    * Erases secret characters stored in this instance so that they are no longer reside in memory.
    */
  def erase(): Unit = {
    util.Arrays.fill(_data, ' ')
  }

  override def hashCode(): Int = {
    util.Arrays.hashCode(_data)
  }

  /** this is adapted version of java.lang.String */
  override def equals(obj: Any): Boolean = {
    if (this eq obj.asInstanceOf[AnyRef]) return true
    if (obj == null) return false
    obj match {
      case anotherString: SecretString if _data.length == anotherString._data.length =>
        var n: Int = _data.length
        val v1: Array[Char] = _data
        val v2: Array[Char] = anotherString._data
        cfor(0)(_ < n, _ + 1) { i =>
          if (v1(i) != v2(i)) return false
        }
        true
      case _ => false
    }
  }

  /**
    * Returns unsecured String with secret characters.
    * The secret characters are copied to the new String instance and cannot be erased in memory.
    * So they leak to GC and may remain in memory until overwritten by new data.
    * Usage of this method is discouraged and the method is provided solely to interact with
    * legacy code which keeps secret characters in String.
    */
  def toStringUnsecure(): String = {
    String.valueOf(_data)
  }

  override def length(): Int = _data.length

  override def charAt(index: Int): Char = _data(index)

  override def subSequence(start: Int, end: Int): CharSequence = {
    val slice = _data.slice(start, end)
    new SecretString(slice)
  }
}
