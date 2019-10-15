package scalan

/** Non-thread safe (but efficient on single thread) immutable lazy value with reset.
  * The `block` may execute potentially many times, but only once before each reset. */
class MutableLazy[A] private (block: => A) {
  @volatile private[this] var _isSet: Boolean = false
  private[this] var _value: A = _

  def value: A = {
    if (!_isSet) {
      _value = block
      _isSet = true
    }
    _value
  }

  @inline def isSet = _isSet

  @inline def reset() = { _isSet = false }

  override def toString = {
    if (!_isSet)
      "<lazy>"
    else
      value.toString
  }
}

object MutableLazy {
  def apply[A](block: => A): MutableLazy[A] = new MutableLazy(block)
}