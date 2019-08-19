package scalan

/** Function interface which support specialization and thus unboxed invocations. */
abstract class DFunc[@specialized(Int) A, B] {
  def apply(x: A): B
}

/** Convenient but SLOW adapter to be used in tests. */
class DFuncAdapter[A,B](f: A => B) extends DFunc[A, B] {
  override def apply(x: A): B = f(x)
}
