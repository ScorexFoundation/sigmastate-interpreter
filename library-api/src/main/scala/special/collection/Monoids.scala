package special.collection

trait Monoid[@specialized(Int, Long) T] {
  def zero: T
  def plus(x: T, y: T): T
  def power(x: T, n: Int): T
}

trait MonoidBuilder {
  def intPlusMonoid: Monoid[Int]
  def longPlusMonoid: Monoid[Long]
}


