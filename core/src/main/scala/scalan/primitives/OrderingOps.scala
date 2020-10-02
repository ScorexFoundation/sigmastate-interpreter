package scalan.primitives

import scalan.{Base, Scalan, ExactOrdering}

trait OrderingOps extends Base { self: Scalan =>
  implicit def repOrderingToOrderingOps[T](x: Ref[T])(implicit n: ExactOrdering[T]) = new OrderingOpsCls(x)
  implicit def OrderingToOrderingOps[T](x: T)(implicit n: ExactOrdering[T], et: Elem[T]) = new OrderingOpsCls(toRep(x))

  class OrderingOpsCls[T](lhs: Ref[T])(implicit val n: ExactOrdering[T]) {
    def <(rhs: Ref[T]) = OrderingLT(n).apply(lhs,rhs)
    def <=(rhs: Ref[T]) = OrderingLTEQ(n).apply(lhs,rhs)
    def >(rhs: Ref[T]) = OrderingGT(n).apply(lhs,rhs)
    def >=(rhs: Ref[T]) = OrderingGTEQ(n).apply(lhs,rhs)
    def max(rhs: Ref[T]): Ref[T] = OrderingMax(n)(lhs.elem).apply(lhs,rhs)
    def min(rhs: Ref[T]): Ref[T] = OrderingMin(n)(lhs.elem).apply(lhs,rhs)
    def compare(rhs: Ref[T]): Ref[Int] = OrderingCompare(n).apply(lhs,rhs)
  }

  case class OrderingLT[T](ord: ExactOrdering[T]) extends BinOp[T, Boolean]("<") {
    override def applySeq(x: T, y: T): Boolean = ord.lt(x, y)
  }

  case class OrderingLTEQ[T](ord: ExactOrdering[T]) extends BinOp[T, Boolean]("<=") {
    override def applySeq(x: T, y: T): Boolean = ord.lteq(x, y)
  }

  case class OrderingGT[T](ord: ExactOrdering[T]) extends BinOp[T, Boolean](">") {
    override def applySeq(x: T, y: T): Boolean = ord.gt(x, y)
  }

  case class OrderingGTEQ[T](ord: ExactOrdering[T]) extends BinOp[T, Boolean](">=") {
    override def applySeq(x: T, y: T): Boolean = ord.gteq(x, y)
  }

  case class OrderingMax[T: Elem](ord: ExactOrdering[T]) extends BinOp[T, T]("max") {
    override def applySeq(x: T, y: T): T = ord.max(x, y)
  }

  case class OrderingMin[T: Elem](ord: ExactOrdering[T]) extends BinOp[T, T]("min") {
    override def applySeq(x: T, y: T): T = ord.min(x, y)
  }

  case class OrderingCompare[T](ord: ExactOrdering[T]) extends BinOp[T, Int]("compare") {
    override def applySeq(x: T, y: T): Int = ord.compare(x, y)
  }
}