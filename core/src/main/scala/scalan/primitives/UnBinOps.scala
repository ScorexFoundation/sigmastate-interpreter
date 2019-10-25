package scalan.primitives

import scalan.{Scalan, Base}

trait UnBinOps extends Base { self: Scalan =>

  class UnOp[A, R](val opName: String, val applySeq: A => R)(implicit val eResult: Elem[R]) {
    override def toString = opName

    def apply(arg: Ref[A]) = applyUnOp(this, arg)

    def shouldPropagate(arg: A) = true
  }

  class BinOp[A, R](val opName: String, val applySeq: (A, A) => R)(implicit val eResult: Elem[R]) {
    override def toString = opName

    def apply(lhs: Ref[A], rhs: Ref[A]) = applyBinOp(this, lhs, rhs)
    def applyLazy(lhs: Ref[A], rhs: Ref[Thunk[A]]) = applyBinOpLazy(this, lhs, rhs)

    // ideally shouldn't be necessary, but
    // we curently can't handle division by zero properly
    def shouldPropagate(lhs: A, rhs: A) = true
  }

  type EndoUnOp[A] = UnOp[A, A]
  type EndoBinOp[A] = BinOp[A, A]

  case class ApplyUnOp[A, R](op: UnOp[A, R], arg: Ref[A]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$op($arg)"
    override def transform(t: Transformer): Def[R] = ApplyUnOp[A,R](op, t(arg))
  }

  case class ApplyBinOp[A, R](op: BinOp[A, R], lhs: Ref[A], rhs: Ref[A]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$op($lhs, $rhs)"
    override def transform(t: Transformer): Def[R] = ApplyBinOp[A,R](op, t(lhs), t(rhs))
  }
  case class ApplyBinOpLazy[A, R](op: BinOp[A, R], lhs: Ref[A], rhs: Ref[Thunk[A]]) extends BaseDef[R]()(op.eResult) {
    override def toString = s"$lhs $op { $rhs }"
    override def transform(t: Transformer): Def[R] = ApplyBinOpLazy[A,R](op, t(lhs), t(rhs))
  }

  def applyUnOp[A, R](op: UnOp[A, R], arg: Ref[A]): Ref[R] = ApplyUnOp(op, arg)

  def applyBinOp[A, R](op: BinOp[A, R], lhs: Ref[A], rhs: Ref[A]): Ref[R] = ApplyBinOp(op, lhs, rhs)
  def applyBinOpLazy[A, R](op: BinOp[A, R], lhs: Ref[A], rhs: Ref[Thunk[A]]): Ref[R] = ApplyBinOpLazy(op, lhs, rhs)
}