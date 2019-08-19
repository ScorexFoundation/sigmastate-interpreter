package scalan.primitives

import scalan.{Base, Scalan}

trait LogicalOps extends Base { self: Scalan =>
  val And = new EndoBinOp[Boolean]("&&", _ && _)

  val Or = new EndoBinOp[Boolean]("||", _ || _)

  val Not = new EndoUnOp[Boolean]("!", !_)

  val BinaryXorOp = new EndoBinOp[Boolean]("^", _ ^ _)

  val BooleanToInt = new UnOp[Boolean, Int]("ToInt", if (_) 1 else 0)

  implicit class RepBooleanOps(value: Ref[Boolean]) {
    def &&(y: Ref[Boolean]): Ref[Boolean] = And(value, y)
    def ||(y: Ref[Boolean]): Ref[Boolean] = Or(value, y)
    def ^(y: Ref[Boolean]): Ref[Boolean] = BinaryXorOp(value, y)

    def lazy_&&(y: Ref[Thunk[Boolean]]): Ref[Boolean] = And.applyLazy(value, y)
    def lazy_||(y: Ref[Thunk[Boolean]]): Ref[Boolean] = Or.applyLazy(value, y)

    def unary_!() : Ref[Boolean] = Not(value)
    def toInt: Ref[Int] = BooleanToInt(value)
  }


  @inline
  final def rewriteBoolConsts(lhs: Sym, rhs: Sym, ifTrue: Sym => Sym, ifFalse: Sym => Sym, ifEqual: Sym => Sym, ifNegated: Sym => Sym): Sym =
    lhs match {
      // op(x, x)
      case `rhs` =>
        ifEqual(lhs)

      // op(!x, x) => ifNegated(!x)
      case Def(ApplyUnOp(op, `rhs`)) if op == Not =>
        ifNegated(lhs)

      // op(true, x) => ifTrue(x) | op(false, x) => ifFalse(x)
      case Def(Const(b: Boolean)) =>
        if (b) ifTrue(rhs) else ifFalse(rhs)

      case _ =>
        rhs.node match {
          // op(x, true) => ifTrue(x) | op(false, x) => ifFalse(x)
          case Const(b: Boolean) =>
            if (b) ifTrue(lhs) else ifFalse(lhs)

          // op(x, !x) => ifNegated(!x)
          case ApplyUnOp(op, `lhs`) if op == Not =>
            ifNegated(rhs)
          case _ => null
        }
    }
}
