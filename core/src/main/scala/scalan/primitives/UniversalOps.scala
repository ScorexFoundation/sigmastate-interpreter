package scalan.primitives

import scalan.{AVHashMap, Base, Scalan, Nullable}

trait UniversalOps extends Base { scalan: Scalan =>
  case class HashCode[A]() extends UnOp[A, Int]("hashCode", _.hashCode)

  case class ToString[A]() extends UnOp[A, String]("toString", _.toString)

  /** Represents calculation of size in bytes of the given value.
    * The descriptor value.elem can be used to decompose value into components.
    */
  case class SizeOf[T](value: Ref[T]) extends BaseDef[Long] {
    override def transform(t: Transformer) = SizeOf(t(value))
  }

  def sizeOf[T](value: Ref[T]): Ref[Long] = SizeOf(value)

  /** Special graph node to represent accumulation of the operation costs.
    * In general, due to node sharing it is incorrect to just sum up all the `args` costs
    * and add `resCost` to that value.
    * Example: <br>
    * <code>
    * val x = ..
    * val y = op1(x)
    * val z = op2(x)
    * val res = op3(y, z)
    * </code>
    * The naive summation will lead to the cost of x` is accumulated both into `cost of y`
    * and into `cost of z`, so in the `cost of res` it is accumulated twice.
    * To avoid this problem OpCost nodes require special handling in during evaluation.
    *
    * @param lambdaVar      the variable of the lambda in which scope this node is created.
    *                       This makes this node belong to the lambda body, even if it doesn't
    *                       otherwise depend on lambda argument.
    * @param costedValueId  The id of the node for which this node represents cost
    * @param args           costs of the arguments, which are here represent dependency information.
    * @param opCost         operation cost, which should be added to the current scope accumulated cost
    */
  case class OpCost(lambdaVar: Sym, costedValueId: Int, args: Seq[Ref[Int]], opCost: Ref[Int]) extends BaseDef[Int] {
    /** When this node is mirrored (as part of mirrorLambda) we apply transformer t for all arguments
      * with standard data flow semantics.
      * However this is not correct to do for lambdaVar.
      * Instead we use current lambda from the top of the stack, which is always points to the most nested
      * lambda.
      */
    override def transform(t: Transformer) = OpCost(lambdaStack.head.x, costedValueId, t(args), t(opCost))
  }

  def opCost(costedValue: Sym, args: Seq[Ref[Int]], opCost: Ref[Int]): Ref[Int] = {
    val id = costedValue.node.nodeId
    val lamVar = lambdaStack.head.x
    OpCost(lamVar, id, args, opCost)
  }

  def assertValueIdForOpCost[A,B](value: Ref[A], cost: Ref[B]): Unit = {
    assert(if (cost.node.isInstanceOf[OpCost]) value.node.nodeId == cost.node.asInstanceOf[OpCost].costedValueId else true,
      s"${value.node} value node id (${value.node.nodeId}) is not equal to OpCost.costedValueId (${cost.node.asInstanceOf[OpCost].costedValueId})")
  }

  case class Downcast[From, To](input: Ref[From], eTo: Elem[To]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Downcast(t(input), eTo)
  }
  case class Upcast[From, To](input: Ref[From], eTo: Elem[To]) extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Upcast(t(input), eTo)
  }

  def downcast[To:Elem](value: Ref[_]): Ref[To] = Downcast(value, element[To])
  def upcast[To:Elem](value: Ref[_]): Ref[To] = Upcast(value, element[To])

  implicit class RepUniversalOps[A](x: Ref[A]) {
    def hashCodeRep: Ref[Int] = HashCode[A]().apply(x)
    def toStringRep = ToString[A]().apply(x)
  }

  case class Convert[From,To](eFrom: Elem[From], eTo: Elem[To], x: Ref[Def[_]], conv: Ref[From => To])
      extends BaseDef[To]()(eTo) {
    override def transform(t: Transformer) = Convert(eFrom, eTo, t(x), t(conv))
  }

  def tryConvert[From, To](eFrom: Elem[From], eTo: Elem[To], x: Ref[Def[_]], conv: Ref[From => To]): Ref[To] = {
    if (x.elem <:< eFrom)
      conv(asRep[From](x))
    else
      Convert(eFrom, eTo, x, conv)
  }

}
