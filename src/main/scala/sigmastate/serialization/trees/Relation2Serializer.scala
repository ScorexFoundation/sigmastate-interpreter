package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position
import sigmastate.serialization.{Constraints, Serializer, ValueSerializer}

case class Relation2Serializer[S1 <: SType, S2 <: SType, R <: Relation[S1, S2]]
(override val opCode: Byte,
 constructor: (Value[S1], Value[S2]) => R,
 constraints: Seq[Constraints.Constraint2]) extends ValueSerializer[R] {

  override def parseBody(bytes: Array[Byte], pos: Position): (R, Position) = {
    val r = Serializer.startReader(bytes, pos)
    if (bytes(pos) == ConcreteCollectionBooleanConstantCode) {
      val _ = r.getByte() // skip collection op code
      val booleans = r.getBits(2)
      val firstArg = BooleanConstant.fromBoolean(booleans(0)).asValue[S1]
      val secondArg = BooleanConstant.fromBoolean(booleans(1)).asValue[S2]
      (constructor(firstArg, secondArg), r.consumed)
    } else {
      val firstArg = r.getValue().asValue[S1]
      val secondArg = r.getValue().asValue[S2]
      assert(constraints.forall(c => c(firstArg.tpe.typeCode, secondArg.tpe.typeCode)))
      (constructor(firstArg, secondArg), r.consumed)
    }
  }

  override def serializeBody(rel: R): Array[Byte] = {
    assert(constraints.forall(c => c(rel.left.tpe.typeCode, rel.right.tpe.typeCode)), s"constraints failed for $rel")
    val w = Serializer.startWriter()
    (rel.left, rel.right) match {
      case (Constant(left, ltpe), Constant(right, rtpe)) if ltpe == SBoolean && rtpe == SBoolean =>
        w.put(ConcreteCollectionBooleanConstantCode)
        w.putBits(Array[Boolean](left.asInstanceOf[Boolean], right.asInstanceOf[Boolean]))
      case _ =>
        w.putValue(rel.left)
        w.putValue(rel.right)
    }
    w.toBytes
  }

}
