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

  import ValueSerializer.{deserialize, serialize}

  override def parseBody(bytes: Array[Byte], pos: Position): (R, Position) = {
    if (bytes(pos) == ConcreteCollectionBooleanConstantCode) {
      val r = Serializer.startReader(bytes, pos)
      val _ = r.getByte() // skip collection op code
      val booleans = r.getBits(2)
      val firstArg = BooleanConstant.fromBoolean(booleans(0)).asValue[S1]
      val secondArg = BooleanConstant.fromBoolean(booleans(1)).asValue[S2]
      (constructor(firstArg, secondArg), r.consumed)
    } else {
      val (firstArg, consumed) = deserialize(bytes, pos)
      val (secondArg, consumed2) = deserialize(bytes, pos + consumed)
      assert(constraints.forall(c => c(firstArg.tpe.typeCode, secondArg.tpe.typeCode)))
      (constructor(firstArg.asInstanceOf[Value[S1]], secondArg.asInstanceOf[Value[S2]]),
        consumed + consumed2)
    }
  }

  override def serializeBody(rel: R): Array[Byte] = (rel.left, rel.right) match {
    case (Constant(left, ltpe), Constant(right, rtpe)) if ltpe == SBoolean && rtpe == SBoolean =>
      val w = Serializer.startWriter()
        .put(ConcreteCollectionBooleanConstantCode)
        .putBits(Array[Boolean](left.asInstanceOf[Boolean], right.asInstanceOf[Boolean]))
      w.toBytes
    case _ => serialize(rel.left) ++ serialize(rel.right)
  }

}
