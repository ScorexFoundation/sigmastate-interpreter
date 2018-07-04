package sigmastate.serialization.trees

import javax.swing.SpringLayout.Constraints
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.Serializer.Position
import sigmastate.serialization.{Serializer, ValueSerializer}
import sigmastate.serialization.{Constraints, ValueSerializer}
import sigmastate.utils.{ByteReader, ByteWriter}

case class Relation2Serializer[S1 <: SType, S2 <: SType, R <: Value[SBoolean.type]]
(override val opCode: Byte,
 constructor: (Value[S1], Value[S2]) => Value[SBoolean.type]) extends ValueSerializer[R] {

  override def parseBody(r: ByteReader): R = {
    if (r.peekByte() == ConcreteCollectionBooleanConstantCode) {
      val _ = r.getByte() // skip collection op code
      val booleans = r.getBits(2)
      val firstArg = BooleanConstant.fromBoolean(booleans(0)).asValue[S1]
      val secondArg = BooleanConstant.fromBoolean(booleans(1)).asValue[S2]
      constructor(firstArg, secondArg)
      (constructor(firstArg, secondArg).asInstanceOf[R], r.consumed)
    } else {
      val firstArg = r.getValue().asValue[S1]
      val secondArg = r.getValue().asValue[S2]
      (constructor(firstArg, secondArg).asInstanceOf[R], r.consumed)
    }
  }

  override def serializeBody(obj: R, w: ByteWriter): Unit = (obj.left, obj.right) match {
      case (Constant(left, ltpe), Constant(right, rtpe)) if ltpe == SBoolean && rtpe == SBoolean =>
        w.put(ConcreteCollectionBooleanConstantCode)
        w.putBits(Array[Boolean](left.asInstanceOf[Boolean], right.asInstanceOf[Boolean]))
      case _ =>
        w.putValue(obj.left)
        w.putValue(obj.right)
    }
}
