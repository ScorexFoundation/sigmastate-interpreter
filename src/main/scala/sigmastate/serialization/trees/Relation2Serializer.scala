package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{ByteReader, ByteWriterSigmaValues}


case class Relation2Serializer[S1 <: SType, S2 <: SType, R <: Value[SBoolean.type]]
(override val opCode: Byte,
 constructor: (Value[S1], Value[S2]) => Value[SBoolean.type]) extends ValueSerializer[R] {

  override def serializeBody(obj: R, w: ByteWriterSigmaValues): Unit = {
    val typedRel = obj.asInstanceOf[Relation[S1, S2]]
    (typedRel.left, typedRel.right) match {
      case (Constant(left, ltpe), Constant(right, rtpe)) if ltpe == SBoolean && rtpe == SBoolean =>
        w.put(ConcreteCollectionBooleanConstantCode)
        w.putBits(Array[Boolean](left.asInstanceOf[Boolean], right.asInstanceOf[Boolean]))
      case _ =>
        w.putValue(typedRel.left)
        w.putValue(typedRel.right)
    }
  }

  override def parseBody(r: ByteReader): R = {
    if (r.peekByte() == ConcreteCollectionBooleanConstantCode) {
      val _ = r.getByte() // skip collection op code
      val booleans = r.getBits(2)
      val firstArg = BooleanConstant.fromBoolean(booleans(0)).asValue[S1]
      val secondArg = BooleanConstant.fromBoolean(booleans(1)).asValue[S2]
      constructor(firstArg, secondArg).asInstanceOf[R]
    } else {
      val firstArg = r.getValue().asValue[S1]
      val secondArg = r.getValue().asValue[S2]
      constructor(firstArg, secondArg).asInstanceOf[R]
    }
  }
}
