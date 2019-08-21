package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.serialization.ValueSerializer._
import sigma.util.Extensions._

case class Relation2Serializer[S1 <: SType, S2 <: SType, R <: Value[SBoolean.type]]
(override val opDesc: RelationCompanion,
 constructor: (Value[S1], Value[S2]) => Value[SBoolean.type]) extends ValueSerializer[R] {
  import SigmaByteWriter._

  override def serialize(obj: R, w: SigmaByteWriter): Unit = {
    val typedRel = obj.asInstanceOf[Relation[S1, S2]]
    cases("(left, right)") {
      (typedRel.left, typedRel.right) match {
        case (Constant(left, lTpe), Constant(right, rTpe)) if lTpe == SBoolean && rTpe == SBoolean =>
          when(1, "(Constant(l, Boolean), Constant(r, Boolean))") {
            w.put(ConcreteCollectionBooleanConstantCode, ArgInfo("opCode", s"always contains OpCode ${ConcreteCollectionBooleanConstantCode.toUByte}"))
            w.putBits(
              Array[Boolean](left.asInstanceOf[Boolean], right.asInstanceOf[Boolean]),
              maxBitsInfo("(l,r)", 2, "two higher bits in a byte"))
          }
        case _ =>
          otherwise {
            w.putValue(typedRel.left, opDesc.argInfos(0))
            w.putValue(typedRel.right, opDesc.argInfos(1))
          }
      }
    }
  }

  /** @hotspot don't beautify this code */
  override def parse(r: SigmaByteReader): R = {
    if (r.peekByte() == ConcreteCollectionBooleanConstantCode) {
      val _ = r.getByte() // skip collection op code
      val booleans = r.getBits(2)
      val firstArg = BooleanConstant.fromBoolean(booleans(0)).asInstanceOf[Value[S1]]
      val secondArg = BooleanConstant.fromBoolean(booleans(1)).asInstanceOf[Value[S2]]
      constructor(firstArg, secondArg).asInstanceOf[R]
    } else {
      val firstArg = r.getValue().asInstanceOf[Value[S1]]
      val secondArg = r.getValue().asInstanceOf[Value[S2]]
      constructor(firstArg, secondArg).asInstanceOf[R]
    }
  }
}
