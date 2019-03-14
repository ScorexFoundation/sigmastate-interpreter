package sigmastate.serialization.trees

import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.OpCodes._
import sigmastate.serialization.ValueSerializer
import sigmastate.utils.{SerializeLog, SigmaByteReader, SigmaByteWriter}
import sigma.util.Extensions._


case class Relation2Serializer[S1 <: SType, S2 <: SType, R <: Value[SBoolean.type]]
(override val opCode: Byte,
 constructor: (Value[S1], Value[S2]) => Value[SBoolean.type]) extends ValueSerializer[R] {

  override def serializeBody(obj: R, w: SigmaByteWriter): Unit = {
    SerializeLog.logPrintf(true, true, false,"Relation2")

    val typedRel = obj.asInstanceOf[Relation[S1, S2]]

    (typedRel.left, typedRel.right) match {
      case (Constant(left, ltpe), Constant(right, rtpe)) if ltpe == SBoolean && rtpe == SBoolean =>
        SerializeLog.logPrintf(true, true, false,"BooleanConstants")

        SerializeLog.logPrintf(true, true, false,"ConcreteCollectionBooleanConstantCode")
        w.put(ConcreteCollectionBooleanConstantCode)
        SerializeLog.logPrintf(false, true, false,"ConcreteCollectionBooleanConstantCode")

        SerializeLog.logPrintf(true, true, false,"LeftRight bits")
        //andruiman: don't understand this
        w.putBits(Array[Boolean](left.asInstanceOf[Boolean], right.asInstanceOf[Boolean]))
        SerializeLog.logPrintf(false, true, false,"LeftRight bits")

        SerializeLog.logPrintf(false, true, false,"BooleanConstants")
      case _ =>
        SerializeLog.logPrintf(true, true, false,"Not BooleanConstants")

        SerializeLog.logPrintf(true, true, false,"left")
        w.putValue(typedRel.left)
        SerializeLog.logPrintf(false, true, false,"left")

        SerializeLog.logPrintf(true, true, false,"right")
        w.putValue(typedRel.right)
        SerializeLog.logPrintf(false, true, false,"right")

        SerializeLog.logPrintf(false, true, false,"Not BooleanConstants")
    }

    SerializeLog.logPrintf(false, true, false,"Relation2")
  }

  override def parseBody(r: SigmaByteReader): R = {
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
