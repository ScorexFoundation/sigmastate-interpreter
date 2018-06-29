package org.ergoplatform

import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox._
import sigmastate.{SBoolean, SType}
import sigmastate.Values.{ByteArrayConstant, Value, EvaluatedValue, LongConstant}
import sigmastate.serialization.Serializer.{Position, Consumed}
import sigmastate.serialization.{ValueSerializer, Serializer}
import sigmastate.utxo.CostTable.Cost

import scala.annotation.tailrec
import scala.util.Try

class ErgoBoxCandidate(val value: Long,
                       val proposition: Value[SBoolean.type],
                       val additionalTokens: Seq[(Array[Byte], Long)] = Seq(),
                       val additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map()) {

  lazy val cost = (bytesWithNoRef.length / 1024 + 1) * Cost.BoxPerKilobyte

  val propositionBytes: Array[Byte] = proposition.bytes

  lazy val bytesWithNoRef: Array[Byte] = ErgoBoxCandidate.serializer.toBytes(this)

  def toBox(txId: Array[Byte], boxId: Short) =
    ErgoBox(value, proposition, additionalTokens, additionalRegisters, txId, boxId)

  def get(identifier: RegisterId): Option[Value[SType]] = {
    identifier match {
      case ValueRegId => Some(LongConstant(value))
      case ScriptRegId => Some(ByteArrayConstant(propositionBytes))
      case ReferenceRegId => None
      case n: NonMandatoryRegisterId => additionalRegisters.get(n)
    }
  }

  override def equals(arg: Any): Boolean = arg match {
    case x: ErgoBoxCandidate =>
      value == x.value &&
        proposition == x.proposition &&
        additionalRegisters == x.additionalRegisters
    case _ => false
  }
}

object ErgoBoxCandidate {

  object serializer extends Serializer[ErgoBoxCandidate, ErgoBoxCandidate] {
    @tailrec
    def collectRegister(obj: ErgoBoxCandidate,
        collectedBytes: Array[Byte],
        collectedRegisters: Byte): (Array[Byte], Byte) = {
      val regIdx = (startingNonMandatoryIndex + collectedRegisters).toByte
      val regByIdOpt = registerByIndex.get(regIdx)
      regByIdOpt.flatMap(obj.get) match {
        case Some(v) =>
          collectRegister(obj, collectedBytes ++ ValueSerializer.serialize(v), (collectedRegisters + 1).toByte)
        case None =>
          (collectedBytes, collectedRegisters)
      }
    }

    override def toBytes(obj: ErgoBoxCandidate): Array[Byte] = {
      val propBytes = obj.propositionBytes
      val (regBytes, regNum) = collectRegister(obj, Array.emptyByteArray, 0: Byte)

      Longs.toByteArray(obj.value) ++ propBytes ++ (regNum +: regBytes)
    }

    override def parseBytes(bytes: Array[Byte]): Try[ErgoBoxCandidate] = Try {
      parseBody(bytes, 0)._1
    }

    override def parseBody(bytes: Array[Byte], pos: Position): (ErgoBoxCandidate, Consumed) = {
      val value = Longs.fromByteArray(bytes.slice(pos, pos + 8))
      val (prop, consumed) = ValueSerializer.deserialize(bytes, pos + 8)
      val posAfterProp = pos + 8 + consumed
      val regNum = bytes(posAfterProp)
      val posAfterRegNum = posAfterProp + 1
      val (regs, finalPos) = (0 until regNum).foldLeft(Map[NonMandatoryRegisterId, EvaluatedValue[SType]]() -> posAfterRegNum) { case ((m, p), regIdx) =>
        val regId = registerByIndex((regIdx + startingNonMandatoryIndex).toByte).asInstanceOf[NonMandatoryRegisterId]
        val (reg, consumed) = ValueSerializer.deserialize(bytes, p)
        (m.updated(regId, reg.asInstanceOf[EvaluatedValue[SType]]), p + consumed)
      }
      val finalConsumed = finalPos - pos
      new ErgoBoxCandidate(value, prop.asInstanceOf[Value[SBoolean.type]], Seq(), regs) -> finalConsumed
    }

    override def serializeBody(obj: ErgoBoxCandidate): Array[Byte] = toBytes(obj)
  }
}
