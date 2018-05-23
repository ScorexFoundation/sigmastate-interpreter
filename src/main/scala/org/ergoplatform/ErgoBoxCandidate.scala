package org.ergoplatform

import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox._
import org.ergoplatform.ErgoBox.serializer.collectRegister
import scorex.crypto.hash.Digest32
import sigmastate.{SBoolean, SType}
import sigmastate.Values.{ByteArrayConstant, EvaluatedValue, IntConstant, Value}
import sigmastate.serialization.Serializer.{Consumed, Position}
import sigmastate.serialization.{Serializer, ValueSerializer}
import sigmastate.utxo.CostTable.Cost

import scala.util.Try

class ErgoBoxCandidate(val value: Long,
                       val proposition: Value[SBoolean.type],
                       val additionalRegisters: Map[NonMandatoryIdentifier, _ <: EvaluatedValue[_ <: SType]] = Map()) {

  lazy val cost = (bytesWithNoRef.length / 1024 + 1) * Cost.BoxPerKilobyte

  val propositionBytes: Array[Byte] = proposition.bytes

  lazy val bytesWithNoRef: Array[Byte] = ErgoBoxCandidate.serializer.toBytes(this)

  def toBox(txId: Digest32, boxId: Short) = ErgoBox(value, proposition, additionalRegisters, txId, boxId)

  def get(identifier: RegisterIdentifier): Option[Value[SType]] = {
    identifier match {
      case R0 => Some(IntConstant(value))
      case R1 => Some(ByteArrayConstant(propositionBytes))
      case R2 => None
      case n: NonMandatoryIdentifier => additionalRegisters.get(n)
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
      val regNum = bytes(pos + 8 + consumed)
      val (regs, finalPos) = (0 until regNum).foldLeft(Map[NonMandatoryIdentifier, EvaluatedValue[SType]]() -> (9 + consumed)) { case ((m, p), regIdx) =>
        val regId = registerByIndex((regIdx + startingNonMandatoryIndex).toByte).asInstanceOf[NonMandatoryIdentifier]
        val (reg, consumed) = ValueSerializer.deserialize(bytes, p)
        (m.updated(regId, reg.asInstanceOf[EvaluatedValue[SType]]), p + consumed)
      }
      new ErgoBoxCandidate(value, prop.asInstanceOf[Value[SBoolean.type]], regs) -> finalPos
    }

    override def serializeBody(obj: ErgoBoxCandidate): Array[Byte] = toBytes(obj)
  }
}
