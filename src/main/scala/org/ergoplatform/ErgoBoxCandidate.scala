package org.ergoplatform

import com.google.common.primitives.Longs
import org.ergoplatform.ErgoBox._
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32
import sigmastate._
import sigmastate.Values._
import sigmastate.serialization.Serializer.{Consumed, Position}
import sigmastate.serialization.{Serializer, ValueSerializer}
import sigmastate.utxo.CostTable.Cost
import STuple.STokenType

import scala.annotation.tailrec
import scala.runtime.ScalaRunTime
import scala.util.Try

class ErgoBoxCandidate(val value: Long,
                       val proposition: Value[SBoolean.type],
                       val additionalTokens: Seq[(TokenId, Long)] = Seq(),
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
      case TokensRegId =>
        val tokenTuples = additionalTokens.map { case (id, amount) =>
          Tuple(ByteArrayConstant(id), LongConstant(amount))
        }.toIndexedSeq
        Some(ConcreteCollection(tokenTuples, STokenType))
      case ReferenceRegId => None
      case n: NonMandatoryRegisterId => additionalRegisters.get(n)
    }
  }

  override def equals(arg: Any): Boolean = arg match {
    case x: ErgoBoxCandidate => Array.equals(bytesWithNoRef, x.bytesWithNoRef)
    case _ => false
  }

  override def hashCode() = ScalaRunTime._hashCode((value, proposition, additionalTokens, additionalRegisters))


  override def toString: Idn = s"ErgoBoxCandidate($value, $proposition," +
    s"tokens: (${additionalTokens.map(t => Base16.encode(t._1)+":"+t._2).mkString(", ")}), $additionalRegisters)"
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

      val tokensCount = obj.additionalTokens.size.toByte
      val tokenBytes = if (obj.additionalTokens.nonEmpty) {
        obj.additionalTokens.map { case (id, amount) =>
          id ++ Longs.toByteArray(amount)
        }.reduce(_ ++ _)
      } else {
        Array.emptyByteArray
      }

      Longs.toByteArray(obj.value) ++ propBytes ++ (tokensCount +: tokenBytes) ++ (regNum +: regBytes)
    }

    override def parseBytes(bytes: Array[Byte]): Try[ErgoBoxCandidate] = Try {
      parseBody(bytes, 0)._1
    }

    override def parseBody(bytes: Array[Byte], pos: Position): (ErgoBoxCandidate, Consumed) = {
      var curPos = pos

      val value = Longs.fromByteArray(bytes.slice(curPos, curPos + 8))
      curPos += 8

      val (prop, consumed) = ValueSerializer.deserialize(bytes, curPos)
      curPos += consumed

      val tokensNum = bytes(curPos)
      curPos += 1

      val additionalTokens = (1 to tokensNum).map{_ =>
        val id = Digest32 @@ (bytes.slice(curPos, curPos + 32))
        val amount = Longs.fromByteArray(bytes.slice(curPos + 32, curPos + 40))
        curPos += 40
        id -> amount
      }

      val regNum = bytes(curPos)
      curPos += 1

      val (regs, finalPos) = (0 until regNum).foldLeft(Map[NonMandatoryRegisterId, EvaluatedValue[SType]]() -> curPos) { case ((m, p), regIdx) =>
        val regId = registerByIndex((regIdx + startingNonMandatoryIndex).toByte).asInstanceOf[NonMandatoryRegisterId]
        val (reg, consumed) = ValueSerializer.deserialize(bytes, p)
        (m.updated(regId, reg.asInstanceOf[EvaluatedValue[SType]]), p + consumed)
      }
      val finalConsumed = finalPos - pos
      new ErgoBoxCandidate(value, prop.asInstanceOf[Value[SBoolean.type]], additionalTokens, regs) -> finalConsumed
    }

    override def serializeBody(obj: ErgoBoxCandidate): Array[Byte] = toBytes(obj)
  }
}
