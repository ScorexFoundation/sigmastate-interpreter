package org.ergoplatform

import java.util

import org.ergoplatform.ErgoBox._
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32
import sigmastate.STuple.STokenType
import sigmastate.Values._
import sigmastate._
import sigmastate.lang.Terms._
import sigmastate.serialization.Serializer
import sigmastate.utils.{ByteReader, ByteWriter}
import sigmastate.utxo.CostTable.Cost
import sigmastate.utils.Extensions._

import scala.runtime.ScalaRunTime

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

  override def equals(arg: Any): Boolean = {
    arg match {
      case x: ErgoBoxCandidate => util.Arrays.equals(bytesWithNoRef, x.bytesWithNoRef)
      case _ => false
    }
  }

  override def hashCode() = ScalaRunTime._hashCode((value, proposition, additionalTokens, additionalRegisters))


  override def toString: Idn = s"ErgoBoxCandidate($value, $proposition," +
    s"tokens: (${additionalTokens.map(t => Base16.encode(t._1)+":"+t._2).mkString(", ")}), $additionalRegisters)"
}

object ErgoBoxCandidate {

  object serializer extends Serializer[ErgoBoxCandidate, ErgoBoxCandidate] {

    override def serializeBody(obj: ErgoBoxCandidate, w: ByteWriter): Unit = {
      w.putULong(obj.value)
      w.putValue(obj.proposition)
      w.putUByte(obj.additionalTokens.size)
      obj.additionalTokens.foreach { case (id, amount) =>
        w.putBytes(id)
        w.putULong(amount)
      }
      val nRegs = obj.additionalRegisters.keys.size
      if (nRegs + ErgoBox.startingNonMandatoryIndex > 255)
        sys.error(s"The number of non-mandatory indexes $nRegs exceeds ${255 - ErgoBox.startingNonMandatoryIndex} limit.")
      w.putUByte(nRegs)
      // we assume non-mandatory indexes are densely packed from startingNonManadatoryIndex
      // this convention allows to save 1 bite for each register
      val startReg = ErgoBox.startingNonMandatoryIndex
      val endReg = ErgoBox.startingNonMandatoryIndex + nRegs - 1
      for (regId <- startReg to endReg) {
        val reg = ErgoBox.findRegisterByIndex(regId.toByte).get
        obj.get(reg) match {
          case Some(v) =>
            w.putValue(v)
          case None =>
            sys.error(s"Set of non-mandatory indexes is not densely packed: " +
              s"register R$regId is missing in the range [$startReg .. $endReg]")
        }
      }
    }

    override def parseBody(r: ByteReader): ErgoBoxCandidate = {
      val value = r.getULong()
      val prop = r.getValue().asBoolValue
      val addTokensCount = r.getByte()
      val addTokens = (0 until addTokensCount).map { _ =>
        val tokenId = Digest32 @@ r.getBytes(TokenId.size)
        val amount = r.getULong()
        tokenId -> amount
      }
      val regsCount = r.getByte()
      val regs = (0 until regsCount).map { iReg =>
        val regId = ErgoBox.startingNonMandatoryIndex + iReg
        val reg = ErgoBox.findRegisterByIndex(regId.toByte).get.asInstanceOf[NonMandatoryRegisterId]
        val v = r.getValue().asInstanceOf[EvaluatedValue[SType]]
        (reg, v)
      }.toMap
      new ErgoBoxCandidate(value, prop, addTokens, regs)
    }
  }
}
