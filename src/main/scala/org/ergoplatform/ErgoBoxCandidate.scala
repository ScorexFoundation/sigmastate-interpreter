package org.ergoplatform

import java.util

import org.ergoplatform.ErgoBox._
import scorex.util.encode.Base16
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigmastate.Values._
import sigmastate._
import sigmastate.SType.AnyOps
import sigmastate.lang.Terms._
import sigmastate.serialization.Serializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.CostTable.Cost
import sigmastate.utils.Extensions._

import scala.runtime.ScalaRunTime

class ErgoBoxCandidate(val value: Long,
                       val proposition: Value[SBoolean.type],
                       val creationHeight: Long,
                       val additionalTokens: Seq[(TokenId, Long)] = Seq(),
                       val additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map()) {

  lazy val cost: Int = (bytesWithNoRef.length / 1024 + 1) * Cost.BoxPerKilobyte

  val propositionBytes: Array[Byte] = proposition.bytes

  lazy val bytesWithNoRef: Array[Byte] = ErgoBoxCandidate.serializer.toBytes(this)

  def toBox(txId: ModifierId, boxId: Short) =
    ErgoBox(value, proposition, creationHeight, additionalTokens, additionalRegisters, txId, boxId)

  def get(identifier: RegisterId): Option[Value[SType]] = {
    identifier match {
      case ValueRegId => Some(LongConstant(value))
      case ScriptRegId => Some(ByteArrayConstant(propositionBytes))
      case TokensRegId =>
        val tokenTuples = additionalTokens.map { case (id, amount) =>
          Array(id, amount)
        }.toArray
        Some(Constant(tokenTuples.asWrappedType, SCollection(STokenType)))
      case ReferenceRegId =>
        val tupleVal = Array(creationHeight, Array.fill(34)(0: Byte))
        Some(Constant(tupleVal.asWrappedType, SReferenceRegType))
      case n: NonMandatoryRegisterId =>
        additionalRegisters.get(n)
    }
  }

  override def equals(arg: Any): Boolean = {
    arg match {
      case x: ErgoBoxCandidate => util.Arrays.equals(bytesWithNoRef, x.bytesWithNoRef)
      case _ => false
    }
  }

  override def hashCode(): Int =
    ScalaRunTime._hashCode((value, proposition, additionalTokens, additionalRegisters, creationHeight))

  override def toString: Idn = s"ErgoBoxCandidate($value, $proposition," +
    s"tokens: (${additionalTokens.map(t => Base16.encode(t._1)+":"+t._2).mkString(", ")}), " +
    s"$additionalRegisters, creationHeight: $creationHeight)"
}

object ErgoBoxCandidate {

  object serializer extends Serializer[ErgoBoxCandidate, ErgoBoxCandidate] {

    def serializeBodyWithIndexedDigests(obj: ErgoBoxCandidate,
                                        digestsInTx: Option[Array[Digest32]],
                                        w: SigmaByteWriter): Unit = {
      w.putULong(obj.value)
      w.putValue(obj.proposition)
      w.putULong(obj.creationHeight)
      w.putUByte(obj.additionalTokens.size)
      obj.additionalTokens.foreach { case (id, amount) =>
        if (digestsInTx.isDefined) {
          val digestIndex = digestsInTx.get.indexOf(id)
          if (digestIndex == -1) sys.error(s"failed to find token id ($id) in tx's digest index")
          w.putUInt(digestIndex)
        } else {
          w.putBytes(id)
        }
        w.putULong(amount)
      }
      val nRegs = obj.additionalRegisters.keys.size
      if (nRegs + ErgoBox.startingNonMandatoryIndex > 255)
        sys.error(s"The number of non-mandatory indexes $nRegs exceeds ${255 - ErgoBox.startingNonMandatoryIndex} limit.")
      w.putUByte(nRegs)
      // we assume non-mandatory indexes are densely packed from startingNonMandatoryIndex
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

    override def serializeBody(obj: ErgoBoxCandidate, w: SigmaByteWriter): Unit = {
      serializeBodyWithIndexedDigests(obj, None, w)
    }

    def parseBodyWithIndexedDigests(digestsInTx: Option[Array[Digest32]], r: SigmaByteReader): ErgoBoxCandidate = {
      val value = r.getULong()
      val prop = r.getValue().asBoolValue
      val creationHeight = r.getULong()
      val addTokensCount = r.getByte()
      val addTokens = (0 until addTokensCount).map { _ =>
        val tokenId = if (digestsInTx.isDefined) {
          val digestIndex = r.getUInt().toInt
          if (!digestsInTx.get.isDefinedAt(digestIndex)) sys.error(s"failed to find token id with index $digestIndex")
          digestsInTx.get.apply(digestIndex)
        } else {
          Digest32 @@ r.getBytes(TokenId.size)
        }
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
      new ErgoBoxCandidate(value, prop, creationHeight, addTokens, regs)
    }

    override def parseBody(r: SigmaByteReader): ErgoBoxCandidate = {
      parseBodyWithIndexedDigests(None, r)
    }
  }
}
