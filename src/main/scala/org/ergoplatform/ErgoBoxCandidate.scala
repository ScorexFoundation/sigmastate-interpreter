package org.ergoplatform

import java.util

import org.ergoplatform.ErgoBox._
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.ModifierId
import sigmastate.Values._
import sigmastate._
import sigmastate.SType.AnyOps
import sigmastate.lang.Terms._
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{SigmaByteReader, SigmaByteWriter}
import special.collection.Coll
import sigmastate.eval._
import sigmastate.eval.Extensions._
import sigmastate.serialization.ErgoTreeSerializer.DefaultSerializer
import spire.syntax.all.cfor

import scala.collection.immutable
import scala.runtime.ScalaRunTime

/**
  * Contains the same fields as `org.ergoplatform.ErgoBox`, except if transaction id and index,
  * that will be calculated after full transaction formation.
  *
  * @see org.ergoplatform.ErgoBox for more details
  *
  * @param value               - amount of money associated with the box
  * @param ergoTree            - guarding script, which should be evaluated to true in order to open this box
  * @param creationHeight      - height when a transaction containing the box was created.
  *                            This height is declared by user and should not exceed height of the block,
  *                            containing the transaction with this box.
  * @param additionalTokens    - secondary tokens the box contains
  * @param additionalRegisters - additional registers the box can carry over
  */
class ErgoBoxCandidate(val value: Long,
                       val ergoTree: ErgoTree,
                       val creationHeight: Int,
                       val additionalTokens: Coll[(TokenId, Long)] = Colls.emptyColl,
                       val additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map()) {

  def proposition: BoolValue = ergoTree.toProposition(ergoTree.isConstantSegregation).asBoolValue

  lazy val propositionBytes: Array[Byte] = ergoTree.bytes

  lazy val bytesWithNoRef: Array[Byte] = ErgoBoxCandidate.serializer.toBytes(this)

  def toBox(txId: ModifierId, boxIndex: Short) =
    new ErgoBox(value, ergoTree, additionalTokens, additionalRegisters, txId, boxIndex, creationHeight)

  def get(identifier: RegisterId): Option[Value[SType]] = {
    identifier match {
      case ValueRegId => Some(LongConstant(value))
      case ScriptRegId => Some(ByteArrayConstant(propositionBytes))
      case TokensRegId =>
        Some(Constant(additionalTokens.map { case (id, v) => (id.toColl, v) }.asWrappedType, STokensRegType))  // TODO optimize using mapFirst
      case ReferenceRegId =>
        val tupleVal = (creationHeight, ErgoBoxCandidate.UndefinedBoxRef)
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
    ScalaRunTime._hashCode((value, ergoTree, additionalTokens, additionalRegisters, creationHeight))

  override def toString: Idn = s"ErgoBoxCandidate($value, $ergoTree," +
    s"tokens: (${additionalTokens.map(t => Base16.encode(t._1) + ":" + t._2).toArray.mkString(", ")}), " +
    s"$additionalRegisters, creationHeight: $creationHeight)"
}

object ErgoBoxCandidate {
  val UndefinedBoxRef: Coll[Byte] = Array.fill(34)(0: Byte).toColl
  object serializer extends SigmaSerializer[ErgoBoxCandidate, ErgoBoxCandidate] {

    def serializeBodyWithIndexedDigests(obj: ErgoBoxCandidate,
                                        tokensInTx: Option[Coll[TokenId]],
                                        w: SigmaByteWriter): Unit = {
      w.putULong(obj.value)
      w.putBytes(DefaultSerializer.serializeErgoTree(obj.ergoTree))
      w.putUInt(obj.creationHeight)
      w.putUByte(obj.additionalTokens.size)
      obj.additionalTokens.foreach { case (id, amount) =>
        if (tokensInTx.isDefined) {
          val tokenIndex = tokensInTx.get.indexWhere(v => util.Arrays.equals(v, id), 0)
          if (tokenIndex == -1) sys.error(s"failed to find token id ($id) in tx's digest index")
          w.putUInt(tokenIndex)
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

    override def serialize(obj: ErgoBoxCandidate, w: SigmaByteWriter): Unit = {
      serializeBodyWithIndexedDigests(obj, None, w)
    }

    /** @hotspot don't beautify the code */
    def parseBodyWithIndexedDigests(digestsInTx: Option[Coll[TokenId]], r: SigmaByteReader): ErgoBoxCandidate = {
      val previousPositionLimit = r.positionLimit
      r.positionLimit = r.position + ErgoBox.MaxBoxSize
      val value = r.getULong()                  // READ
      val tree = DefaultSerializer.deserializeErgoTree(r, SigmaSerializer.MaxPropositionSize)  // READ
      val creationHeight = r.getUInt().toInt    // READ
      val nTokens = r.getUByte()                // READ
      val tokenIds = new Array[Digest32](nTokens)
      val tokenAmounts = new Array[Long](nTokens)
      val tokenIdSize = TokenId.size
      cfor(0)(_ < nTokens, _ + 1) { i =>
        val tokenId = if (digestsInTx.isDefined) {
          val digestIndex = r.getUInt().toInt   // READ
          val digests = digestsInTx.get
          if (!digests.isDefinedAt(digestIndex)) sys.error(s"failed to find token id with index $digestIndex")
          digests(digestIndex)
        } else {
          r.getBytes(tokenIdSize)               // READ
        }
        val amount = r.getULong()               // READ
        tokenIds(i) = tokenId.asInstanceOf[Digest32]
        tokenAmounts(i) = amount
      }
      val tokens = Colls.pairCollFromArrays(tokenIds, tokenAmounts)

      // TODO optimize: hotspot: replace Map with much faster Coll
      val nRegs = r.getUByte()              // READ
      val b = immutable.Map.newBuilder[NonMandatoryRegisterId, EvaluatedValue[_ <: SType]]
      b.sizeHint(nRegs)
      cfor(0)(_ < nRegs, _ + 1) { iReg =>
        val reg = ErgoBox.nonMandatoryRegisters(iReg)
        val v = r.getValue().asInstanceOf[EvaluatedValue[SType]]  // READ
        b += ((reg, v))  // don't use `->` since it incur additional wrapper overhead
      }
      r.positionLimit = previousPositionLimit
      new ErgoBoxCandidate(value, tree, creationHeight, tokens, b.result())
    }

    override def parse(r: SigmaByteReader): ErgoBoxCandidate = {
      parseBodyWithIndexedDigests(None, r)
    }
  }

}
