package org.ergoplatform

import com.google.common.primitives.Shorts
import org.ergoplatform.ErgoBox.{NonMandatoryRegisterId, TokenId}
import org.ergoplatform.settings.ErgoAlgos
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util._
import sigmastate.SCollection.SByteArray
import sigmastate.SType.AnyOps
import sigmastate.Values._
import sigmastate._
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.serialization.SigmaSerializer
import sigmastate.utils.{Helpers, SigmaByteReader, SigmaByteWriter}
import sigmastate.utxo.ExtractCreationInfo
import special.collection._

import scala.runtime.ScalaRunTime

/**
  * Box (aka coin, or an unspent output) is a basic concept of a UTXO-based cryptocurrency. In Bitcoin, such an object
  * is associated with some monetary value (arbitrary, but with predefined precision, so we use integer arithmetic to
  * work with the value), and also a guarding script (aka proposition) to protect the box from unauthorized opening.
  *
  * In other way, a box is a state element locked by some proposition (ErgoTree).
  *
  * In Ergo, box is just a collection of registers, some with mandatory types and semantics, others could be used by
  * applications in any way.
  * We add additional fields in addition to amount and proposition~(which stored in the registers R0 and R1). Namely,
  * register R2 contains additional tokens (a sequence of pairs (token identifier, value)). Register R3 contains height
  * when block got included into the blockchain and also transaction identifier and box index in the transaction outputs.
  * Registers R4-R9 are free for arbitrary usage.
  *
  *
  * A transaction is unsealing a box. As a box can not be open twice, any further valid transaction can not be linked
  * to the same box.
  *
  * @param value               - amount of money associated with the box
  * @param ergoTree            - guarding script, which should be evaluated to true in order to open this box
  * @param additionalTokens    - secondary tokens the box contains
  * @param additionalRegisters - additional registers the box can carry over
  * @param transactionId       - id of transaction which created the box
  * @param index               - number of box (from 0 to total number of boxes the transaction with transactionId created - 1)
  * @param creationHeight      - height when a transaction containing the box was created.
  *                            This height is declared by user and should not exceed height of the block,
  *                            containing the transaction with this box.
  * @hotspot don't beautify the code of this class
  */
class ErgoBox(
         override val value: Long,
         override val ergoTree: ErgoTree,
         override val additionalTokens: Coll[(TokenId, Long)] = Colls.emptyColl[(TokenId, Long)],
         override val additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map.empty,
         val transactionId: ModifierId,
         val index: Short,
         override val creationHeight: Int
       ) extends ErgoBoxCandidate(value, ergoTree, creationHeight, additionalTokens, additionalRegisters) {

  import ErgoBox._

  lazy val id: BoxId = ADKey @@ Blake2b256.hash(bytes)

  override def get(identifier: RegisterId): Option[Value[SType]] = {
    identifier match {
      case ReferenceRegId =>
        val tupleVal = (creationHeight, Helpers.concatArrays(transactionId.toBytes, Shorts.toByteArray(index)).toColl)
        Some(Constant(tupleVal.asWrappedType, SReferenceRegType))
      case _ => super.get(identifier)
    }
  }

  lazy val bytes: Array[Byte] = ErgoBox.sigmaSerializer.toBytes(this)

  override def equals(arg: Any): Boolean = arg match {
    case x: ErgoBox => java.util.Arrays.equals(id, x.id)
    case _ => false
  }

  override def hashCode(): Int =
    ScalaRunTime._hashCode((value, ergoTree, additionalTokens, additionalRegisters, index, creationHeight))

  def toCandidate: ErgoBoxCandidate =
    new ErgoBoxCandidate(value, ergoTree, creationHeight, additionalTokens, additionalRegisters)

  override def toString: String = s"ErgoBox(${ErgoAlgos.encode(id)},$value,$ergoTree," +
    s"tokens: (${additionalTokens.map(t => ErgoAlgos.encode(t._1) + ":" + t._2)}), $transactionId, " +
    s"$index, $additionalRegisters, $creationHeight)"
}

object ErgoBox {
  type BoxId = ADKey
  object BoxId {
    val size: Short = 32
  }

  type TokenId = Digest32
  object TokenId {
    val size: Short = 32
  }

  val MaxBoxSize: Int = SigmaConstants.MaxBoxSize.value

  val STokenType = STuple(SByteArray, SLong)
  val STokensRegType = SCollection(STokenType)
  val SReferenceRegType: STuple = ExtractCreationInfo.ResultType

  type Amount = Long

  trait RegisterId {
    val number: Byte
    def asIndex: Int = number.toInt

    override def toString: Idn = "R" + number
  }

  abstract class MandatoryRegisterId(override val number: Byte, val purpose: String) extends RegisterId
  abstract class NonMandatoryRegisterId(override val number: Byte) extends RegisterId

  object R0 extends MandatoryRegisterId(0, "Monetary value, in Ergo tokens")
  object R1 extends MandatoryRegisterId(1, "Guarding script")
  object R2 extends MandatoryRegisterId(2, "Secondary tokens")
  object R3 extends MandatoryRegisterId(3, "Reference to transaction and output id where the box was created")
  object R4 extends NonMandatoryRegisterId(4)
  object R5 extends NonMandatoryRegisterId(5)
  object R6 extends NonMandatoryRegisterId(6)
  object R7 extends NonMandatoryRegisterId(7)
  object R8 extends NonMandatoryRegisterId(8)
  object R9 extends NonMandatoryRegisterId(9)

  val ValueRegId: MandatoryRegisterId = R0
  val ScriptRegId: MandatoryRegisterId = R1
  val TokensRegId: MandatoryRegisterId = R2
  val ReferenceRegId: MandatoryRegisterId = R3

  val MaxTokens: Int = SigmaConstants.MaxTokens.value

  val maxRegisters: Int = SigmaConstants.MaxRegisters.value

  /** @hotspot don't beautify the code in this companion */
  private val _mandatoryRegisters: Array[MandatoryRegisterId] = Array(R0, R1, R2, R3)
  val mandatoryRegisters: Seq[MandatoryRegisterId] = _mandatoryRegisters

  private val _nonMandatoryRegisters: Array[NonMandatoryRegisterId] = Array(R4, R5, R6, R7, R8, R9)
  val nonMandatoryRegisters: Seq[NonMandatoryRegisterId] = _nonMandatoryRegisters

  val startingNonMandatoryIndex: Byte = nonMandatoryRegisters.head.number
    .ensuring(_ == mandatoryRegisters.last.number + 1)

  val allRegisters: Seq[RegisterId] =
    Helpers.concatArrays[RegisterId](
      Helpers.castArray(_mandatoryRegisters): Array[RegisterId],
      Helpers.castArray(_nonMandatoryRegisters): Array[RegisterId]).ensuring(_.length == maxRegisters)

  val mandatoryRegistersCount: Byte = mandatoryRegisters.size.toByte
  val nonMandatoryRegistersCount: Byte = nonMandatoryRegisters.size.toByte

  val registerByName: Map[String, RegisterId] = allRegisters.map(r => s"R${r.number}" -> r).toMap

  /** @hotspot called from ErgoBox serializer */
  @inline final def registerByIndex(index: Int): RegisterId = allRegisters(index)

  def findRegisterByIndex(i: Int): Option[RegisterId] =
    if (0 <= i && i < maxRegisters) Some(registerByIndex(i)) else None

  val allZerosModifierId: ModifierId = Array.fill[Byte](32)(0.toByte).toModifierId

  def apply(value: Long,
            ergoTree: ErgoTree,
            creationHeight: Int,
            additionalTokens: Seq[(TokenId, Long)] = Nil,
            additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map.empty,
            transactionId: ModifierId = allZerosModifierId,
            boxIndex: Short = 0): ErgoBox =
    new ErgoBox(value, ergoTree,
      Colls.fromArray(additionalTokens.toArray[(TokenId, Long)]),
      additionalRegisters,
      transactionId, boxIndex, creationHeight)

  object sigmaSerializer extends SigmaSerializer[ErgoBox, ErgoBox] {

    override def serialize(obj: ErgoBox, w: SigmaByteWriter): Unit = {
      ErgoBoxCandidate.serializer.serialize(obj, w)
      val txIdBytes = obj.transactionId.toBytes
      val txIdBytesSize = txIdBytes.length
      assert(txIdBytesSize == ErgoLikeTransaction.TransactionIdBytesSize,
        s"Invalid transaction id size: $txIdBytesSize (expected ${ErgoLikeTransaction.TransactionIdBytesSize})")
      w.putBytes(txIdBytes)
      w.putUShort(obj.index)
    }

    override def parse(r: SigmaByteReader): ErgoBox = {
      val ergoBoxCandidate = ErgoBoxCandidate.serializer.parse(r)
      val transactionId = r.getBytes(ErgoLikeTransaction.TransactionIdBytesSize).toModifierId
      val index = r.getUShort()
      ergoBoxCandidate.toBox(transactionId, index.toShort)
    }
  }
}
