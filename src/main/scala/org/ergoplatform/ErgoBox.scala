package org.ergoplatform

import java.util.Arrays

import com.google.common.primitives.Shorts
import org.ergoplatform.ErgoBox.{NonMandatoryIdentifier, _}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.Serializer.{Consumed, Position}
import sigmastate.serialization.{Serializer, ValueSerializer}
import sigmastate.utxo.CostTable.Cost

import scala.annotation.tailrec
import scala.runtime.ScalaRunTime
import scala.util.Try

/**
  * Box (aka coin, or an unspent output) is a basic concept of a UTXO-based cryptocurrency. In bitcoin, such an object
  * is associated with some monetary value (arbitrary, but with predefined precision, so we use integer arithmetic to
  * work with the value), guarding script (aka proposition) to protect the box from unauthorized opening.
  *
  * In other way, a box is a state element locked by some proposition.
  *
  * We add two additional fields to the box. In the first place, for carrying data along we use registers.
  * Corresponding field is called "additional registers", as we consider that amount and proposition are also stored
  * in the registers R1 and R2. In the second place, we have a "nonce" field to guarantee unique id. For a real
  * implementation, nonce should be an output of cryptographic hash function, which inputs prevents identifier collision
  * to happen, even for otherwise identical boxes. For example, a transaction could set
  * nonce = hash(n + box_index + box_input_id_1 + ... + box_input_id_n), where n is number of transaction inputs.
  *
  * A transaction is unsealing a box. As a box can not be open twice, any further valid transaction can not link to the
  * same box.
  *
  * @param value         - amount of money associated with the box
  * @param proposition   guarding script, which should be evaluated to true in order to open this box
  * @param transactionId - id of transaction which created the box
  * @param boxId         - number of box (from 0 to total number of boxes the transaction with transactionId created - 1)
  * @param additionalRegisters
  */
class ErgoBox private(override val value: Long,
                      override val proposition: Value[SBoolean.type],
                      val transactionId: Digest32,
                      val boxId: Short,
                      override val additionalRegisters: Map[NonMandatoryIdentifier, _ <: EvaluatedValue[_ <: SType]] = Map()
                     ) extends ErgoBoxCandidate(value, proposition, additionalRegisters) {

  import ErgoBox._

  lazy val id: BoxId = ADKey @@ Blake2b256.hash(bytes)

  override lazy val cost = (bytesWithNoRef.size / 1024 + 1) * Cost.BoxPerKilobyte

  override def get(identifier: RegisterIdentifier): Option[Value[SType]] = {
    identifier match {
      case R2 => Some(ByteArrayConstant(transactionId ++ Shorts.toByteArray(boxId)))
      case _ => super.get(identifier)
    }
  }

  lazy val bytes: Array[Byte] = serializer.toBytes(this)

  override def equals(arg: Any): Boolean = arg match {
    case x: ErgoBox =>
      super.equals(x) &&
        Arrays.equals(transactionId, x.transactionId) &&
        boxId == x.boxId
    case _ => false
  }

  override def hashCode() = ScalaRunTime._hashCode((value, proposition, additionalRegisters, boxId))

  def toCandidate: ErgoBoxCandidate = new ErgoBoxCandidate(value, proposition, additionalRegisters)
}

object ErgoBox {
  type BoxId = ADKey
  object BoxId {
    val size: Short = 32
  }

  type Amount = Long

  def apply(value: Long,
            proposition: Value[SBoolean.type],
            additionalRegisters: Map[NonMandatoryIdentifier, _ <: EvaluatedValue[_ <: SType]] = Map(),
            transactionId: Digest32 = Digest32 @@ Array.fill(32)(0: Byte),
            boxId: Short = 0): ErgoBox =
    new ErgoBox(value, proposition, transactionId, boxId, additionalRegisters)

  object serializer extends Serializer[ErgoBox, ErgoBox] {

    override def toBytes(obj: ErgoBox): Array[Byte] =
      ErgoBoxCandidate.serializer.toBytes(obj) ++ obj.transactionId ++ Shorts.toByteArray(obj.boxId)


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

    override def parseBytes(bytes: Array[Byte]): Try[ErgoBox] = Try {
      require(bytes.length > 42, "box(long value + proposition + txid + outid) should be 43 bytes at least")
      parseBody(bytes, 0)._1
    }

    override def parseBody(bytes: Array[Byte], pos: Position): (ErgoBox, Consumed) = {
      val (candidate, consumed) = ErgoBoxCandidate.serializer.parseBody(bytes, pos)

      val newPos = pos + consumed

      val txId = Digest32 @@ bytes.slice(newPos, newPos + 32)
      val boxId = Shorts.fromByteArray(bytes.slice(newPos + 32, newPos + 34))
      candidate.toBox(txId, boxId) -> (consumed + 34)
    }
  }


  sealed trait RegisterIdentifier {
    val number: Byte
  }

  sealed trait NonMandatoryIdentifier extends RegisterIdentifier

  object R0 extends RegisterIdentifier {
    override val number = 0
  }

  object R1 extends RegisterIdentifier {
    override val number = 1
  }

  object R2 extends RegisterIdentifier {
    override val number = 2
  }

  object R3 extends RegisterIdentifier with NonMandatoryIdentifier {
    override val number = 3
  }

  object R4 extends RegisterIdentifier with NonMandatoryIdentifier {
    override val number = 4
  }

  object R5 extends RegisterIdentifier with NonMandatoryIdentifier {
    override val number = 5
  }

  object R6 extends RegisterIdentifier with NonMandatoryIdentifier {
    override val number = 6
  }

  object R7 extends RegisterIdentifier with NonMandatoryIdentifier {
    override val number = 7
  }

  object R8 extends RegisterIdentifier with NonMandatoryIdentifier {
    override val number = 8
  }

  object R9 extends RegisterIdentifier with NonMandatoryIdentifier {
    override val number = 9
  }

  val maxRegisters = 10
  val startingNonMandatoryIndex = 3
  val nonMandatoryRegisters = Vector(R3, R4, R5, R6, R7, R8, R9).ensuring(_.head.number == startingNonMandatoryIndex)

  val allRegisters = (Vector(R0, R1, R2) ++ nonMandatoryRegisters).ensuring(_.size == maxRegisters)
  val registerByName: Map[String, RegisterIdentifier] = allRegisters.map(r => s"R${r.number}" -> r).toMap
  val registerByIndex: Map[Byte, RegisterIdentifier] = allRegisters.map(r => r.number -> r).toMap

  def findRegisterByIndex(i: Byte): Option[RegisterIdentifier] = registerByIndex.get(i)
}
