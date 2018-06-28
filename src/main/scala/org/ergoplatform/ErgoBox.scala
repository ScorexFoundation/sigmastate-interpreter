package org.ergoplatform

import java.util.Arrays

import com.google.common.primitives.Shorts
import org.ergoplatform.ErgoBox.NonMandatoryRegisterId
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base16
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate.Values._
import sigmastate._
import sigmastate.serialization.{DataSerializer, Serializer}
import sigmastate.utxo.CostTable.Cost

import scala.runtime.ScalaRunTime

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
  * @param index         - number of box (from 0 to total number of boxes the transaction with transactionId created - 1)
  * @param additionalRegisters
  */
class ErgoBox private(
                       override val value: Long,
                       override val proposition: Value[SBoolean.type],
                       override val additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map(),
                       val transactionId: Array[Byte],
                       val index: Short
) extends ErgoBoxCandidate(value, proposition, additionalRegisters) {

  import ErgoBox._

  lazy val id: BoxId = ADKey @@ Blake2b256.hash(bytes)

  override lazy val cost = (bytesWithNoRef.size / 1024 + 1) * Cost.BoxPerKilobyte

  override def get(identifier: RegisterId): Option[Value[SType]] = {
    identifier match {
      case ReferenceRegId => Some(ByteArrayConstant(transactionId ++ Shorts.toByteArray(index)))
      case _ => super.get(identifier)
    }
  }

  lazy val bytes: Array[Byte] = {
    val w = Serializer.startWriter()
    DataSerializer.serialize[SBox.type](this, SBox, w)
    w.toBytes
  }

  override def equals(arg: Any): Boolean = arg match {
    case x: ErgoBox =>
      super.equals(x) &&
        Arrays.equals(transactionId, x.transactionId) &&
        index == x.index
    case _ => false
  }

  override def hashCode() = ScalaRunTime._hashCode((value, proposition, additionalRegisters, index))

  def toCandidate: ErgoBoxCandidate = new ErgoBoxCandidate(value, proposition, additionalRegisters)

  override def toString: Idn = s"ErgoBox(${Base16.encode(id)},$value,$proposition,${Base16.encode(transactionId)}," +
    s"$index,$additionalRegisters)"
}

object ErgoBox {
  type BoxId = ADKey
  object BoxId {
    val size: Short = 32
  }

  type Amount = Long

  def apply(value: Long,
            proposition: Value[SBoolean.type],
            additionalRegisters: Map[NonMandatoryRegisterId, _ <: EvaluatedValue[_ <: SType]] = Map(),
            transactionId: Array[Byte] = Array.fill(32)(0: Byte),
            boxId: Short = 0): ErgoBox =
    new ErgoBox(value, proposition, additionalRegisters, transactionId, boxId)

  trait RegisterId {val number: Byte}
  abstract class MandatoryRegisterId(override val number: Byte, purpose: String) extends RegisterId
  abstract class NonMandatoryRegisterId(override val number: Byte) extends RegisterId

  object R0 extends MandatoryRegisterId(0, "Monetary value, in Ergo tokens")
  object R1 extends MandatoryRegisterId(1, "Guarding script")
  object R2 extends MandatoryRegisterId(2, "Reference to transaction and output id where the box was created")
  object R3 extends NonMandatoryRegisterId(3)
  object R4 extends NonMandatoryRegisterId(4)
  object R5 extends NonMandatoryRegisterId(5)
  object R6 extends NonMandatoryRegisterId(6)
  object R7 extends NonMandatoryRegisterId(7)
  object R8 extends NonMandatoryRegisterId(8)
  object R9 extends NonMandatoryRegisterId(9)

  val ValueRegId = R0
  val ScriptRegId = R1
  val ReferenceRegId = R2

  val maxRegisters = 10
  val mandatoryRegisters = Vector(R0, R1, R2)
  val nonMandatoryRegisters = Vector(R3, R4, R5, R6, R7, R8, R9)
  val startingNonMandatoryIndex = nonMandatoryRegisters.head.number
    .ensuring(_ == mandatoryRegisters.last.number + 1)

  val allRegisters = (mandatoryRegisters ++ nonMandatoryRegisters).ensuring(_.size == maxRegisters)
  val mandatoryRegistersCount = mandatoryRegisters.size.toByte
  val nonMandatoryRegistersCount = nonMandatoryRegisters.size.toByte

  val registerByName: Map[String, RegisterId] = allRegisters.map(r => s"R${r.number}" -> r).toMap
  val registerByIndex: Map[Byte, RegisterId] = allRegisters.map(r => r.number -> r).toMap

  def findRegisterByIndex(i: Byte): Option[RegisterId] = registerByIndex.get(i)
}