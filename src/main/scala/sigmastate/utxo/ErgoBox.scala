package sigmastate.utxo

import com.google.common.primitives.{Longs, Shorts}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate._
import Values._
import sigmastate.serialization.Serializer
import sigmastate.utxo.ErgoBox.{NonMandatoryIdentifier, serializer}

import scala.util.Try


object Box {
  type Amount = Long
}

class ErgoBoxCandidate ( val value: Long,
                         val proposition: Value[SBoolean.type],
                         val additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value[SType]] = Map()) {

  //TODO: val propositionBytes = ValueSerializer.serialize(proposition)
  val propositionBytes: Array[Byte] = proposition.toString.getBytes

  lazy val bytesWithNoRef: Array[Byte] = serializer.bytesWithNoRef(this)

  def toBox(txId: Digest32, boxId: Short) = ErgoBox(value, proposition, additionalRegisters, txId, boxId)
}


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
  * @param value - amount of money associated with the box
  * @param proposition guarding script, which should be evaluated to true in order to open this box
  * @param transactionId - id of transaction which created the box
  * @param boxId - number of box (from 0 to total number of boxes the transaction with transactionId created - 1)
  * @param additionalRegisters
  */
class ErgoBox private( override val value: Long,
                       override val proposition: Value[SBoolean.type],
                       val transactionId: Digest32,
                       val boxId: Short,
                       override val additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value[SType]] = Map()
                     ) extends ErgoBoxCandidate(value, proposition, additionalRegisters) {

  import sigmastate.utxo.ErgoBox._

  def get(identifier: RegisterIdentifier): Option[Value[SType]] = {
    identifier match {
      case R0 => Some(IntConstant(value))
      case R1 => Some(ByteArrayConstant(propositionBytes))
      case R2 => Some(ByteArrayConstant(transactionId ++ Shorts.toByteArray(boxId)))
      case n: NonMandatoryIdentifier => additionalRegisters.get(n)
    }
  }

  lazy val id: BoxId = ADKey @@ Blake2b256.hash(bytes)

  lazy val bytes: Array[Byte] = serializer.toBytes(this)
}

object ErgoBox {
  type BoxId = ADKey

  def apply(value: Long,
            proposition: Value[SBoolean.type],
            additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value[SType]] = Map(),
            transactionId: Digest32 = Digest32 @@ Array.fill(32)(0: Byte),
            boxId: Short = 0): ErgoBox =
    new ErgoBox(value, proposition, transactionId, boxId, additionalRegisters)

  object serializer extends Serializer[ErgoBox] {
    override def toBytes(obj: ErgoBox): Array[Byte] =
       bytesWithNoRef(obj) ++ obj.transactionId ++ Shorts.toByteArray(obj.boxId)

    //todo: serialize registers
    def bytesWithNoRef(obj: ErgoBoxCandidate): Array[Byte] = Longs.toByteArray(obj.value) ++ obj.propositionBytes

    override def parseBytes(bytes: Array[Byte]): Try[ErgoBox] = ???
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

  val allRegisters = Vector(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9)
  val registerByName = allRegisters.map(r => s"R${r.number}" -> r).toMap
}