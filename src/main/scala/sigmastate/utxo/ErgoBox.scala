package sigmastate.utxo

import com.google.common.primitives.{Longs, Shorts}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.{Blake2b256, Digest32}
import sigmastate._
import Values._
import sigmastate.serialization.Serializer
import sigmastate.utxo.ErgoBox.NonMandatoryIdentifier

import scala.util.Try

/**
  *
  * Box (aka coin, or an unspent output) is a basic concept of a UTXO-based cryptocurrency. In bitcoin, such an object
  * is associated with some monetary value (arbitrary, but with predefined precision, so we use integer arithmetic to
  * work with the value), guarding script (aka proposition) to protect the box from unauthorized opening.
  *
  * In other way, a box is a state element locked by some proposition.
  */
trait Box[P <: Value[SBoolean.type]] {
  val value: Box.Amount
  val proposition: P

  val id: ADKey
}

object Box {
  type Amount = Long
}

/**
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
  * @param value
  * @param proposition guarding script, which should be evaluated to true in order to open this box
  * @param additionalRegisters
  */
class ErgoBox private( override val value: Long,
                       override val proposition: Value[SBoolean.type],
                       val transactionId: Digest32,
                       val boxId: Short,
                       additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value[SType]] = Map()
                     ) extends Box[Value[SBoolean.type]] {

  import sigmastate.utxo.ErgoBox._

  def get(identifier: RegisterIdentifier): Option[Value[SType]] = {
    identifier match {
      case R0 => Some(IntConstant(value))
      case R1 => Some(ByteArrayConstant(propositionBytes))
      case R2 => Some(ByteArrayConstant(transactionId ++ Shorts.toByteArray(boxId)))
      case n: NonMandatoryIdentifier => additionalRegisters.get(n)
    }
  }

  override lazy val id: BoxId = ADKey @@ Blake2b256.hash(bytes)

  //TODO: val propositionBytes = ValueSerializer.serialize(proposition)
  val propositionBytes = proposition.toString.getBytes

  lazy val bytes = serializer.toBytes(this)

  def serializer: Serializer[ErgoBox] = new Serializer[ErgoBox] {
    //todo: serialize registers
    override def toBytes(obj: ErgoBox): Array[Byte] =
      Longs.toByteArray(obj.value) ++ obj.propositionBytes ++ transactionId ++ Shorts.toByteArray(boxId)

    override def parseBytes(bytes: Array[Byte]): Try[ErgoBox] = ???
  }
}

object ErgoBox {
  type BoxId = ADKey

  def apply(value: Long,
            proposition: Value[SBoolean.type],
            additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value[SType]] = Map(),
            transactionId: Digest32 = Digest32 @@ Array.fill(32)(0: Byte),
            boxId: Short = 0
           ): ErgoBox =
    new ErgoBox(value, proposition, transactionId, boxId, additionalRegisters)

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