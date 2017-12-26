package sigmastate.utxo

import com.google.common.primitives.Longs
import io.circe.Json
import scorex.core.serialization.Serializer
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.{Box, BoxUnlocker}
import scorex.core.transaction.proof.Proof
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256
import sigmastate._
import sigmastate.utxo.SigmaStateBox.NonMandatoryIdentifier

import scala.util.Try

/**
  * Box (aka coin, or an unspent output) is a basic concept of a UTXO-based cryptocurrency. In bitcoin, such an object
  * is associated with some monetary value (arbitrary, but with predefined precision, so we use integer arithmetic to
  * work with the value), guarding script (aka proposition) to protect the box from unauthorized opening.
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
  * @param value
  * @param proposition
  * @param additionalRegisters
  * @param nonce
  */
class SigmaStateBox(override val value: Long,
                         override val proposition: SigmaStateTree,
                         additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value[_]] = Map(),
                         nonce: Array[Byte] = Array.fill(32)(0:Byte)) extends Box[SigmaStateTree] {
  import sigmastate.utxo.SigmaStateBox._

  def get(identifier: RegisterIdentifier): Option[_ <: Value[_]] = {
    identifier match {
      case R1 => Some(IntConstant(value))
      case R2 => Some(PropConstant(propositionBytes))
      case n: NonMandatoryIdentifier => additionalRegisters.get(n)
    }
  }

  override lazy val id = ADKey @@ Blake2b256.hash(bytes)

  override type M = SigmaStateBox

  //todo: real implementation
  val propositionBytes = proposition.toString.getBytes

  override def serializer: Serializer[SigmaStateBox] = new Serializer[SigmaStateBox] {

    //todo: serialize registers
    override def toBytes(obj: SigmaStateBox): Array[Byte] =
      Longs.toByteArray(obj.value) ++ obj.propositionBytes ++ nonce

    override def parseBytes(bytes: Array[Byte]): Try[SigmaStateBox] = ???
  }
}

object SigmaStateBox {
  def apply(value: Long,
            proposition: SigmaStateTree,
            additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value[_]] = Map(),
            nonce: Array[Byte] = Array.fill(32)(0: Byte)
           ): SigmaStateBox =
    new SigmaStateBox(value, proposition, additionalRegisters, nonce)

  sealed trait RegisterIdentifier
  sealed trait NonMandatoryIdentifier extends RegisterIdentifier

  object R1 extends RegisterIdentifier
  object R2 extends RegisterIdentifier
  object R3 extends RegisterIdentifier with NonMandatoryIdentifier
  object R4 extends RegisterIdentifier with NonMandatoryIdentifier
  object R5 extends RegisterIdentifier with NonMandatoryIdentifier
  object R6 extends RegisterIdentifier with NonMandatoryIdentifier
  object R7 extends RegisterIdentifier with NonMandatoryIdentifier
  object R8 extends RegisterIdentifier with NonMandatoryIdentifier
  object R9 extends RegisterIdentifier with NonMandatoryIdentifier
  object R10 extends RegisterIdentifier with NonMandatoryIdentifier
}


class SigmaStateBoxUnlocker extends BoxUnlocker[SigmaStateTree] {
  override val closedBoxId: Array[Byte] = ???
  override val boxKey: Proof[SigmaStateTree] = ???
}

case class SigmaStateTransaction(override val unlockers: IndexedSeq[SigmaStateBoxUnlocker],
                                 override val newBoxes: IndexedSeq[SigmaStateBox])
  extends BoxTransaction[SigmaStateTree, SigmaStateBox] {

  override lazy val fee: Long = ???
  override lazy val timestamp: Long = ???

  override type M = SigmaStateTransaction

  override def serializer: Serializer[SigmaStateTransaction] = ???

  override def json: Json = ???
}