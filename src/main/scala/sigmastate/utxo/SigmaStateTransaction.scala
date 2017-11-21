package sigmastate.utxo

import com.google.common.primitives.Longs
import io.circe.Json
import scorex.core.serialization.Serializer
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.{Box, BoxUnlocker}
import scorex.core.transaction.proof.Proof
import sigmastate._
import sigmastate.utxo.SigmaStateBox.NonMandatoryIdentifier

import scala.util.Try


case class SigmaStateBox(override val value: Long,
                         override val proposition: SigmaStateTree,
                         additionalRegisters: Map[NonMandatoryIdentifier, _ <: Value] = Map()
                        ) extends Box[SigmaStateTree] {
  import sigmastate.utxo.SigmaStateBox._

  def get(identifier: RegisterIdentifier): Option[_ <: Value] = {
    identifier match {
      case R1 => Some(IntLeafConstant(value))
      case R2 => Some(PropLeafConstant(propositionBytes))
      case n: NonMandatoryIdentifier => additionalRegisters.get(n)
    }
  }

  override lazy val id = ???

  override type M = SigmaStateBox

  //todo: implement real
  val propositionBytes = proposition.toString.getBytes

  override def serializer: Serializer[SigmaStateBox] = new Serializer[SigmaStateBox] {
    override def toBytes(obj: SigmaStateBox): Array[Byte] =
      Longs.toByteArray(obj.value) ++ obj.propositionBytes

    override def parseBytes(bytes: Array[Byte]): Try[SigmaStateBox] = ???
  }
}

object SigmaStateBox {
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

case class SigmaStateTransaction(override val unlockers: Seq[SigmaStateBoxUnlocker],
                                 override val newBoxes: Seq[SigmaStateBox])
  extends BoxTransaction[SigmaStateTree, SigmaStateBox] {

  override lazy val fee: Long = ???
  override lazy val timestamp: Long = ???

  override type M = SigmaStateTransaction

  override def serializer: Serializer[SigmaStateTransaction] = ???

  override def json: Json = ???
}