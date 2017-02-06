package   sigmastate

import io.circe.Json
import scorex.core.serialization.Serializer
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.{Box, BoxUnlocker}
import scorex.core.transaction.proof.Proof


case class SigmaStateBox(override val value: Box.Amount,
                         override val proposition: SigmaStateProposition) extends Box[SigmaStateProposition] {

  override val id: Array[Byte] = ???

  override type M = SigmaStateBox

  override def serializer: Serializer[SigmaStateBox] = ???
}

class SigmaStateBoxUnlocker extends BoxUnlocker[SigmaStateProposition] {
  override val closedBoxId: Array[Byte] = ???
  override val boxKey: Proof[SigmaStateProposition] = ???
}

class SigmaStateTransaction extends BoxTransaction[SigmaStateProposition, SigmaStateBox] {
  override val unlockers: Traversable[SigmaStateBoxUnlocker] = ???
  override val newBoxes: Traversable[SigmaStateBox] = ???
  override val fee: Long = ???
  override val timestamp: Long = ???

  override type M = SigmaStateTransaction

  override def serializer: Serializer[SigmaStateTransaction] = ???

  override def json: Json = ???
}
