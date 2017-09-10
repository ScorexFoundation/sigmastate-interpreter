package sigmastate.utxo

import com.google.common.primitives.Longs
import io.circe.Json
import scorex.core.serialization.Serializer
import scorex.core.transaction.BoxTransaction
import scorex.core.transaction.box.{Box, BoxUnlocker}
import scorex.core.transaction.proof.Proof
import sigmastate.SigmaStateTree

import scala.util.Try


case class SigmaStateBox(override val value: Long,
                         override val proposition: SigmaStateTree) extends Box[SigmaStateTree] {

  override lazy val id: Array[Byte] = ???

  override type M = SigmaStateBox

  //todo: implement
  override def serializer: Serializer[SigmaStateBox] = new Serializer[SigmaStateBox] {
    override def toBytes(obj: SigmaStateBox): Array[Byte] =
      Longs.toByteArray(obj.value) ++ obj.proposition.toString.getBytes

    override def parseBytes(bytes: Array[Byte]): Try[SigmaStateBox] = ???
  }
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
