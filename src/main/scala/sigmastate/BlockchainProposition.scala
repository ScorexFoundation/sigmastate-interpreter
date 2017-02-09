package sigmastate

import BlockchainProposition._

trait BlockchainProposition extends StateProposition

object BlockchainProposition {
  type Height = Int
}

case class HeightFromProposition(from: Height) extends BlockchainProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class HeightUntilProposition(until: Height) extends BlockchainProposition {
  override lazy val bytes: Array[Byte] = ???
}

case class HeightBetweenProposition(from: Height, until: Height) extends BlockchainProposition {
  override lazy val bytes: Array[Byte] = ???
}