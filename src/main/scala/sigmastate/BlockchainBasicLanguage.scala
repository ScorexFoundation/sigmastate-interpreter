package sigmastate

trait BlockchainBasicLanguage extends StateProposition

case class HeightFromProposition(from: Int) extends BlockchainBasicLanguage {
  override lazy val bytes: Array[Byte] = ???
}

case class HeightUntilProposition(until: Int) extends BlockchainBasicLanguage {
  override lazy val bytes: Array[Byte] = ???
}

case class HeightBetweenProposition(from: Int, until: Int) extends BlockchainBasicLanguage {
  override lazy val bytes: Array[Byte] = ???
}