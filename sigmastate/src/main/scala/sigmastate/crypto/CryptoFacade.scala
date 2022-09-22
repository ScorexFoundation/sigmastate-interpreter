package sigmastate.crypto

object CryptoFacade {
  def createCryptoContext(): CryptoContext = Platform.createContext()
}
