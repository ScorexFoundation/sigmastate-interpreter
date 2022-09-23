package sigmastate.crypto

import java.math.BigInteger

object CryptoFacade {
  def normalizePoint(p: Ecp): Ecp = Platform.normalizePoint(p)

  def createCryptoContext(): CryptoContext = Platform.createContext()
  def negatePoint(p: Ecp): Ecp = Platform.negatePoint(p)
  def isInfinityPoint(p: Ecp): Boolean = Platform.isInfinityPoint(p)
  def multiplyPoint(p: Ecp, n: BigInteger): Ecp = Platform.multiplyPoint(p, n)
  def addPoint(p1: Ecp, p2: Ecp): Ecp = Platform.addPoint(p1, p2)
  def showPoint(p: Ecp): String = Platform.showPoint(p)
  def testBitZeroOfFieldElem(p: ECFieldElem): Boolean = Platform.testBitZeroOfFieldElem(p)
  def getEncodedOfFieldElem(p: ECFieldElem): Array[Byte] = Platform.getEncodedOfFieldElem(p)
  def getXCoord(p: Ecp): ECFieldElem = Platform.getXCoord(p)
  def getYCoord(p: Ecp): ECFieldElem = Platform.getYCoord(p)
  def getAffineXCoord(p: Ecp): ECFieldElem = Platform.getAffineXCoord(p)
  def getAffineYCoord(p: Ecp): ECFieldElem = Platform.getAffineYCoord(p)
}
