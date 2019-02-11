package special.sigma

import special.collection.{Coll, _}
import scalan.{Reified, RType}
import scalan.RType

trait CostedSigmaObject[Val] extends Costed[Val] {
  def dsl: SigmaDslBuilder
  def builder: CostedBuilder = dsl.Costing
}

trait CostedContext extends CostedSigmaObject[Context] {
  def OUTPUTS: CostedColl[Box]
  def INPUTS: CostedColl[Box]
  def HEIGHT: Costed[Int]
  def SELF: CostedBox
  def LastBlockUtxoRootHash: CostedAvlTree
  def MinerPubKey: CostedColl[Byte]
  def getVar[T](id: Byte)(implicit cT: RType[T]): CostedOption[T]
  def getConstant[T](id: Byte)(implicit cT: RType[T]): Costed[T]
}

trait CostedBox extends CostedSigmaObject[Box] {
  def id: CostedColl[Byte]
  def valueCosted: Costed[Long]
  def bytes: CostedColl[Byte]
  def bytesWithoutRef: CostedColl[Byte]
  def propositionBytes: CostedColl[Byte]
  def registers: CostedColl[AnyValue]
  def getReg[@Reified T](id: Int)(implicit cT:RType[T]): CostedOption[T]
  def creationInfo: Costed[(Int, Coll[Byte])]
}

trait CostedAvlTree extends CostedSigmaObject[AvlTree] {
  def startingDigest: CostedColl[Byte]
  def keyLength: Costed[Int]
  def treeFlags: Costed[TreeFlags]
  def valueLengthOpt: CostedOption[Int]
}
