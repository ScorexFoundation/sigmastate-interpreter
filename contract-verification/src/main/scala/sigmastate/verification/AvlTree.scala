package sigmastate.verification

import sigmastate.verification.SigmaDsl.api.Coll
import sigmastate.verification.contract.Helpers.AvlTreeT
import stainless.annotation.extern
import stainless.lang.Option

case class AvlTree(avlTree: AvlTreeT) {
  def digest: Coll[Byte] = ???

  def keyLength: Int = ???

  def valueLengthOpt: Option[Int] = ???

  def enabledOperations: Byte = ???

  def getMany(keys: Coll[Coll[Byte]], proof: Coll[Byte]): Coll[Option[Coll[Byte]]] = ???

  def insert(toAdd: Coll[(Coll[Byte], Coll[Byte])], proof: Coll[Byte]): Option[AvlTree] = ???

  def remove(operations: Coll[Coll[Byte]], proof: Coll[Byte]): Option[AvlTree] = ???
}


