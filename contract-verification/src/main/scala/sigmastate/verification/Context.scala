package sigmastate.verification

import sigmastate.verification.contract.Box
import sigmastate.verification.contract.Helpers.ContextT
import stainless.annotation.{extern, pure}
import stainless.lang._


case class Context(context: ContextT) {
  @extern @pure
  def OUTPUTS: Coll[Box] = ???

  @extern @pure
  def INPUTS: Coll[Box] = ???

  @extern @pure
  def SELF: Box = ???

  @extern @pure
  def HEIGHT: Int = ???

  @extern @pure
  def getVar[T](id: Byte): Option[T] = ???

  @extern @pure
  def longToByteArray(l: Long): Coll[Byte] = ???

  @extern @pure
  def byteArrayToLong(bytes: Coll[Byte]): Long = ???

  @extern @pure
  def blake2b256(bytes: Coll[Byte]): Coll[Byte] = ???
}
