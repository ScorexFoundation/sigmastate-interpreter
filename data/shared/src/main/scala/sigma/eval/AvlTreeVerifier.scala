package sigma.eval

import scala.util.Try

trait AvlTreeVerifier {
  def treeHeight: Int
  def performLookup(key: Array[Byte]): Try[Option[Array[Byte]]]
  def performInsert(key: Array[Byte], value: Array[Byte]): Try[Option[Array[Byte]]]
  def performUpdate(key: Array[Byte], value: Array[Byte]): Try[Option[Array[Byte]]]
  def performRemove(key: Array[Byte]): Try[Option[Array[Byte]]]
  def digest: Option[Array[Byte]]
}
