package sigmastate.js

import scalan.RType

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * Runtime representation of ErgoScript types. Type is a JS friendly
  * wrapper around {@link RType} type descriptor.
  */
@JSExportTopLevel("Type", moduleID = "common")
class Type(private[js] final val rtype: RType[_]) extends js.Object {
  /** Syntactically correct type name (type expression as String) */
  def name: String = rtype.name

  override def toString = s"Type($rtype)"
}

@JSExportTopLevel("Types", moduleID = "common")
object Type extends js.Object {
  /** Descriptor of ErgoScript type Byte. */
  val Byte = new Type(RType.ByteType)

  /** Descriptor of ErgoScript type Short. */
  val Short = new Type(RType.ShortType)

  /** Descriptor of ErgoScript type Int. */
  val Int = new Type(RType.IntType)

  /** Descriptor of ErgoScript type Long. */
  val Long = new Type(RType.LongType)

  /** Constructs a new descriptor of ErgoScript pair type (l, r).
    * @param l first component of the pair
    * @param r second component of the pair
    */
  def pairType(l: Type, r: Type): Type = {
    new Type(RType.pairRType(l.rtype, r.rtype))
  }

  /** Constructs a new descriptor of ErgoScript collection type `Coll[elemType]`.
    *
    * @param elemType type descriptor of collection elements
    */
  def collType(elemType: Type): Type = {
    new Type(special.collection.collRType(elemType.rtype))
  }
}