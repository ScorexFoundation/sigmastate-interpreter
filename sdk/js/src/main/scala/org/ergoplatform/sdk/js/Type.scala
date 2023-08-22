package org.ergoplatform.sdk.js

import scalan.RType

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

/**
  * Runtime representation of ErgoScript types. Type is a JS friendly
  * wrapper around {@link RType} type descriptor.
  */
@JSExportTopLevel("Type")
class Type(private[js] final val rtype: RType[_]) extends js.Object {
  /** Syntactically correct type name (type expression as String) */
  def name: String = rtype.name

  override def toString = s"Type($rtype)"
}

@JSExportTopLevel("TypeObj")
object Type extends js.Object {
  /** Descriptor of ErgoScript type Byte. */
  val Byte = new Type(RType.ByteType)

  /** Descriptor of ErgoScript type Short. */
  val Short = new Type(RType.ShortType)

  /** Descriptor of ErgoScript type Int. */
  val Int = new Type(RType.IntType)

  /** Descriptor of ErgoScript type Long. */
  val Long = new Type(RType.LongType)

  /** Descriptor of ErgoScript type BigInt. */
  val BigInt = new Type(sigma.BigIntRType)

  /** Descriptor of ErgoScript type GroupElement. */
  val GroupElement = new Type(sigma.GroupElementRType)

  /** Descriptor of ErgoScript type SigmaProp. */
  val SigmaProp = new Type(sigma.SigmaPropRType)

  /** Descriptor of ErgoScript type Box. */
  val Box = new Type(sigma.BoxRType)

  /** Descriptor of ErgoScript type AvlTree. */
  val AvlTree = new Type(sigma.AvlTreeRType)

  /** Descriptor of ErgoScript type Context. */
  val Context = new Type(sigma.ContextRType)

  /** Descriptor of ErgoScript type Header. */
  val Header = new Type(sigma.HeaderRType)

  /** Descriptor of ErgoScript type PreHeader. */
  val PreHeader = new Type(sigma.PreHeaderRType)

  /** Descriptor of ErgoScript type Global.
    * @see SigmaDslBuilder, SGlobal
    */
  val SigmaDslBuilder = new Type(sigma.SigmaDslBuilderRType)

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
    new Type(sigma.collection.collRType(elemType.rtype))
  }
}