package sigmastate.js

import scalan.RType

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Type", moduleID = "common")
class Type(private[js] final val rtype: RType[_]) extends js.Object {
  def name: String = rtype.name
}

@JSExportTopLevel("Types", moduleID = "common")
object Type extends js.Object {
  val Byte = new Type(RType.ByteType)
  val Short = new Type(RType.ShortType)
  val Int = new Type(RType.IntType)
  val Long = new Type(RType.LongType)

  def pairType(l: Type, r: Type): Type = {
    new Type(RType.pairRType(l.rtype, r.rtype))
  }

  def collType(elemType: Type): Type = {
    new Type(special.collection.collRType(elemType.rtype))
  }
}