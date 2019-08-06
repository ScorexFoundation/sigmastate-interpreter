package scalan

import special.sigma._

import scala.collection.mutable.ArrayBuffer

trait SigmaLibrary extends Library
    with special.sigma.wrappers.WrappersModule
    with SigmaDslModule
    with CostedObjectsModule
    with SigmaDslCostedModule
{
  import Coll._
  import CollBuilder._
  import SigmaProp._
  import SigmaContract._
  import SigmaDslBuilder._
  import WRType._
  import Size._

  implicit lazy val wRTypeAnyElement = wRTypeElement(AnyElement)
  implicit lazy val sizeAnyElement = sizeElement(AnyElement)


  def sigmaDslBuilder: Ref[SigmaDslBuilder]


}
