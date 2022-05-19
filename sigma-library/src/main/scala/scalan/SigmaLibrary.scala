package scalan

import special.sigma._

import scala.collection.mutable.ArrayBuffer

trait SigmaLibrary extends Library
    with special.sigma.wrappers.WrappersModule
    with SigmaDslModule
{
  import WRType._
  import Size._

  implicit lazy val wRTypeAnyElement = wRTypeElement(AnyElement)
  implicit lazy val sizeAnyElement = sizeElement(AnyElement)


  def sigmaDslBuilder: Ref[SigmaDslBuilder]


}
