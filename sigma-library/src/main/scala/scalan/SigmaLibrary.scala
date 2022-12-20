package scalan

import special.sigma._

trait SigmaLibrary extends Library
    with special.sigma.wrappers.WrappersModule
    with SigmaDslModule
{
  import WRType._

  implicit lazy val wRTypeAnyElement = wRTypeElement(AnyElement)

  /** During compilation represent a global value Global, see also SGlobal type.*/
  def sigmaDslBuilder: Ref[SigmaDslBuilder]
}
